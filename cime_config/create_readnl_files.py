#!/usr/bin/env python3

"""
For each XML file in a list of XML namelist definition files, create
the appropriate read_namelist module and associated metadata.
Also, create a master module to execute these namelist read functions.

To run doctests on this file: python create_readnl_files.py
"""

# Python library imports
import os
import os.path
import re
import argparse
import sys
import logging

# Find and include the ccpp-framework scripts directory
# Assume we are in <CAMROOT>/cime_config and SPIN is in <CAMROOT>/ccpp_framework
_CURRDIR = os.path.abspath(os.path.dirname(__file__))
_CAMROOT = os.path.abspath(os.path.join(_CURRDIR, os.pardir))
_SPINSCRIPTS = os.path.join(_CAMROOT, "ccpp_framework", 'scripts')
_XML_SCHEMAS = os.path.join(_CAMROOT, "cime", "CIME", "data", "config",
                            "xml_schemas")
_PG_SCHEMAS = os.path.join(_CAMROOT, "cime", "CIME", "ParamGen",
                           "xml_schema")
if _SPINSCRIPTS not in sys.path:
    sys.path.append(_SPINSCRIPTS)
# end if

# CCPP framework imports
# pylint: disable=wrong-import-position
from parse_tools import validate_xml_file, read_xml_file
from parse_tools import init_log, CCPPError, ParseInternalError
from fortran_tools import FortranWriter
# pylint: enable=wrong-import-position

###############################################################################
def is_int(token):
###############################################################################
    """Return True if <token> is an integer.
       <token> should be a string or None.

    1. Test that a good string returns True
    >>> is_int("33")
    True

    2. Test that None returns False
    >>> is_int(None)
    False

    3. Test that a non-integer string returns False
    >>> is_int("hi mom")
    False
    >>> is_int("3.14159")
    False

    4. Test that bad input is caught
    >>> is_int(3.14159) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    parse_source.ParseInternalError: <token> should be a string or None, not '<class 'float'>'
    """
    isint = False
    if isinstance(token, str):
        try:
            ival = int(token)
            isint = True
        except ValueError:
            isint = False
        # end try
    elif token is not None:
        emsg = f"<token> should be a string or None, not '{type(token)}'"
        raise ParseInternalError(emsg)
    # end if
    return isint

###############################################################################
def write_ccpp_table_header(name, outfile, indent):
###############################################################################
    """Write the standard Fortran comment block for a CCPP header
    (module, type, scheme)."""
    outfile.write(r"!> \section arg_table_{}  Argument Table".format(name),
                  indent)
    outfile.write(r"!! \htmlinclude {}.html".format(name), indent)

###############################################################################
class NamelistError(ValueError):
###############################################################################
    """Class to indicate errors processing namelist files"""
    def __init__(self, message):
        """Initialize this exception"""
        logging.shutdown()
        super().__init__(message)

###############################################################################
class MpiModuleInfo:
###############################################################################
    """Class to hold information about MPI module version to use as
       well as the names of host model MPI variables such as communicator.
    """

    def __init__(self, arg_mpi_comm, arg_mpi_root,
                 arg_mpi_is_root, arg_mpi_mod):
        """Initialize this MpiModuleObject"""
        self.__mpicomm_arg = arg_mpi_comm
        self.__mpiroot_arg = arg_mpi_root
        self.__mpiisroot_arg = arg_mpi_is_root
        self.__mpi_module = arg_mpi_mod

    def mpi_datatype(self):
        """Return the type of the MPI datatypes (e.g., MPI_Integer)"""
        if self.mpi_module == "mpi_f08":
            return "type(MPI_Datatype)"
        if self.mpi_module == "mpi":
            return "integer"
        # end if
        raise ValueError(f"Unknown MPI module version, '{self.mpi_module}'")

    def mpi_commtype(self):
        """Return the type of an MPI communicator"""
        if self.mpi_module == "mpi_f08":
            return "type(MPI_Comm)"
        if self.mpi_module == "mpi":
            return "integer"
        # end if
        raise ValueError(f"Unknown MPI module version, '{self.mpi_module}'")

    @property
    def mpi_module(self):
        """Return the MPI module to use"""
        return self.__mpi_module

    @property
    def mpi_comm_arg(self):
        """Return the dummy argument name for the MPI communicator to use
           when broadcasting namelist variable values.
        """
        return self.__mpicomm_arg

    @property
    def mpi_root_arg(self):
        """Return the dummy argument name for the MPI root task to use
           when broadcasting namelist variable values.
        """
        return self.__mpiroot_arg

    @property
    def mpi_is_root_arg(self):
        """Return the dummy argument name for the logical for determing if
           this is the task to use for reading the namelist and logging
           the values.
        """
        return self.__mpiisroot_arg

###############################################################################
class NLVar:
###############################################################################
    """Class to hold information about a namelist variable entry"""

    # For unique dimension names
    __new_dim_suffix = {}

    # For validity error messages
    __kind_errstr = "Conflicting kind arguments, '{}' and '{}' for '{}'"
    __kind_badkind = "Illegal kind, '{}', for type, '{}' of '{}'"
    __char_nolen = "Bad 'char' type for '{}', must specify length"
    __nl_types = ["integer", "logical", "real", "character"]
    __mpi_types = {"integer" : "MPI_Integer", "logical" : "MPI_Logical",
                   "real" : "MPI_Real8", "character" : "MPI_Character"}

    # Parse any valid XML <type> text
    __tspec = r"[ ]*([a-z]+)[ ]*"
    __clenspec = r"(?:[*][ ]*([0-9]+))?[ ]*"
    __aspec = r"[ ]*(?:[(][ ]*([0-9 ,]+)[ ]*[)])?"
    __type_re = re.compile(__tspec + __clenspec + __aspec)

    # Formatting parameters
    __type_strlen = 20 # Crude but no loop through variables necessary

    def __init__(self, var_xml):
        """Collect namelist variable information from <var_xml> element"""
        self.__var_name = var_xml.get("id")
        self.__type = None
        self.__group = None
        self.__standard_name = None
        self.__long_name = None
        self.__units = None
        self.__kind = None
        self.__array_lengths = ()
        self.__array_names = ()
        self.__valid = True # speculation to be confirmed
        self.__kind_err = ""
        var_kind = None
        var_type = None
        for element in var_xml:
            elem_type = element.tag
            if elem_type == "type":
                var_type = element.text
            elif elem_type == "group":
                self.__group = element.text
            elif elem_type == "standard_name":
                self.__standard_name = element.text
            elif elem_type == "long_name":
                self.__long_name = element.text
            elif elem_type == "units":
                self.__units = element.text
            elif elem_type == "kind":
                var_kind = element.text
                if self.kind:
                    # Someone already set the kind, probably a char*nnn type
                    self.__valid = False
                    self.__kind_err = self.__kind_errstr.format(self.__kind,
                                                                element.text)
                else:
                    self.__kind = element.text
                # end if
            # end if (ignore unused tag types)
        # end for
        if not var_type:
            # Raise an exception because this XML should not have validated
            raise ParseInternalError(f"ERROR: No type for {self.var_name}")
        # end if
        # Process the type and (optional) kind arguments
        typ, knd, alen = self._parse_xml_type(var_type, self.var_name)
        if typ == 'ERROR':
            self.__kind_err = knd
            self.__valid = False
        elif typ == "character":
            if var_kind:
                self.__kind = f"{knd},kind={var_kind}"
            else:
                self.__kind = knd
            # end if
        else:
            self.__kind = knd
        # end if
        self.__type = typ
        args = self._parse_array_desc(alen, self.var_name)
        self.__array_lengths, self.__array_names = args
        if self.var_type == "character":
            # character arguments at least require a length
            self.__valid &= self.kind is not None
            self.__kind_err = "No length argument for character type"
        # end if
        # Trap any errors
        # Default kind for real variables
        if (not self.kind) and (self.var_type == "real"):
            self.__kind = "kind_phys"
        # end if
        # Final validity check
        self.__valid &= ((self.group is not None) and
                         (self.standard_name is not None) and
                         (self.units is not None))
        self.__valid &= self.var_type in self.__nl_types

    @classmethod
    def _parse_xml_type(cls, type_str, name):
        """Parse a namelist XML type description, <type_str>, and return
           the Fortran type, a character length argument (if the type is
           char, otherwise None), and an array length (or none).
        If an error is detected, set the first return value to "ERROR" and
        the second return value to an error string.

        # Good doctest examples
        >>> NLVar._parse_xml_type("integer", "foo")
        ('integer', None, None)
        >>> NLVar._parse_xml_type("logical", "foo")
        ('logical', None, None)
        >>> NLVar._parse_xml_type("real", "foo")
        ('real', None, None)
        >>> NLVar._parse_xml_type("char*256", "foo")
        ('character', 'len=256', None)
        >>> NLVar._parse_xml_type("integer(10)", "foo")
        ('integer', None, '10')
        >>> NLVar._parse_xml_type("logical(6)", "foo")
        ('logical', None, '6')
        >>> NLVar._parse_xml_type("real(2,2)", "foo")
        ('real', None, '2,2')
        >>> NLVar._parse_xml_type("char*256(19)", "foo")
        ('character', 'len=256', '19')
        >>> NLVar._parse_xml_type("char*256(3,7)", "foo")
        ('character', 'len=256', '3,7')

        # Bad doctest examples
        >>> NLVar._parse_xml_type("char", "foo")
        ('ERROR', "Bad 'char' type for 'foo', must specify length", None)
        >>> NLVar._parse_xml_type("integer*256", "foo")
        ('ERROR', "Illegal kind, '256', for type, 'integer' of 'foo'", None)
        >>> NLVar._parse_xml_type("real*256", "foo")
        ('ERROR', "Illegal kind, '256', for type, 'real' of 'foo'", None)
        >>> NLVar._parse_xml_type("char*len", "foo")
        ('ERROR', "Bad 'char' type for 'foo', must specify length", None)
        """
        var_type = None
        kind = None
        array_len = None
        match = cls.__type_re.match(type_str)
        if match:
            var_type = match.group(1)
            if var_type == 'char':
                var_type = 'character'
            # end if
            kind = match.group(2)
            if kind:
                if var_type == 'character':
                    kind = 'len='+kind
                else:
                    kind = cls.__kind_badkind.format(kind, var_type, name)
                    var_type = "ERROR"
                # end if
            elif var_type == 'character':
                kind = cls.__char_nolen.format(name)
                var_type = "ERROR"
            array_len = match.group(3)
        # end if (no else, None will show as invalid)
        return var_type, kind, array_len

    @classmethod
    def _parse_array_desc(cls, alen, name):
        """Parse the XML type array declaration, <alen>, into an array
           of integer lengths and return this array as a tuple.
        Also, create unique dimension names for each dimension and return
           a tuple of those as a second return argument.
        If an error is detected, raise an exception since the argument
           should have been sanitized by _parse_xml_type.

        >>> NLVar._parse_array_desc("10", "foo")
        ((10,), ('foo_dimension',))
        >>> NLVar._parse_array_desc("15,3", "bar")
        ((15, 3), ('bar_dimension', 'bar_dimension_2'))
        >>> NLVar._parse_array_desc("15,,3", "foobar")
        Traceback (most recent call last):
        ...
        parse_source.ParseInternalError: Invalid array argument, '15,,3', for foobar
        """
        if alen:
            try:
                alens = tuple([int(x.strip()) for x in alen.split(',')])
                anames = tuple([cls._new_nl_dimname(name) for x in alens])
            except:
                emsg = f"Invalid array argument, '{alen}', for {name}"
                raise ParseInternalError(emsg)
            # end try
        else:
            alens = ()
            anames = ()
        # end if
        return alens, anames

    def is_valid(self):
        """Return True if this NLVar object contains all required fields."""
        return self.__valid

    def missing(self):
        """Return a list of the missing required properties of this
        NLVar object."""
        missing_props = []
        if not self.var_type:
            missing_props.append("variable type")
        # end if
        if not self.group:
            missing_props.append("group")
        # end if
        if not self.standard_name:
            missing_props.append("standard_name")
        # end if
        if not self.units:
            missing_props.append("units")
        # end if
        if self.__kind_err:
            missing_props.append(self.__kind_err)
        # end if
        if ((self.var_type not in self.__nl_types) and
            (self.var_type != "ERROR")):
            missing_props.append(f"Unknown variable type, '{self.var_type}'")
        # end if
        if len(missing_props) > 2:
            suff = f", and {missing_props[-1]}"
            missing_props = missing_props[0:-1]
        elif len(missing_props) > 1:
            suff = f" and {missing_props[-1]}"
            missing_props = missing_props[0:-1]
        else:
            suff = ""
        # end if
        return ", ".join(missing_props) + suff

    def write_metadata_entry(self, file):
        """Write a metadata entry for this NLVar object to <file>."""
        if self.array_len:
            # Write unique array dimension variable(s)
            for aname in self.__array_names:
                file.write(f"[ {aname} ]\n")
                file.write(f"  standard_name = {aname}\n")
                file.write("  type = integer | units = 1\n")
                file.write("  dimensions = ()\n")
            # end for
        # end if (no else)
        file.write(f"[ {self.var_name} ]\n")
        file.write(f"  standard_name = {self.standard_name}\n")
        if self.long_name:
            file.write(f"  long_name = {self.long_name}\n")
        # end if
        if self.kind:
            kind_suff = f" | kind = {self.kind}"
        elif self.var_type == "real":
            kind_suff = f" | kind = kind_phys"
        else:
            kind_suff = ""
        # end if
        file.write(f"  type = {self.var_type}{kind_suff}\n")
        file.write(f"  units = {self.units}\n")
        file.write(f"  dimensions = ({', '.join(self.__array_names)})\n")
        file.write("  protected = True\n")

    def write_decl(self, file, indent, intent=None):
        """Write a Fortran declaration of this variable to <file> at
           indent level, <indent>.
        If <intent> is not None, use that as a variable intent in the
           declaration.
        """
        if self.kind:
            type_str = f"{self.var_type}({self.kind}), public, protected"
        else:
            type_str = f"{self.var_type}, public, protected"
        # end if
        pad1 = " "*(self.__type_strlen - len(type_str))
        if intent:
            comma = ','
            intent_str = f"intent({intent})" + " "*(5 - len(intent))
        else:
            comma = ''
            intent_str = ''
        # end if
        if self.array_len:
            if intent:
                array_str = "(:"+",:"*(len(self.array_len) - 1)+")"
            else:
                lens = ", ".join([str(x) for x in self.array_len])
                array_str = f"({lens})"
            # end if
        else:
            array_str = ""
        # end if
        var_str = f"{self.var_name}{array_str}"
        if self.var_type == "integer":
            init_str = " = unset_int"
        elif self.var_type == "logical":
            init_str = " = .false."
        elif self.var_type == "real":
            if self.kind:
                init_str = f" = -HUGE(1.0_{self.kind})"
            else:
                init_str = " = -HUGE(1.0)"
            # end if
        elif self.var_type == "character":
            init_str = " = unset_str"
        # end if
        decl = f"{type_str}{comma}{pad1}{intent_str} :: {var_str}{init_str}"
        file.write(decl, indent)

    @classmethod
    def _new_nl_dimname(cls, var_name):
        """Return a new dimension name for use with a namelist array variable"""

        # Update number of dimensions for the
        # specified namelist variable:
        if var_name in cls.__new_dim_suffix:
            cls.__new_dim_suffix[var_name] += 1
        else:
            cls.__new_dim_suffix[var_name] = 1

        # Create new dimension name based
        # on the variable name and number of
        # variable dimensions:
        if cls.__new_dim_suffix[var_name] > 1:
            return f"{var_name}_dimension_{cls.__new_dim_suffix[var_name]}"
        else:
            return f"{var_name}_dimension"

    def var_mpi_type(self):
        """Return the MPI variable type constant of this NLVar object.
           Note that all real quantities will be passed as mpi_real8
        """
        if self.var_type in self.__mpi_types:
            return self.__mpi_types[self.var_type]
        # end if
        emsg = "No MPI type for variable type, '{}'"
        raise ParseInternalError(emsg.format(self.var_type))

    def write_dimension_decls(self, ofile, indent):
        """Write module declarations for dimensions of NLVar object (if any).
        Declarations are written to <ofile> with indent, <indent>.
        """
        for index, alen in enumerate(self.array_len):
            aname = self.__array_names[index]
            ofile.write(f"integer, public, parameter :: {aname} = {alen}", indent)
        # end for

    @property
    def var_name(self):
        """Return the variable name of this NLVar object"""
        return self.__var_name

    @property
    def var_type(self):
        """Return the variable type of this NLVar object"""
        return self.__type

    @property
    def group(self):
        """Return the group name of this NLVar object"""
        return self.__group

    @property
    def standard_name(self):
        """Return the standard name of this NLVar object"""
        return self.__standard_name

    @property
    def long_name(self):
        """Return the long name of this NLVar object"""
        return self.__long_name

    @property
    def units(self):
        """Return the units of this NLVar object"""
        return self.__units

    @property
    def kind(self):
        """Return the kind of this NLVar object"""
        return self.__kind

    @property
    def array_len(self):
        """Return the array length (if any) for this NLVar object"""
        return self.__array_lengths

###############################################################################
class SchemeNamelistInfo:
###############################################################################
    """Class to store information related to a scheme's namelist"""

    def __init__(self, filename_desc, schema_paths=None, logger=None):
        """Initialize a SchemeNamelistInfo object with a scheme name
        and a path to a namelist definition file.
        """
        self.__errors = [] # To query after creating all object
        self.__scheme = None
        self.__namelist_def_file = None
        self.__groups = {}
        self.__nlread_module = None
        # Set the scheme and namelist definition file and module names
        self._parse_scheme_filename(filename_desc)
        # Check to see if the namelist definition file exists and is valid
        self.validate_namelist_def_file(schema_paths=schema_paths,
                                        logger=logger)
        # A standard name for the namelist reading function
        self.__nlread_func = f"autogen_{self.scheme}_readnl"
        self.__metadata_file = None
        self.__nlread_file = None

    def validate_namelist_def_file(self, schema_paths=None, logger=None):
        """Check that the namelist definition file for this object
           exists and is valid
           Add object errors if detected.
        """
        if not self.errors:
            if not os.path.exists(self.namelist_def_file):
                emsg = "Missing namelist definition file, {}"
                self.__errors.append(emsg.format(self.namelist_def_file))
                if logger:
                    logger.error(emsg.format(self.namelist_def_file))
                # end if
            # end if
        # end if
        if not self.errors:
            # Validate the XML file
            if isinstance(schema_paths, str):
                schema_paths = [schema_paths]
            elif not schema_paths:
                schema_paths = [_XML_SCHEMAS, _PG_SCHEMAS]
            # end if
            # Make sure we have a schema file
            schema_file = None
            for spath in schema_paths:
                if logger:
                    logger.debug("Looking for namelist schema in '%s'", spath)
                # end if
                schema_file = os.path.join(spath, "entry_id_pg.xsd")
                if os.path.isfile(schema_file):
                    break
                # end if
                schema_file = None
            # end for
            if not schema_file:
                if schema_paths:
                    sstr = f"[{', '.join(schema_paths)}]"
                    raise ParseInternalError(f"No schema file found in {sstr}")
                # end if
                raise ParseInternalError("No schema_paths to check")
            # end if
            try:
                emsg = f"Invalid namelist XML file, {self.namelist_def_file}"
                file_ok = validate_xml_file(self.namelist_def_file,
                                            schema_file, "", logger)
            except CCPPError as ccpperr:
                cemsg = "{}".format(ccpperr).split('\n', maxsplit=1)[0]
                if cemsg[0:12] == 'Execution of':
                    xstart = cemsg.find("'")
                    if xstart >= 0:
                        xend = cemsg[xstart + 1:].find("'") + xstart + 1
                        emsg += '\n' + cemsg[xstart + 1:xend]
                    # end if (else, just keep original message)
                elif cemsg[0:18] == 'validate_xml_file:':
                    emsg += "\n" + cemsg
                # end if
                file_ok = False
            # end try
            if not file_ok:
                if logger:
                    logger.error(emsg)
                # end if
                self.__errors.append(emsg)
            # end if
        # end if

    def _write_metadata_file(self, nlvars, outdir, logger):
        """Write the metadata file for the namelist variables for this
           SchemeNamelistInfo object.
        """
        self.__metadata_file = os.path.join(outdir, self.nlread_module+".meta")
        if logger:
            logger.info(f"Writing metadata file, {self.__metadata_file}")
        # end if
        with open(self.__metadata_file, "w") as mfile:
            mfile.write("[ccpp-table-properties]\n")
            mfile.write(f"  name = {self.nlread_module}\n")
            mfile.write("  type = module\n")
            mfile.write("[ccpp-arg-table]\n")
            mfile.write(f"  name  = {self.nlread_module}\n")
            mfile.write("  type  = module\n")
            for var in nlvars:
                var.write_metadata_entry(mfile)
            # end for
        # end with

    def _write_nlread_file(self, nlvars, outdir, log_info,
                           indent, mpi_obj, logger):
        """Write the namelist reading Fortran module to <outdir>
        """
        file_desc = f"Module to read namelist variables for {self.scheme}"
        # Collect all the kinds used in the file
        file_kinds = set()
        for var in nlvars:
            kind = var.kind
            if kind and (not is_int(kind)) and (var.var_type != 'character'):
                file_kinds.add(kind)
            # end if
        # end for
        var_types = set()
        self.__nlread_file = os.path.join(outdir, self.nlread_module+".F90")
        if logger:
            logger.info(f"Writing Fortran module, {self.__nlread_file}")
        # end if
        with FortranWriter(self.__nlread_file, "w", file_desc,
                           self.nlread_module, indent=indent) as ofile:
            # Write out any kinds needed
            for kind in sorted(file_kinds):
                ofile.write(f"use ccpp_kinds,  only: {kind}", 1)
            # end if
            ofile.write("use runtime_obj, only: unset_str, unset_int", 1)
            # Boilerplate
            ofile.write_preamble()
            # Declare the namelist reading function
            ofile.write(f"public :: {self.nlread_func}", 1)
            ofile.blank_line()
            # Write out module variable declarations
            write_ccpp_table_header(self.nlread_module, ofile, 1)
            for var in nlvars:
                var.write_dimension_decls(ofile, 1)
                var.write_decl(ofile, 1)
                var_types.add(var.var_mpi_type())
            # end for
            # end of module header
            ofile.end_module_header()
            ofile.blank_line()
            # Write out the definition of the namelist reading function
            nl_args = ["nl_unit", "mpicomm", "mpiroot", "mpi_isroot"]
            if log_info:
                nl_args.append("logunit")
            # end if
            args = f"({', '.join(nl_args)})"
            ofile.write(f"subroutine {self.nlread_func}{args}", 1)
            mpi = mpi_obj.mpi_module
            spc = ' '*(len("cam_abortutils") - len(mpi))
            mpi_types = ', '.join(sorted(var_types))
            ofile.write(f"use {mpi}, {spc}only: {mpi_types}", 2)
            spc = ' '*(len("cam_abortutils") - len("shr_nl_mod"))
            ofile.write(f"use shr_nl_mod, {spc}only: shr_nl_find_group_name", 2)
            ofile.write("use cam_abortutils, only: endrun", 2)
            ofile.blank_line()
            ofile.comment("Dummy arguments", 2)
            comm_type = mpi_obj.mpi_commtype()
            spc = " "*(len(comm_type) - len("integer"))
            ofile.write(f"integer,{spc} intent(in) :: nl_unit", 2)
            ofile.write(f"{comm_type}, intent(in) :: mpicomm", 2)
            ofile.write(f"integer,{spc} intent(in) :: mpiroot", 2)
            ofile.write(f"logical,{spc} intent(in) :: mpi_isroot", 2)
            if log_info:
                ofile.write(f"integer,{spc} intent(in) :: logunit", 2)
            # end if
            ofile.blank_line()
            ofile.comment("Local variables", 2)
            ofile.write("integer                     :: ierr", 2)
            substr = "character(len=*), parameter :: subname = '{}'"
            ofile.write(substr.format(self.nlread_func), 2)
            # Declare the namelists
            ofile.blank_line()
            for grpname in self.group_names():
                grpvars = ", ".join([v.var_name for v in
                                     self.__groups[grpname]])
                ofile.write(f"namelist /{grpname}/ {grpvars}", 2)
            # end for
            for grpname in self.group_names():
                ofile.blank_line()
                # For each group, read and process the namelist
                ofile.comment("Read the namelist on the root task", 2)
                ofile.write("if (mpi_isroot) then", 2)
                # First, rewind to the head of the file
                ofile.write("rewind(nl_unit)", 3)
                args = f"(nl_unit, '{grpname}', status=ierr)"
                ofile.write(f"call shr_nl_find_group_name{args}", 3)
                ofile.write("if (ierr == 0) then", 3)
                ofile.write(f"read(nl_unit, {grpname}, iostat=ierr)", 4)
                ofile.write("if (ierr /= 0) then", 4)
                errmsg = f"ERROR reading namelist, {grpname}"
                ofile.write(f"call endrun(subname//':: {errmsg}')", 5)
                ofile.write("end if", 4)
                ofile.write("else", 3)
                emsg = f"ERROR: Did not find namelist group, {grpname}."
                ofile.write(f"call endrun(subname//':: {emsg}')", 4)
                ofile.write("end if", 3)
                if log_info:
                    ofile.comment("Print out namelist values", 3)
                    msg = f"Namelist values from {grpname} for {self.scheme}"
                    ofile.write(f"write(logunit, *) '{msg}'", 3)
                    for grpvar in self.__groups[grpname]:
                        msg = f"'{grpvar.var_name} = ', {grpvar.var_name}"
                        ofile.write(f"write(logunit, *) {msg}", 3)
                    # end for
                # end if
                ofile.write("end if", 2)
                ofile.comment("Broadcast the namelist variables", 2)
                for grpvar in self.__groups[grpname]:
                    arglist = [grpvar.var_name]
                    dimsize = 1
                    if grpvar.array_len:
                        # XXgoldyXX: Can be replaced math.prod in python 3.8+
                        for alen in grpvar.array_len:
                            dimsize *= alen
                        # end for
                        dimstr = f"*{dimsize}"
                    else:
                        dimstr = ""
                    # end if
                    if grpvar.var_type == "character":
                        if dimstr:
                            one = "(1)"
                        else:
                            one = ""
                        # end if
                        arglist.append(f"len({grpvar.var_name}{one}){dimstr}")
                    else:
                        arglist.append(str(dimsize))
                    # end if
                    arglist.append(grpvar.var_mpi_type())
                    arglist.extend(["mpiroot", "mpicomm", "ierr"])
                    args = ", ".join(arglist)
                    ofile.write(f"call mpi_bcast({args})", 2)
                # end for
                ofile.blank_line()
            # end for
            ofile.write(f"end subroutine {self.nlread_func}", 1)
        # end with

    def process_namelist_def_file(self, outdir, log_info, indent,
                                  mpi_obj, logger):
        """Read the namelist variables from <nlxml> and produce both a module
           with a routine that reads these variables into module variables and
           an associated CCPP metadata file.
          <outdir> is the directory where the output files are written.
          If <log_info> is True, output statments to log namelist values
          <mpi_obj> is an MpiModuleInfo object used to generate the correct
             MPI Fortran statements.
          <logger> is a Python logger.
        """
        nlvars = []
        logger.info("Reading CAM physics namelist definition file, '%s'",
                    self.namelist_def_file)
        if not self.errors:
            errors = []
            # Throw exception on error (should be validated XML)
            _, nlxml = read_xml_file(self.namelist_def_file)
            for element in nlxml:
                newvar = NLVar(element)
                if newvar.is_valid():
                    nlvars.append(newvar)
                    newgrp = newvar.group
                    if newgrp in self.__groups:
                        self.__groups[newgrp].append(newvar)
                    else:
                        self.__groups[newgrp] = [newvar]
                    # end if
                else:
                    errors.append("{} is missing {}".format(newvar.var_name,
                                                            newvar.missing()))
                # end if
            # end for
        # end if
        if errors:
            errstr = '\n'.join(errors)
            emsg = f"Errors processing {self.namelist_def_file}\n{errstr}"
            logger.error(emsg)
            self.__errors.append(emsg)
        # end if
        if not self.errors:
            self._write_metadata_file(nlvars, outdir, logger)
        # end if
        if not self.errors:
            self._write_nlread_file(nlvars, outdir, log_info,
                                    indent, mpi_obj, logger)
        # end if

    def group_names(self):
        """Return a dict_keys object with the group names for the
           namelist represented by this SchemeNamelistInfo object.
        """
        return self.__groups.keys()

    @property
    def scheme(self):
        """Return the scheme name for this SchemeNamelistInfo object"""
        return self.__scheme

    @property
    def namelist_def_file(self):
        """Return the namelist definition filename for this
        SchemeNamelistInfo object"""
        return self.__namelist_def_file

    @property
    def metadata_file(self):
        """Return the generated metadata filename for this
        SchemeNamelistInfo object"""
        return self.__metadata_file

    @property
    def nlread_file(self):
        """Return the generated namelist read filename for this
        SchemeNamelistInfo object"""
        return self._nlread_file

    @property
    def nlread_module(self):
        """Return the generated namelist read Fortran module name for this
        SchemeNamelistInfo object"""
        return self.__nlread_module

    @property
    def nlread_func(self):
        """Return the generated Fortran namelist read function name for this
        SchemeNamelistInfo object"""
        return self.__nlread_func

    @property
    def errors(self):
        """Return a list of the errors found with this object or an empty
        list to signify no errors."""
        return self.__errors

    def _parse_scheme_filename(self, arg):
        """Parse a scheme name / file name combo and store as instance vars
        The normal format is 'scheme:filename'.
        If there is no colon, we assume that the argument is a filename
        related to the scheme name, <scheme>[_namelist].xml
        """
        cargs = arg.split(':')
        if len(cargs) == 2:
            self.__scheme, self.__namelist_def_file = cargs
            # Fortran module name matches XML filename
            basename = os.path.basename(self.__namelist_def_file)
            self.__nlread_module = os.path.splitext(basename)[0]
        elif len(cargs) == 1:
            self.__namelist_def_file = cargs[0]
            # Fortran module name matches XML filename
            basename = os.path.basename(cargs[0])
            self.__nlread_module = os.path.splitext(basename)[0]
            nindex = self.__nlread_module.find("_namelist")
            if nindex > 0:
                self.__scheme = self.__nlread_module[0:nindex]
            else:
                self.__scheme = self.__nlread_module
            # end if
        else:
            self.__errors.append(f"Invalid namelist file argument, '{arg}'")
        # end if

###############################################################################
class NamelistFiles:
###############################################################################
    """Class to store runtime options and namelist processing information"""

    def __init__(self, args, description, schema_paths=None, logger=None):
        """Initialize this NamelistFiles object"""
        args = self._parse_command_line(args, description)
        self.__scheme_nl_files = {}
        errors = []
        for arg in args.namelist_file_arg:
            new_info_obj = SchemeNamelistInfo(arg, schema_paths=schema_paths,
                                              logger=logger)
            if new_info_obj.errors:
                errors.extend(new_info_obj.errors)
            else:
                self.__scheme_nl_files[new_info_obj.scheme] = new_info_obj
            # end if
        # end for
        if errors:
            raise NamelistError("\n".join(errors))
        # end if
        self.__mpi_obj = MpiModuleInfo(args.mpi_comm_arg, args.mpi_root_arg,
                                       args.mpi_is_root_arg, args.mpi_module)
        self.__logunit_arg = args.logunit_arg
        self.__indent = args.indent
        self.__xml_schema_dir = args.xml_schema_dir
        self.__namelist_read_mod = args.namelist_read_mod
        self.__namelist_read_subname = args.namelist_read_subname
        if args.output_dir is None:
            self.__outdir = os.getcwd()
        else:
            self.__outdir = args.output_dir
        # end if
        if logger:
            if args.debug:
                loglevel = logging.DEBUG
            elif args.quiet:
                loglevel = logging.ERROR
            else:
                loglevel = logging.INFO
            # end if
            logger.setLevel(loglevel)
        # end if
        self.__scheme_read_file = None
        self.__nlfile_arg = "nlfile"
        self.__active_schemes_arg = "active_schemes"

    def _parse_command_line(self, args, description):
        """Parse and return the command line arguments when
        this module is executed"""
        parser = argparse.ArgumentParser(description=description,
                                         formatter_class=argparse.RawTextHelpFormatter)

        parser.add_argument("--namelist-file-arg", type=str, action='append',
                            metavar="scheme:namelist_file", default=[],
                            help="""A colon-separated pair of a scheme name and
an XML namelist definition filename""")
        parser.add_argument("--output-dir", type=str, default=None,
                            help="Directory where output files will be written")
        parser.add_argument("--mpi-comm-arg", type=str, default="mpi_comm",
                            help="Name of the MPI communicator input argument")
        parser.add_argument("--mpi-root-arg", type=str, default="mpi_root",
                            help="Name of the MPI communicator root input argument")
        parser.add_argument("--mpi-is-root-arg", type=str, default="mpi_isroot",
                            help="""Name of the logical communicator root
                            (.true. if the current task is root) input argument""")
        parser.add_argument("--logunit-arg", type=str, default="logunit",
                            help="""Name of the output log input argument.
                            If this argument is not used, namelist schemes
                            will not log output.""")
        parser.add_argument("--indent", type=int, default=3,
                            help="Indent level for Fortran source code")
        parser.add_argument("--xml-schema-dir", type=str, default=_XML_SCHEMAS,
                            help="""Location of schema for validating namelist
                            XML files""")
        parser.add_argument("--namelist-read-mod", type=str,
                            default="cam_ccpp_scheme_namelists",
                            help="""The name of the module containing the
                            routine to read the namelists for all active
                            schemes.""")
        parser.add_argument("--namelist-read-subname", type=str,
                            default="cam_read_ccpp_scheme_namelists",
                            help="""The name of the subroutine that reads the
                            namelists for all active schemes.""")
        parser.add_argument("--mpi-module", type=str, default='mpi',
                            help="""MPI module version to use.
                            --mpi-module=mpi_f08 uses the Fortran 2008 MPI interfaces,
                            --mpi-module=mpi uses the Fortran 77 interfaces.
                            (default is Fortran 77""")
        group = parser.add_mutually_exclusive_group(required=False)
        group.add_argument("--quiet", action='store_true', default=False,
                           help="Suppress most log messages")
        group.add_argument("--debug", action='store_true', default=False,
                           help="Produce more detailed log messages")
        pargs = parser.parse_args(args)
        return pargs

    def _write_active_scheme_reader(self, outdir, logger):
        """Write a Fortran module to read the namelists of all the active
           schemes for a run."""
        self.__scheme_read_file = os.path.join(outdir,
                                               self.namelist_read_mod + ".F90")
        file_desc = """Autogenerated file to read the namelists for all
        active CCPP physics schemes."""
        ##XXgoldyXX: v remove this when hash table is implemented
        file_desc += """\n\nXXgoldyXX: Replace the active scheme finder
           algorithm (is_scheme_active) with a hash table to
           improve performance."""
        ##XXgoldyXX: ^ remove this when hash table is implemented
        with FortranWriter(self.__scheme_read_file, "w", file_desc,
                           self.namelist_read_mod, indent=self.indent) as ofile:
            set_active_schemes = "set_active_schemes"
            is_scheme_active = "is_scheme_active"
            clear_active_schemes = "clear_active_schemes"
            # Boilerplate
            ofile.write_preamble()
            # Declare the namelist reading function
            ofile.comment("Public interface", 1)
            ofile.write(f"public :: {self.namelist_read_subname}", 1)
            ofile.blank_line()
            ofile.comment("Private interfaces", 1)
            ofile.write(f"private :: {set_active_schemes}", 1)
            ofile.write(f"private :: {is_scheme_active}", 1)
            ofile.write(f"private :: {clear_active_schemes}", 1)
            # Write out module variable declarations
            ofile.comment("Private data", 1)
            ofile.write("character(len=63), allocatable :: active_schemes(:)",
                        1)
            # end of module header
            ofile.end_module_header()
            ofile.blank_line()
            # set_active_schemes
            ofile.write(f"subroutine {set_active_schemes}(active_schemes_in)",
                        1)
            ofile.write("use cam_abortutils, only: check_allocate", 2)
            ofile.write("use string_utils,   only: to_str", 2)
            ofile.blank_line()
            ofile.comment("Dummy argument", 2)
            ofile.write("character(len=*), intent(in) :: active_schemes_in(:)",
                        2)
            ofile.comment("Local variables", 2)
            ofile.write("integer                     :: istat", 2)
            ofile.write(f"character(len=*), parameter :: subname = " +        \
                        f"'{set_active_schemes}'", 2)
            ofile.blank_line()
            ofile.write("allocate(active_schemes(size(active_schemes_in))," + \
                        " stat=istat)", 2)
            ofile.write("call check_allocate(istat, subname, " +              \
                        "'active_schemes', file=__FILE__, line=__LINE__)", 2)
            ofile.write("active_schemes(:) = active_schemes_in(:)", 2)
            ofile.blank_line()
            ofile.write(f"end subroutine {set_active_schemes}", 1)
            # namelist read subroutine
            ofile.blank_line()
            ofile.write(f"logical function {is_scheme_active}(scheme_name)", 1)
            ofile.blank_line()
            ofile.comment("Dummy argument", 2)
            ofile.write("character(len=*), intent(in) :: scheme_name",
                        2)
            ofile.comment("Local variable", 2)
            ofile.write("integer :: index", 2)
            ofile.blank_line()
            ofile.write(f"{is_scheme_active} = .false.", 2)
            ofile.write("do index = 1, size(active_schemes)", 2)
            ofile.write("if (trim(scheme_name) == "                           \
                        "trim(active_schemes(index))) then", 3)
            ofile.write(f"{is_scheme_active} = .true.", 4)
            ofile.write("exit", 4)
            ofile.write("end if", 3)
            ofile.write("end do", 2)
            ofile.blank_line()
            ofile.write(f"end function {is_scheme_active}", 1)
            # Clear active schemes routine
            ofile.blank_line()
            ofile.write(f"subroutine {clear_active_schemes}()", 1)
            ofile.blank_line()
            ofile.write("deallocate(active_schemes)", 2)
            ofile.blank_line()
            ofile.write(f"end subroutine {clear_active_schemes}", 1)
            # Main namelist reading module
            # Standard arguments
            arglist = [self.nlfile_arg, self.active_schemes_arg,
                       self.mpi_comm_arg, self.mpi_root_arg,
                       self.mpi_is_root_arg]
            if self.log_values():
                arglist.append(self.logunit_arg)
            # end if
            # Host-level namelist parameters
            #XXgoldyXX: Need a process for this
            args = ", ".join(arglist)
            ofile.blank_line()
            ofile.write(f"subroutine {self.namelist_read_subname}({args})", 1)
            schemes = self.schemes()
            if schemes:
                maxmod = max([len(self.__scheme_nl_files[x].nlread_module)
                              for x in schemes])
            else:
                maxmod = 0
            # end if
            for scheme_name in schemes:
                mod_name = self.__scheme_nl_files[scheme_name].nlread_module
                func_name = self.__scheme_nl_files[scheme_name].nlread_func
                spc = " "*(maxmod - len(mod_name))
                ofile.write(f"use {mod_name},{spc} only: {func_name}", 2)
            # end for
            ofile.blank_line()
            ofile.comment("Dummy arguments", 2)
            char_input = "character(len=*), intent(in)   "
            int_input = "integer,          intent(in)   "
            logical_input = "logical,          intent(in)   "
            ofile.write(f"{char_input} :: {self.nlfile_arg}", 2)
            ofile.write(f"{char_input} :: {self.active_schemes_arg}(:)", 2)
            ofile.write(f"{int_input} :: {self.mpi_comm_arg}", 2)
            ofile.write(f"{int_input} :: {self.mpi_root_arg}", 2)
            ofile.write(f"{logical_input} :: {self.mpi_is_root_arg}", 2)
            if self.log_values():
                ofile.write(f"{int_input} :: {self.logunit_arg}", 2)
            # end if
            ofile.blank_line()
            ofile.comment("Local variable", 2)
            ofile.write("integer :: nl_unit", 2)
            ofile.blank_line()
            ofile.write(f"call {set_active_schemes}(active_schemes)", 2)
            open_args = f"newunit=nl_unit, file=trim({self.nlfile_arg})"
            ofile.write(f"open({open_args}, status='old')", 2)
            nlarglist = ["nl_unit", self.mpi_comm_arg,
                         self.mpi_root_arg, self.mpi_is_root_arg]
            if self.log_values():
                nlarglist.append(self.logunit_arg)
            # end if
            nlargs = ", ".join(nlarglist)
            for scheme_name in self.schemes():
                func_name = self.__scheme_nl_files[scheme_name].nlread_func
                scheme = self.__scheme_nl_files[scheme_name].scheme
                ofile.write(f"if ({is_scheme_active}('{scheme}')) then", 2)
                ofile.write(f"call {func_name}({nlargs})", 3)
                ofile.write("end if", 2)
            # end for
            ofile.blank_line()
            ofile.write("close(nl_unit)", 2)
            ofile.write(f"call {clear_active_schemes}()", 2)
            ofile.blank_line()
            ofile.write(f"end subroutine {self.namelist_read_subname}", 1)
        # end with

    def schemes(self):
        """Return the Scheme names of the SchemeNamelistInfo objects for this
           NamelistFiles object."""
        return self.__scheme_nl_files.keys()

    def write_nl_files(self, outdir, logger):
        """Write the Fortran namelist reader modules, the associated
           metadata files, and the active scheme namelist read
           Fortran module.
        """
        errors = []
        for scheme_name in self.schemes():
            scheme = self.__scheme_nl_files[scheme_name]
            scheme.process_namelist_def_file(outdir, self.log_values(),
                                             self.indent, self.mpi_obj, logger)
            if scheme.errors:
                errors.extend(scheme.errors)
            # endif
        # end for
        if errors:
            raise NamelistError('\n'.join(errors))
        # end if
        self._write_active_scheme_reader(outdir, logger)

    def code_files(self):
        """Return all the Fortran source code files which have been created
           by this object."""
        code_files = []
        for scheme_name in self.schemes():
            nlread_file = self.__scheme_nl_files[scheme_name].nlread_file
            if nlread_file:
                code_files.append(nlread_file)
        # end for
        if self.__scheme_read_file:
            code_files.append(self.__scheme_read_file)
        # end if
        return code_files

    def meta_files(self):
        """Return all the CCPP metadata files which have been created
           by this object."""
        meta_files = []
        for scheme_name in self.schemes():
            meta_file = self.__scheme_nl_files[scheme_name].metadata_file
            if meta_file:
                meta_files.append(meta_file)
        # end for
        return meta_files

    def groups(self, schemes=None):
        """Return all the namelist groups which have been processed
           by this object for each scheme in <schemes>
           (i.e., all namelist groups for configuring <schemes>.
           If <schemes> is None, include all schemes.
        """
        groups = set()
        missing = []
        if schemes is None:
            schemes = self.schemes()
        for scheme_name in schemes:
            if scheme_name in self.__scheme_nl_files:
                sch_groups = self.__scheme_nl_files[scheme_name].group_names()
                groups.update(sch_groups)
            else:
                missing.append(scheme_name)
            # end if
        # end for
        if missing:
            emsg = f"ERROR: Missing groups: {' '.join(missing)}."
            raise ParseInternalError(emsg)
        # end if
        return sorted(groups)

    def log_values(self):
        """Return True if namelist values should be written"""
        return isinstance(self.__logunit_arg, str) and self.__logunit_arg

    @property
    def nlfile_arg(self):
        """Return the name of the input namelist file argument"""
        return self.__nlfile_arg

    @property
    def active_schemes_arg(self):
        """Return the name of the argument containing an array of active
           scheme names"""
        return self.__active_schemes_arg

    @property
    def mpi_obj(self):
        """Return the MpiModuleInfo object for this NamelistFiles object"""
        return self.__mpi_obj

    @property
    def mpi_module(self):
        """Return the MPI module to use"""
        return self.__mpi_obj.mpi_module

    @property
    def mpi_comm_arg(self):
        """Return the dummy argument name for the MPI communicator to use
           when broadcasting namelist variable values.
        """
        return self.__mpi_obj.mpi_comm_arg

    @property
    def mpi_root_arg(self):
        """Return the dummy argument name for the MPI root task to use
           when broadcasting namelist variable values.
        """
        return self.__mpi_obj.mpi_root_arg

    @property
    def mpi_is_root_arg(self):
        """Return the dummy argument name for the logical for determining if
           this is the task to use for reading the namelist and logging
           the values.
        """
        return self.__mpi_obj.mpi_is_root_arg

    @property
    def namelist_read_mod(self):
        """Return the name of the module containing the routine to
           read the namelists for all active schemes.
        """
        return self.__namelist_read_mod

    @property
    def namelist_read_subname(self):
        """Return the name of the subroutine that reads the
           namelists for all active schemes.
        """
        return self.__namelist_read_subname

    @property
    def logunit_arg(self):
        """Return the dummy argument name for write statements used to log
           namelist variable values.
        """
        return self.__logunit_arg

    @property
    def indent(self):
        """Return the indent unit size (in spaces) for this NamelistFiles object
        """
        return self.__indent

def gen_namelist_files(args, outdir, logger):
    """Function to execute to generate namelist files.
       <args> are command-line arguments such as the XML files to process.
       <outdir> is the directory where output files are written.
       <logger> is a Python logger.
    """
    namelist_obj = NamelistFiles(args, __doc__, logger=logger)
    namelist_obj.write_nl_files(outdir, logger)
    return namelist_obj

###############################################################################
