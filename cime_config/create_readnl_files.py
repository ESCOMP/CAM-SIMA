#!/usr/bin/env python3

"""
For each XML file in a list of XML namelist definition files, create
the appropriate read_namelist module and associated metadata.
Also, create a master module to execute these namelist read functions.

To run doctest on this file: python -m doctest generate_registry_data.py
"""

# Python library imports
# NB: ET is used in doctests which are not recognized by pylint
import xml.etree.ElementTree as ET # pylint: disable=unused-import
import os
import os.path
import re
import argparse
import sys
import logging
from collections import OrderedDict

# Find and include the ccpp-framework scripts directory
# Assume we are in <CAMROOT>/src/data and SPIN is in <CAMROOT>/ccpp_framework
_CURRDIR = os.path.abspath(os.path.dirname(__file__))
_CAMROOT = os.path.abspath(os.path.join(_CURRDIR, os.pardir))
_SPINSCRIPTS = os.path.join(_CAMROOT, "ccpp_framework", 'scripts')
_XML_SCHEMAS = os.path.join(_CAMROOT, "cime", "config", "xml_schemas")
_PG_SCHEMAS = os.path.join(_CAMROOT, "cime", "scripts", "lib",
                           "CIME", "ParamGen", "xml_schema")
if _SPINSCRIPTS not in sys.path:
    sys.path.append(_SPINSCRIPTS)
# end if

# CCPP framework imports
# pylint: disable=wrong-import-position
from framework_env import CCPPFrameworkEnv
from parse_tools import validate_xml_file, read_xml_file
from parse_tools import find_schema_file, find_schema_version
from parse_tools import init_log, CCPPError, ParseInternalError
from fortran_tools import FortranWriter
# pylint: enable=wrong-import-position

###############################################################################
def is_int(token):
###############################################################################
    """Return True if <token> is an integer"""
    isint = False
    if token:
        try:
            ival = int(token)
            isint = True
        except ValueError:
            isint = False
        # end try
    else:
        isint = False
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
def parse_command_line(args, description):
###############################################################################
    """Parse and return the command line arguments when
    this module is executed"""
    parser = argparse.ArgumentParser(description=description,
                                     formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument("--namelist-file", type=str, action='append',
                        metavar="namelist_file", default=list(),
                        help="""An XML namelist definition filename""")
    parser.add_argument("--output-dir", type=str, default=None,
                        help="Directory where output files will be written")
    parser.add_argument("--nlfile-arg", type=str, default="nlfile",
                        help="Name of the namelist file path input argument")
    parser.add_argument("--mpicom-arg", type=str, default="mpi_comm",
                        help="Name of the MPI communicator input argument")
    parser.add_argument("--mpiroot-arg", type=str, default="mpi_root",
                        help="Name of the MPI communicator root input argument")
    parser.add_argument("--mpiisroot-arg", type=str, default="mpi_isroot",
                        help="""Name of the logical communicator root
(.true. if the current task is root) input argument""")
    parser.add_argument("--logunit-arg", type=str, default="logunit",
                        help="Name of the output log input argument")
    parser.add_argument("--indent", type=int, default=3,
                        help="Indent level for Fortran source code")
    parser.add_argument("--xml-schema-dir", type=str, default=_XML_SCHEMAS,
                        help="""Location of schema for validating namelist
XML files""")
    pargs = parser.parse_args(args)
    return pargs

###############################################################################
class NamelistError(ValueError):
###############################################################################
    """Class to indicate errors processing namelist files"""
    def __init__(self, message):
        """Initialize this exception"""
        logging.shutdown()
        super().__init__(message)

###############################################################################
class NLVar:
###############################################################################
    """Class to hold information about a namelist variable entry"""

    # For validity error messages
    __kind_errstr = "Conflicting kind arguments, '{}' and '{}'"
    __kind_badkind = "Illegal kind, '{}', for type, '{}'"
    __nl_types = ['integer', 'logical', 'real', 'character']

    # Parse any valid XML <type> text
    __tspec = r"[ ]*([a-z]+)[ ]*"
    __clenspec = r"(?:[*][ ]*([0-9]+))?[ ]*"
    __aspec = r"[ ]*(?:[(][ ]*([0-9]+)[ ]*[)])?"
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
        self.__array_len = None
        self.__valid = True # speculation to be confirmed
        self.__kind_err = ""
        for element in var_xml:
            elem_type = element.tag
            if elem_type == "type":
                typ, knd, alen = self._parse_xml_type(element.text)
                self.__type = typ
                if (typ == "character") and self.__kind and knd:
                    # Can't have a character <type> and a <kind> tag
                    self.__valid = False
                    self.__kind_err = self.__kind_errstr.format(self.__kind,
                                                                knd)
                elif typ == "character":
                    self.__kind = knd
                else:
                    # Only character can have a "kind" in the type tag
                    self.__kind_err = self.__kind_badkind.format(knd, typ)
                # end if
                self.__arraylen = alen
                if self.var_type == "character":
                    # character arguments at least require a length
                    self.__valid &= self.kind is not None
                    self.__kind_err = "No length argument for character type"
                # end if
            elif elem_type == "group":
                self.__group = element.text
            elif elem_type == "standard_name":
                self.__standard_name = element.text
            elif elem_type == "long_name":
                self.__long_name = element.text
            elif elem_type == "units":
                self.__units = element.text
            elif elem_type == "kind":
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
        # Default kind for real variables
        if (not self.kind) and (self.var_type == "real"):
            self.__kind = "kind_phys"
        # end if
        # Final validity check
        self.__valid &= ((self.var_type is not None) and
                         (self.group is not None) and
                         (self.standard_name is not None) and
                         (self.units is not None))
        self.__valid &= self.var_type in self.__nl_types

    def _parse_xml_type(self, type_str):
        """Parse a namelist XML type description, <type_str>, and return
           the Fortran type, a character length argument (if the type is
           char, otherwise None), and an array length (or none).
        """
        var_type = None
        kind = None
        array_len = None
        match = self.__type_re.match(type_str)
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
                    kind = None # This should trigger an error
                # end if
            # end if
            array_len = match.group(3)
        # end if (no else, None will show as invalid)
        return var_type, kind, array_len

    def is_valid(self):
        """Return True if this NLVar object contains all required fields."""
        return self.__valid

    def missing(self):
        """Return a list of the missing required properties of this
        NLVar object."""
        missing_props = list()
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
        if len(missing_props) > 2:
            suff = f", and {missing_props[-1]}"
            missing_props = missing_props[0:-1]
        elif len(missing_props) > 1:
            suff = f" and {missing_props[-1]}"
            missing_props = missing_props[0:-1]
        else:
            suff = ""
        # end if
        if self.__kind_err:
            missing_props.append(self.__kind_err)
        # end if
        if self.var_type not in self.__nl_types:
            missing_props.append(f"Unknown variable type, '{self.var_type}'")
        # end if
        return ", ".join(missing_props) + suff

    def write_metadata_entry(self, file):
        """Write a metadata entry for this NLVar object to <file>."""
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

    def write_decl(self, file, indent, intent=None):
        """Write a Fortran declaration of this variable to <file> at
           indent level, <indent>.
        If <intent> is not None, use that as a variable intent in the
           declaration.
        """
        if self.kind:
            type_str = f"{self.var_type}({self.kind})"
        else:
            type_str = f"{self.var_type}"
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
            var_str = f"{self.var_name}({self.array_len})"
        else:
            var_str = f"{self.var_name}"
        # end if
        if self.var_type == "integer":
            init_str = " = -HUGE(1)"
        elif self.var_type == "logical":
            init_str = " = .false."
        elif self.var_type == "real":
            if self.kind:
                init_str = f" = -HUGE(1.0_{self.kind})"
            else:
                init_str = " = -HUGE(1.0)"
            # end if
        elif self.var_type == "character":
            init_str = " = UNSET"
        # end if
        decl = f"{type_str}{comma}{pad1}{intent_str} :: {var_str}{init_str}"
        file.write(decl, indent)

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
        return self.__array_len

###############################################################################
def process_xml_file(nlxml, basename, outdir, nlfile_arg, mpicom_arg,
                     mpiroot_arg, mpiisroot_arg, logunit_arg, indent, logger):
###############################################################################
    """Read the namelist variables from <nlxml> and produce both a module
       with a routine that reads these variables into module variables and
       an associated CCPP metadata file.
    <basename> is <scheme>_namelist
    The namelist reading module is named <basename>.F90
    The metadata file is named <basename>.meta
    Return four values:
       1) The name of the Fortran file
       2) The name of the metadata file
       3) A set of namelist groups used in this file
       4) An error string.
    Note that if the error string is not blank, then the filename strings
       will be blank
    """
    nlvars = list()
    errors = list()
    groups = set()
    metafile_name = ""
    fortfile_name = ""
    for element in nlxml:
        newvar = NLVar(element)
        if newvar.is_valid():
            nlvars.append(newvar)
            groups.add(newvar.group)
        else:
            errors.append("{} is missing {}".format(newvar.var_name,
                                                    newvar.missing()))
        # end if
    # end for
    if errors:
        logger.error("Errors processing {}\n{}".format(basename,
                                                       "\n".join(errors)))
    else:
        logger.info("Writing metadata and Fortran for {}".format(basename))
    # end if
    # Write the metadata file
    if not errors:
        metafile_name = os.path.join(outdir, basename+".meta")
        with open(metafile_name, "w") as mfile:
            mfile.write("[ccpp-table-properties]\n")
            mfile.write(f"  name = {basename}\n")
            mfile.write("  type = module\n")
            mfile.write("[ccpp-arg-table]\n")
            mfile.write(f"  name  = {basename}\n")
            mfile.write("  type  = module\n")
            for var in nlvars:
                var.write_metadata_entry(mfile)
            # end for
        # end with
    # end if
    if not errors:
        fortfile_name = os.path.join(outdir, basename+".F90")
        nl_index = basename.find("_namelist")
        if nl_index > 0:
            scheme = basename[0:nl_index]
        else:
            scheme = basename
        # end if
        file_desc = f"Module to read namelist variables for {scheme}"
        # Collect all the kinds used in the file
        file_kinds = set()
        for var in nlvars:
            kind = var.kind
            if kind and (not is_int(kind)):
                file_kinds.add(kind)
            # end if
        # end for
        # We need a standard name for the namelist reading function
        func_name = f"autogen_{scheme}_readnl"
        with FortranWriter(fortfile_name, "w", file_desc,
                           basename, indent=indent) as outfile:
            # Write out any kinds needed
            for kind in sorted(file_kinds):
                outfile.write(f"use ccpp_kinds, only: {kind}", 1)
            # end if
            # Boilerplate
            outfile.write_preamble()
            # Declare the namelist reading function
            outfile.write(f"public :: {func_name}", 1)
            outfile.write("", 0)
            # Write out module variable declarations
            write_ccpp_table_header(basename, outfile, 1)
            for var in nlvars:
                var.write_decl(outfile, 1)
            # end if
            # end of module header
            outfile.end_module_header()
            outfile.write("", 0)
    # end if
    return fortfile_name, metafile_name, groups, "\n".join(errors)

###############################################################################
def gen_namelist_files(namelist_files, outdir, nlfile_arg,
                       mpicom_arg, mpiroot_arg, mpiisroot_arg,
                       logunit_arg, indent, loglevel=None, logger=None,
                       schema_paths=None, error_on_no_validate=False):
###############################################################################
    """For each file in <namelist_files>, generate a Fortran module with
       a module variable corresponding to each namelist variable in the file.
    A subroutine to read the namelist and assign the values to these module
       variables is also generated.
    Finally, a CCPP metadata file is generated to document the namelist
       module variables.
    <indent> is the number of spaces between indent levels.
    Return four values:
    1) A return code, zero is success
    2) A list of newly-written Fortran namelist reading module filenames
    3) A list of newly-written metadata files (one per Fortranf file)
    4) A list of
    """
    if not logger:
        if not loglevel:
            loglevel = logging.INFO
        # end if
        logger = init_log(os.path.basename(__file__), loglevel)
    elif loglevel is not None:
        emsg = "gen_registry: Ignoring <loglevel> because logger is present"
        logger.debug(emsg)
    # end if
    if isinstance(schema_paths, str):
        schema_paths = [schema_paths]
    elif not schema_paths:
        schema_paths = [_XML_SCHEMAS, _PG_SCHEMAS]
    # end if
    # Make sure we have a schema file
    for spath in schema_paths:
        logger.debug("Looking for namelist schema in '%s'", spath)
#        schema_file = os.path.join(spath, "entry_id_namelist.xsd")
        schema_file = os.path.join(spath, "entry_id_pg.xsd")
        if os.path.isfile(schema_file):
            break
        # end if
        schema_file = None
    # end for
    code_files = list()
    meta_files = list()
    groups = set()
    errors = list()
    retcode = 0
    file_ok = True
    for nlfile in namelist_files:
        logger.info("Reading CAM physics namelist file, '%s'", nlfile)
        # Validate the XML file
        try:
            emsg = "Invalid namelist XML file, {}".format(nlfile)
            file_ok = validate_xml_file(nlfile, schema_file, "", logger)
        except CCPPError as ccpperr:
            cemsg = "{}".format(ccpperr).split('\n')[0]
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
            if error_on_no_validate:
                raise CCPPError(emsg)
            # end if
            logger.error(emsg)
            retcode += 1
        else:
            # Throw exception on error
            _, nlxml = read_xml_file(nlfile)
            basename = os.path.splitext(os.path.basename(nlfile))[0]
            retvals = process_xml_file(nlxml, basename, outdir, nlfile_arg,
                                       mpicom_arg, mpiroot_arg, mpiisroot_arg,
                                       logunit_arg, indent, logger)
            code, meta, nlgroups, file_errors = retvals
            if file_errors:
                errors.append(file_errors)
            else:
                code_files.append(code)
                meta_files.append(meta)
                groups.update(nlgroups)
            # end if
        # end if
    # end if
    if errors:
        raise NamelistError("\n".join(errors))
    # end if
    return retcode, code_files, meta_files, sorted(groups)

def main():
    """Function to execute when module called as a script"""
    args = parse_command_line(sys.argv[1:], __doc__)
    if args.output_dir is None:
        outdir = os.getcwd()
    else:
        outdir = args.output_dir
    # end if
#    if args.debug:
#        loglevel = logging.DEBUG
#    elif args.quiet:
#        loglevel = logging.ERROR
#    else:
#        loglevel = logging.INFO
#    # end if

    retvals = gen_namelist_files(args.namelist_file, outdir,
                                 args.nlfile_arg, args.mpicom_arg,
                                 args.mpiroot_arg, args.mpiisroot_arg,
                                 args.logunit_arg, args.indent)
    return retvals

###############################################################################
if __name__ == "__main__":
    __RETCODE, _META_FILES, _CODE_FILES, _GROUPS = main()
    sys.exit(__RETCODE)
