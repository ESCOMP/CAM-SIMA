#!/usr/bin/env python

"""
Read CAM registry and produce data and metadata files
"""

# Python library imports
import xml.etree.ElementTree as ET # pylint: disable=unused-import
import os
import os.path
import re
import argparse
import sys
import logging
from collections import OrderedDict

# Find and include the ccpp-framework scripts directory
__CURRDIR = os.path.abspath(os.path.dirname(__file__))
__CAMROOT = os.path.abspath(os.path.join(__CURRDIR, os.pardir, os.pardir))
__CPFROOT = os.path.join(__CAMROOT, "ccpp_framework")
sys.path.append(os.path.join(__CPFROOT, 'scripts'))

# CCPP framework imports
# pylint: disable=wrong-import-position
from parse_tools import validate_xml_file, find_schema_version, read_xml_file
from parse_tools import init_log, CCPPError, ParseInternalError
from fortran_tools import FortranWriter
# pylint: enable=wrong-import-position

def convert_to_long_name(standard_name):
    """Convert <standard_name> to an easier-to-read string
    NB: While this is similar to the CCPP conversion, they do not have to
        have the same form or functionality"""
    return standard_name[0].upper() + re.sub("_", " ", standard_name[1:])

###############################################################################
class TypeEntry(object):
###############################################################################
    "Simple type to capture a type and its source module name"

    def __init__(self, ttype, module, ddt=None):
        """Initialize TypeEntry"""
        self.__type = ttype
        self.__module = module
        self.__ddt = ddt # The actual DDT object, if <ttype> is a DDT

    @property
    def type_type(self):
        """Return type string"""
        return self.__type

    @property
    def module(self):
        """Return module where this type is defined or None for an intrinsic"""
        return self.__module

    @property
    def ddt(self):
        """Return DDT object, or None"""
        return self.__ddt

###############################################################################
class TypeRegistry(dict):
###############################################################################
    """Dictionary of known types. DDTs are associated with the module
    where they are defined"""

    def __init__(self):
        """Initialize TypeRegistry object with intrinsic Fortran types"""
        super(TypeRegistry, self).__init__()
        self['character'] = TypeEntry('character', None)
        self['complex'] = TypeEntry('complex', None)
        self['integer'] = TypeEntry('integer', None)
        self['logical'] = TypeEntry('logical', None)
        self['real'] = TypeEntry('real', None)

    def known_type(self, test_type):
        """Return type and a module name where <test_type> is defined
        or None if <test_type> is not in this TypeRegistry"""
        ttype = test_type.lower()
        if ttype in self:
            return self[ttype]
        # End if
        return None

    def add_type(self, new_type, type_module, type_ddt=None):
        """Add a new type, <new_type>, defined in <type_module> to
        this registry"""
        ttype = new_type
        if ttype in self:
            emsg = 'Trying to add {} to registry, already defined in {}'
            raise ValueError(emsg.format(new_type, self[ttype].module))
        # End if
        self[ttype] = TypeEntry(new_type, type_module, type_ddt)

###############################################################################
class Variable(object):
###############################################################################
    # pylint: disable=too-many-instance-attributes
    """Registry variable
    >>> Variable(ET.fromstring('<variable kind="kind_phys" local_name="u" standard_name="east_wind" type="real" units="m s-1"><dimensions>ccpp_constant_one:horizontal_dimension:two</dimensions></variable>'), TypeRegistry(), VarDict("foo", "module", None), None) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Illegal dimension string, ccpp_constant_one:horizontal_dimension:two, in u, step not allowed.
    >>> Variable(ET.fromstring('<variable kind="kind_phys" local_name="u" standard_name="east_wind" type="real" units="m s-1"><dims>horizontal_dimension</dims></variable>'), TypeRegistry(), VarDict("foo", "module", None), None) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Unknown Variable content, dims
    >>> Variable(ET.fromstring('<variable kkind="kind_phys" local_name="u" standard_name="east_wind" type="real" units="m s-1">></variable>'), TypeRegistry(), VarDict("foo", "module", None), None) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Bad variable attribute, 'kkind', for 'u'
    """

    # Constant dimensions
    __CONSTANT_DIMENSIONS = {'ccpp_constant_one' : 1, 'ccpp_constant_zero' : 0}

    __VAR_ATTRIBUTES = ["access", "allocatable", "dycore", "extends",
                        "kind", "local_name", "name", "standard_name",
                        "type", "units", "version"]

    def __init__(self, var_node, known_types, vdict, logger):
        # pylint: disable=too-many-locals
        """Initialize a Variable from registry XML"""
        # Check attributes
        self.__local_name = var_node.get('local_name')
        for att in var_node.attrib:
            if att not in Variable.__VAR_ATTRIBUTES:
                emsg = "Bad variable attribute, '{}', for '{}'"
                raise CCPPError(emsg.format(att, self.local_name))
            # End if
        # End for
        self.__standard_name = var_node.get('standard_name')
        self.__units = var_node.get('units')
        ttype = var_node.get('type')
        vtype = known_types.known_type(ttype)
        self.__kind = var_node.get('kind', default="")
        if vtype:
            self.__type = vtype
            # We cannot have a kind property with a DDT type2
            if self.module and self.kind:
                emsg = "kind attribute illegal for DDT type {}"
                raise CCPPError(emsg.format(self.var_type))
            # End if
        else:
            emsg = '{} is an unknown Variable type, {}'
            raise CCPPError(emsg.format(self.__local_name, ttype))
        # End if
        self.__access = var_node.get('access', default='public')
        if self.__access == "protected":
            self.__access = "public"
            self.__protected = True
        else:
            self.__protected = False
        # End if
        self.__allocatable = var_node.get('allocatable', default="none")
        if self.__allocatable == "none":
            self.__allocatable = ""
        # End if
        self.__dimensions = list()
        self.__def_dims_str = ""
        self.__initial_value = None
        self.__long_name = ''
        for attrib in var_node:
            if attrib.tag == 'dimensions':
                self.__dimensions = [x.strip() for x in attrib.text.split(' ')
                                     if x]
                def_dims = list() # Dims used for variable declarations
                for dim in self.dimensions:
                    if dim.count(':') > 1:
                        emsg = "Illegal dimension string, '{},' in '{}'"
                        emsg += ', step not allowed.'
                        raise CCPPError(emsg.format(dim, self.local_name))
                    # End if
                    if self.allocatable in ("", "parameter", "target"):
                        # We need to find a local variable for every dimension
                        dimstrs = [x.strip() for x in dim.split(':')]
                        ldimstrs = list()
                        for ddim in dimstrs:
                            lname = Variable.constant_dimension(ddim)
                            if not lname:
                                var = vdict.find_variable_by_standard_name(ddim)
                                if var:
                                    lname = var.local_name
                                # End if
                            # End if
                            if not lname:
                                emsg = "Dimension, '{}', not found for '{}'"
                                raise CCPPError(emsg.format(ddim,
                                                            self.local_name))
                            # End if
                            ldimstrs.append(lname)
                        # End for
                        def_dims.append(':'.join(ldimstrs))
                    else:
                        # We need to allocate this array
                        def_dims.append(':')
                    # End if
                # End for
                if def_dims:
                    self.__def_dims_str = '(' + ', '.join(def_dims) + ')'
                # End if
            elif attrib.tag == 'long_name':
                self.__long_name = attrib.text
            elif attrib.tag == 'initial_value':
                self.__initial_value = attrib.text
            else:
                emsg = "Unknown Variable content, '{}'"
                raise CCPPError(emsg.format(attrib.tag))
            # End if
        # End for
        # Some checks
        if (self.allocatable == 'parameter') and (not self.__initial_value):
            emsg = "parameter, '{}', does not have an initial value"
            raise CCPPError(emsg.format(self.local_name))
        # End if
        if (self.allocatable == 'pointer') and (not self.__initial_value):
            # Initialize pointer to NULL if no initial value
            self.__initial_value = "NULL()"
        # End if
        # Maybe fix up type string
        if self.module:
            self.__type_string = 'type({})'.format(self.var_type)
        elif self.kind:
            self.__type_string = '{}({})'.format(self.var_type, self.kind)
        else:
            self.__type_string = '{}'.format(self.var_type)
        # End if
        if logger:
            dmsg = 'Found registry Variable, {} ({})'
            logger.debug(dmsg.format(self.__local_name, self.__standard_name))
        # End if

    def write_metadata(self, outfile):
        """Write out this variable as CCPP metadata"""
        if self.access != "private":
            outfile.write('[ {} ]\n'.format(self.local_name))
            outfile.write('  {} = {}\n'.format('standard_name',
                                               self.standard_name))
            if self.__long_name:
                outfile.write('  {} = {}\n'.format('long_name', self.long_name))
            # End if
            outfile.write('  {} = {}\n'.format('units', self.units))
            if self.kind:
                outfile.write('  {} = {} | {} = {}\n'.format('type',
                                                             self.var_type,
                                                             'kind', self.kind))
            else:
                outfile.write('  {} = {}\n'.format('type', self.var_type))
            # End if
            outfile.write('  {} = {}\n'.format('dimensions',
                                               self.dimension_string))
            if (self.allocatable == "parameter") or self.protected:
                outfile.write('  protected = True\n')
            # End if
        # End if

    def write_definition(self, outfile, access, indent,
                         maxtyp=0, maxacc=0, maxall=0, has_protect=False):
        """Write the definition for this variable to <outfile>
        with indent, <indent>.
        <access> is the current public/private scope.
        <maxtyp> is the maximum padding to use for the type declaration.
        <maxacc> is the maximum padding to use for any access specification.
        <maxall> is the maximum padding to use for any allocation-type spec.
        <has_protect> specifies whether to leave space for a protected string.
            Note that if <has_protect> is False, output of the protected
            attribute is suppressed (e.g., for a DDT, even 'protected'
            variables cannot have the protected attribute.
        """
        # Protected string
        if has_protect:
            if self.protected:
                pro_str = "protected"
                has_pro = True
            else:
                pro_str = "         "
                has_pro = False
            # End if
        else:
            pro_str = ""
            has_pro = False
        # End if
        # Allocation string
        if self.allocatable:
            apad = ' '*max(0, maxall - len(self.allocatable))
            if has_pro:
                all_str = self.allocatable + ", " + apad
            else:
                all_str = self.allocatable + apad
            # End if
            have_all = True
        else:
            all_str = ' '*(maxall + 2)
            have_all = False
        # End if
        # Access string
        if self.access == access:
            acc_str = ' '*(maxacc + 2)
            have_vis = False
        else:
            vpad = ' '*max(0, maxacc - len(self.access))
            if have_all or has_pro:
                acc_str = self.access + ", " + vpad
            else:
                acc_str = self.access + vpad
            # End if
            have_vis = True
        # End if
        # Type string
        tpad = ' '*max(0, maxtyp - len(self.type_string))
        if have_all or have_vis or has_pro:
            tpad = ", " + tpad
        # End if
        type_str = self.type_string + tpad
        # Initial value
        if self.initial_value:
            if self.allocatable == "pointer":
                init_str = " => {}".format(self.initial_value)
            else:
                init_str = " = {}".format(self.initial_value)
            # End if
        else:
            init_str = ""
        # End if
        if self.long_name:
            comment = ' ! ' + self.__local_name + ": " + self.long_name
        else:
            comment = (' ! ' + self.__local_name + ": " +
                       convert_to_long_name(self.standard_name))
        # End if
        outfile.write(comment, indent)
        outfile.write("{}{}{}{} :: {}{}{}".format(type_str, acc_str,
                                                  all_str, pro_str,
                                                  self.local_name,
                                                  self.__def_dims_str,
                                                  init_str), indent)

    def write_allocate_routine(self, outfile, indent,
                               init_var, reall_var, ddt_str):
        """Write the code to allocate and initialize this Variable
        <init_var> is a string to use to write initialization test code.
        <reall_var> is a string to use to write reallocate test code.
        <ddt_str> is a prefix string (e.g., state%).
        <known_types> is a TypeRegistry.
        """
        # Be careful about dimensions, scalars have none, not '()'
        if self.dimensions:
            dimension_string = self.dimension_string
        else:
            dimension_string = ''
        # End if
        my_ddt = self.__type.ddt
        if my_ddt: # This is a DDT object, allocate entries
            subi = indent
            sub_ddt_str = '{}{}%'.format(ddt_str, self.local_name)
            if dimension_string:
                subi += 1
                emsg = "Arrays of DDT objects not implemented"
                raise ParseInternalError(emsg)
            # End if
            for var in my_ddt.variable_list():
                var.write_allocate_routine(outfile, subi,
                                           init_var, reall_var, sub_ddt_str)
        else:
            # Do we need to allocate this variable?
            lname = '{}{}'.format(ddt_str, self.local_name)
            if self.allocatable == "pointer":
                all_type = 'associated'
            elif self.allocatable == "allocatable":
                all_type = 'allocated'
            else:
                all_type = ''
            # End if
            if all_type:
                outfile.write("if ({}({})) then".format(all_type, lname),
                              indent)
                outfile.write("if ({}) then".format(reall_var), indent+1)
                outfile.write("deallocate({})".format(lname), indent+2)
                if self.allocatable == "pointer":
                    outfile.write("nullify({})".format(lname), indent+2)
                # End if
                outfile.write("else", indent+1)
                emsg = 'subname//": {} is already {}'.format(lname, all_type)
                emsg += ', cannot allocate"'
                outfile.write("call endrun({})".format(emsg), indent+2)
                outfile.write("end if", indent+1)
                outfile.write("end if", indent)
                outfile.write("allocate({}{})".format(lname, dimension_string),
                              indent)
            # End if
            if self.allocatable != "parameter":
                # Initialize the variable
                if self.var_type.lower() == 'real':
                    init_val = 'nan'
                elif self.var_type.lower() == 'integer':
                    init_val = 'HUGE(1)'
                elif self.var_type.lower() == 'character':
                    init_val = '""'
                else:
                    init_val = ''
                # End if
                if init_val:
                    outfile.write("if ({}) then".format(init_var), indent)
                    outfile.write("{} = {}".format(lname, init_val), indent+1)
                    outfile.write("end if", indent)
                    # End if
                # End if
            # End if

    @classmethod
    def constant_dimension(cls, dim):
        """Return dimension value if <dim> is a constant dimension, else None"""
        if dim.lower() in Variable.__CONSTANT_DIMENSIONS:
            dim_val = Variable.__CONSTANT_DIMENSIONS[dim.lower()]
        else:
            dim_val = None
        # End if
        return dim_val

    @property
    def local_name(self):
        """Return the local (variable) name for this variable"""
        return self.__local_name

    @property
    def standard_name(self):
        """Return the standard_name for this variable"""
        return self.__standard_name

    @property
    def units(self):
        """Return the units for this variable"""
        return self.__units

    @property
    def var_type(self):
        """Return the variable type for this variable"""
        return self.__type.type_type

    @property
    def module(self):
        """Return the module where this variable is defined"""
        return self.__type.module

    @property
    def kind(self):
        """Return the kind for this variable"""
        return self.__kind

    @property
    def allocatable(self):
        """Return the allocatable attribute (if any) for this variable"""
        return self.__allocatable

    @property
    def access(self):
        """Return the access attribute for this variable"""
        return self.__access

    @property
    def protected(self):
        """Return True iff this variable is protected"""
        return self.__protected

    @property
    def dimensions(self):
        """Return the dimensions for this variable"""
        return self.__dimensions

    @property
    def dimension_string(self):
        """Return the dimension_string for this variable"""
        return '(' + ', '.join(self.dimensions) + ')'

    @property
    def long_name(self):
        """Return the long_name for this variable"""
        return self.__long_name

    @property
    def initial_value(self):
        """Return the initial_value for this variable"""
        return self.__initial_value

    @property
    def type_string(self):
        """Return the type_string for this variable"""
        return self.__type_string

###############################################################################
class VarDict(OrderedDict):
###############################################################################
    """Ordered dictionary of registry variables"""

    def __init__(self, name, ttype, logger):
        """Initialize a registry variable dictionary"""
        super(VarDict, self).__init__()
        self.__name = name
        self.__type = ttype
        self.__logger = logger
        self.__standard_names = list()
        self.__dimensions = set() # All known dimensions for this dictionary

    @property
    def name(self):
        """Return the name of this dictionary (usually the module name)"""
        return self.__name

    @property
    def module_type(self):
        """Return the module type (e.g., host, module) for this dictionary"""
        return self.__type

    @property
    def known_dimensions(self):
        """Return the set of known dimensions for this dictionary"""
        return self.__dimensions

    def add_variable(self, newvar):
        """Add a variable if it does not conflict with existing entries"""
        local_name = newvar.local_name
        std_name = newvar.standard_name
        if local_name.lower() in self:
            # We already have a matching variable, error!
            emsg = "duplicate variable local_name, '{}', in {}"
            ovar = self[local_name]
            if (ovar is not None) and (ovar.standard_name != std_name):
                emsg2 = ", already defined with standard_name, '{}'"
                emsg += emsg2.format(ovar.standard_name)
            # End if
            raise CCPPError(emsg.format(local_name, self.name))
        # End if
        if std_name.lower() in self.__standard_names:
            # We have a standard name collision, error!
            emsg = "duplicate variable standard_name, '{}' from '{}' in '{}'"
            ovar = None
            for testvar in self.variable_list():
                if testvar.standard_name.lower() == std_name.lower():
                    ovar = testvar
                    break
                # End if
            # End for
            if ovar is not None:
                emsg2 = ", already defined with local_name, '{}'"
                emsg += emsg2.format(ovar.local_name)
            # End if
            raise CCPPError(emsg.format(std_name, local_name, self.name))
        # End if
        self[local_name.lower()] = newvar
        self.__standard_names.append(std_name.lower())
        for dim in newvar.dimensions:
            dimstrs = [x.strip() for x in dim.split(':')]
            for ddim in dimstrs:
                lname = Variable.constant_dimension(ddim)
                if not lname:
                    self.__dimensions.add(dim.lower())
                # End if
            # End for
        # End for

    def find_variable_by_local_name(self, local_name):
        """Return this dictionary's variable matching local name, <local_name>.
        Return None if not found."""
        lname = local_name.lower()
        if lname in self:
            fvar = self[lname]
        else:
            if self.__logger:
                lmsg = 'Local name, {}, not found in {}'
                self.__logger.debug(lmsg.format(local_name, self.name))
            # End if
            fvar = None
        # End if
        return fvar

    def find_variable_by_standard_name(self, std_name):
        """Return this dictionary's variable matching standard name, <std_name>.
        Return None if not found."""
        sname = std_name.lower()
        fvar = None
        for var in self.variable_list():
            if sname == var.standard_name.lower():
                fvar = var
                break
            # End if
        # End for
        if (not fvar) and self.__logger:
            lmsg = 'Standard name, {}, not found in {}'
            self.__logger.debug(lmsg.format(std_name, self.name))
        # End if
        return fvar

    def remove_variable(self, std_name):
        """Remove <std_name> from the dictionary.
        Ignore if <std_name> is not in dict
        """
        var = self.find_variable_by_standard_name(std_name)
        if var:
            del self[var.local_name.lower()]
            # NB: Do not remove standard_name, it is still an error
        else:
            if self.__logger:
                lmsg = 'Cannot remove {} from {}, variable not found.'
                self.__logger.debug(lmsg.format(std_name, self.name))
            # End if
        # End if

    def variable_list(self):
        """Return a list of this dictionary's variables"""
        return self.values()

    def write_metadata(self, outfile):
        """Write out the variables in this dictionary as CCPP metadata"""
        outfile.write('[ccpp_table]\n')
        outfile.write('  name = {}\n'.format(self.name))
        outfile.write('  type = {}\n'.format(self.module_type))
        for var in self.variable_list():
            var.write_metadata(outfile)
        # End if

    def write_definition(self, outfile, access, indent):
        """Write the definition for the variables in this dictionary to
        <outfile> with indent, <indent>.
        <access> is the current public/private scope.
        """
        maxtyp = 0
        maxacc = 0
        maxall = 0
        has_prot = False
        vlist = self.variable_list()
        for var in vlist:
            maxtyp = max(maxtyp, len(var.type_string))
            if var.access != access:
                maxacc = max(maxacc, len(var.access))
            # End if
            maxall = max(maxall, len(var.allocatable))
            has_prot = has_prot or var.protected
        # End for
        for var in vlist:
            var.write_definition(outfile, access, indent, maxtyp=maxtyp,
                                 maxacc=maxacc, maxall=maxall,
                                 has_protect=has_prot)
        # End for

###############################################################################
class DDT(object):
###############################################################################
    """Registry DDT"""

    def __init__(self, ddt_node, known_types, var_dict, dycore, logger):
        """Initialize a DDT from registry XML (<ddt_node>)
        <var_dict> is the dictionary where variables referenced in <ddt_node>
        must reside. Each DDT variable is removed from <var_dict>

        >>> DDT(ET.fromstring('<ddt type="physics_state">><dessert>ice_cream</dessert></ddt>'), TypeRegistry(), VarDict("foo", "module", None), 'eul', None) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        CCPPError: Unknown DDT element type, 'dessert', in 'physics_state'
        """
        self.__type = ddt_node.get('type')
        self.__logger = logger
        self.__data = list()
        extends = ddt_node.get('extends', default=None)
        if extends is None:
            self.__extends = None
        else:
            self.__extends = known_types.known_type(extends)
        # End if
        if extends and (not self.__extends):
            emsg = ("DDT, '{}', extends type '{}', however, this type is "
                    "not known")
            raise CCPPError(emsg.format(self.ddt_type, extends))
        # End if
        self.__bindc = ddt_node.get('bindC', default=False)
        if self.__extends and self.__bindc:
            emsg = ("DDT, '{}', cannot have both 'extends' and 'bindC' "
                    "attributes")
            raise CCPPError(emsg.format(self.ddt_type))
        # End if
        self.__private = ddt_node.get('private', default=False)
        for attrib in ddt_node:
            if attrib.tag == 'data':
                varname = attrib.text
                include_var = True
                attrib_dycores = [x.strip().lower() for x in
                                  attrib.get('dycore', default="").split(',')
                                  if x]
                if attrib_dycores and (dycore not in attrib_dycores):
                    include_var = False
                # End if
                if include_var:
                    var = var_dict.find_variable_by_standard_name(varname)
                    if var:
                        self.__data.append(var)
                        var_dict.remove_variable(varname)
                    else:
                        emsg = ("Variable, '{}', not found for DDT, '{}', "
                                "in '{}'")
                        raise CCPPError(emsg.format(varname, self.ddt_type,
                                                    var_dict.name))
                    # End if
                # End if
            else:
                emsg = "Unknown DDT element type, '{}', in '{}'"
                raise CCPPError(emsg.format(attrib.tag, self.ddt_type))
            # End if
        # End for

    def variable_list(self):
        """Return the variable list for this DDT"""
        vlist = list(self.__data)
        if self.__extends:
            vlist.extend(self.__extends.ddt.variable_list())
        # End if
        return vlist

    def write_metadata(self, outfile):
        """Write out this DDT as CCPP metadata"""
        outfile.write('[ccpp_table]\n')
        outfile.write('  name = {}\n'.format(self.ddt_type))
        outfile.write('  type = ddt\n')
        for var in self.__data:
            var.write_metadata(outfile)
        # End if

    def write_definition(self, outfile, access, indent):
        """Write out the Fortran definition for this DDT

        >>> DDT(ET.fromstring('<ddt type="physics_state">>></ddt>'), TypeRegistry(), VarDict("foo", "module", None), 'eul', None).write_definition(None, 'public', 0) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        CCPPError: DDT, 'physics_state', has no member variables
        """
        # It is an error to have no member variables
        if not self.__data:
            emsg = "DDT, '{}', has no member variables"
            raise CCPPError(emsg.format(self.ddt_type))
        # End if
        my_acc = 'private' if self.private else 'public'
        if self.extends:
            acc_str = ', extends({})'.format(self.extends.type_type)
        elif self.bindC:
            acc_str = ', bind(C)'
        elif my_acc != access:
            acc_str = ', {}'.format(my_acc)
        else:
            acc_str = ''
        # End if
        outfile.write("type{} :: {}".format(acc_str, self.ddt_type), indent)
        maxtyp = max([len(x.type_string) for x in self.__data])
        maxacc = max([len(x.access) for x in self.__data
                      if x.access != 'private'])
        maxall = max([len(x.allocatable) for x in self.__data])
        for var in self.__data:
            var.write_definition(outfile, my_acc, indent+1,
                                 maxtyp=maxtyp, maxacc=maxacc,
                                 maxall=maxall, has_protect=False)
        # End if
        outfile.write("end type {}\n".format(self.ddt_type), indent)

    @property
    def ddt_type(self):
        """Return this DDT's type"""
        return self.__type

    @property
    def private(self):
        """Return True iff this DDT is private"""
        return self.__private

    @property
    def extends(self):
        """Return this DDT's parent class, if any"""
        return self.__extends

    @property
    def bindC(self): # pylint: disable=invalid-name
        """Return True iff this DDT has the bind(C) attribute"""
        return self.__bindc

###############################################################################
class File(object):
###############################################################################
    """Object describing a file object in a registry file

    >>> File(ET.fromstring('<file name="physics_types" type="module"><use module="ccpp_kinds"/></file>'), TypeRegistry(), 'eul', "", None) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Illegal use entry, no reference
    >>> File(ET.fromstring('<file name="physics_types" type="module"><use reference="kind_phys"/></file>'), TypeRegistry(), 'eul', "", None) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Illegal use entry, no module
    >>> File(ET.fromstring('<file name="physics_types" type="module"><user reference="kind_phys"/></file>'), TypeRegistry(), 'eul', "", None) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Unknown registry File element, 'user'
    """

    # Some data for sorting dimension names
    __dim_order = {'horizontal_dimension' : 1,
                   'vertical_layer_dimension' : 2,
                   'vertical_interface_dimension' : 3,
                   'number_of_constituents' : 4}
    __min_dim_key = 5 # For sorting unknown dimensions

    def __init__(self, file_node, known_types, dycore, config, logger):
        """Initialize a File object from a registry node (XML)"""
        self.__var_dict = VarDict(file_node.get('name'), file_node.get('type'),
                                  logger)
        self.__name = file_node.get('name')
        self.__type = file_node.get('type')
        self.__known_types = known_types
        self.__ddts = OrderedDict()
        self.__use_statements = list()
        for obj in file_node:
            if obj.tag == 'variable':
                newvar = Variable(obj, self.__known_types, self.__var_dict,
                                  logger)
                self.__var_dict.add_variable(newvar)
            elif obj.tag == 'ddt':
                newddt = DDT(obj, self.__known_types, self.__var_dict,
                             dycore, logger)
                dmsg = "Adding DDT {} from {} as a known type"
                dmsg = dmsg.format(newddt.ddt_type, self.__name)
                logging.debug(dmsg)
                self.__ddts[newddt.ddt_type] = newddt
                self.__known_types.add_type(newddt.ddt_type,
                                            self.__name, newddt)
            elif obj.tag == 'use':
                module = obj.get('module', default=None)
                if not module:
                    raise CCPPError('Illegal use entry, no module')
                # End if
                ref = obj.get('reference', default=None)
                if not ref:
                    raise CCPPError('Illegal use entry, no reference')
                # End if
                self.__use_statements.append((module, ref))
            else:
                emsg = "Unknown registry File element, '{}'"
                raise CCPPError(emsg.format(obj.tag))
            # End if
        # End for

    def write_metadata(self, outdir, logger):
        """Write out the variables in this file as CCPP metadata"""
        ofilename = os.path.join(outdir, "{}.meta".format(self.name))
        logger.info("Writing registry metadata file, {}".format(ofilename))
        with open(ofilename, "w") as outfile:
            # Write DDTs defined in this file
            for ddt in self.__ddts.values():
                ddt.write_metadata(outfile)
            # End if
            # Write Variables defined in this file
            self.__var_dict.write_metadata(outfile)
        # End with

    @classmethod
    def dim_sort_key(cls, dim_name):
        """Return an integer sort key for <dim_name>"""
        if dim_name not in File.__dim_order:
            key = File.__min_dim_key
            File.__min_dim_key += 1
            File.__dim_order[dim_name] = key
        # End if
        return File.__dim_order[dim_name]

    def write_source(self, outdir, indent, logger):
        """Write out source code for the variables in this file"""
        ofilename = os.path.join(outdir, "{}.F90".format(self.name))
        logger.info("Writing registry source file, {}".format(ofilename))
        with FortranWriter(ofilename, "w", indent=indent) as outfile:
            # Define the module header
            outfile.write('module {}\n'.format(self.name), 0)
            # Use statements (if any)
            module_list = list() # tuple of (module, type)
            for var in self.__var_dict.variable_list():
                mod = var.module
                if mod and (mod.lower() != self.name.lower()):
                    module_list.append((mod, var.var_type))
                # End if
            # End for
            # Add any DDT types
            for ddt in self.__ddts.values():
                for var in ddt.variable_list():
                    mod = var.module
                    if mod and (mod.lower() != self.name.lower()):
                        module_list.append((mod, var.var_type))
                    # End if
                # End for
            # End for
            # Add in any explicit use entries from the registry
            for ref in self.__use_statements:
                module_list.append(ref)
            # End if
            if module_list:
                maxlen = max([len(x[0]) for x in module_list])
            else:
                maxlen = 0 # Don't really need this
            # End if
            for module in module_list:
                mod = module[0]
                mtype = module[1]
                pad = ' '*(maxlen - len(mod))
                outfile.write('use {},{} only: {}'.format(mod, pad, mtype), 1)
            # End for
            # More boilerplate
            outfile.write("\nimplicit none\nprivate\n", 0)
            # Write DDTs defined in this file
            for ddt in self.__ddts.values():
                ddt.write_definition(outfile, 'private', 1)
            # End if
            # Write Variables defined in this file
            self.__var_dict.write_definition(outfile, 'private', 1)
            # Write data management subroutine declarations
            outfile.write('', 0)
            outfile.write('!! public interfaces', 0)
            outfile.write('public :: {}'.format(self.allocate_routine_name()),
                          1)
            # End of module header
            outfile.write("\nCONTAINS\n", 0)
            # Write data management subroutines
            self.write_allocate_routine(outfile)
            # End of module
            outfile.write('\nend module {}'.format(self.name), 0)
        # End with

    def allocate_routine_name(self):
        """Return the name of the allocate routine for this module"""
        return 'allocate_{}_fields'.format(self.name)

    def write_allocate_routine(self, outfile):
        """Write a subroutine to allocate all the data in this module"""
        subname = self.allocate_routine_name()
        args = list(self.__var_dict.known_dimensions)
        args.sort(key=File.dim_sort_key) # Attempt at a consistent interface
        init_var = 'set_to_nan'
        args.append('{}_in'.format(init_var))
        reall_var = 'reallocate'
        args.append('{}_in'.format(reall_var))
        outfile.write('subroutine {}({})'.format(subname, ', '.join(args)), 1)
        # Use statements
        nanmods = 'nan => shr_infnan_nan, assignment(=)'
        outfile.write('use shr_infnan_mod,   only: {}'.format(nanmods), 2)
        outfile.write('use cam_abortutils,   only: endrun', 2)
        # Dummy arguments
        outfile.write('!! Dummy arguments', 2)
        for arg in args:
            if (init_var in arg) or (reall_var in arg):
                typ = 'logical'
                opt = ', optional, '
            else:
                typ = 'integer'
                opt = ',           '
            # End if
            outfile.write('{}{}intent(in) :: {}'.format(typ, opt, arg), 2)
        # End for
        outfile.write('', 0)
        outfile.write('!! Local variables', 2)
        outfile.write('logical                     :: {}'.format(init_var), 2)
        outfile.write('logical                     :: {}'.format(reall_var), 2)
        subn_str = 'character(len=*), parameter :: subname = "{}"'
        outfile.write(subn_str.format(subname), 2)
        outfile.write('', 0)
        outfile.write('! Set optional argument values', 2)
        outfile.write('if (present({}_in)) then'.format(init_var), 2)
        outfile.write('{iv} = {iv}_in'.format(iv=init_var), 3)
        outfile.write('else', 2)
        outfile.write('{} = .true.'.format(init_var), 3)
        outfile.write('end if', 2)
        outfile.write('if (present({}_in)) then'.format(reall_var), 2)
        outfile.write('{iv} = {iv}_in'.format(iv=reall_var), 3)
        outfile.write('else', 2)
        outfile.write('{} = .false.'.format(reall_var), 3)
        outfile.write('end if', 2)
        outfile.write('', 0)
        for var in self.__var_dict.variable_list():
            var.write_allocate_routine(outfile, 2, init_var, reall_var, '')
        # End for
        outfile.write('end subroutine {}'.format(subname), 1)

    @property
    def name(self):
        """Return this File's name"""
        return self.__name

    @property
    def file_type(self):
        """Return this File's type"""
        return self.__type

###############################################################################
def parse_command_line(args, description):
###############################################################################
    """Parse and return the command line arguments when
    this module is executed"""
    parser = argparse.ArgumentParser(description=description,
                                     formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument("registry_file",
                        metavar='<registry XML filename>',
                        type=str, help="XML file with CAM registry library")
    parser.add_argument("--dycore", type=str, required=True,
                        metavar='DYCORE (required)',
                        help="Dycore (EUL, FV, FV3, MPAS, SE)")
    parser.add_argument("--config", type=str, required=True,
                        metavar='CONFIG (required)',
                        help=("Comma-separated onfig items "
                              "(e.g., gravity_waves=True)"))
    parser.add_argument("--output-dir", type=str, default=None,
                        help="Directory where output files will be written")
    group = parser.add_mutually_exclusive_group()
    group.add_argument("--debug", action='store_true',
                       help='Increase logging', default=False)
    group.add_argument("--quiet", action='store_true',
                       help='Disable logging except for errors', default=False)
    parser.add_argument("--indent", type=int, default=3,
                        help="Indent level for Fortran source code")
    pargs = parser.parse_args(args)
    return pargs

###############################################################################
def write_registry_files(registry, dycore, config, outdir, indent, logger):
###############################################################################
    """Write metadata and source files for <registry>

    >>> File(ET.fromstring('<variable name="physics_types" type="module"><user reference="kind_phys"/></variable>'), TypeRegistry(), 'eul', "", None) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Unknown registry object type, 'variable'
    """
    files = list()
    known_types = TypeRegistry()
    for section in registry:
        sec_name = section.get('name')
        logger.info("Parsing {}, {}, from registry".format(section.tag,
                                                           sec_name))
        if section.tag == 'file':
            files.append(File(section, known_types, dycore, config, logger))
        else:
            emsg = "Unknown registry object type, '{}'"
            raise CCPPError(emsg.format(section.tag))
        # End if
    # End for
    # Make sure output directory exists
    if not os.path.exists(outdir):
        os.makedirs(outdir)
    # End if
    # Write metadata
    for file_ in files:
        file_.write_metadata(outdir, logger)
        file_.write_source(outdir, indent, logger)
    # End for

###############################################################################
def gen_registry(registry_file, dycore, config, outdir, indent, loglevel,
                 error_on_no_validate=False):
###############################################################################
    """Parse a registry XML file and generate source code and metadata.
    <dycore> is the name of the dycore for DP coupling specialization.
    <config> is a dictionary containing other configuration items for
       souce code customization.
    Source code and metadata is output to <outdir>.
    <indent> is the number of spaces between indent levels.
    Set <debug> to True for more logging output."""
    logger = init_log(os.path.basename(__file__), loglevel)
    logger.info("Reading CAM registry from {}".format(registry_file))
    _, registry = read_xml_file(registry_file)
    # Validate the XML file
    version = find_schema_version(registry)
    if (loglevel > 0) and (loglevel <= logging.DEBUG):
        verstr = '.'.join([str(x) for x in version])
        logger.debug("Found registry version, v{}".format(verstr))
    # End if
    try:
        emsg = "Invalid registry file, {}".format(registry_file)
        file_ok = validate_xml_file(registry_file, 'registry', version,
                                    logger, schema_path=__CURRDIR,
                                    error_on_noxmllint=error_on_no_validate)
    except CCPPError as ccpperr:
        cemsg = "{}".format(ccpperr).split('\n')[0]
        if cemsg[0:12] == 'Execution of':
            xstart = cemsg.find("'")
            if xstart >= 0:
                xend = cemsg[xstart + 1:].find("'") + xstart + 1
                emsg += '\n' + cemsg[xstart + 1:xend]
            # End if (else, just keep original message)
        elif cemsg[0:18] == 'validate_xml_file:':
            emsg += "\n" + cemsg
        # End if
        file_ok = False
    # End if
    if not file_ok:
        if error_on_no_validate:
            raise CCPPError(emsg)
        # End if
        logger.error(emsg)
        retcode = 1
    else:
        library_name = registry.get('name')
        dmsg = "Parsing registry, {}".format(library_name)
        logger.debug(dmsg)
        write_registry_files(registry, dycore, config, outdir, indent, logger)
        retcode = 0 # Throw exception on error
    # End if
    return retcode

def main():
    """Function to execute when module called as a script"""
    args = parse_command_line(sys.argv[1:], __doc__)
    if args.output_dir is None:
        outdir = os.getcwd()
    else:
        outdir = args.output_dir
    # End if
    if args.debug:
        loglevel = logging.DEBUG
    elif args.quiet:
        loglevel = logging.ERROR
    else:
        loglevel = logging.INFO
    # End if
    retcode = gen_registry(args.registry_file, args.dycore.lower(),
                           args.config, outdir, args.indent, loglevel)
    return retcode

###############################################################################
if __name__ == "__main__":
    __RETCODE = main()
    sys.exit(__RETCODE)
