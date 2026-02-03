#!/usr/bin/env python3

"""
Read CAM registry and produce data and metadata files

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
__CURRDIR = os.path.abspath(os.path.dirname(__file__))
__CAMROOT = os.path.abspath(os.path.join(__CURRDIR, os.pardir, os.pardir))
__SPINSCRIPTS = os.path.join(__CAMROOT, "ccpp_framework", 'scripts')
if __SPINSCRIPTS not in sys.path:
    sys.path.append(__SPINSCRIPTS)
# end if
_ALL_STRINGS_REGEX = re.compile(r'[A-Za-z][A-Za-z_0-9]+')
_FORTRAN_NUMERIC_REGEX = re.compile(r'^[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?(_kind_phys)?$')

# CCPP framework imports
# pylint: disable=wrong-import-position
from framework_env import CCPPFrameworkEnv
from parse_tools import validate_xml_file, read_xml_file
from parse_tools import find_schema_file, find_schema_version
from parse_tools import init_log, CCPPError, ParseInternalError
from metadata_table import parse_metadata_file
from fortran_tools import FortranWriter
# pylint: enable=wrong-import-position

###############################################################################
def convert_to_long_name(standard_name):
###############################################################################
    """Convert <standard_name> to an easier-to-read string
    NB: While this is similar to the CCPP conversion, they do not have to
        have the same form or functionality"""
    return standard_name[0].upper() + re.sub("_", " ", standard_name[1:])

###############################################################################
def write_ccpp_table_header(name, outfile):
###############################################################################
    """Write the standard Fortran comment block for a CCPP header
    (module, type, scheme)."""
    outfile.write(rf"!> \section arg_table_{name}  Argument Table", 0)
    outfile.write(rf"!! \htmlinclude {name}.html", 0)

###############################################################################
class TypeEntry:
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
        super().__init__()
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
        # end if
        return None

    def add_type(self, new_type, type_module, type_ddt=None):
        """Add a new type, <new_type>, defined in <type_module> to
        this registry"""
        ttype = new_type
        if ttype in self:
            emsg = 'Trying to add {} to registry, already defined in {}'
            raise ValueError(emsg.format(new_type, self[ttype].module))
        # end if
        self[ttype] = TypeEntry(new_type, type_module, type_ddt)

    def is_ddt_type(self, ttype):
        """Return <ttype>'s DDT type if it is a DDT, otherwise, return None"""
        if ttype in self:
            return self[ttype].ddt
        return None

    def known_ddt_names(self):
        """Return a list of the known DDT types in this registry"""
        ddt_names = []
        for key in self:
            ddt = self[key].ddt
            if ddt:
                ddt_names.append(ddt.ddt_type)
            # end if
        # end for
        return ddt_names

###############################################################################
class VarBase:
###############################################################################
    """VarBase contains elements common to variables, arrays, and
    array elements."""

    __pointer_def_init = "NULL()"
    __pointer_type_str = "pointer"

    def __init__(self, elem_node, local_name, dimensions, known_types,
                 type_default, units_default="", kind_default='', dycore='',
                 protected=False, index_name='', local_index_name='',
                 local_index_name_str='', alloc_default='none',
                 tstep_init_default=False):
        self.__local_name = local_name
        self.__dimensions = dimensions
        self.__units = elem_node.get('units', default=units_default)
        ttype = elem_node.get('type', default=type_default)
        self.__type = known_types.known_type(ttype)
        self.__kind = elem_node.get('kind', default=kind_default)
        self.__standard_name = elem_node.get('standard_name')
        self.__long_name = ''
        self.__initial_value = ''
        self.__initial_value_match = 0
        self.__initial_val_vars = set()
        self.__ic_names = None
        self.__elements = []
        self.__protected = protected
        self.__index_name = index_name
        self.__local_index_name = local_index_name
        self.__local_index_name_str = local_index_name_str
        self.__allocatable = elem_node.get('allocatable', default=alloc_default)
        self.__constituent = elem_node.get("constituent", default=False)
        self.__advected    = elem_node.get("advected", default=False)
        self.__tstep_init  = elem_node.get("phys_timestep_init_zero",
                                           default=tstep_init_default)
        if self.__allocatable == "none":
            self.__allocatable = ""
        # end if
        if self.__type:
            # We cannot have a kind property with a DDT type2
            if self.is_ddt and self.kind:
                emsg = "kind attribute illegal for DDT type {}"
                raise CCPPError(emsg.format(self.var_type))
            # end if (else this type is okay)
        else:
            emsg = '{} is an unknown Variable type, {}'
            raise CCPPError(emsg.format(local_name, ttype))
        # end if
        for attrib in elem_node:
            if attrib.tag == 'long_name':
                self.__long_name = attrib.text
            elif attrib.tag == 'initial_value':
                # Figure out if we should use this initial_value
                #  If the number of matching attributes is greater than the
                #  default or a previous match, pick this one
                matches = 0
                if not attrib.keys():
                    self.__initial_value = attrib.text
                # end if (no attributes, this is the default)
                for att in attrib.keys():
                    # Check each attribute (for now, this is only the dycore)
                    if att == 'dyn':
                        dycore_list = attrib.get('dyn').lower().split(',')
                        if dycore in dycore_list:
                            matches += 1
                        # end if (dycore matches)
                    # end if (check the dycore)
                # end for
                if matches == self.__initial_value_match and matches != 0:
                    emsg = f"Unclear which initial_value to use for {local_name}. There are at least two configurations with {matches} matching attributes"
                    raise CCPPError(emsg)
                elif matches > self.__initial_value_match:
                    # Use this initial_value (for now)
                    self.__initial_value_match = matches
                    self.__initial_value = attrib.text
                # end if (number of matches)
            elif attrib.tag == 'ic_file_input_names':
                #Separate out string into list:
                self.__ic_names = [x.strip() for x in attrib.text.split(' ') if x]

            # end if (just ignore other tags)
        # end for
        if ((not self.initial_value) and
            (self.allocatable == VarBase.__pointer_type_str)):
            self.__initial_value = VarBase.__pointer_def_init
        # end if

    def write_metadata(self, outfile):
        """Write out this variable as CCPP metadata"""
        outfile.write(f'[ {self.local_name} ]\n')
        outfile.write(f'  standard_name = {self.standard_name}\n')
        if self.long_name:
            outfile.write(f'  long_name = {self.long_name}\n')
        # end if
        outfile.write(f'  units = {self.units}\n')
        if self.is_ddt:
            outfile.write(f'  type = {self.var_type}\n')
        elif self.kind:
            outfile.write(f'  type = {self.var_type} | kind = {self.kind}\n')
        else:
            outfile.write(f'  type = {self.var_type}\n')
        # end if
        outfile.write(f'  dimensions = {self.dimension_string}\n')
        if self.is_advected:
            outfile.write('  advected = true\n')
        # end if

    def write_initial_value(self, outfile, indent, init_var, ddt_str, physconst_vars,
                            tstep_init=False):
        """Write the code for the initial value of this variable
        and/or one of its array elements."""
        #Check if variable has associated array index
        #local string:
        if self.local_index_name_str:
            #Then write variable with local index name:
            # pylint: disable=no-member
            var_name = f'{ddt_str}{self.local_index_name_str}'
            # pylint: enable=no-member
        else:
            #Otherwise, use regular local variable name:
            var_name = f'{ddt_str}{self.local_name}'
        if self.allocatable == VarBase.__pointer_type_str:
            if self.initial_value == VarBase.__pointer_def_init:
                init_val = ''
            else:
                init_val = self.initial_value
            # end if
        else:
            init_val = self.initial_value
        # end if
        if not init_val:
            if self.var_type.lower() == 'real':
                init_val = 'nan'
            elif self.var_type.lower() == 'integer':
                init_val = 'unset_int'
            elif self.var_type.lower() == 'character':
                init_val = 'unset_str'
            elif self.var_type.lower() == 'complex':
                init_val = '(nan, nan)'
            else:
                init_val = ''
            # end if
        # end if
        #Time-step initialization, which is always zero for numerical quantities,
        #empty strings for characters, and "false" for logical quantities:
        if tstep_init:
            if self.var_type.lower() == 'real':
                if self.kind:
                    outfile.write(f'{var_name} = 0._{self.kind}', indent)
                else:
                    outfile.write(f'{var_name} = 0.0', indent)
            elif self.var_type.lower() == 'integer':
                if self.kind:
                    outfile.write(f'{var_name} = 0_{self.kind}', indent)
                else:
                    outfile.write(f'{var_name} = 0'.format(var_name), indent)
            elif self.var_type.lower() == 'character':
                if self.kind:
                    outfile.write(f'{var_name} = {self.kind}_""', indent)
                else:
                    outfile.write(f'{var_name} = ""', indent)
            elif self.var_type.lower() == 'complex':
                if self.kind:
                    outfile.write(f'{var_name} = (0._{self.kind}, 0._{self.kind})', indent)
                else:
                    outfile.write(f'{var_name} = (0.0, 0.0)', indent)
            elif self.var_type.lower() == 'logical':
                outfile.write('{var_name} = .false.', indent)
            else:
                emsg = 'Variable "{}" is of type "{}", which is not a supported type\n'
                emsg += 'for use with "phys_timestep_init_zero".'
                raise TypeError(emsg.format(var_name, self.var_type))
            # end if
        elif init_val:
            outfile.write(f"if ({init_var}) then", indent)
            outfile.write(f"{var_name} = {init_val}", indent+1)
            if self.initial_val_vars and self.initial_val_vars.issubset(physconst_vars):
                outfile.write(f"call mark_as_initialized('{self.standard_name}')", indent+1)
            # end if
            outfile.write("end if", indent)
        # end if

    def set_initial_val_vars(self, init_vars):
        """Set the initial value variable set"""
        self.__initial_val_vars = init_vars

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
    def kind(self):
        """Return the kind for this variable"""
        return self.__kind

    @property
    def allocatable(self):
        """Return the allocatable attribute (if any) for this variable"""
        return self.__allocatable

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
    def initial_val_vars(self):
        """Return the initial_val_var_array for this variable"""
        return self.__initial_val_vars

    @property
    def ic_names(self):
        """Return list of possible Initial Condition (IC) file input names"""
        return self.__ic_names

    @property
    def protected(self):
        """Return True iff this variable is protected"""
        return self.__protected

    @property
    def elements(self):
        """Return elements list for this variable"""
        return self.__elements

    @property
    def index_name(self):
        """Return the standard name of this array element's index value"""
        return self.__index_name

    @property
    def local_index_name(self):
        """Rturn the local name of this array element's index value"""
        return self.__local_index_name

    @property
    def local_index_name_str(self):
        """
        Return the array element's name, but with the local name for the
        index instead of the standard name
        """
        return self.__local_index_name_str

    @property
    def module(self):
        """Return the module where this variable is defined"""
        return self.__type.module

    @property
    def var_type(self):
        """Return the variable type for this variable"""
        return self.__type.type_type

    @property
    def is_ddt(self):
        """Return True iff this variable is a derived type"""
        return self.__type.ddt

    @property
    def is_constituent(self):
        """Return True if this variable is a constituent"""
        return self.__constituent

    @property
    def is_advected(self):
        """Return True if this variable is advected"""
        return self.__advected

    @property
    def tstep_init(self):
        """Return True if variable will be set to zero every physics timestep."""
        return self.__tstep_init

###############################################################################
class ArrayElement(VarBase):
###############################################################################
    """Documented array element of a registry Variable"""

    def __init__(self, elem_node, parent_name, dimensions, known_types,
                 parent_type, parent_kind, parent_units, parent_alloc,
                 parent_tstep_init, vdict):
        """Initialize the Arary Element information by identifying its
        metadata properties
        """

        self.__parent_name = parent_name
        index_name = elem_node.get('index_name')
        pos = elem_node.get('index_pos')

        # Check to make sure we know about this index
        var = vdict.find_variable_by_standard_name(index_name)
        if not var:
            emsg = "Unknown array index, '{}', in '{}'"
            raise CCPPError(emsg.format(self.index_name, parent_name))
        # end if
        #Save index variable local name:
        local_index_name = var.local_name
        # Find the location of this element's index
        found = False
        my_dimensions = []
        my_index = []
        my_local_index = []
        for dim_ind, dim in enumerate(dimensions):
            if dimensions[dim_ind] == pos:
                found = True
                my_index.append(index_name)
                my_local_index.append(var.local_name)
            else:
                my_index.append(':')
                my_local_index.append(':')
                my_dimensions.append(dim)
            # end if
        # end for
        if found:
            self.__index_string = ','.join(my_index)
            #write array string with local variable index name,
            #instead of the standard variable index name.
            #This is used to write initialization code in fortran
            #with the correct index variable name:
            local_index_string = ','.join(my_local_index)
            local_index_name_str = \
                f'{parent_name}({local_index_string})'
        else:
            emsg = "Cannot find element dimension, '{}' in {}({})"
            raise CCPPError(emsg.format(index_name, parent_name,
                                        ', '.join(dimensions)))
        # end if
        local_name = f'{parent_name}({self.index_string})'
        super().__init__(elem_node, local_name, my_dimensions,
                                           known_types, parent_type,
                                           units_default=parent_units,
                                           kind_default=parent_kind,
                                           index_name=index_name,
                                           local_index_name=local_index_name,
                                           local_index_name_str=local_index_name_str,
                                           alloc_default=parent_alloc,
                                           tstep_init_default=parent_tstep_init)

    @property
    def index_string(self):
        """Return the metadata string for locating this element's index in
        its parent array"""
        return self.__index_string

    @property
    def parent_name(self):
        """Return this element's parent's local name"""
        return self.__parent_name

###############################################################################
class Variable(VarBase):
###############################################################################
    # pylint: disable=too-many-instance-attributes
    """Registry variable
    >>> Variable(ET.fromstring('<variable kind="kind_phys" local_name="u" standard_name="east_wind" type="real" units="m s-1"><dimensions>ccpp_constant_one:horizontal_dimension:two</dimensions></variable>'), TypeRegistry(), VarDict("foo", "module", None), 'se', None) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Illegal dimension string, ccpp_constant_one:horizontal_dimension:two, in u, step not allowed.
    >>> Variable(ET.fromstring('<variable kind="kind_phys" local_name="u" standard_name="east_wind" type="real" units="m s-1"><dims>horizontal_dimension</dims></variable>'), TypeRegistry(), VarDict("foo", "module", None), 'se', None) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Unknown Variable content, dims
    >>> Variable(ET.fromstring('<variable kkind="kind_phys" local_name="u" standard_name="east_wind" type="real" units="m s-1"></variable>'), TypeRegistry(), VarDict("foo", "module", None), 'se', None) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Bad variable attribute, 'kkind', for 'u'
    >>> Variable(ET.fromstring('<variable kind="kind_phys" local_name="u" standard_name="east_wind" type="real" units="m s-1" allocatable="target"><dimensions>horizontal_dimension vertical_dimension</dimensions></variable>'), TypeRegistry(), VarDict("foo", "module", None), 'se', None) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Dimension, 'vertical_dimension', not found for 'u'
    """

    # Constant dimensions
    __CONSTANT_DIMENSIONS = {'ccpp_constant_one' : 1, 'ccpp_constant_zero' : 0}

    __VAR_ATTRIBUTES = ["access", "advected", "allocatable",
                        "constituent", "dycore", "extends",
                        "kind", "local_name", "name",
                        "phys_timestep_init_zero", "standard_name",
                        "type", "units", "version"]

    def __init__(self, var_node, known_types, vdict, dycore, logger):
        # pylint: disable=too-many-locals
        """Initialize a Variable from registry XML"""
        local_name = var_node.get('local_name')
        allocatable = var_node.get('allocatable', default="none")
        # Check attributes
        for att in var_node.attrib:
            if att not in Variable.__VAR_ATTRIBUTES:
                emsg = "Bad variable attribute, '{}', for '{}'"
                raise CCPPError(emsg.format(att, local_name))
            # end if
        # end for
        ttype = var_node.get('type')
        self.__access = var_node.get('access', default='public')
        if self.__access == "protected":
            self.__access = "public"
            protected = True
        else:
            protected = False
        # end if
        my_dimensions = []
        self.__def_dims_str = ""
        for attrib in var_node:
            if attrib.tag == 'dimensions':
                my_dimensions = [x.strip() for x in attrib.text.split(' ') if x]
                def_dims = [] # Dims used for variable declarations
                for dim in my_dimensions:
                    if dim.count(':') > 1:
                        emsg = "Illegal dimension string, '{},' in '{}'"
                        emsg += ', step not allowed.'
                        raise CCPPError(emsg.format(dim, local_name))
                    # end if
                    if allocatable in ("", "parameter", "target"):
                        # We need to find a local variable for every dimension
                        dimstrs = [x.strip() for x in dim.split(':')]
                        ldimstrs = []
                        for ddim in dimstrs:
                            lname = Variable.constant_dimension(ddim)
                            if not lname:
                                var = vdict.find_variable_by_standard_name(ddim)
                                if var:
                                    lname = var.local_name
                                # end if
                            # end if
                            if not lname:
                                emsg = "Dimension, '{}', not found for '{}'"
                                raise CCPPError(emsg.format(ddim,
                                                            local_name))
                            # end if
                            ldimstrs.append(lname)
                        # end for
                        def_dims.append(':'.join(ldimstrs))
                    else:
                        # We need to allocate this array
                        def_dims.append(':')
                    # end if
                # end for
                if def_dims:
                    self.__def_dims_str = '(' + ', '.join(def_dims) + ')'
                # end if

            elif attrib.tag == 'long_name':
                pass # picked up in parent
            elif attrib.tag == 'initial_value':
                pass # picked up in parent
            elif attrib.tag == 'element':
                pass # picked up in parent
            elif attrib.tag == 'ic_file_input_names':
                pass # picked up in parent
            else:
                emsg = "Unknown Variable content, '{}'"
                raise CCPPError(emsg.format(attrib.tag))
            # end if
        # end for
        # Initialize the base class
        super().__init__(var_node, local_name,
                                       my_dimensions, known_types, ttype,
                                       dycore=dycore, protected=protected)

        for attrib in var_node:
            # Second pass, only process array elements
            if attrib.tag == 'element':
                self.elements.append(ArrayElement(attrib, local_name,
                                                  my_dimensions, known_types,
                                                  ttype, self.kind,
                                                  self.units, allocatable,
                                                  self.tstep_init, vdict))

            # end if (all other processing done above)
        # end for
        # Some checks
        if (self.allocatable == 'parameter') and (not self.initial_value):
            emsg = "parameter, '{}', does not have an initial value"
            raise CCPPError(emsg.format(local_name))
        # end if
        # Maybe fix up type string
        if self.module:
            self.__type_string = f'type({self.var_type})'
        elif self.kind:
            self.__type_string = f'{self.var_type}({self.kind})'
        else:
            self.__type_string = f'{self.var_type}'
        # end if
        if logger:
            dmsg = f'Found registry Variable, {self.local_name} ({self.standard_name})'
            logger.debug(dmsg)
        # end if

    def write_metadata(self, outfile):
        """Write out this variable as CCPP metadata"""
        #If variable is a constituent,
        #then don't add to metadata file:
        if self.is_constituent:
            return
        # end if
        if self.access != "private":
            super().write_metadata(outfile)
            if (self.allocatable == "parameter") or self.protected:
                outfile.write('  protected = True\n')
            # end if
            for element in self.elements:
                element.write_metadata(outfile)
            # end for
        # end if

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
        #If variable is a constituent, then don't add
        #to source file:
        if self.is_constituent:
            return
        # end if
        # Protected string
        if has_protect and self.protected:
            pro_str = "protected"
            has_pro = True
        else:
            pro_str = "         "
            has_pro = False
        # end if
        # Allocation string
        if self.allocatable:
            apad = ' '*max(0, maxall - len(self.allocatable))
            if has_pro:
                all_str = self.allocatable + ", " + apad
            else:
                all_str = self.allocatable + apad
            # end if
            have_all = True
        else:
            all_str = ' '*(maxall + 2)
            have_all = False
        # end if
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
            # end if
            have_vis = True
        # end if
        # Type string
        tpad = ' '*max(0, maxtyp - len(self.type_string))
        if have_all or have_vis or has_pro:
            tpad = ", " + tpad
        # end if
        type_str = self.type_string + tpad
        # Initial value
        init_str = ""
        if self.initial_value:
            if self.allocatable == "pointer":
                init_str = f" => {self.initial_value}"
            elif not self.allocatable[0:11] == 'allocatable':
                init_str = f" = {self.initial_value}"
            # end if (no else, do not initialize allocatable fields)
        # end if
        if self.long_name:
            comment = ' ! ' + self.local_name + ": " + self.long_name
        else:
            comment = (' ! ' + self.local_name + ": " +
                       convert_to_long_name(self.standard_name))
        # end if
        outfile.write(comment, indent)
        var_dec_str = f"{type_str}{acc_str}{all_str}{pro_str} :: "
        var_dec_str += f"{self.local_name}{self.__def_dims_str}{init_str}"
        outfile.write(var_dec_str, indent)

    def write_allocate_routine(self, outfile, indent,
                               init_var, reall_var, ddt_str, physconst_vars):
        """Write the code to allocate and initialize this Variable
        <init_var> is a string to use to write initialization test code.
        <reall_var> is a string to use to write reallocate test code.
        <ddt_str> is a prefix string (e.g., state%).
        """
        #If variable is a constituent, then don't add
        #to source file:
        if self.is_constituent:
            return
        # end if
        # Be careful about dimensions, scalars have none, not '()'
        if self.dimensions:
            dimension_string = self.dimension_string
        else:
            dimension_string = ''
        # end if
        my_ddt = self.is_ddt
        if my_ddt: # This is a DDT object, allocate entries
            subi = indent
            sub_ddt_str = f'{ddt_str}{self.local_name}%'
            if dimension_string:
                subi += 1
                emsg = "Arrays of DDT objects not implemented"
                raise ParseInternalError(emsg)
            # end if
            for var in my_ddt.variable_list():
                var.write_allocate_routine(outfile, subi,
                                           init_var, reall_var, sub_ddt_str, physconst_vars)
        else:
            # Do we need to allocate this variable?
            lname = f'{ddt_str}{self.local_name}'
            if self.allocatable == "pointer":
                all_type = 'associated'
            elif self.allocatable == "allocatable":
                all_type = 'allocated'
            else:
                all_type = ''
            # end if
            if all_type:
                outfile.write(f"if ({all_type}({lname})) then", indent)
                outfile.write(f"if ({reall_var}) then", indent+1)
                outfile.write(f"deallocate({lname})", indent+2)
                if self.allocatable == "pointer":
                    outfile.write(f"nullify({lname})".format(lname), indent+2)
                # end if
                outfile.write("else", indent+1)
                emsg = f'subname//": {lname} is already {all_type}'
                emsg += ', cannot allocate"'
                outfile.write(f"call endrun({emsg})", indent+2)
                outfile.write("end if", indent+1)
                outfile.write("end if", indent)
                outfile.write(f"allocate({lname}{dimension_string})", indent)
            # end if
            if self.allocatable != "parameter":
                # Initialize the variable
                self.write_initial_value(outfile, indent, init_var, ddt_str, physconst_vars)
                for elem in self.elements:
                    if elem.initial_value:
                        elem.write_initial_value(outfile, indent,
                                                 init_var, ddt_str, physconst_vars)
                    # end if
                # end for
            # end if

    def write_tstep_init_routine(self, outfile, indent,
                                 ddt_str, physconst_vars, init_val=False):
        """
        Write the code to iniitialize this variable to zero at the
        start of each physics timestep.

        <ddt_str> is a prefix string (e.g., state%).
        <init_val> is an optional variable that forces the writing
                   of the variable initiliazation code even if not
                   directly specified in the registry itself.
        """
        #If variable is a constituent, then don't add
        #to source file:
        if self.is_constituent:
            return
        # end if
        # Be careful about dimensions, scalars have none, not '()'
        if self.dimensions:
            dimension_string = self.dimension_string
        else:
            dimension_string = ''
        # end if
        my_ddt = self.is_ddt
        if my_ddt: # This is a DDT object, initalize individual entries

            subi = indent
            sub_ddt_str = f'{ddt_str}{self.local_name}%'
            if dimension_string:
                emsg = "Arrays of DDT objects not implemented"
                raise ParseInternalError(emsg)
            # end if
            for var in my_ddt.variable_list():
                var.write_tstep_init_routine(outfile, subi, sub_ddt_str, physconst_vars,
                                             init_val=self.tstep_init)
        else:

            # Do nothing if a parameter
            if self.allocatable == "parameter":
                return

            # Check if variable should be initialized:
            if init_val or self.tstep_init:
                # Set variables needed for writing source code
                if self.long_name:
                    comment = ' ! ' + self.local_name + ": " + self.long_name
                else:
                    comment = (' ! ' + self.local_name + ": " +
                               convert_to_long_name(self.standard_name))

                # Write source code
                outfile.write("", 0)
                outfile.write(comment, indent)

                # Initialize the variable:
                self.write_initial_value(outfile, indent, "", ddt_str, physconst_vars,
                                         tstep_init=True)

            # end if

    @classmethod
    def constant_dimension(cls, dim):
        """Return dimension value if <dim> is a constant dimension, else None"""
        if dim.lower() in Variable.__CONSTANT_DIMENSIONS:
            dim_val = Variable.__CONSTANT_DIMENSIONS[dim.lower()]
        else:
            dim_val = None
        # end if
        return dim_val

    @property
    def type_string(self):
        """Return the type_string for this variable"""
        return self.__type_string

    @property
    def access(self):
        """Return the access attribute for this variable"""
        return self.__access

###############################################################################
class VarDict(OrderedDict):
###############################################################################
    """Ordered dictionary of registry variables"""

    def __init__(self, name, ttype, logger):
        """Initialize a registry variable dictionary"""
        super().__init__()
        self.__name = name
        self.__type = ttype
        self.__logger = logger
        self.__standard_names = []
        self.__dimensions = set() # All known dimensions for this dictionary
        self.__initial_value_vars = set() # All known initial value variables for this dict

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

    @property
    def known_initial_value_vars(self):
        """Return the set of known initial value variables for this dictionary"""
        return self.__initial_value_vars

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
            # end if
            raise CCPPError(emsg.format(local_name, self.name))
        # end if
        if std_name.lower() in self.__standard_names:
            # We have a standard name collision, error!
            emsg = "duplicate variable standard_name, '{}' from '{}' in '{}'"
            ovar = None
            for testvar in self.variable_list():
                if testvar.standard_name.lower() == std_name.lower():
                    ovar = testvar
                    break
                # end if
            # end for
            if ovar is not None:
                emsg2 = ", already defined with local_name, '{}'"
                emsg += emsg2.format(ovar.local_name)
            # end if
            raise CCPPError(emsg.format(std_name, local_name, self.name))
        # end if
        self[local_name.lower()] = newvar
        self.__standard_names.append(std_name.lower())
        if not newvar.is_constituent: #Don't add dimensions if a constituent
            for dim in newvar.dimensions:
                dimstrs = [x.strip() for x in dim.split(':')]
                for ddim in dimstrs:
                    lname = Variable.constant_dimension(ddim)
                    if not lname:
                        self.__dimensions.add(dim.lower())
                    # end if
                # end for
            # end for
        # end if (constituent)
        # Parse out all strings from initial value
        all_strings = _ALL_STRINGS_REGEX.findall(newvar.initial_value)
        init_val_vars = set()
        excluded_initializations = {'null', 'nan', 'false', 'true'}
        # Exclude NULL and nan variables and valid Fortran numeric values that pass the string regex (e.g. 1.e36, -3.2e5)
        for var in all_strings:
            if var.lower() not in excluded_initializations and not _FORTRAN_NUMERIC_REGEX.match(newvar.initial_value):
                init_val_vars.add(var)
            # end if
        # end if
        self.__initial_value_vars.update(init_val_vars)
        newvar.set_initial_val_vars(init_val_vars)

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
            # end if
            fvar = None
        # end if
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
            # end if
        # end for
        if (not fvar) and self.__logger:
            lmsg = 'Standard name, {}, not found in {}'
            self.__logger.debug(lmsg.format(std_name, self.name))
        # end if
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
            # end if
        # end if

    def variable_list(self):
        """Return a list of this dictionary's variables"""
        return self.values()

    def write_metadata(self, outfile):
        """Write out the variables in this dictionary as CCPP metadata"""
        outfile.write('[ccpp-table-properties]\n')
        outfile.write(f'  name = {self.name}\n')
        outfile.write(f'  type = {self.module_type}\n')
        outfile.write('[ccpp-arg-table]\n')
        outfile.write(f'  name = {self.name}\n')
        outfile.write(f'  type = {self.module_type}\n')
        for var in self.variable_list():
            var.write_metadata(outfile)
        # end if

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
            # end if
            maxall = max(maxall, len(var.allocatable))
            has_prot = has_prot or var.protected
        # end for
        write_ccpp_table_header(self.name, outfile)
        for var in vlist:
            var.write_definition(outfile, access, indent, maxtyp=maxtyp,
                                 maxacc=maxacc, maxall=maxall,
                                 has_protect=has_prot)
        # end for


    def check_initial_values(self, physconst_vars,use_statements):
        """Raise an error if there are any initial values that are set to
        non-"used" and/or non-"physconst" variables"""
        for var in self.known_initial_value_vars:
            if var not in physconst_vars and not any(second == var for _, second in use_statements):
                emsg = f"Initial value '{var}' is not a physconst variable"
                emsg += " or does not have necessary use statement"
                raise CCPPError(emsg)
            # end if
        # end for

###############################################################################
class DDT:
###############################################################################
    """Registry DDT"""

    def __init__(self, ddt_node, known_types, var_dict, dycore):
        """Initialize a DDT from registry XML (<ddt_node>)
        <var_dict> is the dictionary where variables referenced in <ddt_node>
        must reside. Each DDT variable is removed from <var_dict>

        >>> DDT(ET.fromstring('<ddt type="physics_state"><dessert>ice_cream</dessert></ddt>'), TypeRegistry(), VarDict("foo", "module", None), 'eul') #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        CCPPError: Unknown DDT element type, 'dessert', in 'physics_state'
        """
        self.__type = ddt_node.get('type')
        self.__data = []
        extends = ddt_node.get('extends', default=None)
        if extends is None:
            self.__extends = None
        else:
            self.__extends = known_types.known_type(extends)
        # end if
        if extends and (not self.__extends):
            emsg = ("DDT, '{}', extends type '{}', however, this type is "
                    "not known")
            raise CCPPError(emsg.format(self.ddt_type, extends))
        # end if
        self.__bindc = ddt_node.get('bindC', default=False)
        if self.__extends and self.__bindc:
            emsg = ("DDT, '{}', cannot have both 'extends' and 'bindC' "
                    "attributes")
            raise CCPPError(emsg.format(self.ddt_type))
        # end if
        self.__private = ddt_node.get('private', default=False)
        for attrib in ddt_node:
            if attrib.tag == 'data':
                varname = attrib.text
                include_var = True
                attrib_dycores = [x.strip().lower() for x in
                                  attrib.get('dyn', default="").split(',')
                                  if x]
                if attrib_dycores and (dycore not in attrib_dycores):
                    include_var = False
                # end if
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
                    # end if
                # end if
            else:
                emsg = "Unknown DDT element type, '{}', in '{}'"
                raise CCPPError(emsg.format(attrib.tag, self.ddt_type))
            # end if
        # end for

    def variable_list(self):
        """Return the variable list for this DDT"""
        vlist = list(self.__data)
        if self.__extends:
            vlist.extend(self.__extends.ddt.variable_list())
        # end if
        return vlist

    def write_metadata(self, outfile):
        """Write out this DDT as CCPP metadata"""
        outfile.write('[ccpp-table-properties]\n')
        outfile.write(f'  name = {self.ddt_type}\n')
        outfile.write('  type = ddt\n')
        outfile.write('[ccpp-arg-table]\n')
        outfile.write(f'  name = {self.ddt_type}\n')
        outfile.write('  type = ddt\n')
        for var in self.__data:
            var.write_metadata(outfile)
        # end if

    def write_definition(self, outfile, access, indent):
        """Write out the Fortran definition for this DDT

        >>> DDT(ET.fromstring('<ddt type="physics_state">></ddt>'), TypeRegistry(), VarDict("foo", "module", None), 'eul').write_definition(None, 'public', 0) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        CCPPError: DDT, 'physics_state', has no member variables
        """
        # It is an error to have no member variables
        if not self.__data:
            emsg = "DDT, '{}', has no member variables"
            raise CCPPError(emsg.format(self.ddt_type))
        # end if
        my_acc = 'private' if self.private else 'public'
        if self.extends:
            acc_str = f', extends({self.extends.type_type})'
        elif self.bindC:
            acc_str = ', bind(C)'
        elif my_acc != access:
            acc_str = f', {my_acc}'
        else:
            acc_str = ''
        # end if
        # Write the CCPP header
        write_ccpp_table_header(self.ddt_type, outfile)
        # Write the type definition
        outfile.write(f"type{acc_str} :: {self.ddt_type}", indent)
        maxtyp = max([len(x.type_string) for x in self.__data])
        maxacc = max([len(x.access) for x in self.__data
                      if x.access != 'private'])
        maxall = max([len(x.allocatable) for x in self.__data])
        for var in self.__data:
            var.write_definition(outfile, my_acc, indent+1,
                                 maxtyp=maxtyp, maxacc=maxacc,
                                 maxall=maxall, has_protect=False)
        # end if
        outfile.write(f"end type {self.ddt_type}", indent)
        outfile.write("", 0)

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
class File:
###############################################################################
    """Object describing a file object in a registry file

    >>> File(ET.fromstring('<file name="physics_types" type="module"><use module="ccpp_kinds"/></file>'), TypeRegistry(), 'eul', None) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Illegal use entry, no reference
    >>> File(ET.fromstring('<file name="physics_types" type="module"><use reference="kind_phys"/></file>'), TypeRegistry(), 'eul', None) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Illegal use entry, no module
    >>> File(ET.fromstring('<file name="physics_types" type="module"><user reference="kind_phys"/></file>'), TypeRegistry(), 'eul', None) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Unknown registry File element, 'user'
    """

    def __init__(self, file_node, known_types, dycore,
                 logger, gen_code=True, file_path=None):
        """Initialize a File object from a registry node (XML)"""
        self.__var_dict = VarDict(file_node.get('name'), file_node.get('type'),
                                  logger)
        self.__name = file_node.get('name')
        self.__type = file_node.get('type')
        self.__known_types = known_types
        self.__ddts = OrderedDict()
        self.__use_statements = []
        self.__generate_code = gen_code
        self.__file_path = file_path
        self.__dycore = dycore
        for obj in file_node:
            if obj.tag in ['variable', 'array']:
                self.add_variable(obj, logger)
            elif obj.tag == 'ddt':
                newddt = DDT(obj, self.__known_types, self.__var_dict, dycore)
                self.add_ddt(newddt, logger=logger)
            elif obj.tag == 'use':
                module = obj.get('module', default=None)
                if not module:
                    raise CCPPError('Illegal use entry, no module')
                # end if
                ref = obj.get('reference', default=None)
                if not ref:
                    raise CCPPError('Illegal use entry, no reference')
                # end if
                self.__use_statements.append((module, ref))
            else:
                emsg = "Unknown registry File element, '{}'"
                raise CCPPError(emsg.format(obj.tag))
            # end if
        # end for

    def add_variable(self, var_node, logger):
        """Create a Variable from <var_node> and add to this File's
        variable dictionary"""
        newvar = Variable(var_node, self.__known_types, self.__var_dict,
                          self.__dycore, logger)
        self.__var_dict.add_variable(newvar)

    def add_ddt(self, newddt, logger=None):
        """Add <newddt> to this File's DDT dictionary and known types"""
        if logger:
            dmsg = "Adding DDT {} from {} as a known type"
            dmsg = dmsg.format(newddt.ddt_type, self.__name)
            logger.debug(dmsg)
        # end if
        if self.__known_types.known_type(newddt.ddt_type):
            raise CCPPError(f'Duplicate DDT entry, {newddt.ddt_type}')
        # end if
        self.__ddts[newddt.ddt_type] = newddt
        self.__known_types.add_type(newddt.ddt_type,
                                    self.__name, type_ddt=newddt)

    def variable_list(self):
        """Return a list of this File object's variables"""
        return self.var_dict.variable_list()

    def write_metadata(self, outdir, logger):
        """Write out the variables in this file as CCPP metadata"""
        ofilename = os.path.join(outdir, f"{self.name}.meta")
        logger.info(f"Writing registry metadata file, {ofilename}")
        with open(ofilename, "w", encoding='utf-8') as outfile:
            # Write DDTs defined in this file
            for ddt in self.__ddts.values():
                ddt.write_metadata(outfile)
            # end if
            # Write Variables defined in this file
            self.__var_dict.write_metadata(outfile)
        # end with

    def write_source(self, outdir, indent, logger, physconst_vars, var_module_dict):
        """Write out source code for the variables in this file"""
        ofilename = os.path.join(outdir, f"{self.name}.F90")
        logger.info(f"Writing registry source file, {ofilename}")
        file_desc = f"Variables for registry source file, {self.name}"
        with FortranWriter(ofilename, "w", file_desc,
                           self.name, indent=indent) as outfile:
            # Use statements (if any)
            module_list = [] # elements are a tuple, (module, type)
            for var in self.__var_dict.variable_list():
                mod = var.module
                if mod and (mod.lower() != self.name.lower()):
                    module_list.append((mod, var.var_type))
                # end if
            # end for
            # Add any DDT types
            for ddt in self.__ddts.values():
                for var in ddt.variable_list():
                    mod = var.module
                    if mod and (mod.lower() != self.name.lower()):
                        module_list.append((mod, var.var_type))
                    # end if
                # end for
            # end for
            # Add in any explicit use entries from the registry
            for ref in self.__use_statements:
                module_list.append(ref)
            # end if
            if module_list:
                maxlen = max([len(x[0]) for x in module_list])
            else:
                maxlen = 0 # Don't really need this
            # end if
            for module in module_list:
                mod = module[0]
                mtype = module[1]
                pad = ' '*(maxlen - len(mod))
                outfile.write(f'use {mod},{pad} only: {mtype}'.format(mod, pad, mtype), 1)
            # end for
            # More boilerplate
            outfile.write("", 0)
            outfile.write_preamble()
            # Write DDTs defined in this file
            for ddt in self.__ddts.values():
                ddt.write_definition(outfile, 'private', 1)
            # end if
            # Write Variables defined in this file
            self.__var_dict.write_definition(outfile, 'private', 1)
            # Write data management subroutine declarations
            outfile.write('', 0)
            outfile.write('!! public interfaces', 0)
            outfile.write(f'public :: {self.allocate_routine_name()}', 1)
            outfile.write(f'public :: {self.tstep_init_routine_name()}', 1)
            # end of module header
            outfile.end_module_header()
            outfile.write("", 0)
            # Write data management subroutines
            self.write_allocate_routine(outfile, physconst_vars, var_module_dict)
            self.write_tstep_init_routine(outfile, physconst_vars)

        # end with

    def allocate_routine_name(self):
        """Return the name of the allocate routine for this module"""
        return f'allocate_{self.name}_fields'

    def tstep_init_routine_name(self):
        """Return the name of the physics timestep init routine for this module"""
        return f"{self.name}_tstep_init"

    def write_allocate_routine(self, outfile, physconst_vars, var_module_dict):
        """Write a subroutine to allocate all the data in this module"""
        subname = self.allocate_routine_name()
        init_var = 'set_init_val'
        args = [f'{init_var}_in']
        reall_var = 'reallocate'
        args.append(f'{reall_var}_in')
        outfile.write(f'subroutine {subname}({", ".join(args)})', 1)

        # Use statements
        nanmods = 'nan => shr_infnan_nan, assignment(=)'
        outfile.write(f'use shr_infnan_mod,   only: {nanmods}', 2)
        outfile.write('use cam_abortutils,   only: endrun', 2)

        #Bring all host dimension variables
        #in via use statments:
        outfile.blank_line()
        for dim in sorted(self.__var_dict.known_dimensions):
            if dim in var_module_dict:
                dim_module = var_module_dict[dim][0]
                dim_loc_name = var_module_dict[dim][1]
                outfile.write(f'use {dim_module},   only: {dim}=>{dim_loc_name}', 2)

        #Bring in "num_advected" as well if needed, as the
        #standard name in the cam_constituents.meta file doesn't
        #match the standard name that is actually used to represent
        #all constituents. Please note that once the CCPP-framework
        #supports a separation between total and advected constituents
        #then this section of code will likely need to be modified:
        if ('number_of_ccpp_constituents' in self.__var_dict.known_dimensions):
            outfile.write("use cam_constituents,   only: number_of_ccpp_constituents=>num_constituents", 2)
        outfile.blank_line()

        # Dummy arguments
        outfile.write('!! Dummy arguments', 2)
        for arg in args:
            if (init_var in arg) or (reall_var in arg):
                typ = 'logical'
                opt = ', optional, '
            else:
                typ = 'integer'
                opt = ',           '
            # end if
            outfile.write(f'{typ}{opt}intent(in) :: {arg}', 2)
        # end for
        outfile.write('', 0)
        outfile.write('!! Local variables', 2)
        outfile.write(f'logical                     :: {init_var}', 2)
        outfile.write(f'logical                     :: {reall_var}', 2)
        subn_str = f'character(len=*), parameter :: subname = "{subname}"'
        outfile.write(subn_str, 2)
        outfile.write('', 0)
        outfile.write('! Set optional argument values', 2)
        outfile.write(f'if (present({init_var}_in)) then', 2)
        outfile.write(f'{init_var} = {init_var}_in', 3)
        outfile.write('else', 2)
        outfile.write(f'{init_var} = .true.', 3)
        outfile.write('end if', 2)
        outfile.write(f'if (present({reall_var}_in)) then', 2)
        outfile.write(f'{reall_var} = {reall_var}_in', 3)
        outfile.write('else', 2)
        outfile.write(f'{reall_var} = .false.', 3)
        outfile.write('end if', 2)
        outfile.write('', 0)
        for var in self.__var_dict.variable_list():
            var.write_allocate_routine(outfile, 2, init_var, reall_var, '', physconst_vars)
        # end for
        outfile.write(f'end subroutine {subname}', 1)

    def write_tstep_init_routine(self, outfile, physconst_vars):
        """
        Write a subroutine to initialize registered variables
        to zero at the beginning of each physics timestep.
        """
        subname = self.tstep_init_routine_name()
        outfile.write('', 0)
        outfile.write(f'subroutine {subname}()', 1)
        outfile.write('', 0)
        outfile.write('!! Local variables', 2)
        subn_str = f'character(len=*), parameter :: subname = "{subname}"'
        outfile.write(subn_str, 2)
        for var in self.__var_dict.variable_list():
            var.write_tstep_init_routine(outfile, 2, '', physconst_vars)
        # end for
        outfile.write('', 0)
        outfile.write(f'end subroutine {subname}', 1)

    @property
    def name(self):
        """Return this File's name"""
        return self.__name

    @property
    def file_type(self):
        """Return this File's type"""
        return self.__type

    @property
    def ddts(self):
        """Return list of DDTs in File"""
        return self.__ddts

    @property
    def var_dict(self):
        """Return variable dictionary in File"""
        return self.__var_dict

    @property
    def generate_code(self):
        """Return True if code and metadata should be generated for this File"""
        return self.__generate_code

    @property
    def use_statements(self):
        """Return list of use statements"""
        return self.__use_statements

    @property
    def file_path(self):
        """Return file path if provided, otherwise return None"""
        return self.__file_path

    def __str__(self):
        """Return printable string for this File object"""
        return f"<Registry {self.file_type} file: {self.name}>"

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
                        help="Dycore (EUL, FV, FV3, MPAS, SE, none)")
    parser.add_argument("--config", type=str, required=True,
                        metavar='CONFIG (required)',
                        help=("Comma-separated config items "
                              "(e.g., gravity_waves=True)"))
    parser.add_argument("--output-dir", type=str, default=None,
                        help="Directory where output files will be written")
    parser.add_argument("--source-mods", type=str, default=None,
                        help="A SourceMods directory location")
    parser.add_argument("--source-root", type=str, default=None,
                        help="Pathname of top of model code tree")
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
def metadata_file_to_files(file_path, known_types, dycore, run_env):
###############################################################################
    """Read the metadata file at <relative_file_path> and convert it to a
    registry File object.
    """
    known_ddts = known_types.known_ddt_names()
    mfiles = []
    var_module_dict = {} #Dictionary used to find relevant Fortran modules
    if os.path.exists(file_path):
        if run_env.logger:
            run_env.logger.info(f"Parsing metadata_file, '{file_path}'")
        # end if
        meta_tables = parse_metadata_file(file_path, known_ddts, run_env)
    else:
        emsg = f"Metadata file, '{file_path}', does not exist"
        raise CCPPError(emsg)
    # end if
    # Create a File object with no Variables
    for mtable in meta_tables:
        htype = mtable.table_type
        hname = mtable.table_name
        if htype not in ('host', 'module', 'ddt'):
            emsg = f"Metadata type, '{htype}' not supported."
            raise CCPPError(emsg)
        # end if
        section = f'<file name="{hname}" type="{htype}"></file>'
        sect_xml = ET.fromstring(section)
        mfile = File(sect_xml, known_types, dycore, run_env.logger,
                     gen_code=False, file_path=file_path)
        # Add variables
        # Note, we only support one section per table for host variables
        sections = mtable.sections()
        if sections: # CCPP Framework will check for a single section
            mheader = sections[0]
        else:
            emsg = f"Missing metadata section ([ccpp-arg-table]) for {hname}"
            raise CCPPError(emsg)
        # end if
        for var in mheader.variable_list(loop_vars=False, consts=False):
            local_name = var.get_prop_value('local_name')
            std_name   = var.get_prop_value('standard_name')
            vnode_str = f'<variable local_name="{local_name}"'
            vnode_str += f'\n          standard_name="{std_name}"'
            prop = var.get_prop_value('units')
            typ = var.get_prop_value('type')
            kind = var.get_prop_value('kind')
            vnode_str += f'\n          units="{prop}" type="{typ}"'
            if kind and (typ != kind):
                vnode_str += f'\n          kind="{kind}"'
            # end if
            if var.get_prop_value('protected'):
                vnode_str += '\n          access="protected"'
            # end if
            # End of variable attributes
            vnode_str += '>'
            dims = var.get_dimensions()
            if dims:
                vdims = []
                for dim in dims:
                    if dim[0:18] == 'ccpp_constant_one:':
                        vdims.append(dim[18:])
                    else:
                        vdims.append(dim)
                    # end if
                # end for
                vnode_str += f'\n  <dimensions>{" ".join(vdims)}'
                vnode_str += '</dimensions>'
            # end if
            vnode_str += '\n</variable>'
            var_node = ET.fromstring(vnode_str)
            mfile.add_variable(var_node, run_env.logger)
            # Add variable to module dictionary in case it
            # is needed during code generation:
            if std_name not in var_module_dict.keys():
                var_module_dict[std_name] = [mtable.module_name, local_name]
        # end for
        if htype == 'ddt':
            # We defined the variables, now create the DDT for them.
            vnode_str = f'<ddt type="{hname}">'
            for var in mheader.variable_list(loop_vars=False, consts=False):
                prop = var.get_prop_value('standard_name')
                vnode_str += f'\n  <data>{prop}</data>'
            # end for
            vnode_str += '\n</ddt>'
            var_node = ET.fromstring(vnode_str)
            new_ddt = DDT(var_node, known_types, mfile.var_dict, dycore)
            mfile.add_ddt(new_ddt, logger=run_env.logger)
        # end if
        mfiles.append(mfile)
    # end for
    return mfiles, var_module_dict

###############################################################################
def write_registry_files(registry, dycore, outdir, src_mod, src_root,
                         reg_dir, indent, logger):
###############################################################################
    """Write metadata and source files for <registry> to <outdir>
    <src_mod> is the location of the CAM SourceMods. Try this location first
        to locate a metadata file.
    <src_root> is useful if a metadata file path has "$SRCROOT"
    <reg_dir> is used as a parent path if a metadata file is a relative path.

    >>> File(ET.fromstring('<variable name="physics_types" type="module"><user reference="kind_phys"/></variable>'), TypeRegistry(), 'eul', None) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CCPPError: Unknown registry object type, 'variable'
    """
    files = []
    var_module_dict = {}
    known_types = TypeRegistry()
    # Create a fake CCPPFrameworkEnv object to contain the logger
    run_env = CCPPFrameworkEnv(logger, host_files='',
                               scheme_files='', suites='')
    for section in registry:
        sec_name = section.get('name')
        if sec_name:
            logger.info(f"Parsing {section.tag}, {sec_name}, from registry")
        # end if
        if section.tag == 'file':
            files.append(File(section, known_types, dycore, logger))
        elif section.tag == 'metadata_file':
            # Find the correct file path and parse that metadata file
            relative_file_path = section.text
            # First look in SourceMods
            if src_mod:
                file_path = os.path.join(src_mod,
                                         os.path.basename(relative_file_path))
            else:
                #If generate_registry_data.py is called from the command line,
                #but no '--source-mods' argument is given, then check if the
                #metadata file is present in the local directory instead.
                file_path = os.path.basename(relative_file_path)
            # end if
            if not os.path.exists(file_path):
                # Next, see if a substitution can be made
                if src_root:
                    file_path = relative_file_path.replace("$SRCROOT", src_root)
                else:
                    file_path = relative_file_path.replace("$SRCROOT", os.curdir)
                # end if
                # Make sure we have an absolute path
                if not os.path.isabs(file_path):
                    file_path = os.path.abspath(os.path.join(reg_dir,
                                                             file_path))
                # end if
            # end if
            meta_files, loc_var_mod_dict = metadata_file_to_files(file_path, known_types,
                                                                  dycore, run_env)
            files.extend(meta_files)
            var_module_dict.update(loc_var_mod_dict)
        else:
            emsg = "Unknown registry object type, '{}'"
            raise CCPPError(emsg.format(section.tag))
        # end if
    # end for
    # Make sure output directory exists
    if not os.path.exists(outdir):
        os.makedirs(outdir)
    # end if
    for file_ in files:
        # Check to see if any initial values for variables aren't "used" physconst vars
        # First pull out the physconst variables used for this file
        physconst_vars = set()
        for ref in file_.use_statements:
            if ref[0] == 'physconst':
                physconst_vars.add(ref[1])
            # end if
        # end for
        # Then check against the initial values in the variable dictionary
        # Check will raise an exception if there is a rogue variable
        file_.var_dict.check_initial_values(physconst_vars,file_.use_statements)
        # Generate metadata and source
        if file_.generate_code:
            file_.write_metadata(outdir, logger)
            file_.write_source(outdir, indent, logger, physconst_vars, var_module_dict)
        # end if
    # end for

    # Return list of File objects, for use in initialization code generation
    return files

###############################################################################
def _grab_initial_value(var_node):
###############################################################################
    """Helper function for _create_ic_name_list to retrieve the standard name
          and ic_names for the variable or array element, <var_node>."""
    stdname = None
    ic_names = None
    for attrib in var_node:
        if attrib.tag == 'ic_file_input_names':
            stdname = var_node.get('standard_name')
            ic_names = [x.strip() for x in attrib.text.split(' ') if x]
        # end if (ignore other tags for ic_names)
    # end for
    return stdname, ic_names

###############################################################################
def _create_ic_name_dict(registry):
###############################################################################
    """ Build a dictionary of IC-names (key = standard_name)
        These come either from an entry in the registry or from the
           variable's local name
        If this property is ever included in CCPP metadata, this
           section can be replaced by accessing the new metadata
           property and this routine will no longer be needed.
        This function returns a dictionary containing only the variables
           from the registry which have the initial_value property.
    """
    ic_name_dict = {}
    for section in registry:
        if section.tag == 'file':
            for obj in section:
                if obj.tag == 'variable':
                    stdname, ic_names = _grab_initial_value(obj)
                    # Skip duplicate check (done elsewhere for registry)
                    if stdname:
                        ic_name_dict[stdname] = ic_names
                    # end if
                elif obj.tag == 'array':
                    for subobj in obj:
                        if subobj.tag == 'element':
                            stdname, ic_names = _grab_initial_value(subobj)
                            # Skip duplicate check (see above)
                            if stdname:
                                ic_name_dict[stdname] = ic_names
                            # end if
                        # end if
                    # end for
                # end if (ignore other node types)
            # end for
        # end if (ignore other node types)
    # end for
    return ic_name_dict

###############################################################################
def _create_constituent_list(registry):
###############################################################################
    """
    Create a list of all constituents found in the registry.
    To be used by write_init_files.py - need to keep track
    of all constituent variables, not just ones required by
    CCPP metadata, to handle runtime constituents
    """
    constituent_list = []
    for section in registry:
        if section.tag == 'file':
            for obj in section:
                if obj.tag == 'variable':
                    if obj.get('constituent'):
                        stdname = obj.get('standard_name')
                        constituent_list.append(stdname)
                    # end if (ignore non-constituents)
                # end if (ignore other node types)
            # end for
        # end if (ignore other node types)
    # end for
    return constituent_list

###############################################################################
def _create_variables_with_initial_value_list(registry):
###############################################################################
    """
    Create a list of all variables with initial_value defined in the registry.
    To be used by write_init_files.py to allow these variables to
    not error when not found in the initial conditions file.
    """
    vars_init_value_list = []
    for section in registry:
        if section.tag == 'file':
            for obj in section:
                if obj.tag == 'variable':
                    for subobj in obj:
                        if subobj.tag == 'initial_value':
                            stdname = obj.get('standard_name')
                            vars_init_value_list.append(stdname)
                        # end if (only if initial_value node is found)
                    # end for
                # end if (ignore other node types)
            # end for
        # end if (ignore other node types)
    # end for
    return vars_init_value_list

###############################################################################
def gen_registry(registry_file, dycore, outdir, indent,
                 src_mod, src_root, loglevel=None, logger=None,
                 schema_paths=None, error_on_no_validate=False):
###############################################################################
    """Parse a registry XML file and generate source code and metadata.
    <dycore> is the name of the dycore for DP coupling specialization.
    <config> is a dictionary containing other configuration items for
       source code customization.
    Source code and metadata is output to <outdir>.
    <src_mod> is the location of the builds SourceMods/src.cam directory
    <src_root> is the top of the component tree
    <indent> is the number of spaces between indent levels.
    Set <debug> to True for more logging output."""
    if not logger:
        if not loglevel:
            loglevel = logging.INFO
        # end if
        logger = init_log(os.path.basename(__file__), loglevel)
    elif loglevel is not None:
        emsg = "gen_registry: Ignoring <loglevel> because logger is present"
        logger.debug(emsg)
    # end if
    if not schema_paths:
        schema_paths = [__CURRDIR]
    # end if
    logger.info("Reading CAM registry from %s", registry_file)
    _, registry = read_xml_file(registry_file)
    # Validate the XML file
    version = find_schema_version(registry)
    if 0 < logger.getEffectiveLevel() <= logging.DEBUG:
        verstr = '.'.join([str(x) for x in version])
        logger.debug("Found registry version, v%s", verstr)
    # end if
    schema_dir = None
    for spath in schema_paths:
        logger.debug("Looking for registry schema in '%s'", spath)
        schema_dir = find_schema_file("registry", version, schema_path=spath)
        if schema_dir:
            schema_dir = os.path.dirname(schema_dir)
            break
        # end if
    # end for
    try:
        emsg = f"Invalid registry file, {registry_file}"
        file_ok = validate_xml_file(registry_file, 'registry', version,
                                    logger, schema_path=schema_dir,
                                    error_on_noxmllint=error_on_no_validate)
    except CCPPError as ccpperr:
        emsg += f"\n{ccpperr}"
        file_ok = False
    # end try
    if not file_ok:
        if error_on_no_validate:
            raise CCPPError(emsg)
        # end if
        logger.error(emsg)
        retcode = 1
        files = None
        ic_names = None
        registry_constituents = None
        vars_init_value = None
    else:
        library_name = registry.get('name')
        emsg = f"Parsing registry, {library_name}"
        logger.debug(emsg)
        reg_dir = os.path.dirname(registry_file)
        files = write_registry_files(registry, dycore, outdir, src_mod,
                                     src_root, reg_dir, indent, logger)
        # See comment in _create_ic_name_dict
        ic_names = _create_ic_name_dict(registry)
        registry_constituents = _create_constituent_list(registry)
        vars_init_value = _create_variables_with_initial_value_list(registry)
        retcode = 0 # Throw exception on error
    # end if
    return retcode, files, ic_names, registry_constituents, vars_init_value

def main():
    """Function to execute when module called as a script"""
    args = parse_command_line(sys.argv[1:], __doc__)
    if args.output_dir is None:
        outdir = os.getcwd()
    else:
        outdir = args.output_dir
    # end if
    if args.debug:
        loglevel = logging.DEBUG
    elif args.quiet:
        loglevel = logging.ERROR
    else:
        loglevel = logging.INFO
    # end if

    retvals = gen_registry(args.registry_file, args.dycore.lower(),
                           outdir, args.indent, args.source_mods,
                           args.source_root, loglevel=loglevel)
    return retvals

###############################################################################
if __name__ == "__main__":
    __RETCODE, _FILES, _IC_NAMES = main()
    sys.exit(__RETCODE)
