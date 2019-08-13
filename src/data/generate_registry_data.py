#!/usr/bin/env python

"""
Read CAM registry and produce data and metadata files
"""

# Python library imports
import xml.etree.ElementTree as ET
import os
import os.path
import re
import argparse
import sys
import logging
from collections import OrderedDict

# Find and include the ccpp-framework scripts directory
currdir = os.path.abspath(os.path.dirname(__file__))
camroot = os.path.abspath(os.path.join(currdir, os.pardir, os.pardir))
cpfroot = os.path.join(camroot, "ccpp_framework")
sys.path.append(os.path.join(cpfroot, 'scripts'))

# CCPP framework imports
from parse_tools import validate_xml_file, find_schema_version, read_xml_file
from parse_tools import init_log, set_log_level, CCPPError, ParseInternalError
from fortran_tools import FortranWriter

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
        'Initialize TypeEntry'
        self._type = ttype
        self._module = module
        self._ddt = ddt # The actual DDT object, if <ttype> is a DDT

    @property
    def type_type(self):
        'Return type string'
        return self._type

    @property
    def module(self):
        'Return module where this type is defined or None for an intrinsic'
        return self._module

    @property
    def ddt(self):
        'Return DDT object, or None'
        return self._ddt

###############################################################################
class TypeRegistry(dict):
###############################################################################
    """Dictionary of known types. DDTs are associated with the module
    where they are defined"""

    def __init__(self):
        self['character'] = TypeEntry('character', None)
        self['complex']   = TypeEntry('complex',   None)
        self['integer']   = TypeEntry('integer',   None)
        self['logical']   = TypeEntry('logical',   None)
        self['real']      = TypeEntry('real',      None)

    def known_type(self, test_type):
        """Return type and a module name where <test_type> is defined
        or None if <test_type> is not in this TypeRegistry"""
        ttype = test_type.lower()
        if ttype in self:
            return self[ttype]
        else:
            return None
        # End if

    def add_type(self, new_type, type_module, type_ddt=None):
        "Add a new type, <new_type>, defined in <type_module> to this registry"
        ttype = new_type
        if ttype in self:
            emsg = 'Trying to add {} to registry, already defined in {}'
            raise ValueError(emsg.format(new_type, self[ttype]))
        else:
            self[ttype] = TypeEntry(new_type, type_module, type_ddt)
        # End if

###############################################################################
class Variable(object):
###############################################################################
    """Registry variable"""

    # Constant dimensions
    __CONSTANT_DIMENSIONS = {'ccpp_constant_one' : 1, 'ccpp_constant_zero' : 0}

    def __init__(self, var_node, known_types, vdict, logger):
        "Initialize a Variable from registry XML"
        self._local_name = var_node.get('local_name')
        self._standard_name = var_node.get('standard_name')
        self._units = var_node.get('units')
        ttype = var_node.get('type')
        vtype = known_types.known_type(ttype)
        self._kind = var_node.get('kind', default="")
        if vtype:
            self._type = vtype
            # We cannot have a kind property with a DDT type
            if self.module and self.kind:
                emsg = "kind attribute illegal for DDT type {}"
                raise CCPPError(emsg.format(self.var_type))
            # End if
        else:
            emsg = '{} is an unknown Variable type, {}'
            raise CCPPError(emsg.format(self._local_name, ttype))
        # End if
        self._access = var_node.get('access', default='public')
        if self._access == "protected":
            self._access = "public"
            self._protected = True
        else:
            self._protected = False
        # End if
        self._allocatable = var_node.get('allocatable', default="none")
        if self._allocatable == "none":
            self._allocatable = ""
        # End if
        self._dimensions = list()
        self._def_dims_str = ""
        self._initial_value = None
        self._long_name = ''
        for attrib in var_node:
            if attrib.tag == 'dimensions':
                self._dimensions = [x.strip() for x in attrib.text.split(' ')
                                    if x]
                def_dims = list() # Dims used for variable declarations
                for dim in self.dimensions:
                    if dim.count(':') > 1:
                        emsg = 'Illegal dimension string, {}, in {}'
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
                                emsg = 'Dimension, {}, not found for {}'
                                raise CCPPError(emsg.format(lname,
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
                    self._def_dims_str = '(' + ', '.join(def_dims) + ')'
                # End if
            elif attrib.tag == 'long_name':
                self._long_name = attrib.text
            elif attrib.tag == 'initial_value':
                self._initial_value = attrib.text
            else:
                emsg = 'Unknown Variable attribute, {}'
                raise CCPPError(emsg.format(attrib.tag))
            # End if
        # End for
        if (self.allocatable == 'parameter') and (not self._initial_value):
            emsg = 'parameter {} does not have an initial value'
            raise CCPPError(emsg.format(self.local_name))
        # End if
        if self.module:
            self._type_string = 'type({})'.format(self.var_type)
        elif self.kind:
            self._type_string = '{}({})'.format(self.var_type, self.kind)
        else:
            self._type_string = '{}'.format(self.var_type)
        # End if
        if logger:
            dmsg = 'Found registry Variable, {} ({})'
            logger.debug(dmsg.format(self._local_name, self._standard_name))
        # End if

    def write_metadata(self, outfile):
        "Write out this variable as CCPP metadata"
        if self.access != "private":
            outfile.write('[ {} ]\n'.format(self.local_name))
            outfile.write('  {} = {}\n'.format('standard_name',
                                               self.standard_name))
            if self._long_name:
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
            if (self.access == "parameter") or self.protected:
                outfile.write('  constant = True\n')
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
        """
        # Protected string
        if self.protected:
            pro_str = "protected"
            has_pro = True
        elif has_protect:
            pro_str = "         "
            has_pro = False
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
            init_str = " = {}".format(self.initial_value)
        else:
            init_str = ""
        # End if
        if self.long_name:
            comment = ' ! ' + self._local_name + ": " + self.long_name
        else:
            comment = (' ! ' + self._local_name + ": " +
                       convert_to_long_name(self.standard_name))
        # End if
        outfile.write(comment, indent)
        outfile.write("{}{}{}{} :: {}{}{}".format(type_str, acc_str,
                                                  all_str, pro_str,
                                                  self.local_name,
                                                  self._def_dims_str,
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
        my_ddt = self._type.ddt
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

    @classmethod
    def constant_dimension(cls, dim):
        "Return dimension value if <dim> is a constant dimension, else None"
        if dim.lower() in Variable.__CONSTANT_DIMENSIONS:
            dim_val = Variable.__CONSTANT_DIMENSIONS[dim.lower()]
        else:
            dim_val = None
        # End if
        return dim_val

    @property
    def local_name(self):
        "Return the local (variable) name for this variable"
        return self._local_name

    @property
    def standard_name(self):
        "Return the standard_name for this variable"
        return self._standard_name

    @property
    def units(self):
        "Return the units for this variable"
        return self._units

    @property
    def var_type(self):
        "Return the variable type for this variable"
        return self._type.type_type

    @property
    def module(self):
        "Return the module where this variable is defined"
        return self._type.module

    @property
    def kind(self):
        "Return the kind for this variable"
        return self._kind

    @property
    def allocatable(self):
        "Return the allocatable attribute (if any) for this variable"
        return self._allocatable

    @property
    def access(self):
        "Return the access attribute for this variable"
        return self._access

    @property
    def protected(self):
        "Return True iff this variable is protected"
        return self._protected

    @property
    def dimensions(self):
        "Return the dimensions for this variable"
        return self._dimensions

    @property
    def dimension_string(self):
        "Return the dimension_string for this variable"
        return '(' + ', '.join(self.dimensions) + ')'

    @property
    def long_name(self):
        "Return the long_name for this variable"
        return self._long_name

    @property
    def initial_value(self):
        "Return the initial_value for this variable"
        return self._initial_value

    @property
    def type_string(self):
        "Return the type_string for this variable"
        return self._type_string

###############################################################################
class VarDict(OrderedDict):
###############################################################################
    """Ordered dictionary of registry variables"""

    def __init__(self, name, ttype, logger):
        "Initialize a registry variable dictionary"
        super(VarDict, self).__init__()
        self._name = name
        self._type = ttype
        self._logger = logger
        self._standard_names = list()
        self._dimensions = set() # All known dimensions for this dictionary

    @property
    def name(self):
        "Return the name of this dictionary (usually the module name)"
        return self._name

    @property
    def module_type(self):
        "Return the module type (e.g., host, module) for this dictionary"
        return self._type

    @property
    def known_dimensions(self):
        "Return the set of known dimensions for this dictionary"
        return self._dimensions

    def add_variable(self, newvar):
        """Add a variable if it does not conflict with existing entries"""
        local_name = newvar.local_name
        std_name = newvar.standard_name
        if local_name.lower() in self:
            # We already have a matching variable, error!
            emsg = "duplicate variable local_name, {}, in {}"
            ovar = self[local_name]
            if (ovar is not None) and (ovar.standard_name != std_name):
                emsg2 = ", already defined with standard_name, {}"
                emsg += emsg2.format(ovar._context)
            # End if
            raise CCPPError(emsg.format(local_name, self.name))
        elif std_name.lower() in self._standard_names:
            # We have a standard name collision, error!
            emsg = "duplicate variable standard_name, {} in {}"
            ovar = None
            for testvar in self.variable_list():
                if testvar.standard_name.lower() == std_name.lower():
                    ovar = testvar
                    break
                # End if
            # End for
            if ovar is not None:
                emsg2 = ", already defined with local_name, {}"
                emsg += emsg2.format(ovar.local_name)
            # End if
            raise CCPPError(emsg.format(std_name, self.name))
        else:
            self[local_name.lower()] = newvar
            self._standard_names.append(std_name.lower())
            for dim in newvar.dimensions:
                dimstrs = [x.strip() for x in dim.split(':')]
                for ddim in dimstrs:
                    lname = Variable.constant_dimension(ddim)
                    if not lname:
                        self._dimensions.add(dim.lower())
                    # End if
                # End for
            # End for
        # End if

    def find_variable_by_local_name(self, local_name):
        """Return this dictionary's variable matching local name, <local_name>.
        Return None if not found."""
        lname = local_name.lower()
        if lname in self:
            fvar = self[lname]
        else:
            if self._logger:
                lmsg = 'Local name, {}, not found in {}'
                self._logger.debug(lmsg.format(local_name, self.name))
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
        if (not fvar) and self._logger:
            lmsg = 'Standard name, {}, not found in {}'
            self._logger.debug(lmsg.format(std_name, self.name))
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
            if self._logger:
                lmsg = 'Cannot remove {} from {}, variable not found.'
                self._logger.debug(lmsg.format(std_name, self.name))
            # End if
        # End if

    def variable_list(self):
        "Return a list of this dictionary's variables"
        return self.values()

    def write_metadata(self, outfile):
        "Write out the variables in this dictionary as CCPP metadata"
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

    def __init__(self, ddt_node, var_dict, known_types, dycore, logger):
        """Initialize a DDT from registry XML (<ddt_node>)
        <var_dict> is the dictionary where variables referenced in <ddt_node>
        must reside. Each DDT variable is removed from <var_dict>"""
        self._type = ddt_node.get('type')
        self._logger = logger
        self._data = list()
        self._extends = ddt_node.get('extends', default=None)
        self._bindC = ddt_node.get('bindC', default=False)
        if self._extends and self._bindC:
            emsg = "DDT {} cannot have both 'extends' and 'bindC' attributes"
            raise CCPPError(emsg.format(self.ddt_type))
        # End if
        self._private = ddt_node.get('private', default=False)
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
                        self._data.append(var)
                        var_dict.remove_variable(varname)
                    else:
                        emsg = 'Variable, {}, not found for DDT, {}, in {}'
                        raise CCPPError(emsg.format(varname, self.ddt_type,
                                                    var_dict.name))
                    # End if
                # End if
            else:
                emsg = "Unknown DDT element type, '{}'"
                raise CCPPError(emsg.format(attrib.tag))
            # End if
        # End for

    def variable_list(self):
        "Return the variable list for this DDT"
        return self._data

    def write_metadata(self, outfile):
        "Write out this DDT as CCPP metadata"
        outfile.write('[ccpp_table]\n')
        outfile.write('  name = {}\n'.format(self.ddt_type))
        outfile.write('  type = ddt\n')
        for var in self._data:
            var.write_metadata(outfile)
        # End if

    def write_definition(self, outfile, access, indent):
        "Write out the Fortran definition for this DDT"
        my_acc = 'private' if self.private else 'public'
        if self.extends:
            acc_string = ', extends({})'.format(self.extends)
        elif self.bindC:
            acc_string = ', bind(C)'
        elif my_acc != access:
            acc_str = ', {}'.format(my_acc)
        else:
            acc_str = ''
        # End if
        outfile.write("type{} :: {}".format(acc_str, self.ddt_type), indent)
        maxtyp = max([len(x.type_string) for x in self._data])
        maxacc = max([len(x.access) for x in self._data
                      if x.access != 'private'])
        maxall = max([len(x.allocatable) for x in self._data])
        has_prot = any([x.protected for x in self._data])
        for var in self._data:
            var.write_definition(outfile, my_acc, indent+1,
                                 maxtyp=maxtyp, maxacc=maxacc,
                                 maxall=maxall, has_protect=has_prot)
        # End if
        outfile.write("end type {}\n".format(self.ddt_type), indent)

    @property
    def ddt_type(self):
        "Return this DDT's type"
        return self._type

    @property
    def private(self):
        "Return True iff this DDT is private"
        return self._private

    @property
    def extends(self):
        "Return this DDT's parent class, if any"
        return self._extends

    @property
    def bindC(self):
        "Return True iff this DDT has the bind(C) attribute"
        return self._bindC

###############################################################################
class File(object):
###############################################################################
    """Object describing a file object in a registry file"""

    # Some data for sorting dimension names
    __dim_order = {'horizontal_dimension' : 1,
                   'vertical_layer_dimension' : 2,
                   'vertical_interface_dimension' : 3,
                   'number_of_constituents' : 4}
    __min_dim_key = 5 # For sorting unknown dimensions

    def __init__(self, file_node, known_types, dycore, config, logger):
        "Initialize a File object from a registry node (XML)"
        self._var_dict = VarDict(file_node.get('name'), file_node.get('type'),
                                 logger)
        self._name = file_node.get('name')
        self._type = file_node.get('type')
        self._known_types = known_types
        self._ddts = dict()
        self._use_statements = list()
        for obj in file_node:
            if obj.tag == 'variable':
                newvar = Variable(obj, self._known_types, self._var_dict,
                                  logger)
                self._var_dict.add_variable(newvar)
            elif obj.tag == 'ddt':
                newddt = DDT(obj, self._var_dict, self._known_types,
                             dycore, logger)
                dmsg = "Adding DDT {} from {} as a known type"
                logging.debug(dmsg.format(newddt.ddt_type, self._name))
                self._ddts[newddt.ddt_type] = newddt
                self._known_types.add_type(newddt.ddt_type, self._name, newddt)
            elif obj.tag == 'use':
                module = obj.get('module', default=None)
                if not module:
                    raise CCPPError('Illegal use entry, no module')
                # End if
                ref = obj.get('reference', default=None)
                if not ref:
                    raise CCPPError('Illegal use entry, no reference')
                # End if
                self._use_statements.append((module, ref))
            else:
                emsg = "Unknown registry File element, {}"
                raise CCPPError(emsg.format(obj.tag))
            # End if
        # End for

    def write_metadata(self, outdir, logger):
        "Write out the variables in this file as CCPP metadata"
        ofilename = os.path.join(outdir, "{}.meta".format(self.name))
        logger.info("Writing registry metadata file, {}".format(ofilename))
        with open(ofilename, "w") as outfile:
            # Write DDTs defined in this file
            for ddt in self._ddts.values():
                ddt.write_metadata(outfile)
            # End if
            # Write Variables defined in this file
            self._var_dict.write_metadata(outfile)
        # End with

    @classmethod
    def dim_sort_key(cls, dim_name):
        "Return an integer sort key for <dim_name>"
        if dim_name not in File.__dim_order:
            key = File.__min_dim_key
            File.__min_dim_key += 1
            File.__dim_order[dim_name] = key
        # End if
        return File.__dim_order[dim_name]

    def write_source(self, outdir, indent, logger):
        "Write out source code for the variables in this file"
        ofilename = os.path.join(outdir, "{}.F90".format(self.name))
        logger.info("Writing registry source file, {}".format(ofilename))
        with FortranWriter(ofilename, "w", indent=indent) as outfile:
            # Define the module header
            outfile.write('module {}\n'.format(self.name), 0)
            # Use statements (if any)
            module_list = list() # tuple of (module, type)
            for var in self._var_dict.variable_list():
                mod = var.module
                if mod and (mod.lower() != self.name.lower()):
                    module_list.append((mod, var.var_type))
                # End if
            # End for
            # Add any DDT types
            for ddt in self._ddts.values():
                for var in ddt.variable_list():
                    mod = var.module
                    if mod and (mod.lower() != self.name.lower()):
                        module_list.append((mod, var.var_type))
                    # End if
                # End for
            # End for
            # Add in any explicit use entries from the registry
            for ref in self._use_statements:
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
            for ddt in self._ddts.values():
                ddt.write_definition(outfile, 'private', 1)
            # End if
            # Write Variables defined in this file
            self._var_dict.write_definition(outfile, 'private', 1)
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
        "Return the name of the allocate routine for this module"
        return 'allocate_{}_fields'.format(self.name)

    def write_allocate_routine(self, outfile):
        "Write a subroutine to allocate all the data in this module"
        subname = self.allocate_routine_name()
        args = list(self._var_dict.known_dimensions)
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
        for var in self._var_dict.variable_list():
            var.write_allocate_routine(outfile, 2, init_var, reall_var, '')
        # End for
        outfile.write('end subroutine {}'.format(subname), 1)

    @property
    def name(self):
        "Return this File's name"
        return self._name

    @property
    def file_type(self):
        "Return this File's type"
        return self._type

###############################################################################
def parse_command_line(args, description):
###############################################################################
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
    parser.add_argument("--debug", action='store_true',
                        help='Increase logging', default=False)
    parser.add_argument("--indent", type=int, default=3,
                        help="Indent level for Fortran source code")
    pargs = parser.parse_args(args)
    return pargs

###############################################################################
def write_registry_files(registry, dycore, config, outdir, indent, logger):
###############################################################################
    "Write metadata and source files for <registry>"
    files = list()
    known_types = TypeRegistry()
    for section in registry:
        sec_name = section.get('name')
        logger.info("Parsing {}, {}, from registry".format(section.tag,
                                                          sec_name))
        if section.tag == 'file':
            files.append(File(section, known_types, dycore, config, logger))
        else:
            emsg = "Unknown registry object type, {}"
            raise CCPPErorr(emsg.format(section.tag))
        # End if
    # End for
    # Make sure output directory exists
    if not os.path.exists(outdir):
        os.makedirs(outdir)
    # End if
    # Write metadata
    for file in files:
        file.write_metadata(outdir, logger)
        file.write_source(outdir, indent, logger)
    # End for

###############################################################################
def main_func(registry_file, dycore, config, outdir, indent, debug):
###############################################################################
    logger = init_log(os.path.basename(__file__), logging.INFO)
    if debug:
        set_log_level(logger, logging.DEBUG)
    # End if
    logger.info("Reading CAM registry from {}".format(registry_file))
    _, registry = read_xml_file(registry_file)
    # Validate the XML file
    version = find_schema_version(registry, logger)
    if debug:
        verstr = '.'.join([str(x) for x in version])
        logger.debug("Found registry version, v{}".format(verstr))
    # End if
    try:
        emsg = "Invalid registry file, {}".format(registry_file)
        file_ok = validate_xml_file(registry_file, 'registry', version,
                                    logger, schema_path=currdir)
    except CCPPError as ce:
        cemsg = "{}".format(ce).split('\n')[0]
        emsg += cemsg.replace('Execution of', '\nTry debugging').replace('failed:', '').rstrip()
        file_ok = False
    # End if
    if not file_ok:
        logger.error(emsg)
        retcode = 1
    else:
        library_name = registry.get('name')
        write_registry_files(registry, dycore, config, outdir, indent, logger)
        retcode = 0 # Throw exception on error
    # End if
    return retcode


###############################################################################
if __name__ == "__main__":
    args = parse_command_line(sys.argv[1:], __doc__)
    if args.output_dir is None:
        outdir = os.getcwd()
    else:
        outdir = args.output_dir
    # End if
    retcode = main_func(args.registry_file, args.dycore.lower(), args.config,
                        outdir, args.indent, args.debug)
    sys.exit(retcode)
