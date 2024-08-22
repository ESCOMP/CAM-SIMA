#!/usr/bin/env python3

"""
Use variable meta-data from "generate_registry_data.py"
to generate CAM fortran files that manage host model
variable initialization and initial condition inputs.
"""

# Python library import statements:
from collections import OrderedDict
import os.path

# CCPP Framework import statements
from ccpp_state_machine import CCPP_STATE_MACH
from fortran_tools import FortranWriter
from var_props import is_horizontal_dimension, is_vertical_dimension

# Exclude these standard names from init processing
# Some are internal names (e.g., suite_name)
# Some are from the CCPP framework (e.g., ccpp_num_constituents)
# Some are for efficiency and to avoid dependency loops (e.g., log_output_unit)
_EXCLUDED_STDNAMES = {'suite_name', 'suite_part',
                          'number_of_ccpp_constituents',
                          'number_of_ccpp_advected_constituents',
                          'ccpp_constituents',
                          'ccpp_constituent_tendencies',
                          'ccpp_constituent_properties',
                          'ccpp_constituent_minimum_values',
                          'ccpp_error_message',
                          'ccpp_error_code',
                          'log_output_unit', 'do_log_output',
                          'mpi_communicator', 'mpi_root', 'mpi_rank',
                          'number_of_mpi_tasks'}
# Variable input types
_INPUT_TYPES = set(['in', 'inout'])

# Include files to insert in the module preamble
_PHYS_VARS_PREAMBLE_INCS = ["cam_var_init_marks_decl.inc"]
# Include files to insert in the module body
_PHYS_VARS_BODY_INCS = ["cam_var_init_marks.inc"]

# Increase allowed line lengths needed to fit extra-long CCPP standard names:
_LINE_FILL_LEN = 150
_MAX_LINE_LEN = 200

##############
#Main function
##############

def write_init_files(cap_database, ic_names, outdir,
                     file_find_func, source_paths, indent, logger,
                     phys_check_filename=None, phys_input_filename=None):

    """
    Create the "phys_init" Fortran files using a database
       created by the CCPP Framework generator (capgen).
    The two specific Fortran files are:

    1.  phys_vars_init_check.F90

        This file contains five
        variable arrays:

        phys_var_stdnames -
            All registered variable
            standard names

        phys_const_stdnames -
            The "excluded" standard names
            (from _EXCLUDED_STDNAMES)

        input_var_names -
            All registered names for
            each variable that could
            be present in an Initial
            Conditions (IC) input file

        protected_vars -
            Logicals that indicate
            whether or not each
            variable is protected

        initialized_vars -
            Integers that indicate
            whether each variable
            is UNINITIALIZED,
               INITIALIZED,
               PARAM, or
               READ_FROM_FILE

        It also contains the
        "mark_as_initialized"
        subroutine, which can set the
        value in "initialized_vars" to
        INITIALIZED given the variable's
        standard name, and the "is_initialized"
        function, which returns TRUE
        if the value of "initialized_vars"
        for a particualar variable given
        its standard name is INITIALIZED,
        PARAM, or READ_FROM_FILE.

        It also contains the
        "mark_as_read_from_file"
        subroutine, which can set the
        value in "initialized_vars" to
        READ_FROM_FILE given the variable's
        standard name, and the "is_read_from_file"
        function, which returns TRUE
        if the value of "initialized_vars"
        for a particular variable given
        its standard name is READ_FROM_FILE

    2.  physics_inputs.F90

        This file contains the
        "physics_read_data" subroutine,
        which determines whether a
        variable is required by the CCPP
        physics suites, if it is already
        initiailized, and if not, then
        either attempts to read the
        variable data from the user-specified
        "ncdata" IC file, or throws a relevant
        error to the user with a list of
        the offending variables.
    """

    #Initialize return message:
    retmsg = ""

    # Gather all the host model variables that are required by
    #    any of the compiled CCPP physics suites.
    host_vars, constituent_set, retmsg = gather_ccpp_req_vars(cap_database)

    # Quit now if there are missing variables
    if retmsg:
        return retmsg
    # end if

    # Generate "phys_vars_init_check.F90" file:
    # -----------------------------------------

    # Open new file:
    if phys_check_filename:
        ofilename = os.path.join(outdir, phys_check_filename)
        # Get file name, ignoring file type:
        phys_check_fname_str = os.path.splitext(phys_check_filename)[0]
    else:
        ofilename = os.path.join(outdir, "phys_vars_init_check.F90")
        phys_check_fname_str = "phys_vars_init_check"
    # end if

    # Log file creation:
    logger.info(f"Writing initialization-checking source file, {ofilename}")

    # Open file using CCPP's FortranWriter:
    file_desc = "Initialization-checking source file"
    with FortranWriter(ofilename, "w", file_desc,
                       phys_check_fname_str,
                       line_fill=_LINE_FILL_LEN,
                       line_max=_MAX_LINE_LEN,
                       indent=indent) as outfile:

        # Add boilerplate code:
        outfile.write_preamble()

        # Include pre-formatted preamble content
        # Note: Must be before write_ic_params because it defines
        #       parameters used by that code
        for filename in _PHYS_VARS_PREAMBLE_INCS:
            filepath = file_find_func(filename, source_paths)
            outfile.include(filepath)
        # end for

        # Write public parameters:
        retvals = write_ic_params(outfile, host_vars, ic_names)
        ic_names, ic_max_len, stdname_max_len = retvals

        # Write initial condition arrays:
        write_ic_arrays(outfile, ic_names, ic_max_len,
                        stdname_max_len, host_vars)

        # Add "contains" statement:
        outfile.end_module_header()
        outfile.blank_line()

        # Include pre-formatted body content
        for filename in _PHYS_VARS_BODY_INCS:
            filepath = file_find_func(filename, source_paths)
            outfile.include(filepath)
        # end for

    # end with (end of module)
    # --------------------------------------

    # Generate "physics_inputs.F90" file:
    # --------------------------------------

    # Open new file:
    if phys_input_filename:
        ofilename = os.path.join(outdir, phys_input_filename)
        # Get file name, ignoring file type:
        phys_input_fname_str = os.path.splitext(phys_input_filename)[0]
    else:
        ofilename = os.path.join(outdir, "physics_inputs.F90")
        phys_input_fname_str = "physics_inputs"
    # end if

    # Log file creation:
    logger.info(f"Writing initial conditions source file, {ofilename}")

    # Open file using CCPP's FortranWriter:
    file_desc = f"Initial conditions source file, {phys_input_filename}"
    with FortranWriter(ofilename, "w", file_desc,
                       phys_input_fname_str,
                       indent=indent,
                       line_fill=_LINE_FILL_LEN,
                       line_max=_MAX_LINE_LEN) as outfile:

        # Add boilerplate code:
        outfile.write_preamble()
        outfile.blank_line()

        # Add public function declarations:
        outfile.write("!! public interfaces", 0)
        outfile.write("public :: physics_read_data", 1)
        outfile.write("public :: physics_check_data", 1)

        # Add "contains" statement:
        outfile.end_module_header()
        outfile.blank_line()

        # Grab the host dictionary from the database
        host_dict = cap_database.host_model_dict()

        # Collect imported host variables
        host_imports = collect_host_var_imports(host_vars, host_dict, constituent_set)

        # Write physics_read_data subroutine:
        write_phys_read_subroutine(outfile, host_dict, host_vars, host_imports,
                                   phys_check_fname_str, constituent_set)

        outfile.blank_line()

        # Write physics_check_data subroutine:
        write_phys_check_subroutine(outfile, host_dict, host_vars, host_imports,
                                    phys_check_fname_str, constituent_set)

    # --------------------------------------

    # Return retmsg:
    return retmsg

###############################
#Fortran generation error class
###############################

class CamInitWriteError(ValueError):
    """Class used to handle CAM write_init errors
    (e.g., log user errors without backtrace)"""
    # pylint: disable=useless-super-delegation
    def __init__(self, message):
        super().__init__(message)
    # pylint: enable=useless-super-delegation

#################
#HELPER FUNCTIONS
#################

##############################################################################
def _find_and_add_host_variable(stdname, host_dict, var_dict):
    """Find <stdname> in <host_dict> and add it to <var_dict> if found and
          not of type, 'host'.
       If not found, add <stdname> to <missing_vars>.
       If found and added to <var_dict>, also process the standard names of
          any intrinsic sub-elements of <stdname>.
       Return the list of <missing_vars> (if any).
       Note: This function has a side effect (adding to <var_dict>).
    """
    missing_vars = []
    hvar = host_dict.find_variable(stdname)
    if hvar and (hvar.source.ptype != 'host'):
        var_dict[stdname] = hvar
        # Process elements (if any)
        ielem = hvar.intrinsic_elements()
        # List elements are the only ones we care about
        if isinstance(ielem, list):
            for sname in ielem:
                smissing = _find_and_add_host_variable(sname, host_dict,
                                                       var_dict)
                missing_vars.extend(smissing)
            # end for
        # end if
    # end if
    if not hvar:
        missing_vars.append(stdname)
    # end if
    return missing_vars

##############################################################################
def gather_ccpp_req_vars(cap_database):
    """
    Generate a list of host-model and constituent variables
    required by the CCPP physics suites potentially being used
    in this model run.
    <cap_database> is the database object returned by capgen.
    It is an error if any physics suite variable is not accessible in
       the host model.
    Return several values:
    - A list of host model variables
    - An error message (blank for no error)
    """

    # Dictionary of all 'in' and 'inout' suite variables.
    # Key is standard name, value is host-model or constituent variable
    req_vars = {}
    missing_vars = set()
    constituent_vars = set()
    retmsg = ""
    # Host model dictionary
    host_dict = cap_database.host_model_dict()

    # Create CCPP datatable required variables-listing object:
    # XXgoldyXX: Choose only some phases here?
    for phase in CCPP_STATE_MACH.transitions():
        for cvar in cap_database.call_list(phase).variable_list():
            stdname = cvar.get_prop_value('standard_name')
            intent = cvar.get_prop_value('intent')
            is_const = cvar.get_prop_value('advected') or cvar.get_prop_value('constituent')
            if ((intent in _INPUT_TYPES) and
                (stdname not in req_vars) and
                (stdname not in _EXCLUDED_STDNAMES)):
                if is_const:
                    #Variable is a constituent, so may not be known
                    #until runtime, but still need variable names in order
                    #to read from a file if need be:
                    req_vars[stdname] = cvar

                    #Add variable to constituent set:
                    constituent_vars.add(stdname)
                else:
                    # We need to work with the host model version of this variable
                    missing = _find_and_add_host_variable(stdname, host_dict,
                                                          req_vars)
                    missing_vars.update(missing)
                # end if
            # end if (do not include output variables)
        # end for (loop over call list)
    # end for (loop over phases)

    if missing_vars:
        mvlist = ', '.join(sorted(missing_vars))
        retmsg = f"Error: Missing required host variables: {mvlist}"
    # end if
    # Return the required variables as a list
    return list(req_vars.values()), constituent_vars, retmsg

##########################
#FORTRAN WRITING FUNCTIONS
##########################

def write_ic_params(outfile, host_vars, ic_names):

    """
    Write public parameter declarations needed
    by initial condition arrays and functions
    Return the list of all required standard names and an
       expanded initial conditions dictionary (which includes all variables).
    """

    #Create new Fortran integer parameter to store total number of variables:
    outfile.comment("Total number of physics-related variables:", 1)
    num_pvars = len(host_vars)
    outfile.write(f"integer, public, parameter :: phys_var_num = {num_pvars}",
                  1)
    num_cvars = len(_EXCLUDED_STDNAMES)
    outfile.write(f"integer, public, parameter :: phys_const_num = {num_cvars}",
                  1)

    outfile.blank_line()

    #Create another Fortran integer parameter to store max length of
    #variable standard name strings:
    outfile.write("!Max length of physics-related variable standard names:", 1)
    stdname_list = [x.get_prop_value('standard_name') for x in host_vars]
    if stdname_list:
        max_slen = max(len(x) for x in stdname_list)
    else:
        max_slen = 0
    # end if
    outfile.write(f"integer, public, parameter :: std_name_len = {max_slen}", 1)

    outfile.blank_line()

    #Create final Fortran integer parameter to store max length of
    #input variable name string:
    outfile.comment("Max length of input (IC) file variable names:", 1)
    # We need to look either in ic_names or the host variable
    max_loclen = 0
    for hvar in host_vars:
        stdname = hvar.get_prop_value('standard_name')
        if stdname in ic_names:
            max_loclen = max(max_loclen,
                             max(len(x) for x in ic_names[stdname]))
        else:
            locname = hvar.get_prop_value('local_name')
            max_loclen = max(max_loclen, len(locname))
            # Add this variable to the ic_names dictionary
            ic_names[stdname] = [locname]
        # end if
    # end if
    outfile.write(f"integer, public, parameter :: ic_name_len = {max_loclen}",
                  1)

    outfile.blank_line()

    return ic_names, max_loclen, max_slen

######

def write_ic_arrays(outfile, ic_name_dict, ic_max_len,
                    stdname_max_len, host_vars):

    """
    Write initial condition arrays to store
    data on variable initialization status
    and input file names.
    """

    #Create variable name array string lists:
    num_input_vars = len(host_vars)
    stdname_strs = []
    ic_name_strs = []

    #Create fake name and fake name list with proper lengths:
    fake_ic_name = " "*ic_max_len
    if ic_name_dict:
        max_ic_num = max(len(x) for x in ic_name_dict.values())
    else:
        max_ic_num = 0
    # end if
    fake_ic_names = [fake_ic_name]*max_ic_num

    # Create the correct number (<ic_name_num>) of initial-value strings
    #    for each variable with the proper length, <stdname_max_len>:
    for hvar in host_vars:
        var_stdname = hvar.get_prop_value('standard_name')

        # Create standard_name string with proper size, and append to list:
        stdname_strs.append(f"'{var_stdname: <{stdname_max_len}}'")

        #Extract input (IC) names list:
        ic_names = ic_name_dict[var_stdname]

        #Determine number of IC names for variable:
        ic_name_num = len(ic_names)

        #Pad the ic_names to ic_max_len
        ic_names_with_spaces = [f"{x: <{ic_max_len}}" for x in ic_names]
        if ic_name_num < max_ic_num:
            ic_names_with_spaces.extend(fake_ic_names[:-ic_name_num])
        # end if

        #Append new ic_names to string list:
        ic_name_strs.append(', '.join(f"'{n}'" for n in ic_names_with_spaces))
    # end for

    #Write arrays to Fortran file:
    #----------------------------

    #Write starting declaration of input standard name array:
    outfile.comment("Physics-related input variable standard names:", 1)
    vartype = f"character(len={stdname_max_len}), public, protected"
    varname = "phys_var_stdnames(phys_var_num)"
    if stdname_strs:
        outfile.write(f"{vartype} :: {varname} = (/ &", 1)
    else:
        outfile.write(f"{vartype} :: {varname}", 1)
    # end if

    #Write standard names to Fortran array:
    suffix = ", &"
    for index, stdname_str in enumerate(stdname_strs):
        if index == num_input_vars-1:
            suffix = " /)"
        # end if
        outfile.write(f"{stdname_str}{suffix}", 2)
    # end for

    outfile.blank_line()

    # Write excluded standard names
    cname_max_len = max([len(x) for x in _EXCLUDED_STDNAMES])
    num_cvars = len(_EXCLUDED_STDNAMES)
    vartype = f"character(len={cname_max_len}), public, protected"
    varname = "phys_const_stdnames(phys_const_num)"
    outfile.write(f"{vartype} :: {varname} = (/ &", 1)
    suffix = ", &"
    for index, stdname_str in enumerate(sorted(_EXCLUDED_STDNAMES)):
        spc = ' '*(cname_max_len - len(stdname_str))
        if index == num_cvars - 1:
            suffix = " /)"
        # end if
        outfile.write(f'"{stdname_str}{spc}"{suffix}', 2)
    # end for

    #Write starting declaration of IC field input names array:
    outfile.write("!Array storing all registered IC file input names for each variable:", 1)
    vartype = f"character(len={ic_max_len}), public, protected"
    varname = f"input_var_names({max_ic_num}, phys_var_num)"
    if ic_name_strs:
        outfile.write(f"{vartype} :: {varname} = reshape((/ &", 1)
    else:
        outfile.write(f"{vartype} :: {varname}", 1)
    # end if

    #Write IC names to Fortran array:
    suffix = ", &"
    for index, ic_name_str in enumerate(ic_name_strs):
        if index == num_input_vars-1:
            suffix = f" /), (/{max_ic_num}, phys_var_num/))"
        # end if
        outfile.write(f"{ic_name_str}{suffix}", 2)
    # end for
    outfile.blank_line()

    #Write starting declaration of protected logical array:
    outfile.comment("Array indicating whether or not variable is protected:", 1)
    declare_str = "logical, public, protected :: protected_vars(phys_var_num)"
    if host_vars:
        declare_str += "= (/ &"
    # end if
    outfile.write(declare_str, 1)

    # For each required variable, set array value to ".false."
    #    unless the variable is protected:
    arr_suffix = ', &'
    for var_num, hvar in enumerate(host_vars):
        # If at the end of the list, then update suffix:
        if var_num == num_input_vars-1:
            arr_suffix = ' /)'
        # end if
        #Set array values:
        if hvar.get_prop_value('protected'):
            log_arr_str = '.true.' + arr_suffix
        else:
            log_arr_str = '.false.' + arr_suffix
        # end if

        #Write line to file:
        outfile.write(log_arr_str, 2)
    # end for

    outfile.blank_line()

    #Write starting declaration of initialized logical array:
    outfile.comment("Variable state (UNINITIALIZED, INTIIALIZED, PARAM or READ_FROM_FILE):", 1)
    declare_str = "integer, public, protected :: initialized_vars(phys_var_num)"
    if host_vars:
        declare_str += "= (/ &"
    # end if
    outfile.write(declare_str, 1)

    #Write UNINITIALIZED to integer array, unless
    #variable is a parameter:
    arr_suffix = ', &'
    for var_num, hvar in enumerate(host_vars):
        var_stdname = hvar.get_prop_value('standard_name')
        #If at the end of the list, then update suffix:
        if var_num == num_input_vars-1:
            arr_suffix = ' /)'
        # end if
        #Set array values:
        if hvar.get_prop_value('protected'):
            log_arr_str = 'PARAM' + arr_suffix
        else:
            log_arr_str = 'UNINITIALIZED' + arr_suffix
        # end if
        #Write line to file:
        outfile.write(log_arr_str, 2)
    # end for

    outfile.blank_line()
    #----------------------------

######

def _get_host_model_import(hvar, import_dict, host_dict):
    """Add import information (module, local_name) for <hvar> to
       <import_dict>. <host_dict> is used to look up any sub-variables
       (e.g., array indices).
       Note: This function has side effects but no return value
    """
    missing_indices = []
    # Extract module name:
    use_mod_name = hvar.source.name
    # Check if module name is already in dictionary:
    if use_mod_name not in import_dict:
        # Create an empty entry for this module
        import_dict[use_mod_name] = set()
    # end if
    # Add the variable
    var_locname = hvar.var.get_prop_value('local_name')
    import_dict[use_mod_name].add(var_locname)
    aref = hvar.array_ref()
    if aref:
        dimlist = [x.strip() for x in aref.group(2).split(',')]
        for dim in dimlist:
            if dim != ':':
                dvar = host_dict.find_variable(dim)
                if dvar:
                    _get_host_model_import(dvar, import_dict, host_dict)
                else:
                    missing_indices.append(dim)
                # end if
            # end if
        # end for
    # end if
    if missing_indices:
        mi_str = ", ".join(missing_indices)
        raise CamInitWriteError(f"Missing host indices: {mi_str}.")
    # end if

def collect_host_var_imports(host_vars, host_dict, constituent_set):
    """Construct a dictionary of host-model variables to import keyed by
       host-model module name.
       <host_dict> is used to look up array-reference indices.
    Return a list of module / import vars combinations of the following form:
    [[<Module 1>, [<var1, ...]], ...]
    """
    #--------------------------------
    # Create new (empty) dictionary to store use statements:
    use_vars_write_dict = OrderedDict()

    # Loop over all variable standard names:
    for hvar in host_vars:
        # We do not import variables from the 'host' table as they are
        #    passed to physics via the argument list.
        # As such, they are also always considered initialized.
        if hvar.source.ptype == 'host':
            continue
        # end if
        # We also do not want to import constituent variables, as they
        # should be automatically handled by the constituents object:
        if hvar.get_prop_value('standard_name') in constituent_set:
            continue
        # end if
        _get_host_model_import(hvar, use_vars_write_dict, host_dict)
    # end for
    return [[x, sorted(use_vars_write_dict[x])] for x in use_vars_write_dict]

######

def write_use_statements(outfile, use_stmts, indent):
    """Output Fortran module use (import) statements listed in <use_stmts>.
    """

    # The plus one is for a comma
    max_modname = max(len(x[0]) for x in use_stmts) + 1
    # max_modspace is the max chars of the module plus other 'use' statement
    #    syntax (e.g., 'only:')
    max_modspace = (outfile.indent_size * indent) + max_modname + 10
    mod_space = outfile.line_fill - max_modspace
    for use_item in use_stmts:
        # Break up imported interfaces to clean up use statements
        larg = 0
        num_imports = len(use_item[1])
        while larg < num_imports:
            int_str = use_item[1][larg]
            larg = larg + 1
            while ((larg < num_imports) and
                   ((len(int_str) + len(use_item[1][larg]) + 2) < mod_space)):
                int_str += f", {use_item[1][larg]}"
                larg = larg + 1
            # end while
            modname = use_item[0] + ','
            outfile.write(f"use {modname: <{max_modname}} only: {int_str}",
                          indent)
        # end while
    # end for

######

def get_dimension_info(hvar):
    """Retrieve dimension information from <hvar>.
       Return the following values:
       - The local variable name of the vertical dimension (or None)
       - True if <hvar> has one dimension which is a horizontal dimension or
            if <hvar> has two dimensions (horizontal and vertical)
    """
    vdim_name = None
    legal_dims = False
    fail_reason = ""
    dims = hvar.get_dimensions()
    levnm = hvar.has_vertical_dimension()
    # <hvar> is only 'legal' for 2 or 3 dimensional fields (i.e., 1 or 2
    #    dimensional variables). The second dimension must be vertical.
    # XXgoldyXX: If we ever need to read scalars, it would have to be
    #            done using global attributes, not 'infld'.
    ldims = len(dims)
    lname = hvar.get_prop_value('local_name')
    suff = ""
    legal_dims = True
    if not hvar.has_horizontal_dimension():
        legal_dims = False
        fail_reason += f"{suff}{lname} has no horizontal dimension"
        suff = "; "
    # end if
    if (ldims > 2) or ((ldims > 1) and (not levnm)):
        legal_dims = False
        unsupp = []
        for dim in dims:
            if ((not is_horizontal_dimension(dim)) and
                (not is_vertical_dimension(dim))):
                if dim[0:18] == "ccpp_constant_one:":
                    rdim = dim[18:]
                else:
                    rdim = dim
                # end if
                unsupp.append(rdim)
            # end if
        # end for
        if len(unsupp) > 1:
            udims = ', '.join(unsupp[:-1])
            if len(unsupp) > 2:
                udims += ','
            # end if
            udims += f" and {unsupp[-1]}"
            fail_reason += f"{suff}{lname} has unsupported dimensions, {udims}."
        else:
            udims = unsupp[0] if unsupp else "unknown"
            fail_reason += f"{suff}{lname} has unsupported dimension, {udims}."
        # end if
        suff = "; "
    # end if
    if legal_dims and levnm:
        # <hvar> should be legal, find the correct local name for the
        #    vertical dimension
        dparts = levnm.split(':')
        if (len(dparts) == 2) and (dparts[0].lower() == 'ccpp_constant_one'):
            levnm = dparts[1]
        elif len(dparts) == 1:
            levnm = dparts[0]
        else:
            # This should not happen so crash
            raise ValueError(f"Unsupported vertical dimension, '{levnm}'")
        # end if
        if levnm == 'vertical_layer_dimension':
            vdim_name = "lev"
        elif levnm == 'vertical_interface_dimension':
            vdim_name = "ilev"
        # end if (no else, will be processed as an error below)

        if vdim_name is None:
            # This should not happen so crash
            raise ValueError(f"Vertical dimension, '{levnm}', not found")
        # end if
    # end if
    return vdim_name, legal_dims, fail_reason

def write_phys_read_subroutine(outfile, host_dict, host_vars, host_imports,
                               phys_check_fname_str, constituent_set):

    """
    Write the "physics_read_data" subroutine, which
    is used to initialize required physics variables
    by reading in the values from an Initial Conditions
    (IC) input file. This will only be done for registry
    variables which contain
    """

    # ---------------------------------
    # Create Fortran "read_field" calls:
    # ---------------------------------

    # Create new (empty) dictionary to store "read_field" calls:
    call_string_dict = OrderedDict()

    # Loop over all variable standard names:
    for hvar in host_vars:
        # We do not attempt to read values from variables from the 'host'
        #    table as they are passed to physics via the argument list.
        # As such, they are always considered initialized.
        if hvar.source.ptype == 'host':
            continue
        # end if
        var_stdname = hvar.get_prop_value('standard_name')
        var_locname = hvar.call_string(host_dict)

        # Ignore any variable that is listed as a constiutuent,
        # as they will be handled separately by the constituents object:
        if var_stdname in constituent_set:
            continue
        # end if

        # Set "if-statement" call string:
        call_string_key = f"case ('{var_stdname}')"

        # Extract vertical level variable:
        levnm, call_read_field, reason = get_dimension_info(hvar)
        if hvar.get_prop_value('protected'):
            call_read_field = False
            if reason:
                suff = "; "
            else:
                suff = ""
            # end if
            lvar = hvar.get_prop_value('local_name')
            reason += f"{suff}{lvar} is a protected variable"
        # Set "read_field" call string:
        if call_read_field:
            # Replace vertical dimension with local name
            call_str = "call read_field(file, " +                             \
                       f"'{var_stdname}', input_var_names(:,name_idx), "
            if levnm is not None:
                call_str += f"'{levnm}', "
            # end if
            call_str += f"timestep, {var_locname})"
        else:
            call_str = f"call endrun('Cannot read {var_locname} from file'" + \
                f"//', {reason}')"
        # end if

        # Add string to dictionary:
        call_string_dict[call_string_key] = call_str
    # end for
    # ---------------------------------

    # Write actual subroutine code:
    # ----------------------------

    # Add subroutine header:
    sargs = "file, suite_names, timestep, read_initialized_variables"
    outfile.write(f"subroutine physics_read_data({sargs})", 1)

    # Add use statements:
    use_stmts = [["pio", ["file_desc_t"]],
                 ["cam_abortutils", ["endrun"]],
                 ["spmd_utils", ["masterproc"]],
                 ["shr_kind_mod", ["SHR_KIND_CS, SHR_KIND_CL, SHR_KIND_CX"]],
                 ["physics_data", ["read_field", "find_input_name_idx",
                                   "no_exist_idx", "init_mark_idx",
                                   "prot_no_init_idx", "const_idx"]],
                 ["cam_ccpp_cap", ["ccpp_physics_suite_variables", "cam_constituents_array", "cam_model_const_properties"]],
                 ["ccpp_kinds", ["kind_phys"]],
                 [phys_check_fname_str, ["phys_var_num", "phys_var_stdnames",
                                         "input_var_names", "std_name_len"]],
                 ["ccpp_constituent_prop_mod", ["ccpp_constituent_prop_ptr_t"]],
                 ["cam_logfile", ["iulog"]]]

    # Add in host model data use statements
    use_stmts.extend(host_imports)

    # Output required, registered Fortran module use statements:
    write_use_statements(outfile, use_stmts, 2)

    # Write dummy variable declarations:
    outfile.blank_line()
    outfile.comment("Dummy arguments", 2)
    outfile.write("type(file_desc_t),          intent(inout) :: file", 2)
    outfile.write("character(len=SHR_KIND_CS), intent(in)    :: suite_names(:) !Names of CCPP suites", 2)
    outfile.write("integer,                    intent(in)    :: timestep", 2)
    outfile.write("logical,  optional,         intent(in)    :: read_initialized_variables", 2)
    outfile.blank_line()

    # Write local variable declarations:
    outfile.comment("Local variables:", 2)
    outfile.blank_line()
    outfile.comment("Character array containing all CCPP-required " +         \
                    "variable standard names:", 2)
    outfile.write("character(len=std_name_len), allocatable :: ccpp_required_data(:)", 2)
    outfile.blank_line()
    outfile.comment("Strings which store names of any missing or non-initialized vars:", 2)
    outfile.write("character(len=SHR_KIND_CL) :: missing_required_vars", 2)
    outfile.write("character(len=SHR_KIND_CL) :: protected_non_init_vars", 2)
    outfile.blank_line()
    outfile.write("character(len=SHR_KIND_CX) :: errmsg          !CCPP framework error message", 2)
    outfile.write("integer                    :: errflg          !CCPP framework error flag", 2)
    outfile.write("integer                    :: n               !Loop control variable", 2)
    outfile.write("integer                    :: name_idx        !Input variable array index", 2)
    outfile.write("integer                    :: constituent_idx !Constituent table index", 2)
    outfile.write("integer                    :: const_input_idx !input_var_names index for a consituent", 2)
    outfile.write("integer                    :: req_idx         !Required variable array index", 2)
    outfile.write("integer                    :: suite_idx       !Suite array index", 2)
    outfile.write("character(len=2)           :: sep             !String separator used to print err messages", 2)
    outfile.write("character(len=2)           :: sep2            !String separator used to print err messages", 2)
    outfile.write("character(len=2)           :: sep3            !String separator used to print err messages", 2)
    outfile.write("real(kind=kind_phys), pointer :: field_data_ptr(:,:,:)", 2)
    outfile.write("logical                    :: var_found       !Bool to determine if consituent found in data files", 2)
    outfile.blank_line()
    outfile.comment("Fields needed for getting default data value for constituents", 2)
    outfile.write("type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)", 2)
    outfile.write("real(kind=kind_phys)                       :: constituent_default_value", 2)
    outfile.write("integer                                    :: constituent_errflg", 2)
    outfile.write("character(len=512)                         :: constituent_errmsg", 2)
    outfile.write("logical                                    :: constituent_has_default", 2)
    outfile.blank_line()
    outfile.comment("Logical to default optional argument to False:", 2)
    outfile.write("logical                    :: use_init_variables", 2)
    outfile.blank_line()

    # Initialize variables:
    outfile.comment("Initalize missing and non-initialized variables strings:",
                    2)
    outfile.write("missing_required_vars = ' '", 2)
    outfile.write("protected_non_init_vars = ' '", 2)
    outfile.write("sep = ''", 2)
    outfile.write("sep2 = ''", 2)
    outfile.write("sep3 = ''", 2)
    outfile.blank_line()
    outfile.comment("Initialize use_init_variables based on whether it " +    \
                    "was input to function:", 2)
    outfile.write("if (present(read_initialized_variables)) then", 2)
    outfile.write("use_init_variables = read_initialized_variables", 3)
    outfile.write("else", 2)
    outfile.write("use_init_variables = .false.", 3)
    outfile.write("end if", 2)
    outfile.blank_line()

    # Loop over physics suites:
    outfile.comment("Loop over CCPP physics/chemistry suites:", 2)
    outfile.write("do suite_idx = 1, size(suite_names, 1)", 2)
    outfile.blank_line()

    # Determine physics suite required variables:
    outfile.comment("Search for all needed CCPP input variables, so " +       \
                    "that they can be read from input file if need be:", 3)
    outfile.write("call ccpp_physics_suite_variables" +                       \
                  "(suite_names(suite_idx), ccpp_required_data, errmsg, " +   \
                  "errflg, input_vars=.true., output_vars=.false.)", 4)
    outfile.blank_line()

    # Loop over required variables:
    outfile.comment("Loop over all required variables and read from file if uninitialized:", 3)
    outfile.write("do req_idx = 1, size(ccpp_required_data, 1)", 3)
    outfile.blank_line()

    # Call input name search function:
    outfile.comment("Find IC file input name array index for required variable:", 4)
    outfile.write("name_idx = find_input_name_idx(ccpp_required_data(req_idx), use_init_variables, constituent_idx)", 4)

    # Start select-case statement:
    outfile.blank_line()
    outfile.comment("Check for special index values:", 4)
    outfile.write("select case (name_idx)", 4)
    outfile.blank_line()

    # Skip already initialized variables:
    outfile.write("case (init_mark_idx)", 5)
    outfile.blank_line()
    outfile.comment("If variable is already initialized, then do nothing.", 6)
    outfile.blank_line()

    # Generate error message if required variable isn't found:
    outfile.write("case (no_exist_idx)", 5)
    outfile.blank_line()
    outfile.comment("If an index was never found, then save variable name " + \
                    "and check the rest of the variables, after which the " + \
                    "model simulation will end:", 6)
    outfile.write("missing_required_vars(len_trim(missing_required_vars)+1:)"+\
                  " = trim(sep)//trim(ccpp_required_data(req_idx))", 7)
    outfile.blank_line()
    outfile.comment("Update character separator to now include comma:", 6)
    outfile.write("sep = ', '", 6)
    outfile.blank_line()

    # Generate error message if required variable is protected but not initialized:
    outfile.write("case (prot_no_init_idx)", 5)
    outfile.blank_line()
    outfile.comment("If an index was found for a protected variable, but " +  \
                    "that variable was never marked as initialized, then " +  \
                    "save the variable name and check the rest of the "    +  \
                    "variables, after which the model simulation will end:", 6)
    outfile.write("protected_non_init_vars(len_trim(protected_non_init_vars)+1:) = " + \
                  "trim(sep2)//trim(ccpp_required_data(req_idx))", 7)
    outfile.blank_line()
    outfile.comment("Update character separator to now include comma:", 6)
    outfile.write("sep2 = ', '", 6)
    outfile.blank_line()

    # Handle the case where the required variable is a constituent
    outfile.write("case (const_idx)", 5)
    outfile.blank_line()
    outfile.comment("If an index was found in the constituent hash table, then read in the data to that index of the constituent array", 6)
    outfile.blank_line()
    outfile.write("var_found = .false.", 6)
    outfile.write("field_data_ptr => cam_constituents_array()", 6)
    outfile.blank_line()
    outfile.comment("Check if constituent standard name in registered SIMA standard names list:", 6)
    outfile.write("if(any(phys_var_stdnames == ccpp_required_data(req_idx))) then", 6)
    outfile.comment("Find array index to extract correct input names:", 7)
    outfile.write("do n=1, phys_var_num", 7)
    outfile.write("if(trim(phys_var_stdnames(n)) == trim(ccpp_required_data(req_idx))) then", 8)
    outfile.write("const_input_idx = n", 9)
    outfile.write("exit", 9)
    outfile.write("end if", 8)
    outfile.write("end do", 7)
    outfile.write("call read_field(file, ccpp_required_data(req_idx), input_var_names(:,const_input_idx), 'lev', timestep, field_data_ptr(:,:,constituent_idx), mark_as_read=.false., error_on_not_found=.false., var_found=var_found)", 7)
    outfile.write("else", 6)
    outfile.comment("If not in standard names list, then just use constituent name as input file name:",7)
    outfile.write("call read_field(file, ccpp_required_data(req_idx), [ccpp_required_data(req_idx)], 'lev', timestep, field_data_ptr(:,:,constituent_idx), mark_as_read=.false., error_on_not_found=.false., var_found=var_found)", 7)
    outfile.write("end if", 6)
    outfile.write("if(.not. var_found) then", 6)
    outfile.write("const_props => cam_model_const_properties()", 7)
    outfile.write("constituent_has_default = .false.", 7)
    outfile.write("call const_props(constituent_idx)%has_default(constituent_has_default, constituent_errflg, constituent_errmsg)", 7)
    outfile.write("if (constituent_errflg /= 0) then", 7)
    outfile.write("call endrun(constituent_errmsg, file=__FILE__, line=__LINE__)", 8)
    outfile.write("end if", 7)
    outfile.write("if (constituent_has_default) then", 7)
    outfile.write("call const_props(constituent_idx)%default_value(constituent_default_value, constituent_errflg, constituent_errmsg)", 8)
    outfile.write("if (constituent_errflg /= 0) then", 8)
    outfile.write("call endrun(constituent_errmsg, file=__FILE__, line=__LINE__)", 9)
    outfile.write("end if", 8)
    outfile.write("field_data_ptr(:,:,constituent_idx) = constituent_default_value", 8)
    outfile.write("if (masterproc) then", 8)
    outfile.write("write(iulog,*) 'Consitituent ', trim(ccpp_required_data(req_idx)), ' initialized to default value: ', constituent_default_value", 9)
    outfile.write("end if", 8)
    outfile.write("else", 7)
    outfile.write("field_data_ptr(:,:,constituent_idx) = 0._kind_phys", 8)
    outfile.write("if (masterproc) then", 8)
    outfile.write("write(iulog,*) 'Constituent ', trim(ccpp_required_data(req_idx)), ' default value not configured.  Setting to 0.'", 9)
    outfile.write("end if", 8)
    outfile.write("end if", 7)
    outfile.write("end if", 6)
    outfile.blank_line()

    # start default case steps:
    outfile.write("case default", 5)
    outfile.blank_line()

    # Generate "read_field" calls:
    outfile.comment("Read variable from IC file:", 6)
    outfile.blank_line()
    outfile.write("select case (trim(phys_var_stdnames(name_idx)))", 6)
    for case_call, read_call in call_string_dict.items():
        outfile.write(case_call, 7)
        outfile.write(read_call, 8)
        outfile.blank_line()
    outfile.write("end select !read variables", 6)
    # end select

    # End select case and required variables loop:
    outfile.write("end select !special indices", 5)
    outfile.blank_line()
    outfile.write("end do !Suite-required variables", 3)
    outfile.blank_line()

    # Generate endrun statement for missing variables:
    outfile.comment("End simulation if there are missing input variables " +  \
                    "that are required:", 3)
    outfile.write("if (len_trim(missing_required_vars) > 0) then", 3)
    outfile.write('call endrun("Required variables missing from registered list of input variables: "//&', 4)
    outfile.write("trim(missing_required_vars))", 5)
    outfile.write("end if", 3)
    outfile.blank_line()

    # Generate endrun statement for non-initialized protected variables:
    outfile.comment("End simulation if there are protected input " +          \
                    "variables that are not initialized:", 3)
    outfile.write("if (len_trim(protected_non_init_vars) > 0) then", 3)
    outfile.write('call endrun("Required, protected input variables are not initialized: "//&', 4)
    outfile.write("trim(protected_non_init_vars))", 5)
    outfile.write("end if", 3)
    outfile.blank_line()

    # Deallocate ccpp_required_data array:
    outfile.comment("Deallocate required variables array for use in next suite:", 3)
    outfile.write("deallocate(ccpp_required_data)", 3)
    outfile.blank_line()

    # End suite loop:
    outfile.write(" end do !CCPP suites", 2)
    outfile.blank_line()

    # End subroutine:
    outfile.write("end subroutine physics_read_data", 1)

    # ----------------------------

#####

def write_phys_check_subroutine(outfile, host_dict, host_vars, host_imports,
                                phys_check_fname_str, constituent_set):

    """
    Write the "physics_check_data" subroutine, which
    is used to check the physics variables against
    an optionally input check file by reading
    in the values from the check file and comparing
    the values to the variables
    """

    # Create Fortran "check_field" calls:
    # ---------------------------------

    # Create new (empty) dictionary to store "check_field" calls:
    call_string_dict = OrderedDict()
    # Create a check_field call for each host-model variable
    for hvar in host_vars:
        # We do not 'check' variables from the 'host' table as they are
        #    passed to physics via the argument list.
        # As such, they are always considered initialized.
        if hvar.source.ptype == 'host':
            continue
        # end if
        var_stdname = hvar.get_prop_value('standard_name')
        var_locname = hvar.call_string(host_dict)

        # Ignore any variable that is listed as a constiutuent,
        # as they will be handled separately by the constituents object:
        if var_stdname in constituent_set:
            continue
        # end if

        # Set "if-statement" call string:
        call_string_key = f"case ('{var_stdname}')"

        # Extract vertical level variable:
        levnm, call_check_field, reason = get_dimension_info(hvar)

        # Set "check_field" call string:
        if call_check_field:
            call_str = "call check_field(file, input_var_names(:,name_idx), "
            if levnm is not None:
                call_str += f"'{levnm}', "
            # end if
            call_str += f"timestep, {var_locname}, '{var_stdname}', "
            call_str += "min_difference, min_relative_value, is_first)"
        else:
            call_str = f"call endrun('Cannot check status of {var_locname}'" + \
                f"//', {reason}')"
        # end if
        # Add string to dictionary:
        call_string_dict[call_string_key] = call_str
    # end for

    # ---------------------------------
    # Write actual subroutine code:
    # ---------------------------------

    # Add subroutine header:
    outfile.write("subroutine physics_check_data(file_name, suite_names, " +  \
                  "timestep, min_difference, min_relative_value)", 1)

    use_stmts = [["pio", ["file_desc_t", "pio_nowrite"]],
                 ["cam_abortutils", ["endrun"]],
                 ["shr_kind_mod", ["SHR_KIND_CS, SHR_KIND_CL, SHR_KIND_CX"]],
                 ["physics_data", ["check_field", "find_input_name_idx",
                                   "no_exist_idx", "init_mark_idx",
                                   "prot_no_init_idx", "const_idx"]],
                 ["cam_ccpp_cap", ["ccpp_physics_suite_variables", "cam_advected_constituents_array"]],
                 ["cam_constituents", ["const_get_index"]],
                 ["ccpp_kinds", ["kind_phys"]],
                 ["cam_logfile", ["iulog"]],
                 ["spmd_utils", ["masterproc"]],
                 ["phys_vars_init_check", ["is_read_from_file"]],
                 ["ioFileMod", ["cam_get_file"]],
                 ["cam_pio_utils", ["cam_pio_openfile", "cam_pio_closefile"]],
                 [phys_check_fname_str, ["phys_var_num",
                                         "phys_var_stdnames",
                                         "input_var_names",
                                         "std_name_len"]]]

    # Add in host model data use statements
    use_stmts.extend(host_imports)

    # Output required, registered Fortran module use statements:
    write_use_statements(outfile, use_stmts, 2)

    # Write dummy variable declarations:
    outfile.blank_line()
    outfile.comment("Dummy arguments", 2)
    outfile.write("character(len=SHR_KIND_CL), intent(in) :: file_name", 2)
    outfile.write("character(len=SHR_KIND_CS), intent(in) :: suite_names(:) !Names of CCPP suites", 2)
    outfile.write("integer,                    intent(in) :: timestep", 2)
    outfile.write("real(kind_phys),            intent(in) :: min_difference", 2)
    outfile.write("real(kind_phys),            intent(in) :: min_relative_value", 2)
    outfile.blank_line()

    # Write local variable declarations:
    outfile.comment("Local variables:", 2)
    outfile.blank_line()
    outfile.comment("Character array containing all CCPP-required " +         \
                    "variable standard names:", 2)
    outfile.write("character(len=std_name_len), allocatable :: ccpp_required_data(:)", 2)
    outfile.blank_line()
    outfile.comment("Strings which store names of any missing or " +          \
                    "non-initialized vars:", 2)
    outfile.write("character(len=SHR_KIND_CL) :: missing_required_vars", 2)
    outfile.write("character(len=SHR_KIND_CL) :: protected_non_init_vars", 2)
    outfile.write("character(len=SHR_KIND_CL) :: missing_input_names", 2)
    outfile.blank_line()
    outfile.write("character(len=SHR_KIND_CX) :: errmsg    !CCPP framework error message", 2)
    outfile.write("integer                    :: errflg    !CCPP framework error flag", 2)
    outfile.write("integer                    :: n         !Loop control variable", 2)
    outfile.write("integer                    :: name_idx  !Input variable array index", 2)
    outfile.write("integer                    :: constituent_idx !Index of variable in constituent array", 2)
    outfile.write("integer                    :: const_input_idx !input_var_names index for a consituent", 2)
    outfile.write("integer                    :: req_idx   !Required variable array index", 2)
    outfile.write("integer                    :: suite_idx !Suite array index", 2)
    outfile.write("character(len=SHR_KIND_CL) :: ncdata_check_loc", 2)
    outfile.write("type(file_desc_t), pointer :: file", 2)
    outfile.write("logical                    :: file_found", 2)
    outfile.write("logical                    :: is_first", 2)
    outfile.write("logical                    :: is_read", 2)
    outfile.write("real(kind=kind_phys), pointer :: field_data_ptr(:,:,:)", 2)
    outfile.blank_line()

    # Initialize variables:
    outfile.comment("Initalize missing and non-initialized variables strings:",
                    2)
    outfile.write("missing_required_vars = ' '", 2)
    outfile.write("protected_non_init_vars = ' '", 2)
    outfile.write("missing_input_names   = ' '", 2)
    outfile.write("nullify(file)", 2)
    outfile.write("is_first = .true.", 2)
    outfile.blank_line()

    # Begin check data log:
    outfile.write("if (masterproc) then", 2)
    outfile.write("write(iulog,*) ''", 3)
    outfile.write("write(iulog,*) '********** Physics Check Data Results **********'", 3)
    outfile.write("write(iulog,*) ''", 3)
    outfile.write("write(iulog,*) 'TIMESTEP: ', timestep", 3)
    outfile.write("end if", 2)

    # Open check file:
    outfile.write("if (file_name == 'UNSET') then", 2)
    outfile.write("write(iulog,*) 'WARNING: Namelist variable " +             \
                  "ncdata_check is UNSET.', ' Model will run, but " +         \
                  "physics check data will not be printed'", 3)
    outfile.write("return", 3)
    outfile.write("end if", 2)
    outfile.comment("Open check file:", 2)
    outfile.write("call cam_get_file(file_name, ncdata_check_loc, " +         \
                  "allow_fail=.true., lexist=file_found, log_info=.false.)", 2)
    outfile.write("if (.not. file_found) then", 2)
    outfile.write("write(iulog,*) 'WARNING: Check file ', trim(file_name), " + \
                  "' not found. Model will run, but physics check data " +     \
                  "will not be printed'", 3)
    outfile.write("return", 3)
    outfile.write("end if", 2)
    outfile.write("allocate(file)", 2)
    outfile.write("call cam_pio_openfile(file, ncdata_check_loc, " +          \
                  "pio_nowrite, log_info=.false.)", 2)

    # Loop over physics suites:
    outfile.comment("Loop over CCPP physics/chemistry suites:", 2)
    outfile.write("do suite_idx = 1, size(suite_names, 1)", 2)
    outfile.blank_line()

    # Determine physics suite required variables:
    outfile.comment("Search for all needed CCPP input variables, so " +       \
                    "that they can be read from input file if need be:", 3)
    outfile.write("call ccpp_physics_suite_variables(suite_names" +           \
                  "(suite_idx), ccpp_required_data, errmsg, errflg, " +       \
                  "input_vars=.true., output_vars=.false.)", 4)
    outfile.blank_line()

    # Loop over required variables:
    outfile.comment("Loop over all required variables as specified by CCPP suite:",
                    3)
    outfile.write("do req_idx = 1, size(ccpp_required_data, 1)", 3)
    outfile.blank_line()

    # First check if the required variable is a constituent
    outfile.comment("First check if the required variable is a constituent:", 4)
    outfile.write("call const_get_index(ccpp_required_data(req_idx), constituent_idx, abort=.false., warning=.false.)", 4)
    outfile.write("if (constituent_idx > -1) then", 4)
    outfile.comment("The required variable is a constituent. Call check variable routine on the relevant index of the constituent array", 5)
    outfile.write("field_data_ptr => cam_advected_constituents_array()", 5)
    outfile.blank_line()
    outfile.comment("Check if constituent standard name in registered SIMA standard names list:", 5)
    outfile.write("if(any(phys_var_stdnames == ccpp_required_data(req_idx))) then", 5)
    outfile.comment("Find array index to extract correct input names:", 6)
    outfile.write("do n=1, phys_var_num", 6)
    outfile.write("if(trim(phys_var_stdnames(n)) == trim(ccpp_required_data(req_idx))) then", 7)
    outfile.write("const_input_idx = n", 8)
    outfile.write("exit", 8)
    outfile.write("end if", 7)
    outfile.write("end do", 6)
    outfile.write("call check_field(file, input_var_names(:,const_input_idx), 'lev', timestep, field_data_ptr(:,:,constituent_idx), ccpp_required_data(req_idx), min_difference, min_relative_value, is_first)", 6)
    outfile.write("else", 5)
    outfile.comment("If not in standard names list, then just use constituent name as input file name:",6)
    outfile.write("call check_field(file, [ccpp_required_data(req_idx)], 'lev', timestep, field_data_ptr(:,:,constituent_idx), ccpp_required_data(req_idx), min_difference, min_relative_value, is_first)", 6)
    outfile.write("end if", 5)

    outfile.write("else", 4)
    outfile.comment("The required variable is not a constituent. Check if the variable was read from a file", 5)

    # Call input name search function:
    outfile.comment("Find IC file input name array index for required variable:", 5)
    outfile.write("call is_read_from_file(ccpp_required_data(req_idx), " +    \
                  "is_read, stdnam_idx_out=name_idx)", 5)
    outfile.write("if (.not. is_read) then", 5)
    outfile.write("cycle", 6)
    outfile.write("end if", 5)

    # Generate "check_field" calls:
    outfile.comment("Check variable vs input check file:", 5)
    outfile.blank_line()
    outfile.write("select case (trim(phys_var_stdnames(name_idx)))", 5)
    for case_call, read_call in call_string_dict.items():
        outfile.write(case_call, 5)
        outfile.write(read_call, 6)
        outfile.blank_line()
    outfile.write("end select !check variables", 5)
    outfile.write("end if !check if constituent", 4)

    # End select case and required variables loop:
    outfile.write("end do !Suite-required variables", 3)
    outfile.blank_line()

    # Deallocate ccpp_required_data array:
    outfile.comment("Deallocate required variables array for use in next suite:",
                    3)
    outfile.write("deallocate(ccpp_required_data)", 3)
    outfile.blank_line()

    # End suite loop:
    outfile.write("end do !CCPP suites", 2)
    outfile.blank_line()

    # Close check file
    outfile.comment("Close check file:", 2)
    outfile.write("call cam_pio_closefile(file)", 2)
    outfile.write("deallocate(file)", 2)
    outfile.write("nullify(file)", 2)

    # Check if no differences were found
    outfile.write("if (is_first) then", 2)
    outfile.write("if (masterproc) then", 3)
    outfile.write("write(iulog,*) ''", 4)
    outfile.write("write(iulog,*) 'No differences found!'", 4)
    outfile.write("end if", 3)
    outfile.write("end if", 2)

    # End check data log:
    outfile.write("if (masterproc) then", 2)
    outfile.write("write(iulog,*) ''", 3)
    outfile.write("write(iulog,*) '********** End Physics Check Data " +      \
                  "Results **********'", 3)
    outfile.write("write(iulog,*) ''", 3)
    outfile.write("end if", 2)

    # End subroutine:
    outfile.write("end subroutine physics_check_data", 1)

    # ----------------------------

#############
# End of file
#############
