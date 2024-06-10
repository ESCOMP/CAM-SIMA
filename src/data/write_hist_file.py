#!/usr/bin/env python3

"""
Use variable meta-data from "generate_registry_data.py"
to generate a CAM fortran file that manage host model
variable history add and out fields
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

def write_hist_file(cap_database, diag_names, outdir, file_find_func,
                     source_paths, indent, logger, phys_hist_filename=None):

    """
    Create the physics history Fortran file using a database
       created by the CCPP Framework generator (capgen).
    The specific Fortran file is:

    physics_history.F90

      This file contains two public subroutines:

        physics_history_init
            Includes calls to history_add_field
            for each physics variable from the registry

        physics_history_out
            Includes calls to history_out_field
            for each physics variable from the registry

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

    # Generate "physics_history.F90" file:
    # -----------------------------------------

    # Open new file:
    if phys_hist_filename:
        ofilename = os.path.join(outdir, phys_hist_filename)
        # Get file name, ignoring file type:
        physics_history_fname_str = os.path.splitext(phys_hist_filename)[0]
    else:
        ofilename = os.path.join(outdir, "physics_history.F90")
        physics_history_fname_str = "physics_history"
    # end if

    # Log file creation:
    logger.info(f"Writing physics history source file, {ofilename}")

    # Open file using CCPP's FortranWriter:
    file_desc = "Physics history source file"
    with FortranWriter(ofilename, "w", file_desc,
                       physics_history_fname_str,
                       line_fill=_LINE_FILL_LEN,
                       line_max=_MAX_LINE_LEN,
                       indent=indent) as outfile:

        # Add boilerplate code:
        outfile.write_preamble()
        outfile.blank_line()

        # Add public function declarations:
        outfile.write("!! public interfaces", 0)
        outfile.write("public :: physics_history_init", 1)
        outfile.write("public :: physics_history_out", 1)

        # Add "contains" statement:
        outfile.end_module_header()
        outfile.blank_line()

        # Grab the host dictionary from the database
        host_dict = cap_database.host_model_dict()

        # Collect imported host variables
        host_imports = collect_host_var_imports(host_vars, host_dict, constituent_set, diag_names)

        # Write physics_history_init subroutine:
        write_physics_history_init_subroutine(outfile, host_dict, host_vars, host_imports,
                                   diag_names, physics_history_fname_str, constituent_set)

        outfile.blank_line()

        # Write physics_history_out subroutine:
        write_physics_history_out_subroutine(outfile, host_dict, host_vars, host_imports,
                                   diag_names, physics_history_fname_str, constituent_set)

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
            is_const = cvar.get_prop_value('advected')
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

def collect_host_var_imports(host_vars, host_dict, constituent_set, diag_dict):
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
        # We only import variables from the registry
        if hvar.get_prop_value('standard_name') not in diag_dict:
            continue
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

def write_physics_history_init_subroutine(outfile, host_dict, host_vars, host_imports,
                               diag_dict, phys_check_fname_str, constituent_set):

    """
    Write the "physics_history_init" subroutine, which
    is used to call history_add_field for all
    physics variables.
    """

    # -----------------------------------------
    # Write subroutine code:
    # -----------------------------------------

    # Add subroutine header
    outfile.write(f"subroutine physics_history_init()", 1)

    # Add use statements:
    use_stmts = [["cam_ccpp_cap", ["cam_model_const_properties"]],
                 ["cam_history",  ["history_add_field"]],
                 ["cam_history_support", ["horiz_only"]],
                 ["cam_constituents", ["const_get_index"]],
                 ["ccpp_constituent_prop_mod", ["ccpp_constituent_prop_ptr_t"]]]

    # Add in host model data use statements
    use_stmts.extend(host_imports)
    write_use_statements(outfile, use_stmts, 2)
    outfile.blank_line()

    # Write local variable declarations:
    outfile.comment("Local variables:", 2)
    outfile.blank_line()

    outfile.write('integer :: const_index', 2)
    outfile.write('integer :: errcode', 2)
    outfile.write('logical :: const_is_dry', 2)
    outfile.write('character(len=256) :: errmsg', 2)
    outfile.write('type(ccpp_constituent_prop_ptr_t), pointer :: const_props_ptr(:)', 2)
    subn_str = 'character(len=*), parameter :: subname = "physics_history_init"'
    outfile.write(subn_str, 2)
    outfile.blank_line()

    # -----------------------------------------
    # Create Fortran "history_add_field" calls:
    # -----------------------------------------

    # Loop over all variable standard names:
    for hvar in host_vars:
        var_stdname    = hvar.get_prop_value('standard_name')
        var_locname    = hvar.call_string(host_dict)
        var_units      = hvar.get_prop_value('units')
        vdim_name, legal_dims, fail_reason = get_dimension_info(hvar)
        if vdim_name is not None:
           vdim = f"'{vdim_name}'"
        else:
           vdim = 'horiz_only'
        # end if

        # only add add_field call if the variable has a diagnostic name
        if var_stdname not in diag_dict:
            continue
        # end if

        diag_name = diag_dict[var_stdname][0]
        diag_flag = diag_dict[var_stdname][1]

        # Ignore any variable that is listed as a constiutuent,
        # as they will be handled separately by the constituents object:
        if var_stdname in constituent_set:
            outfile.write(f"call const_get_index('{var_stdname}', const_index, abort=.false., warning=.false.)", 2)
            outfile.write("if (const_index >= 0) then", 2)
            outfile.write("const_props_ptr => cam_model_const_properties()", 3)
            outfile.write("call const_props_ptr(const_index)%is_dry(const_is_dry, errcode, errmsg)", 3)
            outfile.write("if (const_is_dry) then", 3)
            outstr = f"call history_add_field('{diag_name}', '{var_stdname}', " \
                     f"{vdim}, '{diag_flag}', '{var_units}', mixing_ratio='dry')"
            outfile.write(outstr, 4)
            outfile.write("else", 3)
            outstr = f"call history_add_field('{diag_name}', '{var_stdname}', " \
                     f"{vdim}, '{diag_flag}', '{var_units}', mixing_ratio='wet')"
            outfile.write(outstr, 4)
            outfile.write("end if", 3)
            outfile.write("end if", 2)
        else:
            outstr = f"call history_add_field('{diag_name}', '{var_stdname}', {vdim}, '{diag_flag}', '{var_units}')"
            outfile.write(outstr, 2)
        # end if
    # end for
    # End subroutine:
    outfile.blank_line()
    outfile.write("end subroutine physics_history_init", 1)

#####

def write_physics_history_out_subroutine(outfile, host_dict, host_vars, host_imports,
                               diag_dict, phys_check_fname_str, constituent_set):

    """
    Write the "physics_history_out" subroutine, which
    is used to call history_out_field for all
    physics variables in the registry.
    """

    # -----------------------------------------
    # Write subroutine code:
    # -----------------------------------------

    # Add subroutine header
    outfile.write(f"subroutine physics_history_out()", 1)

    # Add use statements:
    use_stmts = [["cam_ccpp_cap", ["cam_constituents_array"]],
                 ["cam_history",  ["history_out_field"]],
                 ["cam_constituents", ["const_get_index"]],
                 ["ccpp_kinds",   ["kind_phys"]],
                 ["ccpp_constituent_prop_mod", ["ccpp_constituent_prop_ptr_t"]]]

    # Add in host model data use statements
    use_stmts.extend(host_imports)
    write_use_statements(outfile, use_stmts, 2)
    outfile.blank_line()

    # Write local variable declarations:
    outfile.comment("Local variables:", 2)
    outfile.blank_line()

    outfile.write('!! Local variables', 2)
    outfile.write('real(kind_phys), pointer :: const_data_ptr(:,:,:)', 2)
    outfile.write('character(len=512) :: standard_name', 2)
    outfile.write('integer :: const_index', 2)
    subn_str = 'character(len=*), parameter :: subname = "physics_history_out"'
    outfile.write(subn_str, 2)
    outfile.blank_line()

    # -----------------------------------------
    # Create Fortran "history_add_field" calls:
    # -----------------------------------------

    # Loop over all variable standard names:
    for hvar in host_vars:
        var_stdname    = hvar.get_prop_value('standard_name')
        var_locname    = hvar.call_string(host_dict)

        # only add add_field call if the variable has a diagnostic name
        if var_stdname not in diag_dict:
            continue
        # end if

        diag_name = diag_dict[var_stdname][0]

        # Ignore any variable that is listed as a constiutuent,
        # as they will be handled separately by the constituents object:
        if var_stdname in constituent_set:
            outfile.write(f"call const_get_index('{var_stdname}', const_index, abort=.false., warning=.false.)", 2)
            outfile.write("if (const_index >= 0) then", 2)
            outfile.write("const_data_ptr => cam_constituents_array()", 3)
            outstr = f"call history_out_field('{diag_name}', const_data_ptr(:,:,const_index))"
            outfile.write(outstr, 3)
            outfile.write("end if", 2)
        else:
            outstr = f"call history_out_field('{diag_name}', {var_locname})"
            outfile.write(outstr, 2)
        # end if
    # end for
    # End subroutine:
    outfile.blank_line()
    outfile.write("end subroutine physics_history_out", 1)

    # ----------------------------

#############
# End of file
#############
