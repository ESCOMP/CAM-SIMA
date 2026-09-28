"""
Use variable meta-data from "generate_registry_data.py"
to generate a CAM fortran file to manage physics restart 
information and writing (restart_physics.F90).
"""

# Python library import statements:
import os.path

# CCPP Framework import statements
from ccpp_state_machine import CCPP_STATE_MACH
from fortran_tools import FortranWriter
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
_OUTPUT_TYPES = set(['out', 'inout'])

# Increase allowed line lengths needed to fit extra-long CCPP standard names:
_LINE_FILL_LEN = 150
_MAX_LINE_LEN = 200

##############
#Main function
##############

def write_restart_physics(cap_database, registry_constituents, restart_vars,
                     outdir, indent, logger, phys_restart_filename=None):

    """
    Create restart_physics.F90 using a database created
       by the CCPP Framework generator (capgen).
    """

    #Initialize return message:
    retmsg = ""

    # Gather all the host model variables that are required by
    #    any of the compiled CCPP physics suites.
    in_vars, out_vars, retmsg = gather_ccpp_req_vars(cap_database, registry_constituents)

    # Quit now if there are missing variables
    if retmsg:
        return retmsg
    # end if

    # -----------------------------------------
    # Generate "restart_physics.F90" file:
    # -----------------------------------------

    # Open new file:
    if phys_restart_filename:
        ofilename = os.path.join(outdir, phys_restart_filename)
        # Get file name, ignoring file type:
        phys_restart_fname_str = os.path.splitext(phys_restart_filename)[0]
    else:
        ofilename = os.path.join(outdir, "restart_physics.F90")
        phys_restart_fname_str = "restart_physics"
    # end if

    # Log file creation:
    logger.info(f"Writing restart physics source file, {ofilename}")

    # Open file using CCPP's FortranWriter:
    file_desc = "physics restart source file"
    with FortranWriter(ofilename, "w", file_desc,
                       phys_restart_fname_str,
                       line_fill=_LINE_FILL_LEN,
                       line_max=_MAX_LINE_LEN,
                       indent=indent) as outfile:

        # Add use statements
        outfile.write("use pio, only: var_desc_t", 1)
        outfile.blank_line()
        # Add boilerplate code:
        outfile.write_preamble()

        # Add public function declarations:
        outfile.write("!! public interfaces", 0)
        outfile.write("public :: restart_physics_init", 1)
        outfile.write("public :: restart_physics_write", 1)
        outfile.write("public :: restart_physics_read", 1)

        all_req_vars = in_vars + out_vars
        # Grab the required variables that are also restart variables
        host_dict = cap_database.host_model_dict()
        required_restart_vars, constituent_dimmed_vars, used_vars = gather_required_restart_variables(all_req_vars, restart_vars, host_dict)

        outfile.blank_line()
        outfile.comment("Private module data", 0)
        for _, value in required_restart_vars.items():
            outfile.write(f"type(var_desc_t) :: {value['diag_name'].lower()}_desc", 1)
        # end for

        for _, value in constituent_dimmed_vars.items():
            outfile.write(f"type(var_desc_t), allocatable :: {value['diag_name'].lower()}_desc(:)", 1)
        # end for

        outfile.write("type(var_desc_t), allocatable :: cnst_desc(:)", 1)

        # Add "contains" statement:
        outfile.end_module_header()

        # Write the restart init subroutine
        outfile.blank_line()
        dim_use_stmts, dimensions_dict = write_restart_physics_init(outfile, required_restart_vars, constituent_dimmed_vars, host_dict)

        # Write the restart write subroutine
        outfile.blank_line()
        write_restart_physics_write(outfile, required_restart_vars, constituent_dimmed_vars, used_vars, dim_use_stmts, dimensions_dict)

        # Write the restart read subroutine
        outfile.blank_line()
        write_restart_physics_read(outfile, required_restart_vars, constituent_dimmed_vars, used_vars)

    # end with

    return retmsg

def write_restart_physics_init(outfile, required_vars, constituent_dimmed_vars, host_dict):
    """
    Write the init routine for the physics restart variables. This
    routine initializes the physics fields on the restart (cam.r) file
    """

    outfile.write("subroutine restart_physics_init(file, errmsg, errflg)", 1)

    dimensions_dict = {}
    num_dimensions = 2
    dim_use_stmt_dict = {}
    excluded_dims = ['horizontal_loop_extent']

    # First add default dimensions (ncol, lev) - needed for constituent handling regardless of other fields
    dimensions_dict['horizontal_dimension'] = {'local_name': 'ncol', 'index': 1, 'size': 'num_global_phys_cols', 'import': 'columns_on_task'}
    dimensions_dict['vertical_layer_dimension'] = {'local_name': 'lev', 'index': 2, 'size': 'pver', 'import': 'pver'}

    # Find all unique dimensions
    for _, value in required_vars.items():
        for dimension in value['dims']:
            dim_name = dimension.split(':')[1]
            if dim_name not in dimensions_dict and dim_name not in excluded_dims:
                var = host_dict.find_variable(dim_name)
                dimensions_dict[dim_name] = {'local_name': var.get_prop_value('local_name'), 'index': -1, 'size':'', 'import': var.get_prop_value('local_name')}
                num_dimensions = num_dimensions + 1
                # Add dimension to use statement dictionary
                if var.source.name not in dim_use_stmt_dict:
                    dim_use_stmt_dict[var.source.name] = set()
                # end if
                dim_use_stmt_dict[var.source.name].add(var.get_prop_value('local_name'))
            # end if
        # end for
    # end for
    for _, value in constituent_dimmed_vars.items():
        for dimension in value['dims']:
            dim_name = dimension.split(':')[1]
            if dim_name not in dimensions_dict and dim_name not in excluded_dims and dim_name != 'number_of_ccpp_constituents':
                var = host_dict.find_variable(dim_name)
                dimensions_dict[dim_name] = {'local_name': var.get_prop_value('local_name'), 'index': -1, 'size':'', 'import': var.get_prop_value('local_name')}
                num_dimensions = num_dimensions + 1
                # Add dimension to use statement dictionary
                if var.source.name not in dim_use_stmt_dict:
                    dim_use_stmt_dict[var.source.name] = set()
                # end if
                dim_use_stmt_dict[var.source.name].add(var.get_prop_value('local_name'))
            # end if
        # end for
    # end for

    static_use_stmts = [["pio", ["file_desc_t", "pio_double"]],
                 ["cam_pio_utils", ["cam_pio_def_dim", "cam_pio_def_var"]],
                 ["cam_ccpp_cap", ["cam_model_const_properties", "cam_constituents_array"]],
                 ["physics_grid", ["num_global_phys_cols"]],
                 ["ccpp_constituent_prop_mod", ["ccpp_constituent_prop_ptr_t"]]]

    # Gather up dimension imports
    dim_use_stmts = [['vert_coord', ['pver']], ['physics_grid', ['columns_on_task']]]
    for key in sorted(dim_use_stmt_dict):
        imports = []
        for var_import in sorted(dim_use_stmt_dict[key]):
            imports.append(var_import)
        # end for
        dim_use_stmts.append([key, imports])
    # end for

    # Output required, registered Fortran module use statements:
    write_use_statements(outfile, static_use_stmts, 2)
    write_use_statements(outfile, dim_use_stmts, 2)

    outfile.write("type(file_desc_t), intent(inout) :: file", 2)
    outfile.write("character(len=512),intent(out)   :: errmsg", 2)
    outfile.write("integer,           intent(out)   :: errflg", 2)
    outfile.blank_line()

    outfile.comment("Local variables", 2)
    outfile.write("integer, allocatable :: dimids(:)", 2)
    outfile.write("integer :: constituent_idx", 2)
    outfile.write("integer :: nonadvected_idx", 2)
    outfile.write("logical :: advected", 2)
    outfile.write("type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)", 2)
    outfile.write("character(len=256) :: const_diag_name", 2)

    outfile.blank_line()

    outfile.blank_line()
    outfile.comment("Allocate dimids to the number of unique dimensions", 2)
    outfile.write(f"allocate(dimids({num_dimensions}), stat=errflg, errmsg=errmsg)", 2)
    outfile.write("if (errflg /= 0) then", 2)
    outfile.write("return", 3)
    outfile.write("end if", 2)

    # Define static dimensions
    outfile.write("call cam_pio_def_dim(file, 'ncol', num_global_phys_cols, dimids(1), existOK=.true.)", 2)
    outfile.write("call cam_pio_def_dim(file, 'lev', pver, dimids(2), existOK=.true.)", 2)

    # Start at 3; index=1 is ncol, index=2 is lev
    dim_index = 3
    for key, value in required_vars.items():
        hdimids = []
        for dimension in value['dims']:
            dimname = dimension.split(':')[1]
            if dimensions_dict[dimname]['index'] < 0:
                outfile.comment(f"Define potentially new dimension '{dimname}'", 2)
                if dimname == 'vertical_interface_dimension':
                    dimensions_dict[dimname]['local_name'] = 'ilev'
                    dimensions_dict[dimname]['size'] = 'pverp'
                    dimensions_dict[dimname]['index'] = dim_index
                    dim_index = dim_index + 1
                # Skip ncol, pver; handled by default
                elif dimname not in ('horizontal_dimension', 'vertical_dimension'):
                    dim_loc_name = dimensions_dict[dimname]['local_name']
                    dimensions_dict[dimname]['size'] = dim_loc_name
                    dimensions_dict[dimname]['index'] = dim_index
                    dim_index = dim_index + 1
                # end if
                outfile.write(f"call cam_pio_def_dim(file, '{dimensions_dict[dimname]['local_name']}', {dimensions_dict[dimname]['size']}, dimids({dimensions_dict[dimname]['index']}), existOK=.true.)", 2)
            # end if
            hdimids.append(dimensions_dict[dimname]['index'])
        # end for
        outfile.blank_line()
        outfile.comment("Define required restart variables on the restart file", 2)
        desc_name = f"{value['diag_name'].lower()}_desc"
        dimids_array = ", ".join([f"dimids({i})" for i in hdimids])
        outfile.write(f"call cam_pio_def_var(file, '{value['diag_name']}', pio_double, (/{dimids_array}/), {desc_name}, existOK=.false.)", 2)
        outfile.write("if (errflg /= 0) then", 2)
        outfile.write(f"write(errmsg,*) 'restart_physics_init: error defining variable {value['diag_name']}'", 3)
        outfile.write("return", 3)
        outfile.write("end if", 2)
        outfile.blank_line()
    # end for

    outfile.write("const_props => cam_model_const_properties()", 2)
    outfile.blank_line()

    # Handle constituent-dimensioned variables
    if constituent_dimmed_vars:
        for key, value in constituent_dimmed_vars.items():
            hdimids = []
            for dimension in value['dims']:
                dimname = dimension.split(':')[1]
                if dimname != 'number_of_ccpp_constituents':
                    if dimensions_dict[dimname]['index'] < 0:
                        outfile.comment(f"Define potentially new dimension '{dimname}'", 2)
                        if dimname == 'horizontal_dimension':
                            dim_loc_name = 'ncol'
                            dimsize = 'num_global_phys_cols'
                        elif dimname == 'vertical_layer_dimension':
                            dim_loc_name = 'lev'
                            dimsize = 'pver'
                        elif dimname == 'vertical_interface_dimension':
                            dim_loc_name = 'ilev'
                            dimsize = 'pverp'
                        else:
                            dim_loc_name = dimensions_dict[dimname]['local_name']
                            dimsize = dim_loc_name
                        # end if
                        outfile.write(f"call cam_pio_def_dim(file, '{dim_loc_name}', {dimsize}, dimids({dim_index}), existOK=.true.)", 2)
                        dimensions_dict[dimname]['index'] = dim_index
                        dim_index = dim_index + 1
                    # end if
                    hdimids.append(dimensions_dict[dimname]['index'])
                # end if (ignore number of constituents dimensions)
            # end for
            outfile.comment(f"Handling for constituent-dimensioned variable '{value['diag_name']}'", 2)
            outfile.write(f"allocate({value['diag_name'].lower()}_desc(size(const_props)), stat=errflg, errmsg=errmsg)", 2)
            outfile.write("if (errflg /= 0) then", 2)
            outfile.write("return", 3)
            outfile.write("end if", 2)
            outfile.write("do constituent_idx = 1, size(const_props)", 2)
            outfile.comment("Grab constituent diagnostic name:", 3)
            outfile.write("call const_props(constituent_idx)%diagnostic_name(const_diag_name)", 3)
            desc_name = f"{value['diag_name'].lower()}_desc"
            dimids_array = ", ".join([f"dimids({i})" for i in hdimids])
            outfile.write(f"call cam_pio_def_var(file, '{value['diag_name']}_'//trim(const_diag_name), pio_double, (/{dimids_array}/), {desc_name}(constituent_idx), existOK=.false.)", 3)
            outfile.write("end do", 2)
            outfile.blank_line()
        # end for
    # end if

    # Handle non-advected constituent variables
    outfile.blank_line()
    outfile.comment("Handling for non-advected constituent vars (advected constituents handled by dynamics restart)", 2)
    outfile.comment("Allocate cnst_desc to total size of constituents array; some will be unused", 2)
    outfile.write("if (.not. allocated(cnst_desc)) then", 2)
    outfile.write("allocate(cnst_desc(size(const_props)))", 3)
    outfile.write("end if", 2)
    outfile.write("nonadvected_idx = 1", 2)
    hdimids = []
    hdimids.append(dimensions_dict['horizontal_dimension']['index'])
    hdimids.append(dimensions_dict['vertical_layer_dimension']['index'])
    dimids_array = ", ".join([f"dimids({i})" for i in hdimids])
    outfile.write("do constituent_idx = 1, size(const_props)", 2)
    outfile.write("call const_props(constituent_idx)%is_advected(advected)", 3)
    outfile.write("if (.not. advected) then", 3)
    outfile.write("call const_props(constituent_idx)%diagnostic_name(const_diag_name)", 4)
    outfile.write(f"call cam_pio_def_var(file, trim(const_diag_name), pio_double, (/{dimids_array}/), cnst_desc(nonadvected_idx), existOK=.false.)", 4)
    outfile.write("nonadvected_idx = nonadvected_idx + 1", 4)
    outfile.write("end if", 3)
    outfile.write("end do", 2)

    outfile.write("end subroutine restart_physics_init", 1)
    return dim_use_stmts, dimensions_dict

def write_restart_physics_write(outfile, required_vars, constituent_dimmed_vars, used_vars, dim_use_stmts, dimensions_dict):
    """
    Write the 'write' routine for the physics restart variables. This
    routine writes the physics fields to the restart (cam.r) file
    """
    outfile.write("subroutine restart_physics_write(file, grid_id, errmsg, errflg)", 1)

    use_stmts = [["pio", ["file_desc_t", "io_desc_t", "pio_write_darray", "pio_double"]],
                 ["cam_ccpp_cap", ["cam_model_const_properties","cam_constituents_array"]],
                 ["ccpp_kinds", ["kind_phys"]],
                 ["ccpp_constituent_prop_mod", ["ccpp_constituent_prop_ptr_t"]],
                 ["physics_grid", ["num_global_phys_cols"]],
                 ["cam_grid_support", ["cam_grid_id", "cam_grid_write_dist_array"]]]

    write_use_statements(outfile, use_stmts, 2)
    write_use_statements(outfile, dim_use_stmts, 2)

    for var in sorted(used_vars):
        outfile.write(f"use physics_types, only: {var}", 2)
    # end for

    outfile.blank_line()
    outfile.write("type(file_desc_t), intent(inout) :: file",   2)
    outfile.write("integer,            intent(in)   :: grid_id", 2)
    outfile.write("character(len=512),intent(out)   :: errmsg", 2)
    outfile.write("integer,           intent(out)   :: errflg", 2)

    outfile.blank_line()

    outfile.comment("Local variables", 2)
    outfile.write(f"integer                          :: dims({len(dimensions_dict)})", 2)
    outfile.write("integer                          :: grid_decomp", 2)
    outfile.write("integer                          :: grid_dims(2)", 2)
    outfile.write("integer                          :: field_shape(2)", 2)
    outfile.write("integer                          :: constituent_idx", 2)
    outfile.write("integer                          :: nonadvected_idx", 2)
    outfile.write("logical                          :: advected", 2)
    outfile.write("real(kind=kind_phys), pointer    :: field_data_ptr(:,:,:)", 2)
    outfile.write("type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)", 2)

    outfile.comment("Grab physics grid", 2)
    outfile.write("grid_decomp = cam_grid_id('physgrid')", 2)
    for dim in dimensions_dict:
        outfile.write(f"dims({dimensions_dict[dim]['index']}) = {dimensions_dict[dim]['import']}", 2)
    # end if
    outfile.comment("Write required restart variables to the restart file", 2)
    for key, value in required_vars.items():
        desc_name = f"{value['diag_name'].lower()}_desc"
        if len(value['dims']) == 1 and 'horizontal_dimension' in value['dims'][0]:
            outfile.comment("Handle horizontal-only field", 2)
            outfile.write("field_shape(1) = num_global_phys_cols", 2)
            outfile.write(f"call cam_grid_write_dist_array(file, grid_decomp, (/dims(1)/), (/field_shape(1)/), {key}, {desc_name})", 2)
        elif len(value["dims"]) > 1:
            dimstr = '(/'
            for index, dim in enumerate(value["dims"]):
                dimstr = f"{dimstr}dims({dimensions_dict[dim.split(':')[1]]['index']}),"
                if 'horizontal_dimension' in dim:
                    outfile.write("field_shape(1) = num_global_phys_cols", 2)
                else:
                    outfile.write(f"field_shape({index + 1}) = size({key}, {index + 1})", 2)
                # end if
            # end if
            outfile.write(f"call cam_grid_write_dist_array(file, grid_decomp, {dimstr[:-1]}/), field_shape, {key}, {desc_name})", 2)
        # end if
    # end for

    outfile.write("const_props => cam_model_const_properties()", 2)
    outfile.blank_line()

    # Handle constituent-dimensioned variables
    if constituent_dimmed_vars:
        for key, value in constituent_dimmed_vars.items():
            outfile.comment(f"Handling for constituent-dimensioned variable '{value['diag_name']}'", 2)
            outfile.write("do constituent_idx = 1, size(const_props)", 2)
            desc_name = f"{value['diag_name'].lower()}_desc"
            outfile.write("field_shape(1) = num_global_phys_cols", 3)
            outfile.write(f"call cam_grid_write_dist_array(file, grid_decomp, (/dims(1)/), (/field_shape(1)/), {key}(:,constituent_idx), {desc_name}(constituent_idx))", 3)
            outfile.write("end do", 2)
            outfile.blank_line()
        # end for
    # end if

    # Handle non-advected constituent variables
    outfile.blank_line()
    outfile.comment("Handling for non-advected constituent vars (advected constituents handled by dynamics restart)", 2)
    outfile.write("field_shape(1) = num_global_phys_cols", 2)
    outfile.write("field_shape(2) = pver", 2)
    outfile.write("nonadvected_idx = 1", 2)
    outfile.write("field_data_ptr => cam_constituents_array()", 2)
    outfile.write("do constituent_idx = 1, size(const_props)", 2)
    outfile.write("call const_props(constituent_idx)%is_advected(advected)", 3)
    outfile.write("if (.not. advected) then", 3)
    outfile.write("call cam_grid_write_dist_array(file, grid_decomp, (/dims(1), dims(2)/), field_shape, field_data_ptr(:,:,constituent_idx), cnst_desc(nonadvected_idx))", 4)
    outfile.write("nonadvected_idx = nonadvected_idx + 1", 4)
    outfile.write("end if", 3)
    outfile.write("end do", 2)

    outfile.write("end subroutine restart_physics_write", 1)

def write_restart_physics_read(outfile, required_vars, constituent_dimmed_vars, used_vars):
    """
    Write the 'read' routine for the physics restart variables. This
    routine reads the physics fields from the restart (cam.r) file
    """
    outfile.write("subroutine restart_physics_read(file)", 1)

    use_stmts = [["pio", ["file_desc_t", "io_desc_t", "pio_write_darray", "pio_double"]],
                 ["cam_ccpp_cap", ["cam_model_const_properties","cam_constituents_array"]],
                 ["ccpp_kinds", ["kind_phys"]],
                 ["physics_data", ["read_field"]],
                 ["ccpp_constituent_prop_mod", ["ccpp_constituent_prop_ptr_t"]],
                 ["physics_grid", ["num_global_phys_cols"]],
                 ["cam_grid_support", ["cam_grid_id", "cam_grid_write_dist_array"]]]
    write_use_statements(outfile, use_stmts, 2)
    for var in sorted(used_vars):
        outfile.write(f"use physics_types, only: {var}", 2)
    # end for

    outfile.write("type(file_desc_t), intent(inout) :: file",   2)
    outfile.blank_line()
    outfile.write("integer :: timestep", 2)
    outfile.blank_line()

    outfile.comment("Set timestep to 0; only one frame on the restart file", 2)
    outfile.write("timestep = 0", 2)
    outfile.blank_line()

    for key, value in required_vars.items():
        if len(value['dims']) == 1 and 'horizontal_dimension' in value['dims'][0]:
            outfile.comment("Handle horizontal-only field", 2)
            outfile.write(f"call read_field(file, '{value['stdname']}', (/'{value['diag_name']}'/), timestep, {key}", 2)
        else:
            # PEVERWHEE - TODO: handle nonstandard dimensions!
            if 'layer' in value['dims'][1]:
                vertdim = 'lev'
            else:
                vertdim = 'ilev'
            # end if
            outfile.comment(f"Read required restart variable '{value['stdname']}' from file", 2)
            outfile.write(f"call read_field(file, '{value['stdname']}', (/'{value['diag_name']}'/), '{vertdim}', timestep, {key})", 2)
            outfile.blank_line()
        # end if
    # end for

    outfile.write("end subroutine restart_physics_read", 1)

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
def gather_ccpp_req_vars(cap_database, registry_constituents):
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
    in_vars = {}
    out_vars = {}
    missing_vars = set()
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
                (stdname not in in_vars) and
                (stdname not in _EXCLUDED_STDNAMES)):
                if is_const:
                    #Add variable to required variable list if it's not a registry constituent
                    if stdname not in registry_constituents:
                        in_vars[stdname] = cvar
                    # end if
                else:
                    # We need to work with the host model version of this variable
                    missing = _find_and_add_host_variable(stdname, host_dict,
                                                          in_vars)
                    missing_vars.update(missing)
                # end if
            # end if (only input variables)
            if ((intent in _OUTPUT_TYPES) and
                  (stdname not in out_vars) and
                  (stdname not in _EXCLUDED_STDNAMES)):
                if not is_const:
                    missing = _find_and_add_host_variable(stdname, host_dict,
                                                          out_vars)
                    # do nothing with missing variables
                # end if
            # end if (only output variables)
        # end for (loop over call list)
    # end for (loop over phases)

    if missing_vars:
        mvlist = ', '.join(sorted(missing_vars))
        retmsg = f"Error: Missing required host variables: {mvlist}"
    # end if
    # Return the required variables as a list
    return list(in_vars.values()), list(out_vars.values()), retmsg

##############################################################################
def gather_required_restart_variables(all_req_vars, restart_vars, host_dict):
    """
    Return lists of the required restart non-constituent variables, and the required
    restart constituent variables
    """

    required_restart_vars = {}
    required_constituent_dimensioned_vars = {}
    used_vars = set()

    for required_var in all_req_vars:
        stdname = required_var.get_prop_value('standard_name')
        local_name = required_var.call_string(host_dict)
        used_var = required_var.var.get_prop_value('local_name')
        if stdname in restart_vars:
            diagnostic_name = restart_vars[stdname]
            used_vars.add(used_var)
            dimensions = required_var.get_dimensions()
            if 'ccpp_constant_one:number_of_ccpp_constituents' in dimensions:
                required_constituent_dimensioned_vars[local_name] = {'diag_name': diagnostic_name, 'dims': dimensions, 'stdname': stdname}
            else:
                required_restart_vars[local_name] = {'diag_name': diagnostic_name, 'dims': dimensions, 'stdname': stdname}
            # end if
        # end if (ignore all non-restart variables)
    # end for

    return required_restart_vars, required_constituent_dimensioned_vars, used_vars

#####

def write_use_statements(outfile, use_stmts, indent):
    """Output Fortran module use (import) statements listed in <use_stmts>.
    """
    # Don't do anything if we don't have use statements!
    if len(use_stmts) == 0:
        return
    # end if
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
