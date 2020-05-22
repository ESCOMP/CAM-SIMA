#!/usr/bin/env python

"""
Use variable meta-data from "generate_registry_data.py"
to generate CAM fortran files that manages host model
variable initialization and initial conditions.

Running this file directly on the command line, i.e.:

python write_init_file.py

will run this script's doctests.
"""

#Import statements:
import os.path
from fortran_tools import FortranWriter

def write_init_files(files, outdir, indent, logger):

    """
    Create the "phys_vars_init_check.F90" and
    "physics_inputs" files usig meta-data
    collected from registry generation.
    """

    #Create new (empty) "master" list to store all
    #variable objects:
    variable_list = list()

    #Create new (empty) dictionary wth DDT types
    #as keys, and variables with that associated
    #DDT type as values:
    ddt_type_var_dict = dict()

    #Create new (empty) dictionary which contains
    #variable names as keys, and all DDT types that
    #include that variable as values:
    ddt_varname_dict = dict()

    #Generate variable lists:
    #-----------------------
    #Loop over files:
    for file_obj in files:

       #Loop over all DDTs in file:
       for ddt in file_obj.ddts.values():
           #Concatenate DDT variables onto master list:
           variable_list.extend(ddt.variable_list())

           #Also add variable names to DDT name list:
           for var in ddt.variable_list():
               #Check if variable name already exists in dict:
               if var.local_name in ddt_varname_dict.keys():
                   #Extract current type list:
                   type_list = ddt_varname_dict[var.local_name]
                   #Add appended list back to dictionary:
                   ddt_varname_dict[var.local_name] = type_list.append(var.local_name)
               else:
                   #Create new dictionary entry:
                   ddt_varname_dict[var.local_name] = [ddt.ddt_type]

       #Add file variable list to master list:
       variable_list.extend(list(file_obj.var_dict.variable_list()))

    #Loop through variable list to look for array elements:
    for var_idx, var in enumerate(list(variable_list)):
        #Check if array elements are present:
        if var.elements:
            #If so, then loop over elements:
            for element in var.elements:
                #Append element as new "variable" in variable list:
                variable_list.append(element)

                #Next, check if parent variable was contained
                #within a DDT:
                if var.local_name in ddt_varname_dict.keys():
                    #Add array element to DDT dictionary with
                    #same listed types as parent variable:
                    ddt_varname_dict[element.local_index_name_str] = \
                    ddt_varname_dict[var.local_name]

            #Delete parent variable from DDT dictionary
            #if present, as it is no longer needed:
            if var.local_name in ddt_varname_dict.keys():
                del ddt_varname_dict[var.local_name]

            #Finally, delete parent variable from master
            #variable list, as it will no longer be
            #used directly:
            del variable_list[var_idx]


    #Loop throught variable list again to look for DDTs:
    for var in variable_list:
        #Check if variable is a DDT:
        if var.is_ddt:
            #Check if variable type already
            #exists in dictionary:
            if var.var_type in ddt_type_var_dict.keys():
                #Extract variable name list:
                var_list = ddt_type_var_dict[var.var_type]
                #Add appended list back to dictionary:
                ddt_type_var_dict[var.var_type] = var_list.append(var.local_name)
            else:
                #Create new dictionary entry:
                ddt_type_var_dict[var.var_type] = [var.local_name]

    #-----------------------

    #Determine total number of variables:
    total_var_num = len(variable_list)

    #Determine max standard name string length:
    stdname_max_len = max([len(var.standard_name) for var in variable_list])

    #Determine max number of IC variable names:
    try:
        ic_name_max_num = max([len(var.ic_names) for var in variable_list if var.ic_names is not None])
    except ValueError:
        #If there is a ValueError, then likely no IC
        #input variable names exist, so print warning
        #and exit function:
        lmsg = "No '<ic_file_input_names>' tags exist in registry.xml" \
               ", so no input variable name array will be created."
        logger.warning(lmsg)
        return

    #Deterime max length of input (IC) names:
    ic_name_max_len = find_ic_name_max_len(variable_list)

    #Generate "phys_vars_init_check.F90" file:
    #--------------------------------------

    #Open new file:
    ofilename = os.path.join(outdir, "phys_vars_init_check.F90")
    #Log file creation:
    logger.info("Writing initialization-checking source file, {}".format(ofilename))

    #Open file using CCPP's FortranWriter:
    with FortranWriter(ofilename, "w", indent=indent) as outfile:

        #Add module name:
        outfile.write("module phys_vars_init_check\n", 0)

        #Add boilerplate code:
        outfile.write("implicit none\nprivate\n", 0)

        #Write public parameters:
        write_ic_params(outfile, total_var_num, stdname_max_len,
                        ic_name_max_len)

        #Write initial condition arrays:
        write_ic_arrays(outfile, variable_list, total_var_num,
                        stdname_max_len, ic_name_max_len, ic_name_max_num)

        #Add public function declarations:
        outfile.write("!! public interfaces", 0)
        outfile.write("public :: mark_as_initialized", 1)
        outfile.write("public :: is_initialized", 1)

        #Add "contains" statement:
        outfile.write("\nCONTAINS\n", 0)

        #Write initialization marking subroutine:
        write_init_mark_subroutine(outfile)

        #Add two blank spaces:
        outfile.write("", 0)
        outfile.write("", 0)

        #Write initialization check function:
        write_is_init_func(outfile)

        #End module:
        outfile.write("\nend module phys_vars_init_check", 0)
    #--------------------------------------

    #Generate "physics_inputs.F90" file:
    #--------------------------------------

    #Open new file:
    ofilename = os.path.join(outdir, "physics_inputs.F90")
    #Log file creation:
    logger.info("Writing initial conditions source file, {}".format(ofilename))

    #Open file using CCPP's FortranWriter:
    with FortranWriter(ofilename, "w", indent=indent) as outfile:

        #Add module name:
        outfile.write('module physics_inputs\n', 0)

        #Add boilerplate code:
        outfile.write("implicit none\nprivate\n", 0)

        #Add public function declarations:
        outfile.write("!! public interfaces", 0)
        outfile.write("public :: physics_read_data", 1)

        #Add "contains" statement:
        outfile.write("\nCONTAINS\n", 0)

        #Write physics_read_data subroutine:
        write_phys_read_subroutine(outfile, variable_list,
                                   ddt_varname_dict, ddt_type_var_dict,
                                   total_var_num)

        #Add a blank space:
        outfile.write("", 0)

        #End module:
        outfile.write('\nend module physics_inputs', 0)
    #--------------------------------------

#################
#HELPER FUNCTIONS
#################

def find_ic_name_max_len(variable_list):
        """Determine max length of input (IC) file variable names"""

        #Initialize max IC name string length variable:
        ic_name_max_len = 0

        #Loop over variables in list:
        for var in variable_list:
            #Check if variable actually has IC names:
            if var.ic_names is not None:
                #Loop over all IC input names for given variable:
                for ic_name in var.ic_names:
                    #Determine IC name string length:
                    ic_name_len = len(ic_name)

                    #Determine if standard name string length is longer
                    #then all prvious values:
                    if ic_name_len > ic_name_max_len:
                        #If so, then re-set max length variable:
                        ic_name_max_len = ic_name_len

        #Return max string length of input variable names:
        return ic_name_max_len

######

def write_ic_params(outfile, total_var_num, stdname_max_len, ic_name_max_len):

    """
    Write public parameter declarations needed
    by initial condition arrays and functions
    """

    #Create new Fortran integer parameter to store total number of variables:
    outfile.write("!Total number of physics-related variables:", 1)
    outfile.write("integer, public, parameter :: phys_var_num = {}".format(\
                  total_var_num), 1)

    #Add blank space:
    outfile.write("", 0)

    #Create another Fortran integer parameter to store max length of
    #variable standard name strings:
    outfile.write("!Max length of physics-related variable standard names:", 1)
    outfile.write("integer, public, parameter :: std_name_len = {}".format(\
                   stdname_max_len), 1)

    #Add blank space:
    outfile.write("", 0)

    #Create final Fortran integer parameter to store max length of
    #input variable name string:
    outfile.write("!Max length of input (IC) file variable names:", 1)
    outfile.write("integer, public, parameter :: ic_name_len = {}".format(\
                  ic_name_max_len), 1)

    #Add blank space:
    outfile.write("", 0)

######

def write_ic_arrays(outfile, variable_list, total_var_num,
                    stdname_max_len, ic_name_max_len, ic_name_max_num):

    """
    Write initial condition arrays to store
    data on variable initialization status
    and input file names.
    """

    #Create variable name array string lists:
    stdname_strs = list()
    ic_name_strs = list()

    #Create fake name and fake name list with proper lengths:
    fake_ic_name = " "*ic_name_max_len
    fake_ic_names = [fake_ic_name]*ic_name_max_num

    #Loop over variables in list:
    for var in variable_list:

        #Create standard_name string with proper size,
        #and append to list:
        extra_spaces = " " * (stdname_max_len - len(var.standard_name))
        stdname_strs.append("'{}'".format(var.standard_name + extra_spaces))

        #Check if variable actually has IC names:
        if var.ic_names is not None:

            #Determine number of IC names for variable:
            ic_name_num = len(var.ic_names)

            #Create new (empty) list to store (IC) file
            #input names of variables with the correct
            #number of spaces to match character array
            #dimensions:
            ic_names_with_spaces = list()

            #Loop over possible input file (IC) names:
            for ic_name in var.ic_names:
                #Create ic_name string with proper size:
                extra_spaces = " " * (ic_name_max_len - len(ic_name))
                #Add properly-sized name to list:
                ic_names_with_spaces.append(ic_name + extra_spaces)

            #Create repeating list of empty, "fake" strings that
            #increases array to max size:
            ic_names_with_spaces.extend([fake_ic_name]*(ic_name_max_num - ic_name_num))

            #Append new ic_names to string list:
            ic_name_strs.append(', '.join("'{}'".format(n) for n in ic_names_with_spaces))

        else: #No IC names?

            #Append empty "fake" IC names to input name list:
            ic_name_strs.append(', '.join("'{}'".format(n) for n in fake_ic_names))

    #Write arrays to fortran file:
    #----------------------------

    #Write starting declaration of input standard name array:
    outfile.write("!Array storing all physics-related variable standard names:", 1)
    declare_str = "character(len={}), public, protected :: phys_var_stdnames(phys_var_num) = (/ &".format(\
                  stdname_max_len)
    outfile.write(declare_str, 1)

    #Write standard names to fortran array:
    for index, stdname_str in enumerate(stdname_strs):
        if index == total_var_num-1:
            suffix = ' /)'
        else:
            suffix = ', &'
        # end if
        outfile.write('{}{}'.format(stdname_str, suffix), 2)

    #Write a blank space:
    outfile.write("", 0)

    #Write starting declaration of IC fiel input names array:
    outfile.write("!Array storing all registered IC file input names for each variable:", 1)
    declare_str = \
        "character(len={}), public, protected :: input_var_names({}, phys_var_num) = reshape((/ &".format(\
        ic_name_max_len, ic_name_max_num)
    outfile.write(declare_str, 1)

    #Write IC names to fortran array:
    for index, ic_name_str in enumerate(ic_name_strs):
        if index == total_var_num-1:
            suffix = ' /), (/{}, phys_var_num/))'.format(ic_name_max_num)
        else:
            suffix = ', &'
        # end if
        outfile.write('{}{}'.format(ic_name_str, suffix), 2)

    #Write a blank space:
    outfile.write("", 0)

    #Write starting declaration of initalized logical array:
    outfile.write("!Logical array to indicate whether or not variable is initialized:", 1)
    declare_str = "logical, public, protected :: initialized_vars(phys_var_num) = (/ &"
    outfile.write(declare_str, 1)

    #Write "False" logicals to logical array:
    for var_num in range(total_var_num):
        if var_num == total_var_num-1:
            log_arr_str = '.false. /)'
        else:
            log_arr_str = '.false., &'
        # end if
        outfile.write(log_arr_str, 2)

    #Write a blank space:
    outfile.write("", 0)

    #----------------------------

######

def write_init_mark_subroutine(outfile):

    """
    Write "Mark Initialized" subroutine which
    is used to modify the "initalized_vars"
    array and sets the value for the specified
    variable to "True".
    """

    #Add subroutine header:
    outfile.write("subroutine mark_as_initialized(varname)", 1)

    #Write a blank space:
    outfile.write("", 0)

    #Add subroutine description:
    outfile.write("!This subroutine  marks the variable as\n" \
                  "!initialized in the `initialized_vars` array,\n" \
                  "!which means any initialization check should now\n" \
                  "!now return True.", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Add use statements:
    outfile.write("use cam_abortutils, only: endrun", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Add implicit none statement:
    outfile.write("implicit none", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Add variable declaration statements:
    outfile.write("character(len=*), intent(in) :: varname !Variable name being marked", 2)
    outfile.write("", 0)
    outfile.write("integer :: stdnam_idx !standard name array index", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Add main subroutine section:
    #---------------------------
    outfile.write("!Loop over standard name array:\n" \
                  "do stdnam_idx = 1, phys_var_num", 2)

    outfile.write("!Check if standard name matches provided variable name:\n" \
                  "if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then", 3)

    outfile.write("!If so, then set associated initialized_vars\n" \
                  "!array index to true:\n" \
                  "initialized_vars(stdnam_idx) = .true.", 4)

    outfile.write("", 0)

    outfile.write("!Exit function:\n" \
                  "exit", 4)

    outfile.write("end if", 3)

    outfile.write("end do", 2)

    outfile.write("", 0)

    outfile.write("!If loop has completed with no matches, then endrun with warning\n" \
                  "!that variable didn't exist in standard names array:\n" \
                  "call endrun(&", 2)
    outfile.write('''"Variable '"//trim(varname)//"' is missing from phys_var_stdnames array.")''', 2)

    outfile.write("", 0)
    #---------------------------

    #End subroutine:
    outfile.write("end subroutine mark_as_initialized", 1)

######

def write_is_init_func(outfile):

    """
    Write "Is Initialized" function which
    is used to check if a given function has
    already been initialized according to
    the "initialized_vars" array.
    """


    #Add subroutine header:
    outfile.write("logical function is_initialized(varname)", 1)

    #Write a blank space:
    outfile.write("", 0)

    #Add subroutine description:
    outfile.write("!This function checks if the variable is\n" \
                  "!already initialized according to the\n " \
                  "!`initialized_vars` array.", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Add use statements:
    outfile.write("use cam_abortutils, only: endrun", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Add implicit none statement:
    outfile.write("implicit none", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Add variable declaration statements:
    outfile.write("character(len=*), intent(in) :: varname !Variable name being checked", 2)
    outfile.write("", 0)
    outfile.write("integer :: stdnam_idx !standard name array index", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Initialize return variable:
    outfile.write("is_initialized = .false.", 2)
    outfile.write("", 0)

    #Add main subroutine section:
    #---------------------------
    outfile.write("!Loop over standard name array:\n" \
                  "do stdnam_idx = 1, phys_var_num", 2)

    outfile.write("!Check if standard name matches provided variable name:\n" \
                  "if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then", 3)

    outfile.write("!If so, then return initialized_vars\n" \
                  "!value associated with that index:\n" \
                  "is_initialized = initialized_vars(stdnam_idx)", 4)

    outfile.write("end if", 3)

    outfile.write("end do", 2)

    outfile.write("", 0)

    outfile.write("!If loop has completed with no matches, then endrun with warning\n" \
                  "!that variable didn't exist in standard names array:\n" \
                  "call endrun(&", 2)
    outfile.write('''"Variable '"//trim(varname)//"' is missing from phys_var_stdnames array.")''', 2)

    outfile.write("", 0)
    #---------------------------

    #End subroutine:
    outfile.write("end function is_initialized", 1)

######

def write_phys_read_subroutine(outfile, variable_list,
                               ddt_varname_dict, ddt_type_var_dict,
                               total_var_num):

    """
    Write the "physics_read_data" subroutine, which
    is used to initialize required physics variables
    by reading in the values from an Initial Conditions
    (IC) input file. This will only be done for registry
    variables which contain
    """

    #Create fortran use statements:
    #-----------------------------

    #Create new (empty) list to store use statements:
    use_list = list()

    #Initialize loop counter:
    lpcnt = 0

    #Loop over variables in list:
    for var in variable_list:

        #Check if variable is protected:
        if not hasattr(var, "protected") or not var.protected:

            #Check if variable has "local_index_name_str":
            if hasattr(var, "local_index_name_str"):
                var_name = var.local_index_name_str
            else:
                var_name = var.local_name

            #Is the variable not part of a larger DDT?
            if not var_name in ddt_varname_dict.keys():

                #If loop counter is zero, then
                #create new use string:
                if lpcnt == 0:
                    use_str = \
                        "use physics_types,        only: {}".format(\
                        var_name)

                    #Advance loop counter by one:
                    lpcnt += 1

                elif lpcnt == 2:
                    #If loop counter is three, then append variable
                    #name to use string:
                    use_str += ", {}".format(var_name)

                    #Add use string to list:
                    use_list.append(use_str)

                    #Reset loop counter:
                    lpcnt = 0
                else:
                    #Append variable name to use string
                    use_str += ", {}".format(var_name)

                    #Advance loop counter by one:
                    lpcnt += 1
    #-----------------------------

    #Create fortran "read_field" calls:
    #---------------------------------

    #Create new (empty) dictionary to store "read_field" calls:
    call_dict = dict()

    #Loop over variables in list:
    for var in variable_list:

        #Check if variable is protected:
        if not hasattr(var, "protected") or not var.protected:

            #Check if variable does not have an initial value:
            if var.initial_value == 'NULL()':

                #Check if variable has "local_index_name_str":
                if hasattr(var, "local_index_name_str"):
                    var_name = var.local_index_name_str
                else:
                    var_name = var.local_name

                #Check if variable only has "horizontal_dimensions":
                if len(var.dimensions) == 1 and var.dimensions[0] == \
                   'horizontal_dimension':
                    #Then set vertical level name to None:
                    levnm = None
                else:
                    #If not,e then check variable standard name for "interface":
                    if var.standard_name.find("at_interface") != -1:
                        levnm = "ilev"
                    else:
                        levnm = "lev"

                #Set "if-statement" call string:
                call_string_key = "if (trim(phys_var_stdnames(name_idx)) ==" \
                                  " '{}') then".format(var.standard_name)

                #Generate variable string if part of DDT:
                ddt_strings = ddt_call_string_create(var_name,
                                                     ddt_varname_dict, ddt_type_var_dict)

                #Do DDT strings exist?
                if ddt_strings:
                    #Use DDT strings in the second call string:
                    #===================
                    call_string_val = "" #create initial empty string
                    if levnm is not None:
                        for ddt_idx, ddt_string in enumerate(ddt_strings):
                            if ddt_idx == len(ddt_strings)-1:
                                call_string_val += \
                                "call read_field(file, input_var_names(:,name_idx)," + \
                                " '{}', timestep, {})".format(levnm, ddt_string)
                            else:
                                call_string_val += \
                                "call read_field(file, input_var_names(:,name_idx)," + \
                                " '{}', timestep, {})\n".format(levnm, ddt_string)
                    else:
                        for ddt_idx, ddt_string in enumerate(ddt_strings):
                            if ddt_idx == len(ddt_strings)-1:
                                call_string_val += \
                                "call read_field(file, input_var_names(:,name_idx)," + \
                                " timestep, {})".format(ddt_string)
                            else:
                                call_string_val += \
                                "call read_field(file, input_var_names(:,name_idx)," + \
                                " timestep, {})\n".format(ddt_string)
                    #===================
                else:
                    #Simply use the local name in the second call string:
                    #===================
                    if levnm is not None:
                        call_string_val = \
                          "call read_field(file, input_var_names(:,name_idx), '{}', timestep, {})".format(\
                          levnm, var_name)
                    else:
                        call_string_val = \
                          "call read_field(file, input_var_names(:,name_idx), timestep, {})".format(\
                          var_name)
                    #===================

                #Add strings to dictionary:
                call_dict[call_string_key] = call_string_val

    #---------------------------------

    #Write actual subroutine code:
    #----------------------------

    #Add subroutine header:
    outfile.write("subroutine physics_read_data(file, suite_names, timestep)", 1)

    #Add use statements:
    outfile.write("use pio,                  only: file_desc_t\n" \
                  "use cam_abortutils,       only: endrun\n" \
                  "use shr_kind_mod,         only: SHR_KIND_CS, SHR_KIND_CL\n" \
                  "use physics_data,         only: read_field, find_input_name_idx\n" \
                  "use phys_vars_init_check, only: phys_var_stdnames, input_var_names\n" \
                  "use phys_vars_init_check, only: std_name_len\n" \
                  "use cam_ccpp_cap,         only: ccpp_physics_suite_variables", 2)

    #Loop over use string list:
    for use_str in use_list:
        #Add physics_types use statements:
        outfile.write(use_str, 2)

    #Write dummy variable declarations:
    outfile.write("", 0)
    outfile.write("! Dummy arguments\n" \
                  "type(file_desc_t), intent(inout) :: file\n" \
                  "character(len=SHR_KIND_CS)       :: suite_names(:) !Names of CCPP suites\n" \
                  "integer,           intent(in)    :: timestep", 2)
    outfile.write("", 0)

    #Write local variable declarations:
    outfile.write("!Local variables:", 2)
    outfile.write("", 0)
    outfile.write("!Character array containing all CCPP-required vairable standard names:\n" \
                  "character(len=std_name_len), allocatable :: ccpp_required_data(:)", 2)
    outfile.write("", 0)
    outfile.write("!String which stores names of any missing vars:\n" \
                  "character(len=SHR_KIND_CL) :: missing_required_vars\n" \
                  "character(len=SHR_KIND_CL) :: missing_input_names", 2)
    outfile.write("", 0)
    outfile.write("character(len=512) :: errmsg    !CCPP framework error message\n" \
                  "integer            :: errflg    !CCPP framework error flag\n" \
                  "integer            :: name_idx  !Input variable array index\n" \
                  "integer            :: req_idx   !Required variable array index\n" \
                  "integer            :: suite_idx !Suite array index", 2)
    outfile.write("", 0)

    #Initialize variables:
    outfile.write("!Initalize missing variables string:\n" \
                  "missing_required_vars = ' '\n" \
                  "missing_input_names   = ' '", 2)
    outfile.write("", 0)

    #Loop over physics suites:
    outfile.write("!Loop over CCPP physics/chemistry suites:\n" \
                  "do suite_idx = 1, size(suite_names, 1)", 2)
    outfile.write("", 0)

    #Determine physics suite required variables:
    outfile.write("!Search for all needed CCPP input variables,\n" \
                  "!so that they can bx e read from input file if need be:\n" \
                  "call ccpp_physics_suite_variables(suite_names(suite_idx), ccpp_required_data, &", 3)
    outfile.write("errmsg, errflg, input_vars_in=.true., &\n" \
                  "output_vars_in=.false.)", 4)
    outfile.write("", 0)

    #Loop over required variables:
    outfile.write("!Loop over all required variables as specified by CCPP suite:\n" \
                  "do req_idx = 1, size(ccpp_required_data, 1)", 3)
    outfile.write("", 0)

    #Call input name search function:
    outfile.write("!Find IC file input name array index for required variable:\n" \
                  "name_idx = find_input_name_idx(ccpp_required_data(req_idx))", 4)
    outfile.write("", 0)

    #Skip already initialized variables:
    outfile.write("!If variable is already initialized, then skip it:\n" \
                  "if (name_idx == -2) cycle", 4)
    outfile.write("", 0)

    #Generate error message if required variable isn't found:
    outfile.write("!If an index was never found, then save variable name and check the rest\n" \
                  "!of the variables, after which the model simulation will end:\n" \
                  "if (name_idx == -1) then", 4)
    outfile.write("if (len_trim(missing_required_vars) == 0) then", 5)
    outfile.write("missing_required_vars(len_trim(missing_required_vars)+1:) = &", 6)
    outfile.write("trim(ccpp_required_data(req_idx))", 7)
    outfile.write("else", 5)
    outfile.write("missing_required_vars(len_trim(missing_required_vars)+1:) = &", 6)
    outfile.write(" ', '//trim(ccpp_required_data(req_idx))", 7)
    outfile.write("end if\n" \
                  "!Continue on with variable loop:\n" \
                  "cycle", 5)
    outfile.write("end if", 4)
    outfile.write("", 0)

    #Generate error message if required variable contains no input names
    #(i.e. the <ic_file_input_names> registry tag is missing):
    outfile.write("!Next, check that the input variable names aren't blank.\n" \
                  "!If so, then save variable name and check the rest of the\n" \
                  "!variables, after which the model simulation will end:\n" \
                  "if (len_trim(input_var_names(1,name_idx)) == 0) then", 4)
    outfile.write("if (len_trim(missing_input_names) == 0) then", 5)
    outfile.write("missing_input_names(len_trim(missing_input_names)+1:) = &", 6)
    outfile.write("trim(ccpp_required_data(req_idx))", 7)
    outfile.write("else", 5)
    outfile.write("missing_input_names(len_trim(missing_input_names)+1:) = &", 6)
    outfile.write(" ', '//trim(ccpp_required_data(req_idx))", 7)
    outfile.write("end if\n" \
                  "!Continue on with variable loop:\n" \
                  "cycle", 5)
    outfile.write("end if", 4)
    outfile.write("", 0)


    #Generate "read_field" calls:
    for if_call, read_call in call_dict.items():
        outfile.write(if_call, 4)
        outfile.write(read_call, 5)
        outfile.write("end if", 4)
        outfile.write("", 0)

    #End required variables loop:
    outfile.write("end do !Suite-required variables", 3)
    outfile.write("", 0)

    #Generate endrun statement for missing variables:
    outfile.write("!End simulation if there are missing input\n" \
                  "!variables that are required:\n" \
                  "if (len_trim(missing_required_vars) > 0) then", 3)
    outfile.write('call endrun("Required variables missing from registered list of input variables: "//&', 4)
    outfile.write("trim(missing_required_vars))", 5)
    outfile.write("end if", 3)
    outfile.write("", 0)

    #Generate endrun statement for missing input names:
    outfile.write("!End simulation if there are variables that\n" \
                  "!have no input names:\n" \
                  "if (len_trim(missing_input_names) > 0) then", 3)
    outfile.write("call endrun(&", 4)
    outfile.write(' "Required variables missing a list of input names (<ic_file_input_names>): "//&', 5)
    outfile.write("trim(missing_input_names))", 5)
    outfile.write("end if", 3)
    outfile.write("", 0)

    #Deallocate ccpp_required_data array:
    outfile.write("!Deallocate required variables array for use in next suite:\n" \
                  "deallocate(ccpp_required_data)", 3)
    outfile.write("", 0)

    #End suite loop:
    outfile.write(" end do !CCPP suites", 2)
    outfile.write("", 0)

    #End subroutine:
    outfile.write("end subroutine physics_read_data", 1)

    #----------------------------

######

def ddt_call_string_create(var_name, ddt_varname_dict, ddt_type_var_dict):

    """
    Recursive function that determines the number of "parent"
    DDT variables that are needed in order to properly use
    the variable in the "read_field" call, or in other fortran
    coding situations that need to use the variable directly.
    """

    #Create empty list to store all relevant variable call strings:
    call_string_list = list()

    #Check if variable is contained within a DDT:
    if var_name in ddt_varname_dict.keys():

        #If so, then extract list of DDT types variable
        #is associated with:
        ddt_types =  ddt_varname_dict[var_name]

        #Loop over DDT types for each variabale:
        for ddt_type in ddt_types:
            #Extract all variables that are of this type:
            ddt_type_vars = ddt_type_var_dict[ddt_type]

            #Loop over all DDT-type variables:
            for ddt_var in ddt_type_vars:
                #Repeat this function, to see if this
                #DDT variable is contained inside additional DDTs:
                call_string_list_new = ddt_call_string_create(ddt_var,
                                                              ddt_varname_dict,
                                                              ddt_type_var_dict)

                #Is list non-empty:
                if call_string_list_new:
                    #Loop over all call string entries:
                    for call_string in call_string_list_new:
                        #Append string to main list:
                        call_string_list.append('{}%{}'.format(call_string, var_name))
                else:
                    #Add combined variable name to list:
                    call_string_list.append('{}%{}'.format(ddt_var, var_name))

    #Return list of call strings:
    return call_string_list

###############################################################################
#IGNORE EVERYTHING BELOW HERE UNLESS RUNNING TESTS ON WRITE_INIT_FILE!
###############################################################################

#Call testing routine, if script is run directly
if __name__ == "__main__":

    print("Add tests here!")

#############
# End of file
#############

