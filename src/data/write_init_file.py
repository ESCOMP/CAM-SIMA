#!/usr/bin/env python

"""
Use variable meta-data from "generate_registry_data.py"
to generate a CAM fortran file that manages host model
variable initiliaztion and initial conditions.

Running this file directly on the command line, i.e.:

python write_init_file.py

will run this script's doctests.
"""

#Import statements:
import os.path
from fortran_tools import FortranWriter

def write_init_file(files, outdir, indent, logger):

    """
    Create the "initial_conditions.F90" file
    using meta-data colleced from registry generation.
    """

    #Create new (empty) "master" list to store all
    #variable objects:
    variable_list = list()

    #Generate variable lists:
    #-----------------------
    #Loop over files:
    for file_obj in files:

       #Loop over all DDTs in file:
       for ddt in file_obj.ddts.values():
           #Concatenate DDT variable list onto master list:
           variable_list.extend(ddt.variable_list())

       #Add file variable list to master list:
       variable_list.extend(list(file_obj.var_dict.variable_list()))

    #Loop through variable list to look for array elements:
    for var in list(variable_list):
        #Check if array elements are present:
        if var.elements:
            #If so, then loop over elements:
            for element in var.elements:
                #Append element as new "variable" in variable list:
                variable_list.append(element)
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

    #Generate "initial_conditions.F90" file:
    #--------------------------------------

    #Open new file:
    ofilename = os.path.join(outdir, "initial_conditions.F90")
    #Log file creation:
    logger.info("Writing initial conditions source file, {}".format(ofilename))

    #Open file using CCPP's FortranWriter:
    with FortranWriter(ofilename, "w", indent=indent) as outfile:

        #Add module name:
        outfile.write('module initial_conditions\n', 0)

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

        #Add "contains" statement:
        outfile.write("\nCONTAINS\n", 0)

        #Write initialization functions:
        write_init_mark_subroutine(outfile)

        #End module:
        outfile.write('\nend module initial_conditions', 0)
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
    declare_str = "character(len={}), public :: phys_var_stdnames(phys_var_num) = (/ &".format(\
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
        "character(len={}), public :: input_var_names({}, phys_var_num) = reshape((/ &".format(\
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
    declare_str = "logical, public :: initialized_vars(phys_var_num) = (/ &"
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
                  "if(trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then", 3)

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


###############################################################################
#IGNORE EVERYTHING BELOW HERE UNLESS RUNNING TESTS ON WRITE_INIT_FILE!
###############################################################################

#Call testing routine, if script is run directly
if __name__ == "__main__":

    print("Add tests here!")

#############
# End of file
#############

