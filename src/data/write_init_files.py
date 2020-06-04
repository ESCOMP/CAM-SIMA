#!/usr/bin/env python

"""
Use variable meta-data from "generate_registry_data.py"
to generate CAM fortran files that manage host model
variable initialization and initial condition inputs.
"""

#Import statements:
import os.path
from collections import OrderedDict

from fortran_tools import FortranWriter
from ccpp_datafile import DatatableReport
from ccpp_datafile import datatable_report

##############
#Main function
##############

def write_init_files(files, outdir, indent, cap_datafile, logger,
                     phys_check_filename=None, phys_input_filename=None):

    """
    Create the "phys_vars_init_check.F90" and
    "physics_inputs.F90" files usig meta-data
    collected from registry generation.
    """

    #Initialize return code:
    retcode = 0

    #Initialize a new (empty) variable dictionary, with
    #variable standard name as keys, and variable and
    #associated DDT object as values:
    var_type_dict = OrderedDict()

    #Initialize a new (empty) master DDT dictionary, with
    #DDT types as keys, and the associated DDT object as values:
    ddt_type_dict = OrderedDict()

    #Generate DDT dictionary:
    #-----------------------
    #Loop over all registry files:
    for file_obj in files:
        #Add registry file DDT dictionary to master dictionary:
        ddt_type_dict.update(file_obj.ddts)

        #Loop over all variables in registry file:
        for var in list(file_obj.var_dict.variable_list()):
            if var.is_ddt:
                #Extract associated DDT object:
                ddt = ddt_type_dict[var.var_type]
                #Add variable to dictionary:
                var_type_dict[var.standard_name] = [var, ddt]
            else:
                #If not a DDT, then set value to None:
                var_type_dict[var.standard_name] = [var, None]
    #-----------------------

    #Create Fortran data object:
    fort_data = VarFortData(var_type_dict, ddt_type_dict)

    #Generate CCPP required variables set:
    ccpp_req_vars_set = find_ccpp_req_vars(cap_datafile)

    #Check if all required variable are present:
    #-----------------------
    missing_vars = fort_data.check_req_vars(ccpp_req_vars_set)
    if missing_vars:
        #Are variables missing?  If so then end run here.
        #Create error message:
        emsg = "Required CCPP physics suite variables missing " \
               "from registered host model variable list:\n {}".format(\
               " ".join(missing_vars))

        #Add error-message to logger, and return with non-zero ret-code:
        logger.error(emsg)
        retcode = 1
        return retcode
    #-----------------------

    #Remove all non-required variables from call/use dictionaries:
    fort_data.rm_nonreq_vars(ccpp_req_vars_set)

    #Calculate fortran variable and IC name array parameters:
    fort_data.calc_init_params(logger)

    #Generate "phys_vars_init_check.F90" file:
    #--------------------------------------

    #Open new file:
    if phys_check_filename:
        ofilename = os.path.join(outdir, phys_check_filename)
    else:
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
        write_ic_params(outfile, fort_data)

        #Write initial condition arrays:
        write_ic_arrays(outfile, fort_data)

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
    if phys_input_filename:
        ofilename = os.path.join(outdir, phys_input_filename)
    else:
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
        write_phys_read_subroutine(outfile, fort_data)

        #Add a blank space:
        outfile.write("", 0)

        #End module:
        outfile.write('\nend module physics_inputs', 0)
    #--------------------------------------

    #Return retcode:
    return retcode

############################
#Main fortran code-gen class
############################

class VarFortData:

    """
    Object which stores information needed
    to generate fortran variable initialization
    and file reading modules.  The assumed
    input data is a list of registry variables
    and a dictionary of DDTs with variable standard
    names as keys, which is brought into this object
    using the 'create_data' method.
    """

    def __init__(self, var_type_dict, ddt_type_dict):

        #Initialize variable standard names list:
        self.__standard_names = list()

        #initialize variable input (IC) names dictionary:
        self.__ic_names = OrderedDict()

        #Initialize Fortran call dictionary:
        self.__call_dict = OrderedDict()

        #Initialize Use statement dictionary:
        self.__use_dict = OrderedDict()

        #Initialize vertical dimension dictionary:
        self.__vert_dict = OrderedDict()

        #Initialize parameter-type variable name set:
        self.__parameter_set = set()

        #Initialize variable name array parameters:
        self.__total_var_num = 0
        self.__stdname_max_len = 0
        self.__ic_name_max_num = 0
        self.__ic_name_max_len = 0

        #Loop over Variable type dictionary:
        for var_info_list in var_type_dict.values():
            #create relevant Fortran data
            self.create_data(var_info_list, ddt_type_dict)

    #####

    def create_data(self, var_info_list, ddt_type_dict, no_use=False):

        """
        Recursive function which is used to add
        a registry variable, DDT, and/or Array
        element to VarFortData's internal lists
        and dictionaries.
        """

        #Separate "ddt_list" into variable object
        #and DDT "type":
        var = var_info_list[0]
        var_ddt = var_info_list[1]

        #First check if variable is a parameter:
        if var.allocatable == 'parameter':
            #If so, then only add to variable
            #name lists and parameter set, as this
            #variable will never be read from a file:
            self.__standard_names.append(var.standard_name)
            self.__ic_names[var.standard_name] = var.ic_names
            self.__parameter_set.add(var.standard_name)

            #Also remove variable from call and use dictionaries,
            #if applicable:
            if var.standard_name in self.__call_dict.keys():
                del self.__call_dict[var.standard_name]
                del self.__use_dict[var.standard_name]

            #Quit function, if variable is a parameter:
            return

        #Check if array elements are present:
        if hasattr(var, "elements") and var.elements:

            #If so, then loop over array elements:
            for element in var.elements:

                #Check if element is actually a DDT:
                if element.is_ddt:
                    #If so, then find associated DDT type:
                    elem_ddt = ddt_type_dict[element.var_type]

                    #Create input list with DDT type:
                    new_elem_info = [element, elem_ddt]

                else:
                    new_elem_info = [element, None]


                #Does variable already have a fortran call?
                #If so, then it must have been part of a DDT,
                #so include the DDT text here:
                if var.standard_name in self.__call_dict.keys():
                    self.__call_dict[element.standard_name] = \
                        self.__call_dict[var.standard_name]

                    #Also include the use statement:
                    self.__use_dict[element.standard_name] = \
                        list(self.__use_dict[var.standard_name])
                else:
                    #Otherwise, set only the use statement,
                    #which will use the parent variable name
                    #directly:
                    self.__use_dict[element.standard_name] = \
                        var.local_name

                #Apply this function again:
                self.create_data(new_elem_info, ddt_type_dict, no_use=True)

            #Once all elemnts have been added, remove the original
            #array variable:
            if var.standard_name in self.__call_dict.keys():
                del self.__call_dict[var.standard_name]
                del self.__use_dict[var.standard_name]


        else:
            #Add variable standard names to list:
            self.__standard_names.append(var.standard_name)

            #Add variable IC names to dictionary:
            self.__ic_names[var.standard_name] = var.ic_names

            #Check if variable doesn't have dimensions, or
            #only has "horizontal_dimensions":
            if not var.dimensions or (len(var.dimensions) == 1 \
               and var.dimensions[0]) == 'horizontal_dimension':

                #Then set vertical level name to None:
                self.__vert_dict[var.standard_name] = None
            else:
                #If not, then check variable standard name for "at_interface":
                if var.standard_name.find("at_interface") != -1:
                    self.__vert_dict[var.standard_name] = "ilev"
                else:
                    self.__vert_dict[var.standard_name] = "lev"


            #Check if variable doesn't exist in call dictionary:
            if var.standard_name not in self.__call_dict.keys():
                #Add to dictionary, with a blank string:
                self.__call_dict[var.standard_name] = ''

            #check if variable doesn't exist in use dictionary:
            if var.standard_name not in self.__use_dict.keys():
                #Add to dicttionary, with empty list:
                self.__use_dict[var.standard_name] = list()

            #Check if variable is actually an array:
            if hasattr(var, "index_name"):
                #If so, then all call string with
                #array indexing:
                self.__call_dict[var.standard_name] += \
                    var.local_index_name_str

                #Also add index variable to use
                #statement dictionary:
                self.__use_dict[var.standard_name].append(var.local_index_name)
            else:
                #If not, then simply use local name for both
                #dictionaries:
                self.__call_dict[var.standard_name] += \
                    var.local_name

                #Only add the use statement here if "no_use" is False:
                if not no_use:
                    self.__use_dict[var.standard_name].append(var.local_name)

            #Check if variable is actually a DDT:
            if var_ddt is not None:
                #If so, then loop over all variables in DDT:
                for new_var in var_ddt.variable_list():

                    #Is DDT variable itself a DDT?
                    if new_var.is_ddt:
                        #If so, then find associated DDT type:
                        new_ddt = ddt_type_dict[new_var.var_type]

                        #Create input list with DDT type:
                        new_var_info = [new_var, new_ddt]

                    else:
                        new_var_info = [new_var, None]

                    #Add variables to call and use dictionaries,
                    #with parent DDT included:
                    self.__call_dict[new_var.standard_name] = \
                        self.__call_dict[var.standard_name]+"%"

                    self.__use_dict[new_var.standard_name] = \
                        list(self.__use_dict[var.standard_name])

                    #Apply this function again:
                    self.create_data(new_var_info, ddt_type_dict, no_use=True)

    #####

    def check_req_vars(self, ccpp_req_vars_set):

        """
        Checks if all input variables required by the CCPP physics
        suites are registered in the host model.   Returns set of
        standard names for all missing host model variables.
        """

        #Convert standard name list to a set:
        var_stdnm_set = set(self.__standard_names)

        #Determine what, if any, required variables are missing
        #from registered variable set:
        missing_vars = ccpp_req_vars_set.difference(var_stdnm_set)

        #Return missing variables set:
        return missing_vars

    #####

    def rm_nonreq_vars(self, ccpp_req_vars_set):

        """
        Remove all non-required variables from
        the fortran call and use statement
        dictionaries, as they won't be necessary.
        """

        #First, check that all required variables
        #are present, if not then raise an error:
        missing_vars = self.check_req_vars(ccpp_req_vars_set)
        if missing_vars:
            emsg = "Required variables are missing from registry, "\
                   "but weren't caught at the correct script location!\n" \
                   "Something has gone wrong.  The missing variables are:\n {}".format(\
                   " ".join(missing_vars))
            raise RuntimeError(emsg)

        #convert standard name list to a set:
        var_stdnm_set = set(self.__standard_names)

        #Determine what variables are not included
        #in the CCPP physics-suite required variable set.
        nonreq_vars = var_stdnm_set.difference(ccpp_req_vars_set)

        #Remove the non-required variables from the call and use
        #dictionaries:
        for var_name in nonreq_vars:
            #Remove from call dictionary, if present:
            if var_name in self.__call_dict.keys():
                del self.__call_dict[var_name]

            #Remove from use dictionary, if present:
            if var_name in self.__use_dict.keys():
                del self.__use_dict[var_name]

    #####

    def calc_init_params(self, logger):

        """
        Calculate variable name array parameters
        to use in generated Fortran code.
        """

        #Determine total number of variables:
        self.__total_var_num = len(self.__standard_names)

        #Determine max standard name string length:
        self.__stdname_max_len = \
            max([len(stdname) for stdname in self.__standard_names])

        #Determine max number of IC variable names:
        try:
            self.__ic_name_max_num = \
                max([len(ic_names) for ic_names in self.__ic_names.values() \
                    if ic_names is not None])
        except ValueError:
            #If there is a ValueError, then likely no IC
            #input variable names exist, so print warning
            #and exit function:
            lmsg = "No '<ic_file_input_names>' tags exist in registry.xml" \
                   ", so no input variable name array will be created."
            logger.warning(lmsg)
            return

        #Deterime max length of input (IC) names:
        self.__find_ic_name_max_len()

    #####

    def __find_ic_name_max_len(self):
        """Determine max length of input (IC) file variable names"""

        #Initialize max IC name string length variable:
        ic_name_max_len = 0

        #Loop over variables in list:
        for ic_names in self.__ic_names.values():
            #Check if variable actually has IC names:
            if ic_names is not None:
                #Loop over all IC input names for given variable:
                for ic_name in ic_names:
                    #Determine IC name string length:
                    ic_name_len = len(ic_name)

                    #Determine if standard name string length is longer
                    #then all prvious values:
                    if ic_name_len > ic_name_max_len:
                        #If so, then re-set max length variable:
                        ic_name_max_len = ic_name_len

        #Set max string length of input variable names:
        self.__ic_name_max_len = ic_name_max_len

    #####

    @property
    def standard_names(self):
        """Return list of variable standard names"""
        return self.__standard_names

    @property
    def ic_names(self):
        """Return dictionary of variable input (IC) names"""
        return self.__ic_names

    @property
    def call_dict(self):
        """Return dictionary of variable fortran calls"""
        return self.__call_dict

    @property
    def use_dict(self):
        """Return dictionary of fortran use statements"""
        return self.__use_dict

    @property
    def vert_dict(self):
        """Return dictionary of vertical variable names"""
        return self.__vert_dict

    @property
    def parameter_set(self):
        """Return set of parameter variable standard names"""
        return self.__parameter_set

    @property
    def total_var_num(self):
        """Return total number of variable standard names"""
        return self.__total_var_num

    @property
    def stdname_max_len(self):
        """Return the max string length for a standard name"""
        return self.__stdname_max_len

    @property
    def ic_name_max_num(self):
        """Return the max number of input (IC) names per variable"""
        return self.__ic_name_max_num

    @property
    def ic_name_max_len(self):
        """Return max length of IC name string"""
        return self.__ic_name_max_len

#################
#HELPER FUNCTIONS
#################

def find_ccpp_req_vars(cap_datafile):
    """
    Generate a set of standard names for all variables
    required by the CCPP physics suites potentially being
    used in this model run.
    """

    #Create new (empty) CCPP-required variables
    #master sett:
    ccpp_req_vars_set = set()

    #Create CCPP datatable suite-listting object:
    list_suites_action = DatatableReport("suite_list", True)

    #Create string of possible CCPP physics suites:
    ccpp_suite_str = datatable_report(cap_datafile, list_suites_action, ";")

    #Convert string to list:
    ccpp_suite_list = ccpp_suite_str.split(";")

    #Loop over CCPP suite names:
    for ccpp_suite in ccpp_suite_list:
        #Create CCPP datatable required variables-listing object:
        list_req_vars_action = DatatableReport("input_variables", ccpp_suite)

        #Generate CCPP required variables list string:
        ccpp_req_vars_str = datatable_report(cap_datafile,
                                             list_req_vars_action, ";")

        #Convert string to actual list:
        ccpp_req_vars = ccpp_req_vars_str.split(";")

        #Add required variables to master list:
        ccpp_req_vars_set.update(ccpp_req_vars)

    #Return the required variables list:
    return ccpp_req_vars_set

##########################
#FORTRAN WRITING FUNCTIONS
##########################

def write_ic_params(outfile, fort_data):

    """
    Write public parameter declarations needed
    by initial condition arrays and functions
    """

    #Create new Fortran integer parameter to store total number of variables:
    outfile.write("!Total number of physics-related variables:", 1)
    outfile.write("integer, public, parameter :: phys_var_num = {}".format(\
                  fort_data.total_var_num), 1)

    #Add blank space:
    outfile.write("", 0)

    #Create another Fortran integer parameter to store max length of
    #variable standard name strings:
    outfile.write("!Max length of physics-related variable standard names:", 1)
    outfile.write("integer, public, parameter :: std_name_len = {}".format(\
                  fort_data.stdname_max_len), 1)

    #Add blank space:
    outfile.write("", 0)

    #Create final Fortran integer parameter to store max length of
    #input variable name string:
    outfile.write("!Max length of input (IC) file variable names:", 1)
    outfile.write("integer, public, parameter :: ic_name_len = {}".format(\
                  fort_data.ic_name_max_len), 1)

    #Add blank space:
    outfile.write("", 0)

######

def write_ic_arrays(outfile, fort_data):

    """
    Write initial condition arrays to store
    data on variable initialization status
    and input file names.
    """

    #Create variable name array string lists:
    stdname_strs = list()
    ic_name_strs = list()

    #Create fake name and fake name list with proper lengths:
    fake_ic_name = " "*fort_data.ic_name_max_len
    fake_ic_names = [fake_ic_name]*fort_data.ic_name_max_num

    #Loop over variables in list:
    for var_stdname in fort_data.standard_names:

        #Create standard_name string with proper size,
        #and append to list:
        extra_spaces = " " * (fort_data.stdname_max_len - len(var_stdname))
        stdname_strs.append("'{}'".format(var_stdname + extra_spaces))

        #Check if variable actually has IC names:
        if fort_data.ic_names[var_stdname] is not None:

            #Extract input (IC) names list:
            ic_names = fort_data.ic_names[var_stdname]

            #Determine number of IC names for variable:
            ic_name_num = len(ic_names)

            #Create new (empty) list to store (IC) file
            #input names of variables with the correct
            #number of spaces to match character array
            #dimensions:
            ic_names_with_spaces = list()

            #Loop over possible input file (IC) names:
            for ic_name in ic_names:
                #Create ic_name string with proper size:
                extra_spaces = " " * (fort_data.ic_name_max_len - len(ic_name))
                #Add properly-sized name to list:
                ic_names_with_spaces.append(ic_name + extra_spaces)

            #Create repeating list of empty, "fake" strings that
            #increases array to max size:
            ic_names_with_spaces.extend([fake_ic_name]*(fort_data.ic_name_max_num - ic_name_num))

            #Append new ic_names to string list:
            ic_name_strs.append(', '.join("'{}'".format(n) for n in ic_names_with_spaces))

        else: #No IC names?

            #Append empty "fake" IC names to input name list:
            ic_name_strs.append(', '.join("'{}'".format(n) for n in fake_ic_names))

    #Write arrays to fortran file:
    #----------------------------

    #Write starting declaration of input standard name array:
    outfile.write("!Array storing all physics-related variable standard names:", 1)
    declare_str = \
        "character(len={}), public, protected :: phys_var_stdnames(phys_var_num) = (/ &".format(\
        fort_data.stdname_max_len)
    outfile.write(declare_str, 1)

    #Write standard names to fortran array:
    for index, stdname_str in enumerate(stdname_strs):
        if index == fort_data.total_var_num-1:
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
        fort_data.ic_name_max_len, fort_data.ic_name_max_num)
    outfile.write(declare_str, 1)

    #Write IC names to fortran array:
    for index, ic_name_str in enumerate(ic_name_strs):
        if index == fort_data.total_var_num-1:
            suffix = ' /), (/{}, phys_var_num/))'.format(fort_data.ic_name_max_num)
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
    for var_num in range(fort_data.total_var_num):
        if var_num == fort_data.total_var_num-1:
            if fort_data.standard_names[var_num] in \
               fort_data.parameter_set:
                log_arr_str = '.true. /)'
            else:
                log_arr_str = '.false. /)'
        else:
            if fort_data.standard_names[var_num] in \
               fort_data.parameter_set:
                log_arr_str = '.true., &'
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

def write_phys_read_subroutine(outfile, fort_data):

    """
    Write the "physics_read_data" subroutine, which
    is used to initialize required physics variables
    by reading in the values from an Initial Conditions
    (IC) input file. This will only be done for registry
    variables which contain
    """

    #Construct list of variables in use statements:
    #--------------------------------
    #Create new (empty) list to store use statements:
    use_vars = list()

    #Loop over all variable standard names:
    for var_stdname in fort_data.standard_names:

        #Check if variable is in use dictionary:
        if var_stdname in fort_data.use_dict.keys():

            #If so, then extract used variables list:
            use_var_names = fort_data.use_dict[var_stdname]

            #Loop over used variable list:
            for use_var_name in use_var_names:
                #Is variable not already in the list?
                if use_var_name not in use_vars:
                    #If not, then add it to list:
                    use_vars.append(use_var_name)
    #--------------------------------

    #Create actual fortran use statements:
    #--------------------------------

    #Create new (empty) list to store use statements:
    use_list = list()

    #Initialize loop counter:
    lpcnt = 0

    #Loop over use statement variables:
    for use_var in use_vars:

        #If loop counter is zero, then
        #create new use string:
        if lpcnt == 0:
            use_str = \
                    "use physics_types,        only: {}".format(\
                    use_var)

            #Advance loop counter by one:
            lpcnt += 1

        elif lpcnt == 2:
            #Append variable name to use string
            use_str += ", {}".format(use_var)

            #Add use string to list:
            use_list.append(use_str)

            #Reset use_str variable:
            use_str = None

            #If loop counter is two, then
            #reset loop counter:
            lpcnt = 0
        else:
            #Append variable name to use string
            use_str += ", {}".format(use_var)

            #Advance loop counter by one:
            lpcnt += 1

    #Is there any remaining "use_str" text?
    if use_str:
        #Add to "use statement" list:
        use_list.append(use_str)
    #-----------------------------

    #Create fortran "read_field" calls:
    #---------------------------------

    #Create new (empty) dictionary to store "read_field" calls:
    call_string_dict = OrderedDict()

    #Loop over all variable standard names:
    for var_stdname in fort_data.standard_names:

        #Check if variable is in fortran call dictionary:
        if var_stdname in fort_data.call_dict.keys():

            #Set "if-statement" call string:
            call_string_key = "if (trim(phys_var_stdnames(name_idx)) ==" \
                              " '{}') then".format(var_stdname)

            #Extract vertical level variable:
            levnm = fort_data.vert_dict[var_stdname]

            #Set "read_field" call string:
            if levnm is not None:
                call_string_val = "call read_field(file, input_var_names(:,name_idx)," + \
                                  " '{}', timestep, {})".format(\
                                  levnm, fort_data.call_dict[var_stdname])
            else:
                call_string_val = "call read_field(file, input_var_names(:,name_idx)," + \
                                  " timestep, {})".format(fort_data.call_dict[var_stdname])

            #Add strings to dictionary:
            call_string_dict[call_string_key] = call_string_val

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
    for if_call, read_call in call_string_dict.items():
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

#############
# End of file
#############
