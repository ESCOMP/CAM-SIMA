#!/usr/bin/env python3

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
    Create the "phys_init" fortran
    files usig meta-data collected
    collected from registry generation.
    The two specific fortran files are:

    1.  phys_vars_init_check.F90

        This file contains four
        variable arrays:

        phys_var_stdnames -
            All registered variable
            standard names

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

    #Initialize a new (empty) variable dictionary, with
    #variable standard names as keys, and a list of variable
    #objects, associated DDT objects, and file names as values:
    var_info_dict = OrderedDict()

    #Initialize a new (empty) master DDT dictionary, with
    #DDT types as keys, and the associated DDT object as values:
    ddt_type_dict = OrderedDict()

    #Generate DDT dictionaries and index name set:
    #-----------------------
    #Loop over all registry files:
    for file_obj in files:
        #Add registry file DDT dictionary to master dictionary:
        ddt_type_dict.update(file_obj.ddts)

        #Loop over all variables in registry file:
        for var in file_obj.var_dict.variable_list():
            if var.is_ddt:
                #Extract associated DDT object:
                ddt = ddt_type_dict[var.var_type]
                #Add variable to info dictionary:
                var_info_dict[var.standard_name] = [var, ddt, file_obj.name]
            else:
                #If not a DDT, then set DDT value to None:
                var_info_dict[var.standard_name] = [var, None, file_obj.name]
            # end if
        # end for
    # end for
    #-----------------------

    #Generate CCPP required variables set:
    ccpp_req_vars_set = find_ccpp_req_vars(cap_datafile)

    #Create Fortran data object:
    fort_data = VarFortData(var_info_dict, ddt_type_dict, ccpp_req_vars_set)

    #Check if all required variable are present:
    #-----------------------
    if ccpp_req_vars_set:
        missing_vars = fort_data.check_req_vars()

        if missing_vars:
            #Are variables missing?  If so then end run here.
            #Create error message:
            emsg = "Required CCPP physics suite variables missing " \
                   "from registered host model variable list:\n {}".format(\
                   " ".join(missing_vars))

            #Add error-message to logger, and return with non-zero retmsg:
            logger.error(emsg)
            retmsg = "Required CCPP physics variables missing from host model."
            return retmsg
        # end if
    # end if
    #-----------------------

    #Calculate fortran variable and IC name array parameters:
    retmsg = fort_data.calc_init_params(logger)

    #If a return message exists, then no IC variable names were found,
    #so exit routine here:
    if retmsg:
        return retmsg
    # end if

    #Generate "phys_vars_init_check.F90" file:
    #--------------------------------------

    #Open new file:
    if phys_check_filename:
        ofilename = os.path.join(outdir, phys_check_filename)
        #Get file name, ignoring file type:
        phys_check_fname_str = os.path.splitext(phys_check_filename)[0]
    else:
        ofilename = os.path.join(outdir, "phys_vars_init_check.F90")
        phys_check_fname_str = "phys_vars_init_check"
    # end if

    #Log file creation:
    logger.info("Writing initialization-checking source file, {}".format(ofilename))

    #Open file using CCPP's FortranWriter:
    file_desc = "Initialization-checking source file"
    with FortranWriter(ofilename, "w", file_desc,
                       phys_check_fname_str, indent=indent) as outfile:

        #Add boilerplate code:
        outfile.write_preamble()

        #Write public parameters:
        write_ic_params(outfile, fort_data)

        #Write initial condition arrays:
        write_ic_arrays(outfile, fort_data)

        #Add public function declarations:
        outfile.write("!! public interfaces", 0)
        outfile.write("public :: mark_as_initialized", 1)
        outfile.write("public :: mark_as_read_from_file", 1)
        outfile.write("public :: is_initialized", 1)
        outfile.write("public :: is_read_from_file", 1)

        #Add "contains" statement:
        outfile.end_module_header()
        outfile.write("", 0)

        #Write initialization marking subroutine:
        write_init_mark_subroutine(outfile)

        #Add two blank spaces:
        outfile.write("", 0)
        outfile.write("", 0)

        #Write read from file marking subroutine:
        write_read_from_file_mark_subroutine(outfile)

        #Add two blank spaces:
        outfile.write("", 0)
        outfile.write("", 0)

        #Write initialization check function:
        write_is_init_func(outfile)

        #Add two blank spaces:
        outfile.write("", 0)
        outfile.write("", 0)

        #Write read from file check function:
        write_is_read_from_file_func(outfile)
    # end with (end of module)
    #--------------------------------------

    #Generate "physics_inputs.F90" file:
    #--------------------------------------

    #Open new file:
    if phys_input_filename:
        ofilename = os.path.join(outdir, phys_input_filename)
        #Get file name, ignoring file type:
        phys_input_fname_str = os.path.splitext(phys_input_filename)[0]
    else:
        ofilename = os.path.join(outdir, "physics_inputs.F90")
        phys_input_fname_str = "physics_inputs"
    # end if

    #Log file creation:
    logger.info("Writing initial conditions source file, {}".format(ofilename))

    #Open file using CCPP's FortranWriter:
    file_desc = "Initial conditions source file, {}".format(phys_input_filename)
    with FortranWriter(ofilename, "w", file_desc, phys_input_fname_str,
                       indent=indent) as outfile:

        #Add boilerplate code:
        outfile.write_preamble()
        outfile.write("", 0)

        #Add public function declarations:
        outfile.write("!! public interfaces", 0)
        outfile.write("public :: physics_read_data", 1)
        outfile.write("public :: physics_check_data", 1)

        #Add "contains" statement:
        outfile.write("", 0)
        outfile.end_module_header()

        #Write physics_read_data subroutine:
        write_phys_read_subroutine(outfile, fort_data, phys_check_fname_str)

        #Add a blank space:
        outfile.write("", 0)

        #Write physics_check_data subroutine:
        write_phys_check_subroutine(outfile, fort_data, phys_check_fname_str)

    #--------------------------------------

    #Return retmsg:
    return retmsg

###############################
#Fortran generation error class
###############################

class CamInitWriteError(ValueError):
    """Class used to handle CAM write_init errors
    (e.g., log user errors without backtrace)"""
    # pylint: disable=useless-super-delegation
    def __init__(self, message):
        super(CamInitWriteError, self).__init__(message)
    # pylint: enable=useless-super-delegation

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

    def __init__(self, var_info_dict, ddt_type_dict, ccpp_req_vars_set):

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

        #Initialize standard_name/meta file dictionary,
        #please note that this dictionary is only used
        #for error reporting:
        self.__stdname_file_dict = OrderedDict()

        #Initialize parameter-type variable name set:
        self.__parameter_set = set()

        #Initialize protected variable name set:
        self.__protected_set = set()

        #Initialize internal required variables set:
        self.__ccpp_req_vars = ccpp_req_vars_set

        #Initialize variable name array parameters:
        self.__total_var_num = 0
        self.__stdname_max_len = 0
        self.__ic_name_max_num = 0
        self.__ic_name_max_len = 0

        #Loop over Variable type dictionary:
        for var_info_list in var_info_dict.values():
            #create relevant Fortran data
            self.create_data(var_info_list, ddt_type_dict)
        # end for

    #####

    def create_data(self, var_info_list, ddt_type_dict, no_use=False):

        """
        Recursive function which is used to add
        a registry variable, DDT, and/or Array
        element to VarFortData's internal lists
        and dictionaries.
        """

        #Separate "info_list" into variable object,
        #DDT "type", and associated file name:
        var, var_ddt, var_file = var_info_list

        #Add variable standard name and file to error-reporting dict:
        if var.standard_name in self.__stdname_file_dict:
            self.__stdname_file_dict[var.standard_name].append(var_file)
        else:
            self.__stdname_file_dict[var.standard_name] = [var_file]
        # end if

        #First check if the variable standard name already
        #exists in the standard_names list.  If so then
        #raise an error:
        if var.standard_name in self.__standard_names:
            emsg = "Multiple registered variables have the" \
                   " standard name '{}'.\nThere can only be" \
                   " one registered variable per standard name.\n" \
                   "The meta files containing the conflicting variables" \
                   " are:\n"+"\n".join(self.__stdname_file_dict[var.standard_name])
            raise CamInitWriteError(emsg.format(var.standard_name))
        # end if

        #Currently there is no way to set input names for
        #variables coming from registered meta-files.  To
        #alleviate this concern, all variables that lack
        #any registered input names will instead have their
        #local name assigned as an input name:
        if var.ic_names is None:
            ic_names = [var.local_name]
        else:
            ic_names = var.ic_names
        # end if

        #Check if variable is a parameter:
        if var.allocatable == 'parameter':
            #If so, then only add to variable
            #name lists and parameter set, as this
            #variable will never be read from a file:
            self.__standard_names.append(var.standard_name)
            self.__ic_names[var.standard_name] = ic_names
            self.__parameter_set.add(var.standard_name)

            #Also remove variable from call and use dictionaries,
            #if applicable:
            if var.standard_name in self.__call_dict:
                del self.__call_dict[var.standard_name]
            # end if
            if var.standard_name in self.__use_dict:
                del self.__use_dict[var.standard_name]
            # end if

            #Quit function, if variable is a parameter:
            return
        # end if

        #Check if variable is protected, and if so then
        #add it to the protected set:
        if var.protected:
            self.__protected_set.add(var.standard_name)
        # end if

        #Check if array elements are present:
        if var.elements:

            #If so, then loop over array elements:
            for element in var.elements:

                #Check if element is actually a DDT:
                if element.is_ddt:
                    #If so, then find associated DDT type:
                    elem_ddt = ddt_type_dict[element.var_type]

                    #Create input list with DDT type:
                    new_elem_info = [element, elem_ddt, var_file]

                else:
                    new_elem_info = [element, None, var_file]
                # end if

                #Does variable already have a fortran call?
                #If so, then it must have been part of a DDT,
                #so include the DDT text here:
                if var.standard_name in self.__call_dict:
                    self.__call_dict[element.standard_name] = \
                        str(self.__call_dict[var.standard_name])

                    #Also include the use statement:
                    self.__use_dict[element.standard_name] = \
                        list(self.__use_dict[var.standard_name])
                else:
                    #Otherwise, set only the use statement,
                    #which will use the parent variable name
                    #directly:
                    self.__use_dict[element.standard_name] = \
                        [var_file, var.local_name]
                # end if

                #Apply this function again:
                self.create_data(new_elem_info, ddt_type_dict, no_use=True)
            # end for

            #Once all elemnts have been added, remove the original
            #array variable:
            if var.standard_name in self.__call_dict:
                del self.__call_dict[var.standard_name]
            # end if
            if var.standard_name in self.__use_dict:
                del self.__use_dict[var.standard_name]
            # end if
        else:
            #Add variable info to relevant lists if NOT a DDT:
            if var_ddt is None:
                #Add variable standard name to list:
                self.__standard_names.append(var.standard_name)

                #Add variable IC names to dictionary:
                self.__ic_names[var.standard_name] = ic_names

                #Check if variable is required:
                if (var.standard_name in self.__ccpp_req_vars) and (not var.protected):

                    #If so, then check if non-DDT variable has dimensions:
                    if not var.dimensions:
                        #Variable must have at least one dimension for "read_field"
                        #call.  End the build here:
                        emsg = "Variable '{}' needs at least one dimension in order" \
                               " to be read from a file using 'read_field'."
                        raise CamInitWriteError(emsg.format(var.standard_name))
                    # end if

                    #Currently the file-reading fortran code assumes
                    #that at least one of the dimensions matches
                    #the horizontal dimension (e.g. number of host
                    #model gird columns). Thus for now the dimension
                    #"horizontal_dimension" must be present in the
                    #dimensions list:
                    if "horizontal_dimension" not in var.dimensions:
                        emsg = "Variable '{}' needs at least one" \
                               " registered dimension to be" \
                               " 'horizontal_dimension' in order" \
                               " to be read from a file using 'read_fied'.\n" \
                               "Instead variable has dimensions of: {}"
                        raise CamInitWriteError(emsg.format(var.standard_name,
                                                            var.dimensions))
                    # end if
                    #Check the size of non-DDT variable dimensions:
                    if len(var.dimensions) == 1:
                        #Then set vertical level name to None:
                        self.__vert_dict[var.standard_name] = None
                    elif len(var.dimensions) == 2:
                        #Then check vertical dimension name:
                        if "vertical_interface_dimension" in var.dimensions:
                            self.__vert_dict[var.standard_name] = "ilev"
                        elif "vertical_layer_dimension" in var.dimensions:
                            self.__vert_dict[var.standard_name] = "lev"
                        else:
                            if var.dimensions[0] == 'horizontal_dimension':
                                unsupp_dim = var.dimensions[1]
                            else:
                                unsupp_dim = var.dimensions[0]
                            # end if
                            emsg = "Unsupported vertical dimension"
                            emsg += ", '{}', in {}".format(unsupp_dim,
                                                           var.standard_name)
                            raise CamInitWriteError(emsg)
                        # end if
                    else:
                        #Variable can only have two dimnsions max for "read_field"
                        #call.  End the build here:
                        emsg = "variable '{}' has more than two dimensions, but" \
                               "'read_field' can only manage up to two dimensions" \
                               "when reading a variable from a file."
                        raise CamInitWriteError(emsg.format(var.standard_name))
                    # end if

                    #Check if variable doesn't exist in call dictionary:
                    if var.standard_name not in self.__call_dict:
                        #Add to dictionary, with a blank string:
                        self.__call_dict[var.standard_name] = ''
                    # end if

                    #Check if variable doesn't exist in use dictionary:
                    if var.standard_name not in self.__use_dict:
                        #Add to dictionary, with only file name present:
                        self.__use_dict[var.standard_name] = [var_file]
                    # end if

                    #Check if variable is actually an array:
                    if var.index_name:
                        #If so, then add call string with
                        #array indexing:
                        self.__call_dict[var.standard_name] += \
                                                var.local_index_name_str

                        #Also add index variable to use
                        #statement dictionary:
                        self.__use_dict[var.standard_name].append(var.local_index_name)

                    else:
                        #If not, then simply use local name for both
                        #dictionaries:
                        self.__call_dict[var.standard_name] += var.local_name

                        #Only add the use statement here if "no_use" is False:
                        if not no_use:
                            self.__use_dict[var.standard_name].append(var.local_name)
                        # end if
                    # end if
                else:
                    #If variable is not required, then attempt to delete
                    #entries from the call and use dictionaries, if present:
                    if var.standard_name in self.__call_dict:
                        del self.__call_dict[var.standard_name]
                    # end if
                    if var.standard_name in self.__use_dict:
                        del self.__use_dict[var.standard_name]
                    # end if
                # end if
            # end if
            #Check if variable is actually a DDT:
            if var_ddt is not None:
                #If so, then loop over all variables in DDT:
                for new_var in var_ddt.variable_list():

                    #Is DDT variable itself a DDT?
                    if new_var.is_ddt:
                        #If so, then find associated DDT type:
                        new_ddt = ddt_type_dict[new_var.var_type]

                        #Create input list with DDT type:
                        new_var_info = [new_var, new_ddt, var_file]

                    else:
                        new_var_info = [new_var, None, var_file]
                    # end if

                    #Add variables to call and use dictionaries,
                    #with parent DDT included, assuming variable is
                    #not protected:
                    if not var.protected:
                        #Does variable already exist in call dictionary?
                        if var.standard_name in self.__call_dict:
                            #Then use parent variable entry in call:
                            self.__call_dict[new_var.standard_name] = \
                                self.__call_dict[var.standard_name] + \
                                var.local_name+"%"
                        else:
                            #If not, then create a new entry:
                            self.__call_dict[new_var.standard_name] = \
                                var.local_name+"%"
                        # end if
                        #Does variable already exist in use dictionary?
                        if var.standard_name in self.__use_dict:
                            #Then use parent variable for dictionary call:
                            self.__use_dict[new_var.standard_name] = \
                                list(self.__use_dict[var.standard_name])
                        else:
                            #If not, then create a new entry:
                            self.__use_dict[new_var.standard_name] = \
                                [var_file, var.local_name]
                        # end if
                    # end if
                    #Apply this function again:
                    self.create_data(new_var_info, ddt_type_dict, no_use=True)
                # end for
            # end if
        # end if
    #####

    def check_req_vars(self, ccpp_req_vars_set=None):

        """
        Checks if all input variables required by the CCPP physics
        suites are registered in the host model.   Returns set of
        standard names for all missing host model variables.
        """

        #Convert standard name list to a set:
        var_stdnm_set = set(self.__standard_names)

        #Determine what, if any, required variables are missing
        #from registered variable set:
        if ccpp_req_vars_set:
            missing_vars = ccpp_req_vars_set.difference(var_stdnm_set)
        else:
            missing_vars = self.__ccpp_req_vars.difference(var_stdnm_set)
        # end if

        #Return missing variables set:
        return missing_vars

    #####

    def calc_init_params(self, logger):

        """
        Calculate variable name array parameters
        to use in generated Fortran code.
        """

        #Initialize return message:
        retmsg = ""

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
            #input variable names exist, so print error
            #and exit function with proper return message:
            lmsg = "No '<ic_file_input_names>' tags exist in registry.xml" \
                   ", so no input variable name array will be created."
            logger.error(lmsg)

            retmsg = "No input names (<ic_file_input_names>) present in registry."
            return retmsg
        # end try

        #Deterime max length of input (IC) names:
        self.__find_ic_name_max_len()

        #Exit function normally:
        return retmsg

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
                    # end if
                # end for
            # end if
        # end for

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
    def protected_set(self):
        """Return set of protected variable standard names"""
        return self.__protected_set

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

    #Create CCPP datatable suite-listing object:
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
        ccpp_req_vars_str = datatable_report(cap_datafile, list_req_vars_action,
                                             ";", excl_prot=True)

        if ccpp_req_vars_str:
            #Convert string to actual list, if it exists:
            ccpp_req_vars = ccpp_req_vars_str.split(";")

            #Add required variables to master list:
            ccpp_req_vars_set.update(ccpp_req_vars)
        # end if
    # end for

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

    #Add parameters for initialized_vars options:
    outfile.write("!Parameterized initialized_vars options - order matters", 1)
    outfile.write("integer, public, parameter ::  UNINITIALIZED = 0", 1)
    outfile.write("integer, public, parameter ::    INITIALIZED = 1", 1)
    outfile.write("integer, public, parameter ::          PARAM = 2", 1)
    outfile.write("integer, public, parameter :: READ_FROM_FILE = 3", 1)

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
            # end for

            #Create repeating list of empty, "fake" strings that
            #increases array to max size:
            ic_names_with_spaces.extend([fake_ic_name]*(fort_data.ic_name_max_num - ic_name_num))

            #Append new ic_names to string list:
            ic_name_strs.append(', '.join("'{}'".format(n) for n in ic_names_with_spaces))

        else: #No IC names?

            #Append empty "fake" IC names to input name list:
            ic_name_strs.append(', '.join("'{}'".format(n) for n in fake_ic_names))
        # end if
    # end for

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
    # end for

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
    # end for
    #Write a blank space:
    outfile.write("", 0)

    #Write starting declaration of protected logical array:
    outfile.write("!Logical array to indicate whether or not variable is protected:", 1)
    declare_str = "logical, public, protected :: protected_vars(phys_var_num) = (/ &"
    outfile.write(declare_str, 1)

    #Write "False" logicals to logical array, unless variable
    #is protected:
    arr_suffix = ', &'
    for var_num, var_name in enumerate(fort_data.standard_names):
        #If at the end of the list, then update suffix:
        if var_num == fort_data.total_var_num-1:
            arr_suffix = ' /)'
        # end if
        #Set array values:
        if var_name in fort_data.protected_set:
            log_arr_str = '.true.' + arr_suffix
        else:
            log_arr_str = '.false.' + arr_suffix
        # end if

        #Write line to file:
        outfile.write(log_arr_str, 2)
    # end for

    #Write a blank space:
    outfile.write("", 0)

    #Write starting declaration of initialized logical array:
    outfile.write("!array to indicate: variable is UNINITIALIZED, INTIIALIZED, PARAM or READ_FROM_FILE:", 1)
    declare_str = "integer, public, protected :: initialized_vars(phys_var_num) = (/ &"
    outfile.write(declare_str, 1)

    #Write UNINITIALIZED to integer array, unless
    #variable is a parameter:
    arr_suffix = ', &'
    for var_num, var_name in enumerate(fort_data.standard_names):
        #If at the end of the list, then update suffix:
        if var_num == fort_data.total_var_num-1:
            arr_suffix = ' /)'
        # end if
        #Set array values:
        if var_name in fort_data.parameter_set:
            log_arr_str = 'PARAM' + arr_suffix
        else:
            log_arr_str = 'UNINITIALIZED' + arr_suffix
        # end if
        #Write line to file:
        outfile.write(log_arr_str, 2)
    # end for

    #Write a blank space:
    outfile.write("", 0)
    #----------------------------

######

def write_init_mark_subroutine(outfile):

    """
    Write "Mark Initialized" subroutine which
    is used to modify the "initalized_vars"
    array and sets the value for the specified
    variable to INITIALIZED.
    """

    #Add subroutine header:
    outfile.write("subroutine mark_as_initialized(varname)", 1)

    #Write a blank space:
    outfile.write("", 0)

    #Add subroutine description:
    outfile.write("!This subroutine  marks the variable as\n" \
                  "!INITIALIZED in the `initialized_vars` array,\n" \
                  "!which means any initialization check should\n" \
                  "!now return True.", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Add use statements:
    outfile.write("use cam_abortutils, only: endrun", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Add variable declaration statements:
    outfile.write("character(len=*), intent(in) :: varname !Variable name being marked", 2)
    outfile.write("", 0)
    outfile.write("integer :: stdnam_idx !Standard name array index", 2)
    outfile.write("", 0)
    outfile.write("logical :: found_var !Logical which indicates variable exists in array", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Add main subroutine section:
    #---------------------------
    outfile.write("found_var = .false.", 2)
    outfile.write("!Loop over standard name array:", 2)
    outfile.write("do stdnam_idx = 1, phys_var_num", 2)

    outfile.write("!Check if standard name matches provided variable name:", 3)
    outfile.write("if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then", 3)

    outfile.write("!Only set to INITIALIZED if not already PARAM or READ_FROM_FILE", 4)
    outfile.write("if (initialized_vars(stdnam_idx) < PARAM) then", 4)

    outfile.write("!If so, then set associated initialized_vars\n" \
                  "!array index to INITIALIZED:", 5)
    outfile.write("initialized_vars(stdnam_idx) = INITIALIZED", 5)

    outfile.write("end if", 4)

    outfile.write("", 0)
    outfile.write("!Indicate variable has been found:", 4)
    outfile.write("found_var = .true.", 4)
    outfile.write("exit ! Exit loop", 4)

    outfile.write("end if", 3)

    outfile.write("end do", 2)

    outfile.write("", 0)
    outfile.write("if (.not. found_var) then", 2)
    outfile.write("!If loop has completed with no matches, then endrun with warning\n" \
                  "!that variable didn't exist in standard names array:", 3)
    outfile.write('''call endrun("Variable '"//trim(varname)//"' is missing from phys_var_stdnames array.")''', 3)
    outfile.write("end if", 2)

    outfile.write("", 0)
    #---------------------------

    #End subroutine:
    outfile.write("end subroutine mark_as_initialized", 1)

######

def write_read_from_file_mark_subroutine(outfile):
    """
    Write "Mark Read From File" subroutine which
    is used to modify the "initialized_vars"
    array and set the value for the specified
    variable to READ_FROM_FILE
    """

    #Add subroutine header:
    outfile.write("subroutine mark_as_read_from_file(varname)", 1)

    #Write a blank space:
    outfile.write("", 0)

    #Add subroutine description:
    outfile.write("!This subroutine marks the varible as \n" \
                  "!READ_FROM_FILE in the initialized_vars array", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Add use statements:
    outfile.write("use cam_abortutils, only: endrun", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Add variable declaration statements:
    outfile.write("character(len=*), intent(in) :: varname !Variable name being marked", 2)
    outfile.write("", 0)
    outfile.write("integer :: stdnam_idx !Standard name array index", 2)
    outfile.write("", 0)
    outfile.write("logical :: found_var !Logical which indicates variable exists in array", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Add main subroutine section:
    #---------------------------
    outfile.write("found_var = .false.", 2)
    outfile.write("!Loop over input name array:", 2)
    outfile.write("do stdnam_idx = 1, phys_var_num", 2)

    outfile.write("!Check if input variable name matches provided variable name:", 3)
    outfile.write("if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then", 3)

    outfile.write("!Check if initialized_vars at that index has already been set to PARAM", 4)
    outfile.write("if (initialized_vars(stdnam_idx) == PARAM) then", 4)
    outfile.write("!If so, call endrun because that should not happen", 5)
    outfile.write('''call endrun("Variable '"//trim(varname)//"' was read from file, but was a parameter")''', 5)
    outfile.write("end if", 4)

    outfile.write("!Otherwise, set associated initialized_vars\n" \
                  "!array index to READ_FROM_FILE:", 4)
    outfile.write("initialized_vars(stdnam_idx) = READ_FROM_FILE", 4)

    outfile.write("", 0)
    outfile.write("!Indicate variable has been found:", 4)
    outfile.write("found_var = .true.", 4)
    outfile.write("exit ! Exit loop", 4)

    outfile.write("end if", 3)

    outfile.write("end do", 2)

    outfile.write("", 0)
    outfile.write("if (.not. found_var) then", 2)
    outfile.write("!If loop has completed with no matches, then endrun with warning\n" \
                  "!that variable didn't exist in standard names array:", 3)
    outfile.write('''call endrun("Variable '"//trim(varname)//"' is missing from phys_var_stdnames array.")''', 3)
    outfile.write("end if", 2)
    outfile.write("", 0)
    #--------------------------

    #End subroutine:
    outfile.write("end subroutine mark_as_read_from_file", 1)

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

    #Write a blank space:
    outfile.write("", 0)

    #Add variable declaration statements:
    outfile.write("character(len=*), intent(in) :: varname !Variable name being checked", 2)
    outfile.write("character(len=*), parameter  :: subname = 'is_initialized: '", 2)
    outfile.write("", 0)
    outfile.write("integer :: stdnam_idx !standard name array index", 2)
    outfile.write("logical :: found      !check that <varname> was found", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Initialize return variable:
    outfile.write("is_initialized = .false.", 2)
    outfile.write("found = .false.", 2)
    outfile.write("", 0)

    #Add main function section:
    #-------------------------
    outfile.write("!Loop over standard name array:", 2)
    outfile.write("do stdnam_idx = 1, phys_var_num", 2)

    outfile.write("!Check if standard name matches provided variable name:", 3)
    outfile.write("if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then", 3)

    outfile.write("!If so, then return True if PARAM, INITIALIZED, OR READ_FROM_FILE", 4)
    outfile.write("is_initialized = (initialized_vars(stdnam_idx) > UNINITIALIZED)", 4)

    outfile.write("found = .true.", 4)
    outfile.write("exit ! Exit loop", 4)

    outfile.write("end if", 3)

    outfile.write("end do", 2)

    outfile.write("", 0)

    outfile.write("if (.not. found) then", 2)
    outfile.write("!If loop has completed with no matches, then endrun with warning\n" \
                  "!that variable didn't exist in standard names array:", 3)
    outfile.write('''call endrun(subname//"Variable '"//trim(varname)//"' is missing from phys_var_stdnames array.")''', 3)
    outfile.write("end if", 2)

    outfile.write("", 0)
    #-------------------------

    #End subroutine:
    outfile.write("end function is_initialized", 1)

######

def write_is_read_from_file_func(outfile):

    """
    Write "Is Read From File" function which
    is used to check if a given variable has
    been read from file according to
    the "initialized_vars" array.
    """


    #Add subroutine header:
    outfile.write("logical function is_read_from_file(varname, stdnam_idx_out)", 1)

    #Write a blank space:
    outfile.write("", 0)

    #Add subroutine description:
    outfile.write("!This function checks if the variable is", 2)
    outfile.write("!read from file according to the", 2)
    outfile.write("!`initialized_vars` array.", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Add use statements:
    outfile.write("use cam_abortutils, only: endrun", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Write a blank space:
    outfile.write("", 0)

    #Add variable declaration statements:
    outfile.write("character(len=*), intent(in)   :: varname !Variable name being checked", 2)
    outfile.write("integer, optional, intent(out) :: stdnam_idx_out", 2)
    outfile.write("", 0)
    outfile.write("character(len=*), parameter    :: subname = 'is_read_from_file: '", 2)
    outfile.write("integer                        :: stdnam_idx !standard name array index", 2)
    outfile.write("logical                        :: found      !check that <varname> was found", 2)

    #Write a blank space:
    outfile.write("", 0)

    #Initialize return variable:
    outfile.write("is_read_from_file = .false.", 2)
    outfile.write("found = .false.", 2)
    outfile.write("", 0)

    #Add main function section:
    #-------------------------
    outfile.write("!Loop over standard name array:", 2)
    outfile.write("do stdnam_idx = 1, phys_var_num", 2)

    outfile.write("!Check if standard name matches provided variable name:", 3)
    outfile.write("if (trim(phys_var_stdnames(stdnam_idx)) == trim(varname)) then", 3)

    outfile.write("!If so, then return True if READ_FROM_FILE:", 4)
    outfile.write("is_read_from_file = (initialized_vars(stdnam_idx) == READ_FROM_FILE)", 4)

    outfile.write("!Mark as found:", 4)
    outfile.write("found = .true.", 4)
    outfile.write("exit ! Exit loop", 4)

    outfile.write("end if", 3)

    outfile.write("end do", 2)

    outfile.write("", 0)

    outfile.write("if (.not. found) then", 2)
    outfile.write("!If loop has completed with no matches, then endrun with warning", 3)
    outfile.write("!that variable didn't exist in standard names array:", 3)
    outfile.write('''call endrun(subname//"Variable '"//trim(varname)//"' is missing from phys_var_stdnames array.")''', 3)
    outfile.write("end if", 2)

    #Handle optional output variable:
    outfile.write("if (present(stdnam_idx_out)) then", 2)
    outfile.write("stdnam_idx_out = stdnam_idx", 3)
    outfile.write("end if", 2)

    outfile.write("", 0)
    #-------------------------

    #End subroutine:
    outfile.write("end function is_read_from_file", 1)

######

def write_phys_read_subroutine(outfile, fort_data, phys_check_fname_str):

    """
    Write the "physics_read_data" subroutine, which
    is used to initialize required physics variables
    by reading in the values from an Initial Conditions
    (IC) input file. This will only be done for registry
    variables which contain
    """

    #Construct dictionary of modules
    #and variables in use statements:
    #--------------------------------
    #Create new (empty) dictionary to store use statements:
    use_vars_write_dict = OrderedDict()

    #Loop over all variable standard names:
    for var_stdname in fort_data.standard_names:

        #Check if variable is in use dictionary:
        if var_stdname in fort_data.use_dict:

            #If so, then extract variable use statement list:
            var_use_info = fort_data.use_dict[var_stdname]

            #Extract module name (always first item in list):
            use_mod_name = var_use_info[0]

            #Check if module name is already in dictionary:
            if use_mod_name in use_vars_write_dict:
                #If so, then loop over variable use list:
                for use_var in var_use_info[1:]:
                    #If variable doesn't already exist in list, then add it:
                    if use_var not in use_vars_write_dict[use_mod_name]:
                        use_vars_write_dict[use_mod_name].append(use_var)
                    # end if
                # end for
            else:
                #Add module name as new key to dictionary:
                use_vars_write_dict[use_mod_name] = var_use_info[1:]
            # end if
        # end if
    # end for
    #--------------------------------

    #Create actual fortran use statements:
    #--------------------------------
    #Create new (empty) list to store use statements:
    use_list = list()

    #Loop over use statement modules:
    for use_mod in use_vars_write_dict:

        #Loop over use statement variables:
        for use_var in use_vars_write_dict[use_mod]:

            #create new use string:
            use_str = "use {},        only: {}".format(use_mod, use_var)

            #Add to "use statement" list:
            use_list.append(use_str)
        # end for
    # end for
    #-----------------------------

    #Create fortran "read_field" calls:
    #---------------------------------

    #Create new (empty) dictionary to store "read_field" calls:
    call_string_dict = OrderedDict()

    #Loop over all variable standard names:
    for var_stdname in fort_data.standard_names:

        #Check if variable is in fortran call dictionary:
        if var_stdname in fort_data.call_dict:

            #Set "if-statement" call string:
            call_string_key = "case ('{}')".format(var_stdname)

            #Extract vertical level variable:
            levnm = fort_data.vert_dict[var_stdname]

            #Set "read_field" call string:
            if levnm is not None:
                call_str = "call read_field(file, '{}', " + \
                           "input_var_names(:,name_idx), '{}', timestep, {})"
                call_string_val = call_str.format(var_stdname, levnm,
                                                  fort_data.call_dict[var_stdname])
            else:
                call_str = "call read_field(file, '{}', " + \
                           "input_var_names(:,name_idx), timestep, {})"
                call_string_val = call_str.format(var_stdname,
                                                  fort_data.call_dict[var_stdname])
            # end if

            #Add strings to dictionary:
            call_string_dict[call_string_key] = call_string_val
        # end if
    # end for
    #---------------------------------

    #Write actual subroutine code:
    #----------------------------

    #Add subroutine header:
    outfile.write("subroutine physics_read_data(file, suite_names, timestep, read_initialized_variables)", 1)

    #Add use statements:
    outfile.write("use pio,                  only: file_desc_t", 2)
    outfile.write("use cam_abortutils,       only: endrun", 2)
    outfile.write("use shr_kind_mod,         only: SHR_KIND_CS, SHR_KIND_CL, SHR_KIND_CX", 2)
    outfile.write("use physics_data,         only: read_field, find_input_name_idx", 2)
    outfile.write("use physics_data,         only: no_exist_idx, init_mark_idx, prot_no_init_idx", 2)
    outfile.write("use cam_ccpp_cap,         only: ccpp_physics_suite_variables", 2)
    outfile.write("use {}, only: phys_var_stdnames, input_var_names".format(phys_check_fname_str), 2)
    outfile.write("use {}, only: std_name_len".format(phys_check_fname_str), 2)


    #Loop over use string list:
    for use_str in use_list:
        #Add required, registered fortran module use statements:
        outfile.write(use_str, 2)
    # end for
    #Write dummy variable declarations:
    outfile.write("", 0)
    outfile.write("! Dummy arguments", 2)
    outfile.write("type(file_desc_t), intent(inout) :: file", 2)
    outfile.write("character(len=SHR_KIND_CS)       :: suite_names(:) !Names of CCPP suites", 2)
    outfile.write("integer,           intent(in)    :: timestep", 2)
    outfile.write("logical,  intent(in),  optional  :: read_initialized_variables", 2)
    outfile.write("", 0)

    #Write local variable declarations:
    outfile.write("!Local variables:", 2)
    outfile.write("", 0)
    outfile.write("!Character array containing all CCPP-required variable standard names:", 2)
    outfile.write("character(len=std_name_len), allocatable :: ccpp_required_data(:)", 2)
    outfile.write("", 0)
    outfile.write("!Strings which store names of any missing or non-initialized vars:", 2)
    outfile.write("character(len=SHR_KIND_CL) :: missing_required_vars", 2)
    outfile.write("character(len=SHR_KIND_CL) :: protected_non_init_vars", 2)
    outfile.write("character(len=SHR_KIND_CL) :: missing_input_names", 2)
    outfile.write("", 0)
    outfile.write("character(len=SHR_KIND_CX) :: errmsg    !CCPP framework error message", 2)
    outfile.write("integer                    :: errflg    !CCPP framework error flag", 2)
    outfile.write("integer                    :: name_idx  !Input variable array index", 2)
    outfile.write("integer                    :: req_idx   !Required variable array index", 2)
    outfile.write("integer                    :: suite_idx !Suite array index", 2)
    outfile.write("character(len=2)           :: sep  = '' !String separator used to print error messages", 2)
    outfile.write("character(len=2)           :: sep2 = '' !String separator used to print error messages", 2)
    outfile.write("character(len=2)           :: sep3 = '' !String separator used to print error messages", 2)
    outfile.write("", 0)
    outfile.write("!Logical to default optional argument to False:", 2)
    outfile.write("logical                    :: use_init_variables", 2)
    outfile.write("", 0)

    #Initialize variables:
    outfile.write("!Initalize missing and non-initialized variables strings:", 2)
    outfile.write("missing_required_vars = ' '", 2)
    outfile.write("protected_non_init_vars = ' '", 2)
    outfile.write("missing_input_names   = ' '", 2)
    outfile.write("", 0)
    outfile.write("!Initialize use_init_variables based on whether it was input to function:", 2)
    outfile.write("if (present(read_initialized_variables)) then", 2)
    outfile.write("use_init_variables = read_initialized_variables", 3)
    outfile.write("else", 2)
    outfile.write("use_init_variables = .false.", 3)
    outfile.write("end if", 2)
    outfile.write("", 0)

    #Loop over physics suites:
    outfile.write("!Loop over CCPP physics/chemistry suites:", 2)
    outfile.write("do suite_idx = 1, size(suite_names, 1)", 2)
    outfile.write("", 0)

    #Determine physics suite required variables:
    outfile.write("!Search for all needed CCPP input variables,", 3)
    outfile.write("!so that they can bx e read from input file if need be:", 3)
    outfile.write("call ccpp_physics_suite_variables(suite_names(suite_idx), ccpp_required_data, &", 3)
    outfile.write("errmsg, errflg, input_vars=.true., output_vars=.false.)", 4)
    outfile.write("", 0)

    #Loop over required variables:
    outfile.write("!Loop over all required variables as specified by CCPP suite:", 3)
    outfile.write("do req_idx = 1, size(ccpp_required_data, 1)", 3)
    outfile.write("", 0)

    #Call input name search function:
    outfile.write("!Find IC file input name array index for required variable:", 4)
    outfile.write("name_idx = find_input_name_idx(ccpp_required_data(req_idx), use_init_variables)", 4)

    #Start select-case statement:
    outfile.write("", 0)
    outfile.write("!Check for special index values:", 4)
    outfile.write("select case (name_idx)", 4)
    outfile.write("", 0)

    #Skip already initialized variables:
    outfile.write("case (init_mark_idx)", 5)
    outfile.write("", 0)
    outfile.write("!If variable is already initialized, then do nothing.", 6)
    outfile.write("", 0)

    #Generate error message if required variable isn't found:
    outfile.write("case (no_exist_idx)", 5)
    outfile.write("", 0)
    outfile.write("!If an index was never found, then save variable name and check the rest", 6)
    outfile.write("!of the variables, after which the model simulation will end:", 6)
    outfile.write("missing_required_vars(len_trim(missing_required_vars)+1:) = &", 6)
    outfile.write(" trim(sep)//trim(ccpp_required_data(req_idx))", 7)
    outfile.write("", 0)
    outfile.write("!Update character separator to now include comma:", 6)
    outfile.write("sep = ', '", 6)
    outfile.write("", 0)

    #Generate error message if required variable is protected but not initialized:
    outfile.write("case (prot_no_init_idx)", 5)
    outfile.write("", 0)
    outfile.write("!If an index was found for a protected variable, but that variable", 6)
    outfile.write("!was never marked as initialized, then save the variable name and check", 6)
    outfile.write("!the rest of the variables, after which the model simulation will end:", 6)
    outfile.write("protected_non_init_vars(len_trim(protected_non_init_vars)+1:) = &", 6)
    outfile.write(" trim(sep2)//trim(ccpp_required_data(req_idx))", 7)
    outfile.write("", 0)
    outfile.write("!Update character separator to now include comma:", 6)
    outfile.write("sep2 = ', '", 6)
    outfile.write("", 0)

    #start default case steps:
    outfile.write("case default", 5)
    outfile.write("", 0)

    #Generate error message if required variable contains no input names
    #(i.e. the <ic_file_input_names> registry tag is missing):
    outfile.write("!Check that the input variable names aren't blank.", 6)
    outfile.write("!If so, then save variable name and check the rest of the", 6)
    outfile.write("!variables, after which the model simulation will end:", 6)
    outfile.write("if (len_trim(input_var_names(1,name_idx)) == 0) then", 6)
    outfile.write("missing_input_names(len_trim(missing_input_names)+1:) = &", 7)
    outfile.write(" trim(sep3)//trim(ccpp_required_data(req_idx))", 8)
    outfile.write("", 0)
    outfile.write("!Update character separator to now include comma:", 7)
    outfile.write("sep3 = ', '", 7)
    outfile.write("", 0)
    outfile.write("!Continue on with variable loop:", 7)
    outfile.write("cycle", 7)
    outfile.write("end if", 6)
    outfile.write("", 0)

    #Generate "read_field" calls:
    outfile.write("!Read variable from IC file:", 6)
    outfile.write("", 0)
    outfile.write("select case (trim(phys_var_stdnames(name_idx)))", 6)
    for case_call, read_call in call_string_dict.items():
        outfile.write(case_call, 7)
        outfile.write(read_call, 8)
        outfile.write("", 0)
    outfile.write("end select !read variables", 6)
    # end select

    #End select case and required variables loop:
    outfile.write("end select !special indices", 5)
    outfile.write("", 0)
    outfile.write("end do !Suite-required variables", 3)
    outfile.write("", 0)

    #Generate endrun statement for missing variables:
    outfile.write("!End simulation if there are missing input", 3)
    outfile.write("!variables that are required:", 3)
    outfile.write("if (len_trim(missing_required_vars) > 0) then", 3)
    outfile.write('call endrun("Required variables missing from registered list of input variables: "//&', 4)
    outfile.write("trim(missing_required_vars))", 5)
    outfile.write("end if", 3)
    outfile.write("", 0)

    #Generate endrun statement for non-initialized protected variables:
    outfile.write("!End simulation if there are protected input", 3)
    outfile.write("!variables that are not initialized:", 3)
    outfile.write("if (len_trim(protected_non_init_vars) > 0) then", 3)
    outfile.write('call endrun("Required, protected input variables are not initialized: "//&', 4)
    outfile.write("trim(protected_non_init_vars))", 5)
    outfile.write("end if", 3)
    outfile.write("", 0)

    #Generate endrun statement for missing input names:
    outfile.write("!End simulation if there are variables that", 3)
    outfile.write("!have no input names:", 3)
    outfile.write("if (len_trim(missing_input_names) > 0) then", 3)
    outfile.write("call endrun('Required variables missing a list of input names (<ic_file_input_names>): '//trim(missing_input_names))", 5)
    outfile.write("end if", 3)
    outfile.write("", 0)

    #Deallocate ccpp_required_data array:
    outfile.write("!Deallocate required variables array for use in next suite:", 3)
    outfile.write("deallocate(ccpp_required_data)", 3)
    outfile.write("", 0)

    #End suite loop:
    outfile.write(" end do !CCPP suites", 2)
    outfile.write("", 0)

    #End subroutine:
    outfile.write("end subroutine physics_read_data", 1)

    #----------------------------

#####

def write_phys_check_subroutine(outfile, fort_data, phys_check_fname_str):

    """
    Write the "physics_check_data" subroutine, which
    is used to check the physics variables against
    an optionally input check file by reading
    in the values from the check file and comparing
    the values to the variables
    """

    #Construct dictionary of modules
    #and variables in use statements:
    #--------------------------------
    #Create new (empty) dictionary to store use statements:
    use_vars_write_dict = OrderedDict()

    #Loop over all variable standard names:
    for var_stdname in fort_data.standard_names:

        #Check if variable is in use dictionary:
        if var_stdname in fort_data.use_dict:

            #If so, then extract variable use statement list:
            var_use_info = fort_data.use_dict[var_stdname]

            #Extract module name (always first item in list):
            use_mod_name = var_use_info[0]

            #Check if module name is already in dictionary:
            if use_mod_name in use_vars_write_dict:
                #If so, then loop over variable use list:
                for use_var in var_use_info[1:]:
                    #If variable doesn't already exist in list, then add it:
                    if use_var not in use_vars_write_dict[use_mod_name]:
                        use_vars_write_dict[use_mod_name].append(use_var)
            else:
                #Add module name as new key to dictionary:
                use_vars_write_dict[use_mod_name] = var_use_info[1:]
    #--------------------------------

    #Create actual fortran use statements:
    #--------------------------------
    #Create new (empty) list to store use statements:
    use_list = list()

    #Loop over use statement modules:
    for use_mod in use_vars_write_dict:

        #Loop over use statement variables:
        for use_var in use_vars_write_dict[use_mod]:

            #create new use string:
            use_str = "use {},        only: {}".format(use_mod, use_var)

            #Add to "use statement" list:
            use_list.append(use_str)
    #-----------------------------

    #Create fortran "check_field" calls:
    #---------------------------------

    #Create new (empty) dictionary to store "check_field" calls:
    call_string_dict = OrderedDict()

    #Loop over all variable standard names:
    for var_stdname in fort_data.standard_names:

        #Check if variable is in fortran call dictionary:
        if var_stdname in fort_data.call_dict:

            #Set "if-statement" call string:
            call_string_key = "case ('{}')".format(var_stdname)

            #Extract vertical level variable:
            levnm = fort_data.vert_dict[var_stdname]

            #Set "check_field" call string:
            if levnm is not None:
                call_str = "call check_field(file, input_var_names(:,name_idx), '{}'," + \
                                  " timestep, {}, '{}', min_difference, min_relative_value, is_first)"
                call_string_val = call_str.format(\
                                  levnm, fort_data.call_dict[var_stdname], var_stdname.strip())
            else:
                call_str = "call check_field(file, input_var_names(:,name_idx)," + \
                                  " timestep, {}, '{}', min_difference, min_relative_value, is_first)"
                call_string_val = call_str.format(fort_data.call_dict[var_stdname], var_stdname.strip())
            #Add strings to dictionary:
            call_string_dict[call_string_key] = call_string_val

    #---------------------------------

    #Write actual subroutine code:
    #----------------------------

    #Add subroutine header:
    outfile.write("subroutine physics_check_data(file_name, suite_names, timestep, min_difference, min_relative_value)", 1)

    #Add use statements:
    outfile.write("use pio,                  only: file_desc_t, pio_nowrite", 2)
    outfile.write("use cam_abortutils,       only: endrun", 2)
    outfile.write("use shr_kind_mod,         only: SHR_KIND_CS, SHR_KIND_CL, SHR_KIND_CX", 2)
    outfile.write("use physics_data,         only: check_field, find_input_name_idx", 2)
    outfile.write("use physics_data,         only: no_exist_idx, init_mark_idx, prot_no_init_idx", 2)
    outfile.write("use cam_ccpp_cap,         only: ccpp_physics_suite_variables", 2)
    outfile.write("use ccpp_kinds,           only: kind_phys", 2)
    outfile.write("use cam_logfile,          only: iulog", 2)
    outfile.write("use spmd_utils,           only: masterproc", 2)
    outfile.write("use phys_vars_init_check, only: is_read_from_file", 2)
    outfile.write("use ioFileMod,            only: cam_get_file", 2)
    outfile.write("use cam_pio_utils,        only: cam_pio_openfile, cam_pio_closefile", 2)

    outfile.write("use {}, only: phys_var_stdnames, input_var_names".format(phys_check_fname_str), 2)
    outfile.write("use {}, only: std_name_len".format(phys_check_fname_str), 2)


    #Loop over use string list:
    for use_str in use_list:
        #Add required, registered fortran module use statements:
        outfile.write(use_str, 2)

    #Write dummy variable declarations:
    outfile.write("", 0)
    outfile.write("! Dummy arguments", 2)
    outfile.write("character(len=SHR_KIND_CL), intent(in) :: file_name", 2)
    outfile.write("character(len=SHR_KIND_CS)             :: suite_names(:) !Names of CCPP suites", 2)
    outfile.write("integer,                    intent(in) :: timestep", 2)
    outfile.write("real(kind_phys),            intent(in) :: min_difference", 2)
    outfile.write("real(kind_phys),            intent(in) :: min_relative_value", 2)
    outfile.write("", 0)

    #Write local variable declarations:
    outfile.write("!Local variables:", 2)
    outfile.write("", 0)
    outfile.write("!Character array containing all CCPP-required variable standard names:", 2)
    outfile.write("character(len=std_name_len), allocatable :: ccpp_required_data(:)", 2)
    outfile.write("", 0)
    outfile.write("!Strings which store names of any missing or non-initialized vars:", 2)
    outfile.write("character(len=SHR_KIND_CL) :: missing_required_vars", 2)
    outfile.write("character(len=SHR_KIND_CL) :: protected_non_init_vars", 2)
    outfile.write("character(len=SHR_KIND_CL) :: missing_input_names", 2)
    outfile.write("", 0)
    outfile.write("character(len=SHR_KIND_CX) :: errmsg    !CCPP framework error message", 2)
    outfile.write("integer                    :: errflg    !CCPP framework error flag", 2)
    outfile.write("integer                    :: name_idx  !Input variable array index", 2)
    outfile.write("integer                    :: req_idx   !Required variable array index", 2)
    outfile.write("integer                    :: suite_idx !Suite array index", 2)
    outfile.write("character(len=SHR_KIND_CL) :: ncdata_check_loc", 2)
    outfile.write("type(file_desc_t), pointer :: file", 2)
    outfile.write("logical                    :: file_found", 2)
    outfile.write("logical                    :: is_first", 2)
    outfile.write("", 0)

    #Initialize variables:
    outfile.write("!Initalize missing and non-initialized variables strings:", 2)
    outfile.write("missing_required_vars = ' '", 2)
    outfile.write("protected_non_init_vars = ' '", 2)
    outfile.write("missing_input_names   = ' '", 2)
    outfile.write("nullify(file)", 2)
    outfile.write("is_first = .true.", 2)
    outfile.write("", 0)

    #Begin check data log:
    outfile.write("if (masterproc) then", 2)
    outfile.write("write(iulog,*) ''", 3)
    outfile.write("write(iulog,*) '********** Physics Check Data Results **********'", 3)
    outfile.write("write(iulog,*) ''", 3)
    outfile.write("write(iulog,*) 'TIMESTEP: ', timestep", 3)
    outfile.write("end if", 2)

    #Open check file:
    outfile.write("if (file_name == 'UNSET') then", 2)
    outfile.write("write(iulog,*) 'WARNING: Namelist variable ncdata_check is UNSET.', ' Model will run, but physics check data will not be printed'", 3)
    outfile.write("return", 3)
    outfile.write("end if", 2)
    outfile.write("!Open check file:", 2)
    outfile.write("call cam_get_file(file_name, ncdata_check_loc, iflag=1, lexist=file_found, log_info=.false.)", 2)
    outfile.write("if (.not. file_found) then", 2)
    outfile.write("write(iulog,*) 'WARNING: Check file '//file_name//' not found. Model will run, but physics check data will not be printed'", 3)
    outfile.write("return", 3)
    outfile.write("end if", 2)
    outfile.write("allocate(file)", 2)
    outfile.write("call cam_pio_openfile(file, ncdata_check_loc, pio_nowrite, log_info=.false.)", 2)

    #Loop over physics suites:
    outfile.write("!Loop over CCPP physics/chemistry suites:", 2)
    outfile.write("do suite_idx = 1, size(suite_names, 1)", 2)
    outfile.write("", 0)

    #Determine physics suite required variables:
    outfile.write("!Search for all needed CCPP input variables,", 3)
    outfile.write("!so that they can be read from input file if need be:", 3)
    outfile.write("call ccpp_physics_suite_variables(suite_names(suite_idx), ccpp_required_data, &", 3)
    outfile.write("errmsg, errflg, input_vars=.true., output_vars=.false.)", 4)
    outfile.write("", 0)

    #Loop over required variables:
    outfile.write("!Loop over all required variables as specified by CCPP suite:", 3)
    outfile.write("do req_idx = 1, size(ccpp_required_data, 1)", 3)
    outfile.write("", 0)

    #Call input name search function:
    outfile.write("!Find IC file input name array index for required variable:", 4)
    outfile.write("if (.not. is_read_from_file(ccpp_required_data(req_idx), name_idx)) then", 4)
    outfile.write("continue", 5)
    outfile.write("end if", 4)

    #Generate "check_field" calls:
    outfile.write("!Check variable vs input check file:", 4)
    outfile.write("", 0)
    outfile.write("select case (trim(phys_var_stdnames(name_idx)))", 4)
    for case_call, read_call in call_string_dict.items():
        outfile.write(case_call, 5)
        outfile.write(read_call, 6)
        outfile.write("", 0)
    outfile.write("end select !check variables", 4)

    #End select case and required variables loop:
    outfile.write("end do !Suite-required variables", 3)
    outfile.write("", 0)

    #Deallocate ccpp_required_data array:
    outfile.write("!Deallocate required variables array for use in next suite:", 3)
    outfile.write("deallocate(ccpp_required_data)", 3)
    outfile.write("", 0)

    #End suite loop:
    outfile.write(" end do !CCPP suites", 2)
    outfile.write("", 0)

    #Close check file
    outfile.write("!Close check file:", 2)
    outfile.write("call cam_pio_closefile(file)", 2)
    outfile.write("deallocate(file)", 2)
    outfile.write("nullify(file)", 2)

    #Check if no differences were found
    outfile.write("if (is_first) then", 2)
    outfile.write("write(iulog,*) ''", 3)
    outfile.write("write(iulog,*) 'No differences found!'", 3)
    outfile.write("end if", 2)

    #End check data log:
    outfile.write("write(iulog,*) ''", 2)
    outfile.write("write(iulog,*) '********** End Physics Check Data Results **********'", 2)
    outfile.write("write(iulog,*) ''", 2)

    #End subroutine:
    outfile.write("end subroutine physics_check_data", 1)

    #----------------------------

#############
# End of file
#############
