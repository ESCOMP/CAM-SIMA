"""
Location of CAM's "config" python data structure, which
is used to pass any needed data between the relevant CAM 
"cime_config" scripts, and which stores all meta-data and
descriptions associated with CAM's configuration.
"""

#---------------------------------------
#Import generic python libraries/modules:
#---------------------------------------
import re 

####################################################################################

class Config_gen:

    """
    Generic configuration class used to
    store CAM configuration names and 
    descriptions.
    """

    def __init__(self, name, desc):

        #Check that "name" is a string:
        if not isinstance(name, str):
            raise SystemExit("ERROR:  Configuration variable name {} must be a string, not {}".format(name, type(name)))

        #Check that "desc" is a string:
        if not isinstance(desc, str):
            raise SystemExit("ERROR:  Configuration variable {} must have a string-type description, not {}".format(name, type(desc)))

        #Add name and description to object:
        self.name = name
        self.desc = desc

####################################################################################

class Config_integer(Config_gen):

    """
    Configuration class used to store
    integer-based CAM configuration 
    options.
    """

    def __init__(self, name, desc, val, valid_vals):

        #Add generic attributes:
        Config_gen.__init__(self, name, desc)

        #Check that "valid_vals" is either "None", a list, or a tuple:
        if valid_vals is not None:
            if not isinstance(valid_vals, (list,tuple)):
                raise SystemExit("ERROR:  The valid values for variable {} must either be None, a list, or a tuple, not {}".format(name, type(valid_vals)))

            #If list or tuple, check that all entries are either "None" or integers:
            for n in valid_vals:
                if n is not None and not isinstance(n,int):
                    raise SystemExit("ERROR:  Valid value for variable {} must be either None or an integer.  Currently it is {}".format(name, type(n)))

        #Next, check that provided value is "valid" based on the valid values list or tuple:
        self.check_value(val,valid_vals)

        #If everything is ok, then add inputs to object:
        self.value      = val
        self.valid_vals = valid_vals        

    #++++++++++++++++++++++++

    def check_value(self,val,valid_vals):

        """
        Checks input/given value to make sure
        it is valid according to the
        object's valid values list or tuple.
        """

        #Only check the given value if valid_vals is not "None":
        if valid_vals is not None:

            #Check if valid values is a tuple:
            if isinstance(valid_vals, tuple):

                #Check that length of valid values tuple is 2:
                if len(valid_vals) != 2:                               
                    raise SystemExit("Error: Valid valudes tuple for variable {} must have two elements, not {} elements".format(self.name, len(valid_vals)))

                #If first valid value is "None", then just check that given value is less than second 
                #valid value, and that second value is an integer:
                if valid_vals[0] is None:
                    if valid_vals[1] is None:
                        raise SystemExit("Error:  Valid values tuple for variable {} must contain at least one integer".format(self.name))
                    elif val > valid_vals[1]:
                        raise SystemExit("Error:  Value {} provided for variable {} is greater than max valid value {}".format(val, self.name, valid_vals[1]))

                #Next check if second value is "None".  If so, then just check that given value is greater 
                #than first valid value:
                elif valid_vals[1] is None:
                      if val < valid_vals[0]:
                          raise SystemExit("Error:  Value {} provided for variable {} is less than minimum valid value {}".format(val, self.name, valid_vals[0]))

                #If both valid values are integers, then check that given value is between both valid values:
                else:
                      if val < valid_vals[0] or val > valid_vals[1]:
                          raise SystemExit("Error:  Value {} provided fro variable {} is outside valid value range {}".format(val, self.name, valid_vals))

            #If valid_vals is a list, then just check that the given value 
            #matches one of the valid values in the list:
            else:
                if not any(n == val for n in valid_vals):
                    raise SystemExit("ERROR:  Value {} provided for variable {} does not match any of the valid values: {}".format(val, self.name, valid_vals))
            
    #++++++++++++++++++++++++

    def set_value(self, val):

        """
        Set configure object's value to the one provided.
        """

        #First, check that the provided value is valid:
        self.check_value(val, self.valid_vals)

        #If ok, then set object's value to one provided:
        self.value = val

####################################################################################

class Config_string(Config_gen):

    """
    Configuration class used to store
    string-based CAM configuration
    options.
    """

    def __init__(self, name, desc, val, valid_vals):

        #Add generic attributes:
        Config_gen.__init__(self, name, desc)    

        #Check that "valid_vals" is either "None" or a list:
        if valid_vals is not None:
            if not isinstance(valid_vals, list):
                raise SystemExit("ERROR:  The valid values for variable {} must either be None or a list, not {}".format(name, type(valid_vals)))

            #If list, check that every entry is a string:
            if not all(isinstance(n, str) for n in valid_vals):
                raise SystemExit("ERROR:  All valid values for variable {} must be strings.".format(name))

        #Next, check that provided value is "valid" based on the valid values list or tuple:
        self.check_value(val,valid_vals)

        #If everything is ok, then add inputs to object:
        self.value      = val
        self.valid_vals = valid_vals

    #++++++++++++++++++++++++

    def check_value(self,val,valid_vals):
        
        """
        Checks input/given value to make sure
        it is valid according to the
        object's valid values list.
        """

        #Only check the given value if valid_vals is not "None":
        if valid_vals is not None:

            #If not None, then just check that the given value 
            #matches one of the valid values in the list:
            if not any(n == val for n in valid_vals):
                raise SystemExit("ERROR:  Value {} provided for variable {} does not match any of the valid values: {}".format(val, self.name, valid_vals))

    #++++++++++++++++++++++++

    def set_value(self, val):

        """
        Set configure object's value to the one provided.
        """    

        #First, check that the provided value is valid:
        self.check_value(val, self.valid_vals)

        #If ok, then set object's value to one provided:
        self.value = val

####################################################################################
#MAIN CAM CONFIGURE OBJECT:
####################################################################################

class Config_CAM:

    """
    Main CAM configuration object.
    """

    def __init__(self, case, logger, set_cppdefs=False):

        """
        Initalize configuration object
        and associated dictionary.
        """

        #Read in needed case variables:
        atm_grid = case.get_value("ATM_GRID")               # Atmosphere (CAM) grid 
        cam_config_opts = case.get_value("CAM_CONFIG_OPTS") # CAM configuration options 
        case_nx  = case.get_value("ATM_NX")                 # Number of x-dimension grid-points (longitudes)
        case_ny  = case.get_value("ATM_NY")                 # Number of y-dimension grid-points (latitudes)
        docn_mode = case.get_value("DOCN_MODE")             # Data-ocean (docn) mode 
        comp_ocn = case.get_value("COMP_OCN")               # CESM ocean component

        #If setting cpp-defs, then read in additional case variables:
        if set_cppdefs:
            mpilib = case.get_value("MPILIB")               # MPI library

        #The following translation is hard-wired for backwards compatibility
        #to support the differences between how the scripts specify the land grid
        #and how it is specified internally

        if atm_grid == 'T31':
            atm_grid = '48x96'
        if atm_grid == 'T42':
            atm_grid = '64x128'
        if atm_grid == 'T85':
            atm_grid = '128x256'
        if atm_grid == 'T341':
            atm_grid = '512x1024'
        if atm_grid == 'ne30pg3':
            atm_grid = 'ne30np4.pg3'  

        #Level information for CAM is part of the atm grid name - and must be stripped out
        case_nlev = ''
        match = re.match(r'(.+)z(\d+)', atm_grid)
        if match:
            atm_grid = match.groups()[0]
            case_nlev = match.groups()[1]

        #Save user options as list:
        user_config_opts = [opt for opt in cam_config_opts.split(" ") if opt]

        #------------------------------------------------
        # If "-dyn none" is specifed in CAM_CONFIG_OPTS, 
        # then set all grid variables to "null"
        #------------------------------------------------
        #Check if "-dyn" is specifed in user_config_opts:
        if any("-dyn" == opt for opt in user_config_opts):
            #If so, then check if dyn option is set to "none":
            dyn_idx = user_config_opts.index("-dyn")
            if user_config_opts[dyn_idx+1] == "none":
                #If so, then set the atmospheric grid to "null":
                atm_grid  = "null" 
                case_nlev = "null"
                case_nx   = "null"
                case_ny   = "null"
        #------------------------------------------------

        #Create empty dictonary:
        self.config = dict()

        #----------------------------------------
        # Set CAM grid variables (nlat,nlon,nlev):
        #----------------------------------------

        # Set number of vertical levels:
        if case_nlev:
            # Save variable for CPPDEFs:
            nlev = case_nlev
        else:
            # Save variable for CPPDEFs:
            nlev = 30

        # Add vertical levels to configure object:
        nlev_desc = "Number of vertical levels."
        self.create_config("nlev", nlev_desc, nlev, None)

        # Add number of latitudes in grid to configure object:
        nlat_desc = "Number of unique latitude points in rectangular lat/lon grid."
        self.create_config("nlat", nlat_desc, case_ny, None)

        # Add number of longitudes in grid to configure object:
        nlon_desc = "Number of unique longitude points in rectangular lat/lon grid."
        self.create_config("nlon", nlon_desc, case_nx, None)

        #------------------------
        # Set CAM physics columns
        #------------------------

        #Physics column per chunk:
        pcols_desc = "Maximum number of columns in a chunk (physics data structure)."
        self.create_config("pcols", pcols_desc, 16, (1,None))

        #Physics sub-columns:
        psubcols_desc = "Maximum number of sub-columns in a column (physics data structure)."
        self.create_config("psubcols", psubcols_desc, 1, (1,None))

        #-----------------------
        # Set CAM dynamical core
        #-----------------------

        dyn_desc = "Dynamics package, which is set by the horizontal grid specified."   
        dyn_valid_vals = ["eul", "fv", "se", "none"]

        #Finite-Volume (FV) horizontal grid values:
        fv_grid_vals = ["0.23x0.31", "0.47x0.63", "0.5x0.625", "0.9x1.25",
                            "1x1.25", "1.9x2.5", "2x2.5", "2.5x3.33",
                            "4x5", "10x15"]

        #Spectral Element (SE) horizontal grid values:
        se_grid_vals = ["ne5np4", "ne5np4.pg2", "ne5np4.pg3", "ne5np4.pg4",
                            "ne16np4", "ne16np4.pg3", "ne30np4", "ne30np4.pg2",
                            "ne30np4.pg3", "ne30np4.pg4", "ne60np4", "ne60np4.pg2",
                            "ne60np4.pg3", "ne60np4.pg4", "ne120np4", "ne120np4.pg2",
                            "ne120np4.pg3", "ne120np4.pg4", "ne240np4", "ne240np4.pg2",
                            "ne240np4.pg3", "ne0np4TESTONLY.ne5x4", "ne0np4CONUS.ne30x8"]

        #Eulerian (spectral) horizontal grid values:
        eul_grid_vals = ["512x1024", "256x512", "128x256", "64x128",
                            "48x96", "32x64", "8x16", "1x1"]

        #Check if "-dyn" is specifed in user_config_opts:
        if any("-dyn" == opt for opt in user_config_opts):
            #If so, then check if dyn option is set to "none":
            dyn_idx = user_config_opts.index("-dyn")
            if user_config_opts[dyn_idx+1] == "none":
                #If so, then set the atmospheric grid to "null":
                atm_grid = "null"    

        #Check if specified grid matches any of the pre-defined grid options:
        if any(atm_grid == grid for grid in fv_grid_vals):
            dyn = "fv"
            hgrid_valid_vals = fv_grid_vals
        elif any(atm_grid == grid for grid in se_grid_vals):
            dyn = "se"
            hgrid_valid_vals = se_grid_vals
        elif any(atm_grid == grid for grid in eul_grid_vals):   
            dyn = "eul"
            hgrid_valid_vals = eul_grid_vals

            #If using the Eulerian dycore, then add wavenumber variables as well:

            #wavenumber variable descriptions:
            trm_desc = "Maximum Fourier wavenumber."
            trn_desc = "Highest degree of the Legendre polynomials for m=0."
            trk_desc = "Highest degree of the associated Legendre polynomials."

            #Add variables to configure object:
            self.create_config("trm", trm_desc, 1, (1,None))
            self.create_config("trn", trn_desc, 1, (1,None))
            self.create_config("trk", trk_desc, 1, (1,None)) 

        elif atm_grid == "null":
            dyn = "none"
            hgrid_valid_vals = ["null"]
        else:
            total_hgrid_list = fv_grid_vals + se_grid_vals + eul_grid_vals + ["null"]
            raise SystemExit("ERROR:  The specified CAM horizontal grid {} does not match any of the valid values: \n {}".format(atm_grid, total_hgrid_list))

        #Add dynamical core to CAM config object:
        self.create_config("dyn", dyn_desc, dyn, dyn_valid_vals)

        #--------------------
        # Set horizontal grid
        #--------------------

        hgrid_desc = "Horizontal grid specifier."
        self.create_config("hgrid", hgrid_desc, atm_grid, hgrid_valid_vals)

        #--------------------
        # Set ocean component
        #--------------------

        ocn_valid_vals = ["docn", "dom", "som", "socn", "aquaplanet", "pop", "mom"]

        ocn_desc = "\n \
                Use data ocean model (docn or dom), stub ocean (socn), or aqua planet ocean \n \
                (aquaplanet) in cam build.  When built from the CESM scripts the value of \n \
                ocn may be set to pop or mom.  This doesn't impact how CAM is built, only how \n \
                attributes are matched when searching for namelist defaults.  If ocn is set \n \
                to som then the docn component is used."

        # The ocean component setting is only used by CAM to do attribute matching for
        # setting default tuning parameter values.  In SOM mode we want to use the same
        # tunings as the fully coupled B compset, so set the ocean component to pop in
        # that case.

        if docn_mode == 'som':
            self.create_config("ocn", ocn_desc, "pop", ocn_valid_vals)
        else:
            self.create_config("ocn", ocn_desc, comp_ocn, ocn_valid_vals)

        #--------------------------------------------------------
        # Print CAM configure settings and values to debug logger:
        #--------------------------------------------------------
        self.print_all(logger)

        #--------------------------------------
        #Set CAM CPP Definitions (if requested):
        #--------------------------------------
        if set_cppdefs:
            #Retrieve number of physics columns:
            pcols = self.get_value("pcols")

            #Retrieve number of physics subcolumns:
            psubcols = self.get_value("psubcols")

            #Set grid-related cppdefs:
            cam_cppdefs = \
            " -DPLON={} -DPLAT={} -DPLEV={} -DPCOLS={} -DPSUBCOLS={}".format(case_nx,case_ny,nlev,pcols,psubcols)

            #Set wavenumber-related cppdefs if using Eulerian dycore:
            if dyn == "eul":

                #Retrieve wavenumber values:
                trm = self.get_value("trm")
                trn = self.get_value("trn")
                trk = self.get_value("trk")

                #Add to cppdefs:
                cam_cppdefs += " -DPTRM={} -DPTRN={} -DPTRK={}".format(trm,trn,trk)  

            # Set system-related cppdefs:
            if mpilib == 'mpi-serial':
                cam_cppdefs += " -DSPMD"

            # Update the case variable CAM_CPPDEFS with the above CPP definitions:
            case.set_value("CAM_CPPDEFS", cam_cppdefs)

            # Write the case variables to the case's XML files:
            case.flush()

    #++++++++++++++++++++
    #Config_CAM functions:
    #++++++++++++++++++++

    def create_config(self, name, desc, val, valid_vals):

        """
        Create new CAM "configure" object, and add it
        to the configure dictionary.
        """

        #Check for given value type:
        if isinstance(val, int):
            #If integer, then call integer configure object:
            conf_obj = Config_integer(name, desc, val, valid_vals)

        elif isinstance(val, str):
            #If string, then call string configure object:   
            conf_obj = Config_string(name, desc, val, valid_vals)

        else:  
            #If neither an integer or a string, then throw an error:
            raise SystemExit("ERROR: The input value for new CAM config variable {} must be either an integer or string.".format(name))

        #Add object to dictionary:
        self.config[conf_obj.name] = conf_obj

    #++++++++++++++++++++++++

    def print_config(self, obj_name, logger):

        """
        Print the value and description of a specified
        CAM configure object to the CIME debug log.  
        """

        #Check that the given object name exists in the dictionary:
        if obj_name in self.config:
            obj = self.config[obj_name]
        else:
            raise SystemExit("ERROR: Invalid configuration name, {}".format(obj_name))

        #Print variable to logger: 
        logger.debug("CAM config variable: {}".format(obj.name))
        logger.debug("CAM config variable description: {}".format(obj.desc))
        logger.debug("CAM config variable value: {}".format(obj.value))

    #++++++++++++++++++++++++

    def print_all(self, logger):

        """
        Print the names, descriptions, and values of all CAM 
        configuration objects.
        """
   
        #Print separator:
        logger.debug("CAM configuration variables:")
        logger.debug("-----------------------------")

        #Loop over config dictionary values:
        for obj_name in self.config:
            #Print variable to logger:
            self.print_config(obj_name, logger)

        #Print additional separator (to help seperate this output from additional CIME output):
        logger.debug("-----------------------------")

    #++++++++++++++++++++++++

    def set_value(self, obj_name, val):

        """
        Set configure object's value to the value given.
        """

        #First, check that the given object name exists in the dictionary:
        if obj_name in self.config:
            obj = self.config[obj_name]
        else:
            raise SystemExit("ERROR: Invalid configuration name, {}".format(obj_name))

        #Next, check that the given value is either an integer or a string:
        if not isinstance(val, (int, str)):
            raise SystemExit("ERROR:  Value provided for variable {} must be either an integer or a string.  Currently it is type {}".format(name,type(val)))

        #Finally, set configure object's value to the value given:
        obj.set_value(val)

    #++++++++++++++++++++++++

    def get_value(self, obj_name):

        """
        return value for specified configure object.
        """  
 
        #First check that the given object name exists in the dictionary:
        if obj_name in self.config:
            obj = self.config[obj_name]
        else:
            raise SystemExit("ERROR: Invalid configuration name, {}".format(obj_name)) 

        #If it does, then return the object's value:
        return obj.value  

####################################################################################
#IGNORE EVERYTHING BELOW HERE UNLESS RUNNING TESTS ON CAM_CONFIG!
####################################################################################

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Main cam_config testing routine, used when script is run directly
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

def cam_config_test_routine():

    """
    Runs all CAM configuriation tests, to ensure
    that the scripts and error-handling methods
    are running properly.
    """

    #Initalize test counters:
    num_pass = 0
    num_fail = 0

    #++++++++++++++++++++++++++++++++
    #Print start of testing to screen:
    #++++++++++++++++++++++++++++++++

    print("\n")
    print("CAM config testing has started:")
    print("------------------------------")
    print("\n")

    #++++++++++++++++++++++++++++++++++++++++
    #Check generic config class initalization:
    #++++++++++++++++++++++++++++++++++++++++

    #set configuration options:
    name = "test"
    desc = "test object description"

    #Run Config_gen initalization check:
    num_pass = generate_config_gen_pass(name, desc, num_pass)

    #+++++++++++++++++++++++++++++++++
    #check generic name error-handling:
    #+++++++++++++++++++++++++++++++++

    #set name value:
    name = None

    #set error test message:
    err_type = "Name error"

    #Run Conf_gen error-handle check:
    num_pass, num_fail = generate_config_gen_error(name, desc, err_type, num_pass, num_fail)

    #++++++++++++++++++++++++++++++++++++++++
    #check generic description error-handling:
    #++++++++++++++++++++++++++++++++++++++++

    #set configuration options:
    name = "test"
    desc = None

    #set error test message:
    err_type = "Description error"

    #Run Conf_gen error-handle check:
    num_pass, num_fail = generate_config_gen_error(name, desc, err_type, num_pass, num_fail)

    #++++++++++++++++++++++++++++++++++++++++
    #check integer config class initalization:
    #++++++++++++++++++++++++++++++++++++++++

    #set configuration options:
    desc = "test object description"
    val  = 1
    valid_vals = None

    #Set valid value string to help with error-reporting:
    val_string = "None"

    #Run Config_integer initalization check:
    num_pass = generate_config_integer_pass(name, desc, val, valid_vals, val_string, num_pass)

    #+++++++++++++++++++++++++++++++++++++++++++++++
    #Check valid_vals None/list/tuple error-handling:
    #+++++++++++++++++++++++++++++++++++++++++++++++

    valid_vals = 5

    #set error test message:
    err_type = "valid values type error"

    #Run Conf_gen error-handle check:
    num_pass, num_fail = generate_config_integer_error(name, desc, val, valid_vals, 
                                                       err_type, num_pass, num_fail)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that valid values tuple also works for integer configration:
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    #set valued values:
    valid_vals = (0,2)

    #Set valid value string to help with error-reporting:
    val_string = "tuple"

    #Run Config_integer initalization check:
    num_pass = generate_config_integer_pass(name, desc, val, valid_vals, val_string, num_pass)

    #++++++++++++++++++++++++++++++++++++++++++
    #Check valid_vals all "None" error-handling:
    #++++++++++++++++++++++++++++++++++++++++++

    #set valued values:
    valid_vals = (None,None)

    #set error test message:
    err_type = "valid values all-None tuple error"

    #Run Conf_gen error-handle check:
    num_pass, num_fail = generate_config_integer_error(name, desc, val, valid_vals,
                                                       err_type, num_pass, num_fail)

    #++++++++++++++++++++++++++++++++++++++
    #Check valid_vals length error-handling:
    #++++++++++++++++++++++++++++++++++++++

    #set valued values:
    valid_vals = (0,)

    #set error test message:
    err_type = "Tuple length error"

    #Run Conf_gen error-handle check:
    num_pass, num_fail = generate_config_integer_error(name, desc, val, valid_vals,
                                                       err_type, num_pass, num_fail) 

    #+++++++++++++++++++++++++++++
    #Evalulate minimum value check
    #+++++++++++++++++++++++++++++

    #set valued values:
    valid_vals = (2,None)

    #set error test message:
    err_type = "Minimum integer value error"

    #Run Conf_gen error-handle check:
    num_pass, num_fail = generate_config_integer_error(name, desc, val, valid_vals,
                                                       err_type, num_pass, num_fail)
    
    #+++++++++++++++++++++++++++++
    #Evalulate maximum value check
    #+++++++++++++++++++++++++++++

    #Set valid values:
    valid_vals = (None,0)

    #set error test message:
    err_type = "Minimum integer value error"

    #Run Conf_gen error-handle check:
    num_pass, num_fail = generate_config_integer_error(name, desc, val, valid_vals, 
                                                       err_type, num_pass, num_fail)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that valid values list also works for integer configration:
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    valid_vals = [0,1,2]

    #Set valid value string to help with error-reporting:
    val_string = "list"

    #Run Config_integer initalization check:
    num_pass = generate_config_integer_pass(name, desc, val, valid_vals, val_string, num_pass)

    #+++++++++++++++++++++++++++++++++++++++++
    #Evalulate Config_integer list-match check
    #+++++++++++++++++++++++++++++++++++++++++

    #set valid values:
    valid_vals = [0,2,3]

    #set error test message:
    err_type = "valid values match error"

    #Run Conf_gen error-handle check:
    num_pass, num_fail = generate_config_integer_error(name, desc, val, valid_vals, 
                                                       err_type, num_pass, num_fail)

    #+++++++++++++++++++++++++++++++++++++++
    #check string config class initalization:
    #+++++++++++++++++++++++++++++++++++++++

    val  = "option a"
    valid_vals = None

    #Set valid value string to help with error-reporting:
    val_string = "None"

    #Run Config_integer initalization check:
    num_pass = generate_config_string_pass(name, desc, val, valid_vals, val_string, num_pass)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that valid values list also works for string configration:
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    val  = "option a"
    valid_vals = ["option a", "option b"]

    #Set valid value string to help with error-reporting:
    val_string = "list"

    #Run Config_integer initalization check:
    num_pass = generate_config_string_pass(name, desc, val, valid_vals, val_string, num_pass)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check Config_string valid_vals None/list error-handling:
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++

    valid_vals = 5

    #set error test message:
    err_type = "valid values type error"

    #Run Conf_gen error-handle check:
    num_pass, num_fail = generate_config_string_error(name, desc, val, valid_vals,
                                                       err_type, num_pass, num_fail)

    #++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check Config_string all-string list error-handling:
    #++++++++++++++++++++++++++++++++++++++++++++++++++

    valid_vals = ["option a", 5]

    #set error test message:
    err_type = "valid values non-string list error"

    #Run Conf_gen error-handle check:
    num_pass, num_fail = generate_config_string_error(name, desc, val, valid_vals,
                                                       err_type, num_pass, num_fail)

    #++++++++++++++++++++++++++++++++++++++++
    #Evalulate Config_string list-match check
    #++++++++++++++++++++++++++++++++++++++++

    valid_vals = ["option b", "option c"]

    #set error test message:
    err_type = "valid values match error"

    #Run Conf_gen error-handle check:
    num_pass, num_fail = generate_config_string_error(name, desc, val, valid_vals,
                                                       err_type, num_pass, num_fail)

    #++++++++++++++++++++
    #Print testing totals
    #++++++++++++++++++++

    print("\n")
    print("------------------------------")
    print("\n")
    print("Total number of test PASSES: {}".format(num_pass))
    print("Tootal number of test FAILS: {}".format(num_fail))

#+++++++++++++++++
#Testing functions:
#+++++++++++++++++

def generate_config_gen_pass(name, desc, num_pass):

    """
    Tests if a "Config_gen" object can
    be created successfully.
    """

    try:
        test_obj = Config_gen(name, desc)

        #Remove test object to avoid possible future conflicts:
        del test_obj

        #Add to passed tests list:
        print("Generic class configuration PASSED!")
        num_pass += 1

    except:
        print("Something un-exexpected happened creating generic config class!")
        raise

    #Return number of passed tests:
    return num_pass

#-------------------------------

def generate_config_integer_pass(name, desc, val, valid_vals, val_string, num_pass):

    """
    Tests if a "Config_integer" object can 
    be created successfully.
    """

    try:
        test_obj = Config_integer(name, desc, val, valid_vals)

        #Remove test object to avoid possible future conflicts:
        del test_obj

        #Add to passed tests list:
        print("\n")
        print("Integer class configuration check with valid values being {} PASSED!".format(val_string))
        num_pass += 1

    except:
        print("\n")
        print("Something un-exexpected happened creating integer config class with valid values being {}!".format(val_string))
        raise

    #Return number of passed tests:
    return num_pass

#-------------------------------

def generate_config_string_pass(name, desc, val, valid_vals, val_string, num_pass):

    """
    Tests if a "Config_string" object can
    be created successfully.
    """

    try:
        test_obj = Config_string(name, desc, val, valid_vals)

        #Remove test object to avoid possible future conflicts:
        del test_obj

        #Add to passed tests list:
        print("\n")
        print("Integer class configuration check with valid values being {} PASSED!".format(val_string))
        num_pass += 1

    except:
        print("\n")
        print("Something un-exexpected happened creating integer config class with valid values being {}!".format(val_string))
        raise

    #Return number of passed tests:
    return num_pass

#-------------------------------

def generate_config_gen_error(name, desc, err_type, num_pass, num_fail):

    """
    Tests if a "Config_gen" object can
    properly handle bad input values.
    """

    try:
        test_obj = Config_gen(name, desc)

        #Remove test object to avoid possible future conflicts:
        del test_obj

        #Add to failed tests list:
        print("{} check FAILED!".format(err_type))
        num_fail += 1

    except SystemExit as err_msg:
        print("\n")
        print(err_msg)

        #Add to passed tests list:
        print("{} check PASSED!".format(err_type))
        num_pass += 1

    except:
        print("\n")
        print("Something un-exexpected happened testing {} checking!".format(err_type))
        raise

    #Return number of passed and failed tests:
    return num_pass, num_fail

#------------------------------

def generate_config_integer_error(name, desc, val, valid_vals, err_type, num_pass, num_fail):

    """
    Tests if a "Config_integer" object 
    can properly handle bad input values.
    """

    try:
        test_obj = Config_integer(name, desc, val, valid_vals)

        #Remove test object to avoid possible future conflicts:
        del test_obj

        #Add to failed tests list:
        print("{} check for Config_integer FAILED!".format(err_type))
        num_fail += 1

    except SystemExit as err_msg:
        print("\n")
        print(err_msg)

        #Add to passed tests list:
        print("{} for Config_integer PASSED!".format(err_type))
        num_pass += 1

    except:
        print("\n")
        print("Something un-exexpected happened testing Config_integer {} checking!".format(err_type))
        raise

    #Return number of passed and failed tests:
    return num_pass, num_fail

#-----------------------------

def generate_config_string_error(name, desc, val, valid_vals, err_type, num_pass, num_fail):

    """
    Tests if a "Config_string" object 
    can properly handle bad input values.
    """

    try:
        test_obj = Config_string(name, desc, val, valid_vals)

        #Remove test object to avoid possible future conflicts:
        del test_obj

        #Add to failed tests list:
        print("{} check for Config_string FAILED!".format(err_type))
        num_fail += 1

    except SystemExit as err_msg:
        print("\n")
        print(err_msg)

        #Add to passed tests list:
        print("{} for Config_string PASSED!".format(err_type))
        num_pass += 1

    except:
        print("\n")
        print("Something un-exexpected happened testing Config_string {} checking!".format(err_type))
        raise

    #Return number of passed and failed tests:
    return num_pass, num_fail

#+++++++++++++++++++++++++++++++++++++++++++++++
#Call testing routine, if script is run directly
#+++++++++++++++++++++++++++++++++++++++++++++++

if __name__ == "__main__":

   #Add CIME scripts to python:
   

   #Call testing routine:
   cam_config_test_routine()

############
#End of file
############
 
  
