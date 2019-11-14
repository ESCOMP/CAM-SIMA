"""
Location of CAM's "config" python data structure, which
is used to pass any needed data between the relevant CAM 
"cime_config" scripts, and which stores all meta-data and
descriptions associated with the CAM configuration of a
CIME case.
"""

#---------------------------------------
#Import generic python libraries/modules:
#---------------------------------------
import re 

####################################################################################
#Error-handling classes
####################################################################################

class Cam_config_val_error(ValueError):
    """Class used to handle CAM config value errors (e.g., log user errors without backtrace)"""
    def __init__(self, message):
        super(Cam_config_val_error, self).__init__(message)

####################################################################################

class Cam_config_type_error(TypeError):
    """Class used to handle CAM config type errors (e.g., log user errors without  backtrace)"""
    def __init_(self, message):
        super(Cam_config_type_error, self).__init__(message)

####################################################################################
#CAM configure option classes
####################################################################################

class Config_gen:

    """
    Generic configuration class used to
    store CAM configuration names and 
    descriptions.

    Inputs to initalize class are:
    name -> Name of new CAM configure option
    desc -> Text description of CAM configure option
    """

    def __init__(self, name, desc):

        #Check that "name" is a string:
        if not isinstance(name, str):
            raise Cam_config_type_error("ERROR:  Configuration variable name {} must be a string, not {}".format(name, type(name)))
             
        #Check that "desc" is a string:
        if not isinstance(desc, str):
            raise Cam_config_type_error("ERROR:  Configuration variable {} must have a string-type description, not {}".format(name, type(desc)))

        #Add name and description to object:
        self.__name = name
        self.__desc = desc

    #++++++++++++++++++++++++

    #Create properties needed to return name and description without underscores:
    @property
    def name(self):
        """Return the name of this config object"""
        return self.__name

    @property
    def desc(self):
        """Return the description of this config object"""
        return self.__desc

####################################################################################

class Config_integer(Config_gen):

    """
    Configuration class used to store
    integer-based CAM configuration 
    options.

    Inputs to initalize class are:
    val                   -> Integer value for CAM configure option
    valid_vals (optional) -> Range or list of valid CAM configure option values (default is None)
    """

    def __init__(self, name, desc, val, valid_vals=None):

        #Add generic attributes:
        Config_gen.__init__(self, name, desc)

        #Check that "valid_vals" is either "None", a list, or a tuple:
        if valid_vals is not None:
            if not isinstance(valid_vals, (list,tuple)):
                raise Cam_config_type_error("ERROR:  The valid values for variable {} must either be None, a list, or a tuple, not {}".format(name, type(valid_vals)))

            #If list or tuple, check that all entries are either "None" or integers:
            for n in valid_vals:
                if n is not None and not isinstance(n,int):
                    raise Cam_config_type_error("ERROR:  Valid value for variable {} must be either None or an integer.  Currently it is {}".format(name, type(n)))

        #Next, check that provided value is "valid" based on the valid values list or tuple:
        self.check_value(val,valid_vals)

        #If everything is ok, then add inputs to object:
        self.__value      = val
        self.__valid_vals = valid_vals        

    #++++++++++++++++++++++++

    #Create properties needed to return name and description without underscores:
    @property
    def value(self):
        """Return the value of this config object"""
        return self.__value

    @property
    def valid_vals(self):
        """Return the valid values of this config object"""
        return self.__valid_vals

    #++++++++++++++++++++++++

    def check_value(self,val,valid_vals):

        """
        Checks input/given value to make sure
        it is valid according to the
        object's valid values list or tuple.
        
        If a tuple, then assume the two
        valid values in the tuple represent a range,
        and check that the given value is
        in-between that range. 

        If a list, then assume the given value
        must match at least one of the valid values
        included in that list.  
        """

        #Only check the given value if valid_vals is not "None":
        if valid_vals is not None:

            #Check if valid values is a tuple:
            if isinstance(valid_vals, tuple):

                #Check that length of valid values tuple is 2:
                if len(valid_vals) != 2:
                    raise Cam_config_val_error("Error: Valid valudes tuple for variable {} must have two elements, not {} elements".format(self.name, len(valid_vals)))    

                #If first valid value is "None", then just check that given value is less than second 
                #valid value, and that second value is an integer:
                if valid_vals[0] is None:
                    if valid_vals[1] is None:
                        raise Cam_config_val_error("Error:  Valid values tuple for variable {} must contain at least one integer".format(self.name))
                    elif val > valid_vals[1]:
                        raise Cam_config_val_error("Error:  Value {} provided for variable {} is greater than max valid value {}".format(val, self.name, valid_vals[1]))

                #Next check if second value is "None".  If so, then just check that given value is greater 
                #than first valid value:
                elif valid_vals[1] is None:
                      if val < valid_vals[0]:
                          raise Cam_config_val_error("Error:  Value {} provided for variable {} is less than minimum valid value {}".format(val, self.name, valid_vals[0]))

                #If both valid values are integers, then check that given value is between both valid values:
                else:
                      if val < valid_vals[0] or val > valid_vals[1]:
                          raise Cam_config_val_error("Error:  Value {} provided for variable {} is outside valid value range {}".format(val, self.name, valid_vals))

            #If valid_vals is a list, then just check that the given value 
            #matches one of the valid values in the list:
            else:
                if not any(n == val for n in valid_vals):
                    raise Cam_config_val_error("ERROR:  Value {} provided for variable {} does not match any of the valid values: {}".format(val, self.name, valid_vals))
            
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

    Inputs to initalize class are:
    val                   -> String value for CAM configure option
    valid_vals (optional) -> List of valid CAM configure option values (default is None)
    """

    def __init__(self, name, desc, val, valid_vals=None):

        #Add generic attributes:
        Config_gen.__init__(self, name, desc)    

        #Determine regular rexpression type instance:
        regex_type = type(re.compile(r" ")) 

        #Check if Valid_vals is not None:
        if valid_vals is not None:
            #If not None, check if valid_vals is either a list or a regular expression (regex) object:
            if not isinstance(valid_vals, (list, regex_type)):
                raise Cam_config_type_error("ERROR:  The valid values for variable {} must either be None, a list, or a regex object, not {}".format(name, type(valid_vals)))

            #If list, check that every entry is a string:
            if isinstance(valid_vals, list):
                if not all(isinstance(n, str) for n in valid_vals):
                    raise Cam_config_type_error("ERROR:  All valid values for variable {} must be strings.".format(name))

        #Next, check that provided value is "valid" based on the valid values list or tuple:
        self.check_value(val,valid_vals)

        #If everything is ok, then add inputs to object:
        self.__value      = val
        self.__valid_vals = valid_vals

    #++++++++++++++++++++++++

    #Create properties needed to return name and description without underscores:
    @property
    def value(self):
        """Return the value of this config object"""
        return self.__value

    @property
    def valid_vals(self):
        """Return the valid values of this config object"""
        return self.__valid_vals
        

    #++++++++++++++++++++++++

    def check_value(self,val,valid_vals):
        
        """
        Checks input/given value to make sure
        it is valid according to the
        object's valid values list or
        regular expression.

        If a list, then assume the given value
        must match at least one of the valid values
        included in that list.

        If a compiled regular expression, then
        assume the value must match the regular
        expression.. 
        """

        #Only check the given value if valid_vals is not None:
        if valid_vals is not None:

            #If a list, then check that the given value 
            #matches one of the valid values in the list:
            if isinstance(valid_vals, list):
                if not any(n == val for n in valid_vals):
                    raise Cam_config_val_error("ERROR:  Value {} provided for variable {} does not match any of the valid values: {}".format(val, self.name, valid_vals))

            else:
                #If a regular expression object, then check that
                #value is matched by the expression:
                if valid_vals.match(val) is None:
                    raise Cam_config_val_error("ERROR:  Value {} provided for variable {} does not match the valid regular expression:".format(val, self.name))

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

        #-----------------------------------------------

        #Check if "-dyn" is specifed in user_config_opts:
        if any("-dyn" == opt for opt in user_config_opts):
            #If so, then set variable to value specified:
            dyn_idx = user_config_opts.index("-dyn")
            user_dyn_opt = user_config_opts[dyn_idx+1]

            #Also, check if dyn option is set to "none":
            if user_dyn_opt == "none":
                #If so, then set the atmospheric grid to "null":
                atm_grid  = "null" 
                case_nlev = "null" 
                case_nx   = "null"
                case_ny   = "null"

        else:
            #If not, then just set variable to None:
            user_dyn_opt = None

        #-----------------------------------------------

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
        self.create_config("nlev", nlev_desc, nlev)

        # Add number of latitudes in grid to configure object:
        nlat_desc = "Number of unique latitude points in rectangular lat/lon grid."
        self.create_config("nlat", nlat_desc, case_ny)

        # Add number of longitudes in grid to configure object:
        nlon_desc = "Number of unique longitude points in rectangular lat/lon grid."
        self.create_config("nlon", nlon_desc, case_nx)

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
        dyn_valid_vals = ["eul", "fv", "se", "fv3", "mpas", "none"]

        #Create regex expressions to search for the different dynamics grids:
        eul_grid_re = re.compile(r"T[0-9]+")                      # Eulerian dy-core
        fv_grid_re = re.compile(r"[0-9][0-9.]*x[0-9][0-9.]*")     # FV dy-core
        se_grid_re = re.compile(r"ne[0-9]+np[1-8](.*)(pg[1-9])?") # SE dy-core
        fv3_grid_re = re.compile(r"C[0-9]+")                      # FV3 dy-core
        mpas_grid_re = re.compile(r"mpasa[0-9]+")                 # MPAS dy-core (not totally sure about this pattern) 

        #Check if specified grid matches any of the pre-defined grid options:
        if fv_grid_re.match(atm_grid) is not None:
            dyn = "fv"
            hgrid_valid_vals = fv_grid_re
        elif se_grid_re.match(atm_grid) is not None:
            dyn = "se"
            hgrid_valid_vals = se_grid_re
        elif fv3_grid_re.match(atm_grid) is not None:
            dyn = "fv3"
            hgrid_valid_vals = fv3_grid_re
        elif mpas_grid_re.match(atm_grid) is not None:
            dyn = "mpas" 
            hgrid_valid_vals = mpas_grid_re  
        elif eul_grid_re.match(atm_grid) is not None:   
            dyn = "eul"
            hgrid_valid_vals = eul_grid_re

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
            hgrid_valid_vals = None
        else:
            raise Cam_config_val_error("ERROR:  The specified CAM horizontal grid {} does not match any expected value".format(atm_grid))

        #If user-specified dynamics option is present, check that it matches the grid-derived value:
        if user_dyn_opt is not None and user_dyn_opt != dyn:
            raise Cam_config_val_error("ERROR: User-specified dynamics option {} does not match dycore expected from CIME grid: {}".format(user_dyn_opt, dyn))

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
                The ocean model being used.  Valid values include prognostic \n \
                ocean models (pop or mom), data ocean models (docn or dom), \n \
                a stub ocean (socn), and an aqua planet ocean (aquaplanet). \n \
                This doesn't impact how the case is built, only how \n \
                attributes are matched when searching for namelist defaults."

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

            # Update the case variable CAM_CPPDEFS with the above CPP definitions:
            case.set_value("CAM_CPPDEFS", cam_cppdefs)

            # Write the case variables to the case's XML files:
            case.flush()

    #++++++++++++++++++++
    #Config_CAM functions:
    #++++++++++++++++++++

    def create_config(self, name, desc, val, valid_vals=None):

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
        logger.debug("#{}".format(obj.desc))
        logger.debug("{} = {}".format(obj.name,obj.value))

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

#Call testing routine, if script is run directly
if __name__ == "__main__":

    #import python "sys" module:
    import sys

    #Add "test" directory to python's path:
    sys.path.append("test")

    #Import cam_config unit-testing routine:
    from config_cam_tests import cam_config_test_routine

    #Call testing routine:
    cam_config_test_routine()

############
#End of file
############
 
  
