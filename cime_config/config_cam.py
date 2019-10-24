"""
Location of CAM's "configure" python data structure, which
is used to pass any needed data between the relevant CAM 
"cime_config" scripts, and which stores all meta-data and
descriptions associated with CAM's configuration.
"""

#Import CIME libraries/functions:
#-------------------------------
from CIME.utils import expect
from standard_script_setup import *
#--------------------------------

class Config_gen:

    """
    Generic configuration class used
    to store CAM configuration options.
    """

    def __init__(self, name, desc, val, valid_vals):

        #Check that "name" is a string:
        if not isinstance(name, str):
            raise SystemExit("ERROR: Configuration variable name {} must be a string, not {}".format(name, type(name)))

        #Check that "desc" is a string:
        if not isinstance(desc, str):
            raise SystemExit("ERROR: Configuration variable {} must have a string-type description, not {}".format(name, type(desc)))

        #Add name and description to object:
        self.name = name
        self.desc = desc

        #Check that the given value is either an integer or a string:
        if not isinstance(val, (int, str)):
            raise SystemExit("ERROR:  Value provided for variable {} must be either an integer or a string. Currently it is {}".format(name, type(val)))

        #Check that "valid_vals" is either "None" or a tuple:
        if valid_vals is not None:
            if not isinstance(valid_vals, tuple):
                raise SystemExit("ERROR: The valid values for variable {} must either be None or a tuple, not {}".format(name, type(valid_vals)))

            #If tuple, check that the first entry is either an integer or a string (all entries are examined later in "check_value"):
            if not isinstance(valid_vals[0], (int, str)):
                raise SystemExit("ERROR:  Valid value for variable {} must be either an integer or a string.  Currently it is {}".format(name, type(valid_vals[0])))

        #Check that provided value is "valid" based on the valid values tuple:
        self.check_value(val,valid_vals)

        #Add inputs to object:
        self.value      = val
        self.valid_vals = valid_vals

    #----------------

    def check_value(self,val,valid_vals):
        
        """
        Checks input/given value to make sure
        it is valid according to the
        object's valid values tuple.
        """

        #Only check the given value if valid_vals is not "None":
        if valid_vals is not None:

            #Determine the length of valid values tuple:
            valid_len = len(valid_vals)

            #Pull out first valid value:
            first_valid = valid_vals[0]

            #Check if there is only one valid value:
            if valid_len == 1:

                #If valid value is only one integer, then just check that the given value is greater than it:
                if isinstance(first_valid, int) and val < first_valid:
                    raise SystemExit("Error: Value {} provided for variable {} is less than minimum valid value {}".format(val, self.name, first_valid))

                #If valid value is only one string, then just check that the given value matches it:
                if isinstance(first_valid, str) and val != first_valid:
                    raise SystemExit("Error: Value {} provided for variable {} doesn't match valid value {}".format(val, self.name, first_valid))

            else:

                #Check that the valid values are all of the same type:
                if any(not isinstance(n, type(first_valid)) for n in valid_vals):
                    raise SystemExit("ERROR: Valid values for variable {} must all be of the same type. Instead valid values are: {}".format(self.name, valid_vals))

                #If valid values are two integers, then just check that the given value is in-between them:
                if valid_len == 2 and isinstance(first_valid, int) and (val < first_valid or val > valid_vals[1]):
                    raise SystemExit("ERROR: Value {} provided for variable {} is outside the valid range {} to {}".format(val, self.name, first_valid, valid_vals[1]))

                #Otherwise, just make sure that the given value matches one of the valid values:
                elif not any(n == val for n in valid_vals):
                    raise SystemExit("ERROR:  Value {} provided for variable {} does not match any of the valid values: {}".format(val, self.name, valid_vals))

    #-------------------

    def set_value(self, val):

        """
        Set configure object's value to the one provided.
        """    

        #First, check that the provided value is valid:
        self.check_value(val, self.valid_vals)

        #If ok, then set object's value to one provided:
        self.value = val

######################

class Config_CAM:

    """
    Main CAM configuration object.
    """

    def __init__(self):

        """
        Initalize configuration object
        and associated dictionary.
        """

        self.config = dict()

    #---------------

    def create_config(self, name, desc, val, valid_vals):

        """
        Create new CAM "configure" object, and add it
        to the configure dictionary.
        """

        #Create new CAM configure object:
        conf_obj = Config_gen(name, desc, val, valid_vals)

        #Add object to dictionary:
        self.config[conf_obj.name] = conf_obj

    #---------------

    def print_config(self, obj_name):

        """
        Print the value and description of a specified
        CAM configure object to the CIME debug log.  
        """

        #Check that the given object name exists in the dictionary:
        if obj_name in self.config:
            obj = self.config[obj_name]
        else:
            raise SystemExit("ERROR: Invalid configuration name, {}".format(obj_name))

        #Extract CIME log:
        logger = logging.getLogger(__name__)

        #Print variable to logger: 
        logger.debug("CAM config variable: {}".format(obj.name))
        logger.debug("CAM config variable description: {}".format(obj.desc))
        logger.debug("CAM config variable value: {}".format(obj.value))

    #---------------

    def print_all(self):

        """
        Print the names, descriptions, and values of all CAM 
        configuration objects.
        """
   
        #Extract CIME log:
        logger = logging.getLogger(__name__)

        #Print separator:
        logger.debug("CAM configuration variables:")
        logger.debug("-----------------------------")

        #Loop over config dictionary values:
        for obj_name in self.config:
            #Print variable to logger:
            self.print_config(obj_name)

        #Print additional separator (to help seperate this output from additional CIME output):
        logger.debug("-----------------------------")

    #---------------

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

    #---------------

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

############
#End of file
############
 
  
