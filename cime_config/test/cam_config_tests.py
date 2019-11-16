"""
Location of testing suite for the various
CAM configure object classes, including
their error-handling methods.
"""

#----------------------------------------
#Import required python libraries/modules:
#----------------------------------------
from cam_config import Config_CAM
from cam_config import Cam_config_type_error, Cam_config_val_error
import re 
import logging

#++++++++++++++++++++++++++++++++++++++++++
#Create "fake" CIME case to test Config_CAM
#++++++++++++++++++++++++++++++++++++++++++

class fake_case:

    """
    Fake CIME case class with variables needed to test 
    the "Config_CAM" object.
    """

    def __init__(self):
    

        #Create dictionary (so get_value works properly):
        self.conf_opts = { 
                           "ATM_GRID" : "f19_f19_mg17",
                           "ATM_NX"   : 180,
                           "ATM_NY"   : 90,
                           "COMP_OCN" : "socn",
                           "CAM_CONFIG_OPTS" : "-dyn none -physics_suites adiabatic_suite"
                         }

    def get_value(self, key):

        """
        Function used to return value 
        from conf_opts dictionary,
        with the key as input.
        """

        val = self.conf_opts[key]  

        return val

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

    #create fake case:
    fcase = fake_case()

    #create CAM configure object:
    config = Config_CAM(fcase, logging)

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
    name       = "test"
    desc       = "test object description"
    val        = 1
    valid_vals = None

    val_string = "integer"

    #Run create_config initalization check:
    num_pass = generate_config_pass(config, name, desc, val, valid_vals, val_string, num_pass)

    #++++++++++++++++++++++++++++++++++++++++
    #check repeating list name error-handling:
    #++++++++++++++++++++++++++++++++++++++++

    #Don't change any inputs.

    #set error test message:
    err_type = "config list error"

    #Run create_config error-handle check:
    num_pass, num_fail = generate_config_error(config, name, desc, val,
                                               valid_vals, err_type, num_pass, num_fail)
    
    #+++++++++++++++++++++++++++++++++
    #check generic name error-handling:
    #+++++++++++++++++++++++++++++++++

    #set name value:
    name = None

    #set error test message:
    err_type = "Name error"

    #Run create_config error-handle check:
    num_pass, num_fail = generate_config_error(config, name, desc, val,
                                               valid_vals, err_type, num_pass, num_fail)

    #++++++++++++++++++++++++++++++++++++++++
    #check generic description error-handling:
    #++++++++++++++++++++++++++++++++++++++++

    #set configuration options:
    name = "test_fail"
    desc = None

    #set error test message:
    err_type = "Description error"

    #Run create_config error-handle check:
    num_pass, num_fail = generate_config_error(config, name, desc, val,
                                               valid_vals, err_type, num_pass, num_fail)

    #+++++++++++++++++++++++++++++++++++++++++++++++
    #Check valid_vals None/list/tuple error-handling:
    #+++++++++++++++++++++++++++++++++++++++++++++++

    #set "bad" valid values:
    valid_vals = 5
   
    #set description back to test string:
    desc = "test object description"

    #set error test message:
    err_type = "valid values type error"

    #Run create_config error-handle check:
    num_pass, num_fail = generate_config_error(config, name, desc, val, 
                                               valid_vals, err_type, num_pass, num_fail)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that valid values tuple also works for integer configration:
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    #set name:
    name = "test_tuple"

    #set valued values:
    valid_vals = (0,2)

    #Set valid value string to help with error-reporting:
    val_string = "tuple"

    #Run create_config initalization check:
    num_pass = generate_config_pass(config, name, desc, val, valid_vals, val_string, num_pass)

    #++++++++++++++++++++++++++++++++++++++++++
    #Check valid_vals all "None" error-handling:
    #++++++++++++++++++++++++++++++++++++++++++

    #set name:
    name = "test_fail"

    #set valued values:
    valid_vals = (None,None)

    #set error test message:
    err_type = "valid values all-None tuple error"

    #Run create_config error-handle check:
    num_pass, num_fail = generate_config_error(config, name, desc, val,
                                               valid_vals, err_type, num_pass, num_fail)

    #++++++++++++++++++++++++++++++++++++++
    #Check valid_vals length error-handling:
    #++++++++++++++++++++++++++++++++++++++

    #set valued values:
    valid_vals = (0,)

    #set error test message:
    err_type = "Tuple length error"

    #Run create_config error-handle check:
    num_pass, num_fail = generate_config_error(config, name, desc, val,
                                               valid_vals, err_type, num_pass, num_fail)

    #+++++++++++++++++++++++++++++
    #Evalulate minimum value check
    #+++++++++++++++++++++++++++++

    #set valued values:
    valid_vals = (2,None)

    #set error test message:
    err_type = "Minimum integer value error"

    #Run create_config error-handle check:
    num_pass, num_fail = generate_config_error(config, name, desc, val,
                                               valid_vals, err_type, num_pass, num_fail)    

    #+++++++++++++++++++++++++++++
    #Evalulate maximum value check
    #+++++++++++++++++++++++++++++

    #Set valid values:
    valid_vals = (None,0)

    #set error test message:
    err_type = "Maximum integer value error"

    #Run create_config error-handle check:
    num_pass, num_fail = generate_config_error(config, name, desc, val,
                                               valid_vals, err_type, num_pass, num_fail)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that valid values list also works for integer configration:
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    name = "test_int_list"
    valid_vals = [0,1,2]

    #Set valid value string to help with error-reporting:
    val_string = "list"

    #Run create_config initalization check:
    num_pass = generate_config_pass(config, name, desc, val, valid_vals, val_string, num_pass)

    #+++++++++++++++++++++++++++++++++++++++++
    #Evalulate Config_integer list-match check
    #+++++++++++++++++++++++++++++++++++++++++

    #set valid values:
    valid_vals = [0,2,3]

    #set error test message:
    err_type = "valid values integer match error"

    #Run create_config error-handle check:
    num_pass, num_fail = generate_config_error(config, name, desc, val,
                                               valid_vals, err_type, num_pass, num_fail)

    #+++++++++++++++++++++++++++++++++++++++
    #Check string config class initalization:
    #+++++++++++++++++++++++++++++++++++++++

    name = "test_string"
    val  = "option a"
    valid_vals = None

    #Set valid value string to help with error-reporting:
    val_string = "None (String value)"

    #Run create_config initalization check:
    num_pass = generate_config_pass(config, name, desc, val, valid_vals, val_string, num_pass)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that valid values list also works for string configration:
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    name = "test_str_list"
    val  = "option a"
    valid_vals = ["option a", "option b"]

    #Set valid value string to help with error-reporting:
    val_string = "string list"

    #Run create_config initalization check:
    num_pass = generate_config_pass(config, name, desc, val, valid_vals, val_string, num_pass)

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that valid values regular expression works for string configration as well:
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    name = "test_str_regex"
    val  = "option a"
    valid_vals = re.compile(r"option")

    #Set valid value string to help with error-reporting:
    val_string = "regular expression"

    #Run create_config initalization check:
    num_pass = generate_config_pass(config, name, desc, val, valid_vals, val_string, num_pass)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check Config_string valid_vals None/list error-handling:
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++

    name = "test_fail"
    valid_vals = 5

    #set error test message:
    err_type = "valid values string type error"

    #Run create_config error-handle check:
    num_pass, num_fail = generate_config_error(config, name, desc, val,
                                               valid_vals, err_type, num_pass, num_fail)

    #++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check Config_string all-string list error-handling:
    #++++++++++++++++++++++++++++++++++++++++++++++++++

    valid_vals = ["option a", 5]

    #set error test message:
    err_type = "valid values non-string list error"

    #Run create_config error-handle check:
    num_pass, num_fail = generate_config_error(config, name, desc, val,
                                               valid_vals, err_type, num_pass, num_fail)

    #++++++++++++++++++++++++++++++++++++++++
    #Evalulate Config_string list-match check
    #++++++++++++++++++++++++++++++++++++++++

    valid_vals = ["option b", "option c"]

    #set error test message:
    err_type = "valid values string match error"

    #Run create_config error-handle check:
    num_pass, num_fail = generate_config_error(config, name, desc, val,
                                               valid_vals, err_type, num_pass, num_fail)

    #+++++++++++++++++++++++++++++++++++++++++
    #Evalulate Config_string regex-match check
    #+++++++++++++++++++++++++++++++++++++++++

    valid_vals = re.compile(r"badval")

    #set error test message:
    err_type = "valid values string regex match error"

    #Run create_config error-handle check:
    num_pass, num_fail = generate_config_error(config, name, desc, val,
                                               valid_vals, err_type, num_pass, num_fail)

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

def generate_config_pass(conf_obj, name, desc, val, valid_vals, val_string, num_pass):

    """
    Tests if a configre object can
    be created successfully.
    """

    try:
        conf_obj.create_config(name, desc, val, valid_vals)

        #Add to passed tests list:
        print("Class configuration check with valid values being {} PASSED!".format(val_string))
        num_pass += 1

    except:
        print("Something un-exexpected happened creating generic config class!")
        raise

    #Return number of passed tests:
    return num_pass

#-------------------------------

def generate_config_error(conf_obj, name, desc, val, valid_vals,
                          err_type, num_pass, num_fail):

    """
    Tests if "create_config"can
    properly handle bad input values.
    """

    try:
        conf_obj.create_config(name, desc, val, valid_vals)

        #Add to failed tests list:
        print("{} check FAILED!".format(err_type))
        num_fail += 1

    except Cam_config_type_error as err_msg:
        print("\n")
        print(err_msg)

        #Add to passed tests list:
        print("{} check PASSED!".format(err_type))
        num_pass += 1

    except Cam_config_val_error as err_msg:
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

############
#End of file
############
 
