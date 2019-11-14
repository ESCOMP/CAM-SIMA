"""
Location of testing suite for the various
CAM configure object classes, including
their error-handling methods.
"""

#---------------------------------------
#Import generic python libraries/modules:
#---------------------------------------
from config_cam import Config_gen, Config_integer, Config_string
from config_cam import Cam_config_type_error, Cam_config_val_error
import re 


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
    err_type = "Maximum integer value error"

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
    #Check string config class initalization:
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

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that valid values regular expression works for string configration as well:
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    val  = "option a"
    valid_vals = re.compile(r"option")

    #Set valid value string to help with error-reporting:
    val_string = "regular expression"

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

    #+++++++++++++++++++++++++++++++++++++++++
    #Evalulate Config_string regex-match check
    #+++++++++++++++++++++++++++++++++++++++++

    valid_vals = re.compile(r"badval")

    #set error test message:
    err_type = "valid values regex match error"

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
        print("String class configuration check with valid values being {} PASSED!".format(val_string))
        num_pass += 1

    except:
        print("\n")
        print("Something un-exexpected happened creating string config class with valid values being {}!".format(val_string))
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

    except Cam_config_type_error as err_msg:
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

    except Cam_config_type_error as err_msg:
        print("\n")
        print(err_msg)

        #Add to passed tests list:
        print("{} for Config_integer PASSED!".format(err_type))
        num_pass += 1

    except Cam_config_val_error as err_msg:
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

    except Cam_config_type_error as err_msg:
        print("\n")
        print(err_msg)

        #Add to passed tests list:
        print("{} for Config_string PASSED!".format(err_type))
        num_pass += 1

    except Cam_config_val_error as err_msg:
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

#if __name__ == "__main__":

   #Add CIME scripts to python:
   

   #Call testing routine:
#   cam_config_test_routine()

############
#End of file
############
 
  
