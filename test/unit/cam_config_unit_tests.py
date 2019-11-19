"""
Python unit testing collection for the various
CAM configure object classes, including
their error-handling methods.

To run these unit tests, simply type:

python cam_config_unit_tests.py

or (for more verbose output):

python cam_config_unit_tests.py -v

which will currently run 19 tests, all of which should pass.
"""

#----------------------------------------
#Import required python libraries/modules:
#----------------------------------------
import re 
import logging
import os
import os.path
import sys

#Python unit-testing library:
import unittest

#Adddirectory to python path:
currdir      = os.path.abspath(os.path.dirname(__file__))
cam_root_dir = os.path.join(currdir,os.pardir,os.pardir)
cam_conf_dir = os.path.abspath(os.path.join(cam_root_dir,"cime_config"))

#Add "cime_config" directory to python path:
sys.path.append(cam_conf_dir)

#Import CAM configure objects:
from cam_config import Config_CAM
from cam_config import Cam_config_type_error, Cam_config_val_error

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

class cam_config_test_routine(unittest.TestCase):

    """
    Runs all CAM configuriation tests, to ensure
    that the scripts and error-handling methods
    are running properly.
    """

    #Set-up unit tests:
    def setUp(self):

        """Initalize Config_CAM object being tested."""
  
        #Initalize test counters:
        num_pass = 0
        num_fail = 0

        #create fake case:
        fcase = fake_case()

        #create CAM configure object:
        test_config_cam = Config_CAM(fcase, logging)

        #create list to add to test TestCase object:
        self.obj_list = list()

        #Add CAM configure object to unittest object list:
        self.test_config_cam = test_config_cam

    #++++++++++++++++++++++++++++++++++++++++
    #Check generic config class initalization:
    #++++++++++++++++++++++++++++++++++++++++

    def test_create_config(self):

        """Check that create_config can be run successfully."""

        #set configuration options:
        name       = "test"
        desc       = "test object description"
        val        = 1
        valid_vals = None

        #Read in CAM config object:
        test_config = self.test_config_cam

        #create CAM configure object:
        test_config.create_config(name, desc, val, valid_vals)

        #Check that object was created:
        self.assertTrue(name in test_config.config)

    #++++++++++++++++++++++++++++++++++++++++
    #check repeating list name error-handling:
    #++++++++++++++++++++++++++++++++++++++++

    def test_config_exist_check(self):

        """Check that a configuration option can't be created twice."""

        #Use same input options as "test_create_config":
        name       = "test"
        desc       = "test object description"
        val        = 1
        valid_vals = None

        #Read in CAM config object:
        test_config = self.test_config_cam

        #Create "test" configure option:
        test_config.create_config(name, desc, val, valid_vals)

        #Check that exception is raised properly:
        with self.assertRaises(Cam_config_val_error):
            #Try creating the same option again:
            test_config.create_config(name, desc, val, valid_vals)
    
    #+++++++++++++++++++++++++++++++++
    #check generic name error-handling:
    #+++++++++++++++++++++++++++++++++

    def test_config_name_check(self):

        """Check that a configuration option's name cannot be None."""

        #Use same input options, but with name being None: 
        name       = None
        desc       = "test object description"
        val        = 1
        valid_vals = None

        #Read in CAM config object:
        test_config = self.test_config_cam

        #Check that exception is raised properly:
        with self.assertRaises(Cam_config_type_error):
            test_config.create_config(name, desc, val, valid_vals)

    #++++++++++++++++++++++++++++++++++++++++
    #check generic description error-handling:
    #++++++++++++++++++++++++++++++++++++++++

    def test_config_desc_check(self):

        """Check that a configuration option's description cannot be None.""" 

        #Set input options, with desc being None:
        name       = "test"
        desc       = None
        val        = 1
        valid_vals = None

        #Read in CAM config object:
        test_config = self.test_config_cam

        #Check that exception is raised properly:
        with self.assertRaises(Cam_config_type_error):
            test_config.create_config(name, desc, val, valid_vals)

    #+++++++++++++++++++++++++++++++++++++++++++++++
    #Check valid_vals None/list/tuple error-handling:
    #+++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_valid_vals_type_check(self):

        """Check that the valid_vals variable cannot be a single integer."""

        #Set input options, with valid vals being an integer:
        name       = "test"
        desc       = "test object description"
        val        = 1
        valid_vals = 5

        #Read in CAM config object:
        test_config = self.test_config_cam

        #Check that exception is raised properly:
        with self.assertRaises(Cam_config_type_error):
            test_config.create_config(name, desc, val, valid_vals)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that valid values tuple also works for integer configration:
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_valid_vals_tuple(self):

        """Check that a configure object can be created with a valid_vals tuple."""

        #set configuration options, with valid_vals being a tuple:
        name       = "test"
        desc       = "test object description"
        val        = 1
        valid_vals = (0,2)

        #Read in CAM config object:
        test_config = self.test_config_cam

        #create CAM configure object:
        test_config.create_config(name, desc, val, valid_vals)

        #Check that object was created:
        self.assertTrue(name in test_config.config)

    #++++++++++++++++++++++++++++++++++++++++++
    #Check valid_vals all "None" error-handling:
    #++++++++++++++++++++++++++++++++++++++++++

    def test_config_tuple_Nones_check(self):

        """Check that a valid_vals tuple cannot be all Nones."""

        #Set input options, with valid_vals being a tuple of Nones:
        name       = "test"
        desc       = "test object description"
        val        = 1
        valid_vals = (None, None)

        #Read in CAM config object:
        test_config = self.test_config_cam

        #Check that exception is raised properly:
        with self.assertRaises(Cam_config_val_error):
            test_config.create_config(name, desc, val, valid_vals)

    #++++++++++++++++++++++++++++++++++++++
    #Check valid_vals length error-handling:
    #++++++++++++++++++++++++++++++++++++++

    def test_config_tuple_length_check(self):

        """Check that a valid_vals tuple cannot be length one."""

        #Set input options, with valid_vals being a
        #tuple of length one:
        name       = "test"
        desc       = "test object description"
        val        = 1
        valid_vals = (0,)

        #Read in CAM config object:
        test_config = self.test_config_cam

        #Check that exception is raised properly:
        with self.assertRaises(Cam_config_val_error):
            test_config.create_config(name, desc, val, valid_vals)

    #+++++++++++++++++++++++++++++
    #Evalulate minimum value check
    #+++++++++++++++++++++++++++++

    def test_config_min_integer_check(self):

        """Check that a given value cannot be less than the valid_vals min value."""

        #Set input options, with given value less than
        #valid_vals defined minimum:
        name       = "test"
        desc       = "test object description"
        val        = 1
        valid_vals = (2, None)

        #Read in CAM config object:
        test_config = self.test_config_cam

        #Check that exception is raised properly:
        with self.assertRaises(Cam_config_val_error):
            test_config.create_config(name, desc, val, valid_vals)

    #+++++++++++++++++++++++++++++
    #Evalulate maximum value check
    #+++++++++++++++++++++++++++++

    def test_config_max_integer_check(self):

        """Check that a given value cannot be greater than the valid_vals max value.""" 

        #Set input options, with given value more than
        #valid_vals defined maximum:
        name       = "test"
        desc       = "test object description"
        val        = 1
        valid_vals = (None, 0)

        #Read in CAM config object:
        test_config = self.test_config_cam

        #Check that exception is raised properly:
        with self.assertRaises(Cam_config_val_error):
            test_config.create_config(name, desc, val, valid_vals)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that valid values list also works for integer configration:
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_valid_vals_list(self):

        """Check that a configure object can be created with a valid_vals integer list."""

        #set configuration options, with valid_vals being a list:
        name       = "test"
        desc       = "test object description"
        val        = 1
        valid_vals = [0,1,2]

        #Read in CAM config object:
        test_config = self.test_config_cam

        #create CAM configure object:
        test_config.create_config(name, desc, val, valid_vals)

        #Check that object was created:
        self.assertTrue(name in test_config.config)

    #+++++++++++++++++++++++++++++++++++++++++
    #Evalulate Config_integer list-match check
    #+++++++++++++++++++++++++++++++++++++++++

    def test_config_integer_list_match_check(self):

        """Check that a given integer value must be present in the valid_vals list."""

        #Set input options, with given value missing
        #from valid_vals list:
        name       = "test"
        desc       = "test object description"
        val        = 1
        valid_vals = [0,2,3]

        #Read in CAM config object:
        test_config = self.test_config_cam

        #Check that exception is raised properly:
        with self.assertRaises(Cam_config_val_error):
            test_config.create_config(name, desc, val, valid_vals)

    #+++++++++++++++++++++++++++++++++++++++
    #Check string config class initalization:
    #+++++++++++++++++++++++++++++++++++++++

    def test_config_string_value(self):

        """Check that a configure object can be created with a given string value."""

        #set configuration options,  with val being a string:
        name       = "test"
        desc       = "test object description"
        val        = "option a"
        valid_vals = None

        #Read in CAM config object:
        test_config = self.test_config_cam

        #create CAM configure object:
        test_config.create_config(name, desc, val, valid_vals)

        #Check that object was created:
        self.assertTrue(name in test_config.config)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that valid values list also works for string configration:
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_valid_vals_string_list(self):

        """Cheack that a configure object can be created with a valid_vals string list."""

        #set configuration options,  with vallid_vals 
        #being a list of strings:
        name       = "test"
        desc       = "test object description"
        val        = "option a"
        valid_vals = ["option a", "option b"]

        #Read in CAM config object:
        test_config = self.test_config_cam

        #create CAM configure object:
        test_config.create_config(name, desc, val, valid_vals)

        #Check that object was created:
        self.assertTrue(name in test_config.config)

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that valid values regular expression works for string configration as well:
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_valid_vals_regex(self):

        """Check that a configure object can be created with a valid_vals regular expression."""

        #set configuration options,  with valid_vals
        #being a regular expression:
        name       = "test"
        desc       = "test object description"
        val        = "option a"
        valid_vals = re.compile(r"option")

        #Read in CAM config object:
        test_config = self.test_config_cam

        #create CAM configure object:
        test_config.create_config(name, desc, val, valid_vals)

        #Check that object was created:
        self.assertTrue(name in test_config.config)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check Config_string valid_vals None/list error-handling:
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_string_valid_vals_type_check(self):

        """Check that the valid_vals variable cannot be an integer when the given value is a string."""

        #Set input options, with given value a string
        #and valid_vals an integer:
        name       = "test"
        desc       = "test object description"
        val        = "option a"
        valid_vals = 5

        #Read in CAM config object:
        test_config = self.test_config_cam

        #Check that exception is raised properly:
        with self.assertRaises(Cam_config_type_error):
            test_config.create_config(name, desc, val, valid_vals)

    #++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check Config_string all-string list error-handling:
    #++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_valid_vals_all_strings_check(self):

        """Check that a valid_vals list cannot contain both strings and integers."""

        #Set input options, with given value a string
        #and valid_vals a list with a string and integer:
        name       = "test"
        desc       = "test object description"
        val        = "option a"
        valid_vals = ["option a", 5]

        #Read in CAM config object:
        test_config = self.test_config_cam

        #Check that exception is raised properly:
        with self.assertRaises(Cam_config_type_error):
            test_config.create_config(name, desc, val, valid_vals)

    #++++++++++++++++++++++++++++++++++++++++
    #Evalulate Config_string list-match check
    #++++++++++++++++++++++++++++++++++++++++

    def test_config_string_list_match_check(self):

        """Check that a given string value must be present in the valid_vals list."""

        #Set input options, with given value a string
        #and valid_vals a list with a string and integer:
        name       = "test"
        desc       = "test object description"
        val        = "option a"
        valid_vals = ["option b", "option c"] 

        #Read in CAM config object:
        test_config = self.test_config_cam

        #Check that exception is raised properly:
        with self.assertRaises(Cam_config_val_error):
            test_config.create_config(name, desc, val, valid_vals)

    #+++++++++++++++++++++++++++++++++++++++++
    #Evalulate Config_string regex-match check
    #+++++++++++++++++++++++++++++++++++++++++

    def test_config_string_regex_match_check(self):

        """Check that a given string value must match the valid_vals regular expression."""

        #Set input options, with given value a string
        #and valid_vals a regular expression that doesn't
        #match given value.
        name       = "test"
        desc       = "test object description"
        val        = "option a"
        valid_vals = re.compile(r"badval")

        #Read in CAM config object:
        test_config = self.test_config_cam

        #Check that exception is raised properly:
        with self.assertRaises(Cam_config_val_error):
            test_config.create_config(name, desc, val, valid_vals)


#################################################
#Run unit tests if this script is called directly
#################################################

if __name__ == "__main__":
    unittest.main()

############
#End of file
############
 
