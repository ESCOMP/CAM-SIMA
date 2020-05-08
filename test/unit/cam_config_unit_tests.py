"""
Python unit testing collection for the various
public Config_CAM methods, including their
error-handling processes.

To run these unit tests, simply type:

python cam_config_unit_tests.py

or (for more verbose output):

python cam_config_unit_tests.py -v

which will currently run 19 tests, all of which should pass.
"""

#----------------------------------------
#Import required python libraries/modules:
#----------------------------------------
import logging
import os
import os.path
import sys

#Python unit-testing library:
import unittest

#Adddirectory to python path:
CURRDIR = os.path.abspath(os.path.dirname(__file__))
CAM_ROOT_DIR = os.path.join(CURRDIR, os.pardir, os.pardir)
CAM_CONF_DIR = os.path.abspath(os.path.join(CAM_ROOT_DIR, "cime_config"))

#Add "cime_config" directory to python path:
sys.path.append(CAM_CONF_DIR)

#Import CAM configure objects:
# pylint: disable=wrong-import-position
from cam_config import ConfigCAM
from cam_config import CamConfigTypeError, CamConfigValError
# pylint: enable=wrong-import-position

#++++++++++++++++++++++++++++++++++++++++++
#Create "fake" CIME case to test Config_CAM
#++++++++++++++++++++++++++++++++++++++++++

class FakeCase:

    # pylint: disable=too-few-public-methods
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
            "CAM_CONFIG_OPTS" : "-dyn none --physics-suites adiabatic_suite"
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

class CamConfigTestRoutine(unittest.TestCase):

    """
    Runs all CAM configuriation tests, to ensure
    that the scripts and error-handling methods
    are running properly.
    """

    #Set-up unit tests:
    def setUp(self):

        """Initalize Config_CAM object being tested."""

        #create fake case:
        fcase = FakeCase()

        #Create python logger object:
        logger = logging.getLogger("cam_config")

        #create CAM configure object:
        test_config_cam = ConfigCAM(fcase, logger)

        #Add CAM configure object to unittest object list:
        self.test_config_cam = test_config_cam

    #++++++++++++++++++++++++++++++++++++++++++++
    #check that "get_value" method works properly
    #++++++++++++++++++++++++++++++++++++++++++++

    def test_config_get_value_check(self):

        """Check that Config_CAM.get_value properly retrieves value"""

        #Get nlat ("ATM_NY") value:
        testval = self.test_config_cam.get_value("pcols")

        #Check that testval matches ATM_NY set in the "fake" case:
        self.assertEqual(testval, 16)

    #++++++++++++++++++++++++++++++++++++++++++++
    #check that "set_value" method works properly
    #++++++++++++++++++++++++++++++++++++++++++++

    def test_config_set_value_check(self):

        """Check that Config_CAM.set_value properly sets value"""

        #Set new value:
        newval = 200

        #Set nlev to "newval":
        self.test_config_cam.set_value("nlev", newval)

        #Get new value:
        testval = self.test_config_cam.get_value("nlev")

        #Check that testval matches
        self.assertEqual(testval, newval)

    #+++++++++++++++++++++++++++++++++++++++++++++++
    #check that "print_config" method works properly
    #+++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_print_config_check(self):

        """
        Check that Config_CAM.print_config properly prints to log

        Please note that this check only works with python 3.4
        or greater, so if an earlier version is used this test
        is skipped.
        """

        if sys.version_info[0] < 3:
            raise unittest.SkipTest("This test doesn't work with Python 2")

        if sys.version_info[1] < 4:
            raise unittest.SkipTest("This test requires Python version 3.4 or later")


        #Create new logger for print_config test:
        print_log = logging.getLogger("print_config")

        #Create unittest log:
        with self.assertLogs(print_log, level='DEBUG') as cmplog:
            #Print variable information to logger via "print_config":
            self.test_config_cam.print_config("nlon", print_log)

            #Check that log output matches what is expected:
            logmsg = "#Number of unique longitude points in rectangular lat/lon" \
                     " grid.\nTotal number of columns for unstructured grids."

            self.assertEqual(cmplog.output, ['DEBUG:print_config:'+logmsg,
                                             'DEBUG:print_config:nlon = null'])

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check "get_value" non-created variable error-handling
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_get_value_list_check(self):

        """Check that "get_value" throws the proper error when non-existent variable is requested"""

        #Set error message:
        ermsg = "ERROR: Invalid configuration name, 'fake variable'"

        #Expect "Cam_config_val_error":
        with self.assertRaises(CamConfigValError) as valerr:
            #Run "get_value" method on made-up variable name:
            self.test_config_cam.get_value("fake variable")

            #Check that error message matches what's expected:
            self.assertEqual(ermsg, str(valerr.exception))

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check "set_value" non-created variable error-handling
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_set_value_list_check(self):

        """Check that "set_value" throws the proper error when non-existent variable is requested"""

        #Set error message:
        ermsg = "ERROR:  Invalid configuration name, 'fake variable'"

        #Expect "Cam_config_val_error":
        with self.assertRaises(CamConfigValError) as valerr:
            #Run "set_value" method on made-up variable name:
            self.test_config_cam.set_value("fake variable", 200)

            #Check that error message matches what's expected:
            self.assertEqual(ermsg, str(valerr.exception))


    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check "print_config" non-created variable error-handling
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_print_config_list_check(self):

        """Check that "print_config" throws the proper error when non-existent variable is requested"""

        #Set error message:
        ermsg = "ERROR:  Invalid configuration name, 'fake variable'"

        #Create new logger for print_config test:
        print_log = logging.getLogger("print_config")

        #Expect "Cam_config_val_error":
        with self.assertRaises(CamConfigValError) as valerr:
            #Run "print_config" method on made-up variable name:
            self.test_config_cam.print_config("fake variable", print_log)

            #Check that error message matches what's expected:
            self.assertEqual(ermsg, str(valerr.exception))


    #++++++++++++++++++++++++++++++++++++++++++++
    #Check "set_value" input value error-handling
    #++++++++++++++++++++++++++++++++++++++++++++

    def test_config_set_value_type_check(self):

        """
        Check that "set_value" throws the proper error when
        the input value is of the wrong type.
        """

        #Set error message:
        ermsg = "ERROR:  Value provided for variable, 'nlev', must be either an integer or a string.  Currently it is type <type 'float'>"

        #Expect "Cam_config_type_error":
        with self.assertRaises(CamConfigTypeError) as typerr:
            #Run "set_value" method on made-up variable name:
            self.test_config_cam.set_value("nlev", 5.0)

            #Check that error message matches what's expected:
            self.assertEqual(ermsg, str(typerr.exception))


#################################################
#Run unit tests if this script is called directly
#################################################

if __name__ == "__main__":
    unittest.main()

############
#End of file
############
