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

#Add directory to python path:
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
            "COMP_ATM" : "cam",
            "EXEROOT"  : "/some/made-up/path",
            "CASEROOT" : "/another/made-up/path",
            "CAM_CONFIG_OPTS" : "-dyn none --physics-suites adiabatic;kessler",
            "COMP_ROOT_DIR_ATM" : "/a/third/made-up/path",
            "CAM_CPPDEFS" : "UNSET",
            "NTHRDS_ATM" : 1,
            "RUN_STARTDATE" : "101"
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

        #Set pcols to "newval":
        self.test_config_cam.set_value("pcols", newval)

        #Get new value:
        testval = self.test_config_cam.get_value("pcols")

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
            logmsg1 = "# Number of unique longitude points in rectangular lat/lon grid."
            logmsg2 = "#    Total number of columns for unstructured grids."
            logmsg = 'DEBUG:print_config:'+logmsg1+'\n'+logmsg2

            self.assertEqual(cmplog.output, [logmsg,
                                             'DEBUG:print_config:nlon = null'])

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check "get_value" non-created variable error-handling
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_get_value_list_check(self):

        """Check that "get_value" throws the proper error when non-existent variable is requested"""

        #Set error message:
        ermsg = "ERROR:  Invalid configuration name, 'fake variable'"

        #Expect "CamConfigValError":
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

        #Expect "CamConfigValError":
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

        #Expect "CamConfigValError":
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
        ermsg = "ERROR:  Value provided for variable, 'pcols', must be either an integer or a string.  Currently it is type <class 'float'>"

        #Expect "CamConfigTypeError":
        with self.assertRaises(CamConfigTypeError) as typerr:
            #Run "set_value" method on made-up variable name:
            self.test_config_cam.set_value("pcols", 5.0)

        #Check that error message matches what's expected:
        self.assertEqual(ermsg, str(typerr.exception))

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check "generate_cam_src" missing "ccpp_framework" error-handling
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_gen_cam_src_ccpp_check(self):

        """
        Check that "generate_cam_src" throws the proper error
        if the "ccpp_framework" external directory doesn't exist.
        """

        #Set error message:
        ermsg = "ERROR: ccpp_framework/scripts directory doesn't exist! Has 'checkout_externals' been run?"

        #Expect "CamConfigValError":
        with self.assertRaises(CamConfigValError) as valerr:
            #Run "generate_cam_src" method, which should fail
            #due to the case paths being "fake":
            self.test_config_cam.generate_cam_src(0)

        #Check that error message matches what's expected:
        self.assertEqual(ermsg, str(valerr.exception))

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that "ccpp_phys_set" works as expected with one physics suite entry
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_ccpp_phys_set_check_single_suite(self):

        """
        Check that "ccpp_phys_set" works as expected
        when given a correctly formatted namelist file
        and a single physics suite in the config object.
        """

        #Save "physics_suites" value:
        cam_config_suites_orig = self.test_config_cam.get_value("physics_suites")


        #Set "new" physics_suites value with one physics suite:
        self.test_config_cam.set_value("physics_suites", "kessler")

        #Create (empty) namelist attribute dictionary:
        cam_nml_attr_dict = {}

        #Create namelist file:
        with open("test.txt", "w", encoding='UTF-8') as test_fil:
            test_fil.write('!Namelist test file\n')
            test_fil.write('physics_suite = "kessler"\n')

        #Run ccpp_phys_set config method:
        self.test_config_cam.ccpp_phys_set(cam_nml_attr_dict, "test.txt")

        #Check that dictonary entries are correct:
        self.assertEqual(cam_nml_attr_dict["phys_suite"], "kessler")

        #Remove text file:
        os.remove("test.txt")

        #Set physics_suites back to its original value:
        self.test_config_cam.set_value("physics_suites", cam_config_suites_orig)


    #++++++++++++++++++++++++++++++++++++++++++++
    #Check that "ccpp_phys_set" works as expected
    #++++++++++++++++++++++++++++++++++++++++++++

    def test_config_ccpp_phys_set_check_multi_suite(self):

        """
        Check that "ccpp_phys_set" works as expected
        when given a correctly formatted namelist file
        and multiple physics suites in the config object.
        """

        #Create namelist attribute dictionary:
        cam_nml_attr_dict = {}

        #Create namelist file:
        with open("test.txt", "w", encoding='UTF-8') as test_fil:
            test_fil.write('!Namelist test file\n')
            test_fil.write('physics_suite = "adiabatic"\n')

        #Run ccpp_phys_set config method:
        self.test_config_cam.ccpp_phys_set(cam_nml_attr_dict, "test.txt")

        #Check that dictonary entries are correct:
        self.assertEqual(cam_nml_attr_dict["phys_suite"], "adiabatic")

        #Remove text file:
        os.remove("test.txt")

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check "ccpp_phys_set" missing "physics_suite" error-handling
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_ccpp_phys_set_missing_phys(self):

        """
        Check that "ccpp_phys_set" throws the proper
        error if there is more than one CCPP suite and the
        "physics_suite" namelist variable is missing.
        """

        #Create namelist attribute dictionary:
        cam_nml_attr_dict = {}

        #Set error message:
        ermsg  = "No 'physics_suite' variable is present in user_nl_cam.\n"
        ermsg += "This is required if more than one suite is listed\n"
        ermsg += "in CAM_CONFIG_OPTS."

        #Create namelist file:
        with open("test.txt", "w", encoding='UTF-8') as test_fil:
            test_fil.write('!Namelist test file\n')

        #Expect "CamConfigValError":
        with self.assertRaises(CamConfigValError) as valerr:
            #Run ccpp_phys_set config method, which should fail
            #due to missing "physics_suite" namelist variable:
            self.test_config_cam.ccpp_phys_set(cam_nml_attr_dict, "test.txt")

        #Check that error message matches what's expected:
        self.assertEqual(ermsg, str(valerr.exception))

        #Remove text file:
        os.remove("test.txt")

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check "ccpp_phys_set" multiple namelist entries error-handling
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_ccpp_phys_set_two_phys(self):

        """
        Check that "ccpp_phys_set" throws the proper
        error if there is more than one CCPP suite and
        more than one "physics_suite" namelist variable.
        """

        #Create namelist attribute dictionary:
        cam_nml_attr_dict = {}

        #Set error message:
        ermsg  = "More than one 'physics_suite' variable is present in user_nl_cam.\n"
        ermsg += "Only one 'physics_suite' line is allowed."

        #Create namelist file:
        with open("test.txt", "w", encoding='UTF-8') as test_fil:
            test_fil.write('!Namelist test file\n')
            test_fil.write('physics_suite = "adiabatic"\n')
            test_fil.write('physics_suite = "kessler"\n')

        #Expect "CamConfigValError":
        with self.assertRaises(CamConfigValError) as valerr:
            #Run ccpp_phys_set config method, which should fail
            #due to multiple "physics_suite" namelist variable:
            self.test_config_cam.ccpp_phys_set(cam_nml_attr_dict, "test.txt")

        #Check that error message matches what's expected:
        self.assertEqual(ermsg, str(valerr.exception))

        #Remove text file:
        os.remove("test.txt")

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check "ccpp_phys_set" missing equals-sign error-handling
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_ccpp_phys_set_missing_equals(self):

        """
        Check that "ccpp_phys_set" throws the proper
        error if there is a missing equals (=) sign
        after the "physics_suite" namelist variable.
        """

        #Create namelist attribute dictionary:
        cam_nml_attr_dict = {}

        #Set error message:
        ermsg = "No equals (=) sign was found with the 'physics_suite' variable."


        #Create namelist file:
        with open("test.txt", "w", encoding='UTF-8') as test_fil:
            test_fil.write('!Namelist test file\n')
            test_fil.write('physics_suite  "adiabatic"\n')

        #Expect "CamConfigValError":
        with self.assertRaises(CamConfigValError) as valerr:
            #Run ccpp_phys_set config method, which should fail
            #due to a missing equals sign in the namelist entry:
            self.test_config_cam.ccpp_phys_set(cam_nml_attr_dict, "test.txt")

        #Check that error message matches what's expected:
        self.assertEqual(ermsg, str(valerr.exception))

        #Remove text file:
        os.remove("test.txt")

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check "ccpp_phys_set" multiple equals-signs error-handling
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_ccpp_phys_set_two_equals(self):

        """
        Check that "ccpp_phys_set" throws the proper
        error if there is more than one equals (=) sign
        after the "physics_suite" namelist variable.
        """

        #Create namelist attribute dictionary:
        cam_nml_attr_dict = {}

        #Set error message:
        ermsg = "There must only be one equals (=) sign in the 'physics_suite' namelist line."

        #Create namelist file:
        with open("test.txt", "w", encoding='UTF-8') as test_fil:
            test_fil.write('!Namelist test file\n')
            test_fil.write('physics_suite == "adiabatic"\n')

        #Expect "CamConfigValError":
        with self.assertRaises(CamConfigValError) as valerr:
            #Run ccpp_phys_set config method, which should fail
            #due to an incorrect number of equal signs in the
            #namelist entry:
            self.test_config_cam.ccpp_phys_set(cam_nml_attr_dict, "test.txt")

        #Check that error message matches what's expected:
        self.assertEqual(ermsg, str(valerr.exception))

        #Remove text file:
        os.remove("test.txt")

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check "ccpp_phys_set" non-matching physics_suite error-handling
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_ccpp_phys_set_no_physics_suite_match(self):

        """
        Check that "ccpp_phys_set" throws the proper
        error if the "physics_suite" namelist variable
        value doesn't match any of the options listed
        in "CAM_CONFIG_OPTS".
        """

        #Create namelist attribute dictionary:
        cam_nml_attr_dict = {}

        #Set error message:
        ermsg  = "physics_suite specified in user_nl_cam, 'cam6', doesn't match any suites\n"
        ermsg += "listed in CAM_CONFIG_OPTS"

        #Create namelist file:
        with open("test.txt", "w", encoding='UTF-8') as test_fil:
            test_fil.write('!Namelist test file\n')
            test_fil.write('physics_suite = "cam6"\n')

        #Expect "CamConfigValError":
        with self.assertRaises(CamConfigValError) as valerr:
            #Run ccpp_phys_set config method, which should fail
            #due to a mis-match between the "physics_suite" namelist
            #variable and the physics suite options listed in the
            #physics_suites config variable:
            self.test_config_cam.ccpp_phys_set(cam_nml_attr_dict, "test.txt")

        #Check that error message matches what's expected:
        self.assertEqual(ermsg, str(valerr.exception))

        #Remove text file:
        os.remove("test.txt")


#################################################
#Run unit tests if this script is called directly
#################################################

if __name__ == "__main__":
    unittest.main()

############
#End of file
############
