"""
Python unit testing collection for the various
public Config_CAM methods, including their
error-handling processes.  Please note that
these tests will only work with Python 3.7
or later.

To run these unit tests, simply type:

python cam_config_unit_tests.py

or (for more verbose output):

python cam_config_unit_tests.py -v

which will currently run 23 tests, all of which should pass.
"""

#----------------------------------------
#Import required python libraries/modules:
#----------------------------------------
import logging
import os
import os.path
import sys
from collections import OrderedDict

#Python unit-testing library:
import unittest

#Add directory to python path:
CURRDIR = os.path.abspath(os.path.dirname(__file__))
CAM_ROOT_DIR = os.path.join(CURRDIR, os.pardir, os.pardir, os.pardir)
CAM_CONF_DIR = os.path.abspath(os.path.join(CAM_ROOT_DIR, "cime_config"))

#Add "cime_config" directory to python path:
sys.path.append(CAM_CONF_DIR)

#Import CAM configure objects:
# pylint: disable=wrong-import-position
from cam_autogen import CamAutoGenError
from cam_config import ConfigCAM
from cam_config_classes import CamConfigTypeError, CamConfigValError
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
            "ATM_GRID" : "mpasa480z32_mpasa480",
            "ATM_NX"   : 180,
            "ATM_NY"   : 90,
            "COMP_OCN" : "socn",
            "COMP_ATM" : "cam",
            "EXEROOT"  : "/some/made-up/path",
            "CASEROOT" : "/another/made-up/path",
            "CAM_CONFIG_OPTS" : "--dyn none --physics-suites mango;papaya",
            "COMP_ROOT_DIR_ATM" : "/a/third/made-up/path",
            "CAM_CPPDEFS" : "UNSET",
            "NTHRDS_ATM" : 1,
            "RUN_STARTDATE" : "101",
            "DEBUG" : False
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

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #check that "get_value" method works properly for non-null dycores
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_get_dycore_values_check(self):

        """
        Check that Config_CAM.get_value properly retrieves the dycore
        name and horizontal grid for a non-null dycore.
        """

        #Create new "fake" case:
        fcase_dyn = FakeCase()

        #Remove "none" dyn option:
        fcase_dyn.conf_opts["CAM_CONFIG_OPTS"] = "--physics-suites mango;papaya"

        #Create python logger object:
        logger = logging.getLogger("cam_config")

        #create CAM configure object:
        test_config_dyn = ConfigCAM(fcase_dyn, logger)

        #Get dycore name:
        test_dyn = test_config_dyn.get_value("dyn")

        #Check that dycore name matches what is specified by the grid
        #in the "fake" CIME case:
        self.assertEqual(test_dyn, "mpas")

        #Get dycore horizontal grid:
        test_hgrid = test_config_dyn.get_value("hgrid")

        #Check that dycore grid matches what is specified by the grid
        #in the "fake" CIME case:
        self.assertEqual(test_hgrid, "mpasa480")

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

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #check that "create_config" with an integer value works properly
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_create_config_int(self):

        """
        Check that Config_CAM.create_config properly sets an integer value
        """

        #Set test value:
        testval = 5

        #Create new config integer variable:
        self.test_config_cam.create_config("test_int", "test object description", testval)

        #Get value of new config variable:
        conf_val = self.test_config_cam.get_value("test_int")

        #Check that the test value matches what is expected:
        self.assertEqual(conf_val, testval)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #check that "create_config" with a string value works properly
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_create_config_str(self):

        """
        Check that Config_CAM.create_config properly sets a string value
        """

        #Set test value:
        testval = "test_val"

        #Create new config integer variable:
        self.test_config_cam.create_config("test_str", "test object description", testval)

        #Get value of new config variable:
        conf_val = self.test_config_cam.get_value("test_str")

        #Check that the test value matches what is expected:
        self.assertEqual(conf_val, testval)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++
    #check that "create_config" with a list works properly
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_create_config_list(self):

        """
        Check that Config_CAM.create_config properly sets a list
        """

        #Set test value:
        testval = [1,2]

        #Create new config integer variable:
        self.test_config_cam.create_config("test_list", "test object description", testval)

        #Get value of new config variable:
        conf_val = self.test_config_cam.get_value("test_list")

        #Check that the test value matches what is expected:
        self.assertEqual(conf_val, testval)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that the same config variable cannot be created twice
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_create_config_twice(self):

        """
        Check that Config_CAM.create_config raises the
        correct error if a config variable is created
        more than once.
        """

        #Set error message:
        ermsg = "ERROR:  The CAM config variable, 'test_int',"
        ermsg += " already exists! Any new config variable"
        ermsg += " must be given a different name"

        #Create new config variable:
        self.test_config_cam.create_config("test_int", "test object description", 5)

        #Expect "CamConfigValError":
        with self.assertRaises(CamConfigValError) as valerr:
            #Now attempt to create that same config variable again:
            self.test_config_cam.create_config("test_int", "new test object", 6)
        #end with

        #Check that error message matches what's expected:
        self.assertEqual(ermsg, str(valerr.exception))

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that a config variable must be a string, integer, or list
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_create_config_bad_type(self):

        """
        Check that Config_CAM.creaeconfig raises the
        correct error if a config variable is created
        with a non-supported type.
        """

        #Set error message:
        ermsg = "ERROR:  The input value for new CAM config variable,"
        ermsg += " 'test_dict', must be an integer, string, or list,"
        ermsg += " not <class 'dict'>"

        #Expect "CamConfigTypeError":
        with self.assertRaises(CamConfigTypeError) as typerr:
            #Now attempt to create a config variable with type dict:
            self.test_config_cam.create_config("test_dict", "test object description", {"x": 5})
        #end with

        #Check that error message matches what's expected:
        self.assertEqual(ermsg, str(typerr.exception))

    #+++++++++++++++++++++++++++++++++++++++++++++++
    #check that "print_config" method works properly
    #+++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_print_config_check(self):

        """
        Check that Config_CAM.print_config properly prints to the log.
        """

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

    #++++++++++++++++++++++++++++++++++++++++++++++++
    #check that the "print_all" method works properly
    #++++++++++++++++++++++++++++++++++++++++++++++++

    def test_print_all(self):

        """
        Check that Config_CAM.print_all properly prints to the log.
        """

        #Set expected log output:
        logmsg = ['DEBUG:print_config:CAM configuration variables:',
                  'DEBUG:print_config:-----------------------------',
                  'DEBUG:print_config:# Start date of model run.',
                  'DEBUG:print_config:ic_ymd = 101',
                  'DEBUG:print_config:# Flag to check if debug mode is enabled.',
                  'DEBUG:print_config:debug = 0',
                  'DEBUG:print_config:# Maximum number of columns assigned to a thread.',
                  'DEBUG:print_config:pcols = 16',
                  'DEBUG:print_config:# Maximum number of sub-columns in a column.',
                  'DEBUG:print_config:psubcols = 1',
                  'DEBUG:print_config:# Dynamics package, which is set by the horizontal grid specified.',
                  'DEBUG:print_config:dyn = none',
                  'DEBUG:print_config:# Horizontal grid specifier.',
                  'DEBUG:print_config:hgrid = null',
                  "DEBUG:print_config:# Comma-separated list of local directories containing\n#"+\
                  "    dynamics package source code.\n#"+\
                  "    These directories are assumed to be located under\n#"+\
                  "    src/dynamics, with a slash ('/') indicating directory hierarchy.",
                  "DEBUG:print_config:dyn_src_dirs = ['none']",
                  'DEBUG:print_config:# Number of unique latitude points in rectangular lat/lon grid.\n#'+\
                  '    Set to 1 (one) for unstructured grids.',
                  'DEBUG:print_config:nlat = null',
                  'DEBUG:print_config:# Number of unique longitude points in rectangular lat/lon grid.\n#'+\
                  '    Total number of columns for unstructured grids.',
                  'DEBUG:print_config:nlon = null',
                  'DEBUG:print_config:# Switch to turn on analytic initial conditions for the dynamics state: \n#'+\
                  '    0 => no \n#    1 => yes.',
                  'DEBUG:print_config:analytic_ic = 0',
                  'DEBUG:print_config:# Switch to use aquaplanet configuration: \n#'+\
                  '    0 => no \n#    1 => yes.',
                  'DEBUG:print_config:aquaplanet = 0',
                  "DEBUG:print_config:# A semicolon-separated list of physics suite definition file (SDF) names.\n#"+\
                  "    To specify the Kessler and Held-Suarez suites as \n#"+\
                  "    run time options, use '--physics-suites kessler;held_suarez_1994'.",
                  'DEBUG:print_config:physics_suites = mango;papaya',
                  'DEBUG:print_config:# Fortran kind used in dycore for type real.',
                  'DEBUG:print_config:dyn_kind = REAL64',
                  'DEBUG:print_config:# Fortran kind used in physics for type real.',
                  'DEBUG:print_config:phys_kind = REAL64', 'DEBUG:print_config:-----------------------------']

        #Create new logger for print_config test:
        print_log = logging.getLogger("print_config")

        #Create unittest log:
        with self.assertLogs(print_log, level='DEBUG') as cmplog:
            #Print variable information to logger via "print_config":
            self.test_config_cam.print_all(print_log)
        #end with

        self.assertEqual(cmplog.output, logmsg)

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
        ermsg = "ERROR:  Value provided for variable, 'pcols',"
        ermsg += " must be either an integer or a string."
        ermsg += "  Currently it is type <class 'float'>"

        #Expect "CamConfigTypeError":
        with self.assertRaises(CamConfigTypeError) as typerr:
            #Run "set_value" method on made-up variable name:
            self.test_config_cam.set_value("pcols", 5.0)

        #Check that error message matches what's expected:
        self.assertEqual(ermsg, str(typerr.exception))

    #++++++++++++++++++++++++++++++++++++
    #Check that add_cppdef works properly
    #++++++++++++++++++++++++++++++++++++

    def test_add_cppdef(self):

        """
        Check that adding a new CPP def works as expected.
        """

        #Set expected result:
        cppdef_result = ["-DTEST"]

        #Add new CPP definition:
        self.test_config_cam.add_cppdef("TEST")

        #Check result
        self.assertEqual(self.test_config_cam.cpp_defs, cppdef_result)

        #Set new expected result:
        cppdef_result_new = ["-DTEST", "-DTEST_NUM=100"]

        #Add another CPP definition, this time with a set value:
        self.test_config_cam.add_cppdef("TEST_NUM", 100)

        #Check result:
        self.assertEqual(self.test_config_cam.cpp_defs, cppdef_result_new)

    #++++++++++++++++++++++++++++++++++++++++++++++
    #Check that a duplicate cppdef creates an error
    #++++++++++++++++++++++++++++++++++++++++++++++

    def test_duplicate_cppdef(self):

        """
        Check that "add_cppdef" throws the proper error when
        a CPP definition is added twice.
        """

        #Set error message:
        ermsg = "ERROR: CPP definition 'TEST_CPPDEF' has already been set"

        #Add new CPP definition:
        self.test_config_cam.add_cppdef("TEST_CPPDEF")

        #Expect "CamConfigValError" to be raised:
        with self.assertRaises(CamConfigValError) as valerr:
            #Now try to add that CPP def again:
            self.test_config_cam.add_cppdef("TEST_CPPDEF")
        #end with

        #Check that error message matches what's expected:
        self.assertEqual(ermsg, str(valerr.exception))

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that a duplicate cppdef creates an error even if an equals
    #sign is present in the stored copy but not the passed variable
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_duplicate_cppdef_val_set(self):

        """
        Check that "add_cppdef" throws the proper error when a
        duplicate CPP def is added, even if the duplicate value
        does (or does not) have a value set.
        """

        #Set error message:
        ermsg = "ERROR: CPP definition 'NEW_TEST' has already been set"

        #Add CPP defs to list:
        self.test_config_cam.add_cppdef("NEW_TEST")
        self.test_config_cam.add_cppdef("NEW_TEST_VAL", "test_text")

        #Expect "CamConfigValError" to be raised:
        with self.assertRaises(CamConfigValError) as valerr:
            #Try adding "NEW_TEST", but with a value:
            self.test_config_cam.add_cppdef("NEW_TEST", 3)
        #end with

        #Check that error message matches what's expected:
        self.assertEqual(ermsg, str(valerr.exception))

        #Set new error message:
        ermsg = "ERROR: CPP definition 'NEW_TEST_VAL' has already been set"

        #Expect "CamConfigValError" to be raised:
        with self.assertRaises(CamConfigValError) as valerr:
            #Try adding "NEW_TEST_VAL", but without a value:
            self.test_config_cam.add_cppdef("NEW_TEST_VAL")
        #end with

        #Check that error message matches what's expected:
        self.assertEqual(ermsg, str(valerr.exception))

    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Check "generate_cam_src" missing "ccpp_framework" error-handling
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_config_gen_cam_src_ccpp_check(self):

        """
        Check that "generate_cam_src" throws the proper error
        if the "ccpp_framework" external directory doesn't exist.
        """

        #Set error message:
        ermsg = "ERROR: Unable to find CAM registry, registry.xml, in " +     \
            "[/another/made-up/path/SourceMods/src.cam, " +                   \
            "/a/third/made-up/path/src/data]"

        #Expect "CamConfigValError":
        with self.assertRaises(CamAutoGenError) as valerr:
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
        self.test_config_cam.set_value("physics_suites", "papaya")

        #Create (empty) namelist attribute dictionary:
        cam_nml_attr_dict = {}

        #Create fake 'atm_in' ParamGen dictionary:
        phys_nl_pg_dict = {'physics_suite': {'values': 'papaya'}}

        #Run ccpp_phys_set config method:
        self.test_config_cam.ccpp_phys_set(cam_nml_attr_dict, phys_nl_pg_dict)

        #Check that dictonary entries are correct:
        self.assertEqual(cam_nml_attr_dict["phys_suite"], "papaya")

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

        #Create fake 'atm_in' ParamGen dictionary:
        phys_nl_pg_dict = {'physics_suite': {'values': 'mango'}}

        #Run ccpp_phys_set config method:
        self.test_config_cam.ccpp_phys_set(cam_nml_attr_dict, phys_nl_pg_dict)

        #Check that dictonary entries are correct:
        self.assertEqual(cam_nml_attr_dict["phys_suite"], "mango")

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

        #Create fake 'atm_in' ParamGen dictionary:
        phys_nl_pg_dict = {'physics_suite': {'values': 'UNSET'}}

        #Set error message:
        ermsg  = "No 'physics_suite' variable is present in user_nl_cam.\n"
        ermsg += "This is required because more than one suite is listed\n"
        ermsg += "in CAM_CONFIG_OPTS: 'mango;papaya'"

        #Expect "CamConfigValError":
        with self.assertRaises(CamConfigValError) as valerr:
            #Run ccpp_phys_set config method, which should fail
            #due to missing "physics_suite" namelist variable:
            self.test_config_cam.ccpp_phys_set(cam_nml_attr_dict, phys_nl_pg_dict)

        #Check that error message matches what's expected:
        self.assertEqual(ermsg, str(valerr.exception))

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

        #Create fake 'atm_in' ParamGen dictionary:
        phys_nl_pg_dict = {'physics_suite': {'values': 'starfruit'}}

        #Set error message:
        ermsg  = "physics_suite specified in user_nl_cam, 'starfruit', doesn't match any suites\n"
        ermsg += "listed in CAM_CONFIG_OPTS: 'mango;papaya'"

        #Expect "CamConfigValError":
        with self.assertRaises(CamConfigValError) as valerr:
            #Run ccpp_phys_set config method, which should fail
            #due to a mis-match between the "physics_suite" namelist
            #variable and the physics suite options listed in the
            #physics_suites config variable:
            self.test_config_cam.ccpp_phys_set(cam_nml_attr_dict, phys_nl_pg_dict)

        #Check that error message matches what's expected:
        self.assertEqual(ermsg, str(valerr.exception))

        #-----
        #Same test, but with only one physics suite available:
        #-----

        #Set "new" physics_suites value with one physics suite:
        self.test_config_cam.set_value("physics_suites", "papaya")

        #Set new error message:
        ermsg  = "physics_suite specified in user_nl_cam, 'starfruit', does not\n"
        ermsg += "match the suite listed in CAM_CONFIG_OPTS: 'papaya'"

        #Expect "CamConfigValError":
        with self.assertRaises(CamConfigValError) as valerr:
            #Run ccpp_phys_set config method, which should fail
            #due to a mis-match between the "physics_suite" namelist
            #variable and the physics suite options listed in the
            #physics_suites config variable:
            self.test_config_cam.ccpp_phys_set(cam_nml_attr_dict, phys_nl_pg_dict)

        #Check that error message matches what's expected:
        self.assertEqual(ermsg, str(valerr.exception))

    #+++++++++++++++++++++++++++++++++++++++++++
    #Check that _add_xml_nml_file works properly
    #+++++++++++++++++++++++++++++++++++++++++++

    def test_xml_nml_file(self):

        """
        Check that "_add_xml_nml_file" works as expected.
        """

        #Create new, expected dictionary:
        xml_fil_list = OrderedDict()

        #Create path to "src/data" directory:
        data_path = os.path.join(CAM_CONF_DIR, os.pardir,
                                 "src", "data")

        #Create path to "src/cpl/nuopc" directory:
        cpl_path = os.path.join(CAM_CONF_DIR, os.pardir,
                                "src", "cpl", "nuopc")

        #These files will always be present:
        xml_fil_list['namelist_definition_cam.xml'] = os.path.join(CAM_CONF_DIR,
                                               'namelist_definition_cam.xml')
        xml_fil_list['namelist_definition_physconst.xml'] = os.path.join(data_path,
                                               'namelist_definition_physconst.xml')
        xml_fil_list['namelist_definition_ref_pres.xml'] = os.path.join(data_path,
                                               'namelist_definition_ref_pres.xml')
        xml_fil_list['namelist_definition_atm_stream_ndep.xml'] = os.path.join(cpl_path,
                                               'namelist_definition_atm_stream_ndep.xml')

        #This is the file being added:
        xml_fil_list['test_file.xml'] = '/fake/path/test_file.xml'

        #Run xml addition function:
        self.test_config_cam._add_xml_nml_file('/fake/path/', 'test_file.xml')

        #Check that the output matches the expected value:
        self.assertEqual(xml_fil_list, self.test_config_cam.xml_nml_def_files)

#################################################
#Run unit tests if this script is called directly
#################################################

if __name__ == "__main__":
    unittest.main()

############
#End of file
############
