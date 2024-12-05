"""
Python unit testing collection for the
AtmInParamGen namelist-generation class,
including its error-handling processes.

To run these unit tests, simply type:

python test_atm_in_paramgen.py

or (for more verbose output):

python test_atm_in_paramgen.py -v

which will currently run 23 tests, all of which should pass.
"""

#----------------------------------------
#Import required python libraries/modules:
#----------------------------------------
import logging
import os
import os.path
import sys
import glob
import filecmp
import xml.etree.ElementTree as ET

#Python unit-testing library:
import unittest

#Add directory to python path:
_TEST_DIR = os.path.abspath(os.path.dirname(__file__))
_CAM_ROOT_DIR = os.path.join(_TEST_DIR, os.pardir, os.pardir, os.pardir)
_CIME_CONF_DIR = os.path.abspath(os.path.join(_CAM_ROOT_DIR, "cime_config"))

_SAMPLES_DIR = os.path.join(os.path.join(_TEST_DIR, "sample_files"), "atm_in_files")
_PRE_TMP_DIR = os.path.join(_TEST_DIR, "tmp")
_TMP_DIR = os.path.join(_PRE_TMP_DIR, "atm_in_paramgen")

#Check for all necessary directories:
if not os.path.exists(_CIME_CONF_DIR):
    EMSG = "Cannot find cime_config directory where 'atm_in_paramgen.py' should be located."
    raise ImportError(EMSG)

if not os.path.exists(_SAMPLES_DIR):
    raise ImportError("Cannot find sample files directory")

#Add "cime_config" directory to python path:
sys.path.append(_CIME_CONF_DIR)

#Import CAM configure objects:
# pylint: disable=wrong-import-position
from atm_in_paramgen import AtmInParamGen
from atm_in_paramgen import AtmInParamGenError
# pylint: enable=wrong-import-position

#################
#Helper functions
#################

def remove_files(file_list):
    """Remove files in <file_list> if they exist"""
    for fpath in file_list:
        if os.path.exists(fpath):
            os.remove(fpath)
        # End if
    # End for

#################

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
            "CAM_CONFIG_OPTS" : "-dyn none --physics-suites something;otherthing",
            "COMP_ROOT_DIR_ATM" : "/a/third/made-up/path",
            "CAM_CPPDEFS" : "UNSET",
            "NTHRDS_ATM" : 1,
            "RUN_STARTDATE" : "101",
            "DIN_LOC_ROOT" : "/NOT/HERE",
            "feel_lucky" : 1 #For testing
            }

    def get_value(self, key):

        """
        Function used to return value
        from conf_opts dictionary,
        with the key as input.
        """

        if key in self.conf_opts:
            val = self.conf_opts[key]
        else:
            val = None
        #End if

        return val


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Main AtmInParamGen testing routine, used when script is run directly
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

class AtmInParamGenTestRoutine(unittest.TestCase):

    """
    Runs all AtmInParamGen tests, to ensure
    that the class (and error-handling) methods
    are running properly.
    """

    @classmethod
    def setUpClass(cls):

        """Clean output directory (tmp) before running tests"""
        #Does "tmp" directory exist?  If not then create it:
        if not os.path.exists(_PRE_TMP_DIR):
            os.mkdir(_PRE_TMP_DIR)

        #Now check if ""atm_in_paramgen"" directory exists:
        if not os.path.exists(_TMP_DIR):
            os.mkdir(_TMP_DIR)

        #Clear out all files:
        remove_files(glob.iglob(os.path.join(_TMP_DIR, '*.*')))

        #Run inherited setup method:
        super().setUpClass()

    #++++++++++++++++++++++++++++++++++++++++++++++++

    def test_namelist_from_xml(self):

        """
        Check that AtmInParamGen can properly parse
        an XML namelist defition file and generate
        the correct "atm_in" fortran namelist file.
        """

        # Create fake CIME case:
        fcase = FakeCase()

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")

        # Get expected atm_in file:
        atm_in_output = os.path.join(_SAMPLES_DIR, "test_simple_atm_in")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Set all ParamGen namelist values:
        pg_test.reduce_atm_in(fcase, {})

        # Create test atm_in namelist file name:
        test_output = os.path.join(_TMP_DIR, "test_in")

        # Create CAM namelist using CIME's nmlgen routine:
        pg_test.write(test_output)

        # Check that output file was written:
        amsg = f"{test_output} does not exist"
        self.assertTrue(os.path.exists(test_output), msg=amsg)

        # Check that output file matches expected file:
        amsg = f"{test_output} does not match {atm_in_output}"
        self.assertTrue(filecmp.cmp(test_output, atm_in_output, shallow=False), \
                        msg=amsg)

    #++++++++++++++++++++++++++++++++++++++++++++++++

    def test_namelist_from_xml_using_attrs(self):

        """
        Check that AtmInParamGen can properly parse
        an XML namelist defition file and generate
        the correct "atm_in" fortran namelist file
        when namelist attributes/guards are being used.
        """

        # Create fake CIME case:
        fcase = FakeCase()

        # Create namelist attribute dictionary:
        nml_attr_dict = {"bird" : "goose", "never_read" : "0"}

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")

        # Get expected atm_in file:
        atm_in_output = os.path.join(_SAMPLES_DIR, "test_attr_in")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Set all ParamGen namelist values:
        pg_test.reduce_atm_in(fcase, nml_attr_dict)

        # Create test atm_in namelist file name:
        test_output = os.path.join(_TMP_DIR, "test_attr_in")

        # Create CAM namelist using CIME's nmlgen routine:
        pg_test.write(test_output)

        # Check that output file was written:
        amsg = f"{test_output} does not exist"
        self.assertTrue(os.path.exists(test_output), msg=amsg)

        # Check that output file matches expected file:
        amsg = f"{test_output} does not match {atm_in_output}"
        self.assertTrue(filecmp.cmp(test_output, atm_in_output, shallow=False), \
                        msg=amsg)

    #++++++++++++++++++++++++++++++++++++++++++++++++

    def test_namelist_from_xml_using_multi_attrs(self):

        """
        Check that AtmInParamGen can properly parse
        an XML namelist defition file and generate
        the correct "atm_in" fortran namelist file
        when multiple namelist attributes/guards are
        being used.
        """

        # Create fake CIME case:
        fcase = FakeCase()

        # Create namelist attribute dictionary:
        nml_attr_dict = {"bird" : "goose", "never_read" : "1"}

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")

        # Get expected atm_in file:
        atm_in_output = os.path.join(_SAMPLES_DIR, "test_multi_attr_in")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Set all ParamGen namelist values:
        pg_test.reduce_atm_in(fcase, nml_attr_dict)

        # Create test atm_in namelist file name:
        test_output = os.path.join(_TMP_DIR, "test_multi_attr_in")

        # Create CAM namelist using CIME's nmlgen routine:
        pg_test.write(test_output)

        # Check that output file was written:
        amsg = f"{test_output} does not exist"
        self.assertTrue(os.path.exists(test_output), msg=amsg)

        # Check that output file matches expected file:
        amsg = f"{test_output} does not match {atm_in_output}"
        self.assertTrue(filecmp.cmp(test_output, atm_in_output, shallow=False), \
                        msg=amsg)

    #++++++++++++++++++++++++++++++++++++++++++++++++

    def test_namelist_xml_missing_elems(self):

        """
        Check that AtmInParamGen throws the correct
        error message when an XML namelist file
        is missing required namelist entry elements.
        """

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_missing_elems.xml")

        # Attempt to run ParamGen:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Check exception message
        emsg = "The XML namelist definition file:\n"
        emsg += f"{xml_test_fil}\n"
        emsg += "has namelist entries that are missing required elements.\n"
        emsg += "Those entries and missing elements are:\n"
        emsg += "duck_quack : type\n"
        emsg += "straw_into_gold : type, desc, category\n"

        self.assertEqual(emsg, str(cerr.exception))

    #++++++++++++++++++++++++++++++++++++++++++++++++

    def test_mutli_xml_namelist_defs(self):

        """
        Check that using multiple XML namelist
        definition files that are then appended
        together works as expected.
        """

        # Create fake CIME case:
        fcase = FakeCase()

        # Create namelist attribute dictionary:
        nml_attr_dict = {"bird" : "goose", "spaceship" : "x-wing"}

        # Get XML file paths:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")
        extra_xml_fil = os.path.join(_SAMPLES_DIR, "test_extra_nml_def.xml")
        third_xml_fil = os.path.join(_SAMPLES_DIR, "test_third_nml_def.xml")

        # Get expected atm_in file:
        atm_in_output = os.path.join(_SAMPLES_DIR, "test_multi_xml_in")

        # Create the ParamGen objects:
        pg_test  = AtmInParamGen.from_namelist_xml(xml_test_fil)
        pg_ext   = AtmInParamGen.from_namelist_xml(extra_xml_fil)
        pg_third = AtmInParamGen.from_namelist_xml(third_xml_fil)

        # Append two ParamGen objects together:
        pg_ext.append(pg_third)

        # Append the final PG objects together:
        pg_test.append_atm_in_pg(pg_ext)

        # Set all ParamGen namelist values:
        pg_test.reduce_atm_in(fcase, nml_attr_dict)

        # Create test atm_in namelist file name:
        test_output = os.path.join(_TMP_DIR, "test_multi_xml_in")

        # Create CAM namelist using CIME's nmlgen routine:
        pg_test.write(test_output)

        # Check that output file was written:
        amsg = f"{test_output} does not exist"
        self.assertTrue(os.path.exists(test_output), msg=amsg)

        # Check that output file matches expected file:
        amsg = f"{test_output} does not match {atm_in_output}"
        self.assertTrue(filecmp.cmp(test_output, atm_in_output, shallow=False), \
                        msg=amsg)

    #++++++++++++++++++++++++++++++++++++++++++++++++

    def test_mutli_xml_same_nl_group(self):

        """
        Check that using multiple XML namelist
        definition files that have the same
        namelist group throws an error and
        that the error message is correct.
        """

        # Get XML file paths:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")
        extra_xml_fil = os.path.join(_SAMPLES_DIR, "test_extra_nml_same_group.xml")

        # Create the ParamGen objects:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)
        pg_ext  = AtmInParamGen.from_namelist_xml(extra_xml_fil)

        # Append the extra PG object to the other:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_atm_in_pg(pg_ext)

        # Check exception message:
        emsg = f"Cannot append:\n'{extra_xml_fil}'\n"
        emsg += " The following namelist groups conflict with those in"
        emsg += f"\n'{xml_test_fil} :'\n"
        emsg += "bird_sounds_nl"
        self.assertEqual(emsg, str(cerr.exception))

    #++++++++++++++++++++++++++++++++++++++++++++++++

    def test_mutli_xml_same_nl_var(self):

        """
        Check that using multiple XML namelist
        definition files that have the same
        namelist entry id throws an error and
        that the error message is correct.
        """

        # Get XML file paths:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")
        extra_xml_fil = os.path.join(_SAMPLES_DIR, "test_extra_nml_same_var.xml")

        # Create the ParamGen objects:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)
        pg_ext  = AtmInParamGen.from_namelist_xml(extra_xml_fil)

        # Append the extra PG object to the other:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_atm_in_pg(pg_ext)

        # Check exception message:
        emsg = f"Cannot append:\n'{extra_xml_fil}'\n"
        emsg += " The following namelist variablesconflict with those in"
        emsg += f"\n'{xml_test_fil} :'\n"
        emsg += "duck_quack"
        self.assertEqual(emsg, str(cerr.exception))

    #++++++++++++++++++++++++++++++++++++++++++++++++

    def test_namelist_mode_from_user_nl_cam(self):

        """
        Check that AtmInParamGen can properly change
        the value of a namelist entry based on
        a provided user_nl_cam file.
        """

        # Create fake CIME case:
        fcase = FakeCase()

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")

        # Get "user_nl_cam" file path:
        user_nl_fil = os.path.join(_SAMPLES_DIR, "test_user_nl_simple")

        # Get expected atm_in file:
        atm_in_output = os.path.join(_SAMPLES_DIR, "test_user_in")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Append user_nl_cam file:
        pg_test.append_user_nl_file(user_nl_fil)

        # Set all ParamGen namelist values:
        pg_test.reduce_atm_in(fcase, {})

        # Create test atm_in namelist file name:
        test_output = os.path.join(_TMP_DIR, "test_user_in")

        # Create CAM namelist using CIME's nmlgen routine:
        pg_test.write(test_output)

        # Check that output file was written:
        amsg = f"{test_output} does not exist"
        self.assertTrue(os.path.exists(test_output), msg=amsg)

        # Check that output file matches expected file:
        amsg = f"{test_output} does not match {atm_in_output}"
        self.assertTrue(filecmp.cmp(test_output, atm_in_output, shallow=False), \
                        msg=amsg)

    #++++++++++++++++++++++++++++++++++++++++++++++++

    def test_namelist_user_nl_bad_format_entry(self):

        """
        Check that a user_nl_cam file with a
        namelist entry that is missing an equals
        sign or has an equals sign at the beginning of
        the line throws an error and that the error
        message is correct.
        """

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")

        # Get "user_nl_cam" file paths:
        user_nl_no_equals  = os.path.join(_SAMPLES_DIR, "test_user_nl_no_equals")
        user_nl_bad_equals = os.path.join(_SAMPLES_DIR, "test_user_nl_bad_equals")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Attempt to append user_nl_cam file with line that
        # contains no equals ('=') sign:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file(user_nl_no_equals)

        # Check exception message:
        emsg = f"Cannot parse the following line in '{user_nl_no_equals}' :\n'turkey_leg  22.7\n'"
        self.assertEqual(emsg, str(cerr.exception))

        # Now attempt to append user_nl_cam file with with line
        # that contains an equal('=') sign at the beginning of the line:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file(user_nl_bad_equals)

        # Check exception message:
        emsg = f"Cannot parse the following line in '{user_nl_bad_equals}'"
        emsg += """ :\n'=straw_into_gold = "Rapunzel"\n'"""
        self.assertEqual(emsg, str(cerr.exception))

    #++++++++++++++++++++++++++++++++++++++++++++++++

    def test_namelist_user_nl_undefined_entry(self):

        """
        Check that a user_nl_cam file with a
        namelist entry that has yet to be defined
        in a namelist definition file throws an
        error and that the error message is correct.
        """

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")

        # Get "user_nl_cam" file path:
        user_nl_fil = os.path.join(_SAMPLES_DIR, "test_user_nl_undefined_var")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Attempt to append user_nl_cam file:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file(user_nl_fil)

        # Check exception message:
        emsg = "Variable 'banana_peel' not found in any namelist definition files."
        emsg += " Please double-check 'user_nl_cam'."
        self.assertEqual(emsg, str(cerr.exception))

    #+++++++++++++++++++++++++++++++++++++++++++++++

    def test_namelist_user_nl_double_entry(self):

        """
        Check that a user_nl_cam file with a
        namelist entry that has been included twice
        in the file throws an error and that the
        error message is correct. Also check that
        the "allow_dupl" flag works as expected
        and that no error is thrown, and the
        correct output is written.
        """

        # Create fake CIME case:
        fcase = FakeCase()

        # Get expected atm_in file:
        atm_in_output = os.path.join(_SAMPLES_DIR, "test_nl_duplicate_atm_in")

        # Get XML file paths:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")
        extra_xml_fil = os.path.join(_SAMPLES_DIR, "test_extra_nml_def.xml")

        # Get "user_nl_cam" file path:
        user_nl_fil = os.path.join(_SAMPLES_DIR, "test_user_nl_dupl_var")

        # Get "user_nl_cam" file that allow duplicates:
        user_nl_fil_allow_dupl = os.path.join(_SAMPLES_DIR, "test_user_nl_allow_dupl_var")

        # Create the ParamGen objects:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)
        pg_ext  = AtmInParamGen.from_namelist_xml(extra_xml_fil)

        # Append ParamGenObjects together:
        pg_test.append_atm_in_pg(pg_ext)

        # Attempt to append user_nl_cam file:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file(user_nl_fil)

        # Check exception message:
        emsg = f"Namelist variable 'turkey_leg' set more than once in '{user_nl_fil}'"
        emsg += "\nPlease set each variable only once."
        self.assertEqual(emsg, str(cerr.exception))

        # Now attempt to append user_nl_cam file, but
        # with duplicates allowed:
        pg_test.append_user_nl_file(user_nl_fil_allow_dupl)

        # Set all ParamGen namelist values:
        pg_test.reduce_atm_in(fcase, {})

        # Create test atm_in namelist file name:
        test_output = os.path.join(_TMP_DIR, "test_nl_duplicate_atm_in")

        # Create CAM namelist using CIME's nmlgen routine:
        pg_test.write(test_output)

        # Check that output file was written:
        amsg = f"{test_output} does not exist"
        self.assertTrue(os.path.exists(test_output), msg=amsg)

        # Check that output file matches expected file:
        amsg = f"{test_output} does not match {atm_in_output}"
        self.assertTrue(filecmp.cmp(test_output, atm_in_output, shallow=False), \
                        msg=amsg)

    #+++++++++++++++++++++++++++++++++++++++++++++++

    def test_user_nl_non_array_comma(self):

        """
        Check that a user_nl_cam file with
        a namelist entry that is not an array,
        followed by a comma-leading section of
        text, fails with the appropriate error.
        """
        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Create temporary user_nl_cam file:
        with open("user_nl_tmp", "w", encoding='utf-8') as nl_file:
            nl_file.write("duck_quack = 1 \n")
            nl_file.write(", .false.")
        # End with

        # Attempt to append user_nl_cam file:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file("user_nl_tmp")
        # End with

        # Check exception message:
        emsg  = "Line number 2 in 'user_nl_cam'"
        emsg += " starts with a comma (,) but the"
        emsg += " associated namelist variable is not an array."
        self.assertEqual(emsg, str(cerr.exception))

        #Remove temporary user_nl_cam file
        os.remove("user_nl_tmp")

    #+++++++++++++++++++++++++++++++++++++++++++++++

    def test_user_nl_double_comma(self):

        """
        Check that a user_nl_cam file with a
        namelist entry that is an array, but
        that has both a trailing and leading
        comma separated only with a newline
        and blank spaces fails with the
        appropriate error.
        """

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Create temporary user_nl_cam file:
        with open("user_nl_tmp", "w", encoding='utf-8') as nl_file:
            nl_file.write("marx_bros = 'mario', 'luigi',\n")
            nl_file.write(", 'wario', 'karl'")
        # End with

        # Attempt to append user_nl_cam file:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file("user_nl_tmp")
        # End with

        # Check exception message:
        emsg = "Line number 2 in 'user_nl_cam'"
        emsg += " starts with a comma (,) but the"
        emsg += " previous line ended with a comma."
        emsg += "\nPlease remove one of the commas."
        self.assertEqual(emsg, str(cerr.exception))

        #Remove temporary user_nl_cam file
        os.remove("user_nl_tmp")

    #+++++++++++++++++++++++++++++++++++++++++++++++

    def test_user_nl_trailing_comma(self):

        """
        Check that a user_nl_cam file with a
        namelist entry that is an array, but
        ends with a trailing comma, fails
        with the appropriate error.
        """

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Create temporary user_nl_cam file:
        with open("user_nl_tmp", "w", encoding='utf-8') as nl_file:
            nl_file.write("marx_bros = 'mario', 'luigi',\n")
            nl_file.write("duck_quack = .false.")
        # End with

        # Attempt to append user_nl_cam file:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file("user_nl_tmp")
        # End with

        # Check exception message:
        emsg = "Line number 2 in 'user_nl_cam' appears"
        emsg += " to be starting a new namelist entry,\nbut"
        emsg += " the previous entry has a trailing comma (,).  Please fix."
        self.assertEqual(emsg, str(cerr.exception))

        #Remove temporary user_nl_cam file
        os.remove("user_nl_tmp")

    #+++++++++++++++++++++++++++++++++++++++++++++++

    def test_user_nl_non_array_dims(self):

        """
        Check that a user_nl_cam file with
        a namelist variable that is not
        an array, but is given specific array
        dimensions, fails with the appropriate
        error.
        """

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Create temporary user_nl_cam file:
        with open("user_nl_tmp", "w", encoding='utf-8') as nl_file:
            nl_file.write("duck_quack(5:6) = .true.")
        # End with

        # Attempt to append user_nl_cam file:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file("user_nl_tmp")
        # End with

        # Check exception message:
        emsg = "Variable 'duck_quack' is not an array, but array"
        emsg += " dimensions are being specified in 'user_nl_cam'."
        self.assertEqual(emsg, str(cerr.exception))

        #Remove temporary user_nl_cam file
        os.remove("user_nl_tmp")

    #+++++++++++++++++++++++++++++++++++++++++++++++

    def test_user_nl_too_many_array_dims(self):

        """
        Check that a user_nl_cam file with
        a namelist variable that is an array,
        but that is listed with too many dimensions,
        fails with the appropriate error.
        """

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Create temporary user_nl_cam file:
        with open("user_nl_tmp", "w", encoding='utf-8') as nl_file:
            nl_file.write("marx_bros(5,8) = .true.")
        # End with

        # Attempt to append user_nl_cam file:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file("user_nl_tmp")
        # End with

        # Check exception message:
        emsg = "Variable 'marx_bros' has 2 dimensions"
        emsg += " used in 'user_nl_cam', but is defined"
        emsg += f" to have 1 dimension."
        self.assertEqual(emsg, str(cerr.exception))

        #Remove temporary user_nl_cam file
        os.remove("user_nl_tmp")

    #+++++++++++++++++++++++++++++++++++++++++++++++

    def test_user_nl_too_few_array_dims(self):

        """
        Check that a user_nl_cam file with
        a namelist variable that is an array,
        but that is listed with too few dimensions,
        fails with the appropriate error.
        """

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_third_nml_def.xml")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Create temporary user_nl_cam file:
        with open("user_nl_tmp", "w", encoding='utf-8') as nl_file:
            nl_file.write("body_snatchers(1) = Alien!!!")
        # End with

        # Attempt to append user_nl_cam file:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file("user_nl_tmp")
        # End with

        # Check exception message:
        emsg = "Variable 'body_snatchers' has 1 dimension"
        emsg += " used in 'user_nl_cam', but is defined"
        emsg += f" to have 2 dimensions."
        self.assertEqual(emsg, str(cerr.exception))

        #Remove temporary user_nl_cam file
        os.remove("user_nl_tmp")

    #+++++++++++++++++++++++++++++++++++++++++++++++

    def test_user_nl_no_stride_val(self):

        """
        Check that a user_nl_cam file with
        a namelist variable that is an array,
        and that has two colons for a given
        array dimension, but no specified
        stride value, fails with the appropriate
        error.
        """

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Create temporary user_nl_cam file:
        with open("user_nl_tmp", "w", encoding='utf-8') as nl_file:
            nl_file.write("marx_bros(2::) = 'Gummo'")
        # End with

        # Attempt to append user_nl_cam file:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file("user_nl_tmp")
        # End with

        # Check exception message:
        emsg = "Two colons were provided for variable 'marx_bros'"
        emsg += " in 'user_nl_cam', but no stride value was provided."
        emsg += "\nPlease provide either a stride value, or remove the "
        emsg += "extra colon."
        self.assertEqual(emsg, str(cerr.exception))

        #Remove temporary user_nl_cam file
        os.remove("user_nl_tmp")

    #+++++++++++++++++++++++++++++++++++++++++++++++

    def test_user_nl_three_colons(self):

        """
        Check that a user_nl_cam file
        with a namelist variable that is
        an array, and that has three colons
        for a given array dimension, fails
        with the appropriate error.
        """

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Create temporary user_nl_cam file:
        with open("user_nl_tmp", "w", encoding='utf-8') as nl_file:
            nl_file.write("marx_bros(1:2:3:4) = 'Gummo'")
        # End with

        # Attempt to append user_nl_cam file:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file("user_nl_tmp")
        # End with

        # Check exception message:
        emsg = f"Variable 'marx_bros' has 3 colons (:) "
        emsg += "listed in its dimension indexing in 'user_nl_cam'."
        emsg += " This is not a valid Fortran array section specification."
        self.assertEqual(emsg, str(cerr.exception))

        #Remove temporary user_nl_cam file
        os.remove("user_nl_tmp")

    #+++++++++++++++++++++++++++++++++++++++++++++++

    def test_user_nl_colon_dupl(self):

        """
        Check that a user_nl_cam file
        with a namelist variable that is
        an array with a colon dimension,
        and that is listed twice in the file,
        fails with the appropriate error.
        """

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Create temporary user_nl_cam file:
        with open("user_nl_tmp", "w", encoding='utf-8') as nl_file:
            nl_file.write("marx_bros(:) = 'Gummo'\n")
            nl_file.write("marx_bros(:) = 'Groucho'")
        # End with

        # Attempt to append user_nl_cam file:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file("user_nl_tmp")
        # End with

        # Check exception message:
        emsg = "Variable 'marx_bros' has all values"
        emsg += " being set multiple times for"
        emsg += " dimension 1."
        self.assertEqual(emsg, str(cerr.exception))

        #Remove temporary user_nl_cam file
        os.remove("user_nl_tmp")

    #+++++++++++++++++++++++++++++++++++++++++++++++++

    def test_user_nl_arr_index_dupl(self):

        """
        Check that a user_nl_cam file with
        a namelist variable that has an
        array index that is specified
        twice (i.e. not with just a colon)
        fails with the appropriate error.
        """

        # Get XML file paths:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")
        extra_xml_fil = os.path.join(_SAMPLES_DIR, "test_extra_nml_def.xml")

        # Create the ParamGen objects:
        pg_test  = AtmInParamGen.from_namelist_xml(xml_test_fil)
        pg_ext   = AtmInParamGen.from_namelist_xml(extra_xml_fil)

        # Append the PG objects together:
        pg_test.append_atm_in_pg(pg_ext)

        # Create temporary user_nl_cam file:
        with open("user_nl_tmp", "w", encoding='utf-8') as nl_file:
            nl_file.write("marx_bros(:) = 'Gummo'\n")
            nl_file.write("marx_bros(4) = 'Groucho'\n\n")
            nl_file.write("lets_ask_computer(:) = 2\n")
            nl_file.write("lets_ask_computer(7) = 3\n")
            nl_file.write("lets_ask_computer(7) = 4 !Should fail here")
        # End with

        # Attempt to append user_nl_cam file:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file("user_nl_tmp")
        # End with

        # Check exception message:
        emsg = "Variable 'lets_ask_computer' has values"
        emsg += " at the following indices being"
        emsg += " set multiple times for dimension"
        emsg += " (1) :\n"
        emsg += "7"
        self.assertEqual(emsg, str(cerr.exception))

        #Remove temporary user_nl_cam file
        os.remove("user_nl_tmp")

    #+++++++++++++++++++++++++++++++++++++++++++++++++

    def test_user_nl_arr_index_range_dupl(self):

        """
        check that a user_nl_cam file with a
        namelist variable that has an array
        index range (e.g. min:max) that covers
        a previous index range for that same
        variable fails with the appropriate error
        """

        # Get XML file paths:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")
        extra_xml_fil = os.path.join(_SAMPLES_DIR, "test_extra_nml_def.xml")

        # Create the ParamGen objects:
        pg_test  = AtmInParamGen.from_namelist_xml(xml_test_fil)
        pg_ext   = AtmInParamGen.from_namelist_xml(extra_xml_fil)

        # Append the PG objects together:
        pg_test.append_atm_in_pg(pg_ext)

        # Create temporary user_nl_cam file:
        with open("user_nl_tmp", "w", encoding='utf-8') as nl_file:
            nl_file.write('marx_bros = "Gummo"\n')
            nl_file.write('marx_bros(2:) = "Groucho"\n')
            nl_file.write('marx_bros(1) = "Karl"\n')
            nl_file.write('lets_ask_computer = 2\n')
            nl_file.write('lets_ask_computer(5:20) = 3\n')
            nl_file.write('lets_ask_computer(4) = 4\n')
            nl_file.write("lets_ask_computer(:11) = 5 !Should fail here")
        # End with

        # Attempt to append user_nl_cam file:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file("user_nl_tmp")
        # End with

        # Check exception message:
        emsg = "Variable 'lets_ask_computer' has values"
        emsg += " at the following indices being"
        emsg += " set multiple times for dimension"
        emsg += " (1) :\n"
        emsg += "4, 5, 6, 7, 8, 9, 10, 11"
        self.assertEqual(emsg, str(cerr.exception))

        #Remove temporary user_nl_cam file
        os.remove("user_nl_tmp")

    #+++++++++++++++++++++++++++++++++++++++++++++++++

    def test_user_nl_complex_array_dims(self):

        """
        Check that a user_nl_cam file with
        a namelist variable that is an array,
        using complex array syntax produces the correct atm_in file
        """

        # Create fake CIME case:
        fcase = FakeCase()

        # Get expected atm_in file:
        atm_in_output = os.path.join(_SAMPLES_DIR, "test_cmplx_array_atm_in")

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_third_nml_def.xml")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Create temporary user_nl_cam file:
        with open("user_nl_tmp", "w", encoding='utf-8') as nl_file:
            nl_file.write("swedish_chef(15:10:-2) = 'bork', 'bork', 'bork'\n")
            nl_file.write("swedish_chef(7:9:2) = 'not bork', 'not bork'\n")
            nl_file.write("swedish_chef(1) = ''\n")
        # End with

        # Attempt to append user_nl_cam file:
        pg_test.append_user_nl_file("user_nl_tmp")

        # Set all ParamGen namelist values:
        pg_test.reduce_atm_in(fcase, {})

        # Create test atm_in namelist file name:
        test_output = os.path.join(_TMP_DIR, "test_cmplx_array_atm_in")

        # Create CAM namelist using CIME's nmlgen routine:
        pg_test.write(test_output)

        # Check that output file was written:
        amsg = f"{test_output} does not exist"
        self.assertTrue(os.path.exists(test_output), msg=amsg)

        # Check that output file matches expected file:
        amsg = f"{test_output} does not match {atm_in_output}"
        self.assertTrue(filecmp.cmp(test_output, atm_in_output, shallow=False), \
                        msg=amsg)

        #Remove temporary user_nl_cam file
        os.remove("user_nl_tmp")

#################################################
#Run unit tests if this script is called directly
#################################################

if __name__ == "__main__":
    unittest.main()

############
#End of file
############
