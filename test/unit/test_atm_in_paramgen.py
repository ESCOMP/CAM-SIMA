"""
Python unit testing collection for the
AtmInParamGen namelist-generation class,
including its error-handling processes.

To run these unit tests, simply type:

python test_atm_in_paramgen.py

or (for more verbose output):

python test_atm_in_paramgen.py -v

which will currently run XXX tests, all of which should pass.
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
_CAM_ROOT_DIR = os.path.join(_TEST_DIR, os.pardir, os.pardir)
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
    #Check that a namelist can be built with a proper
    #XML namelist definition file:
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
    #Check that a namelist can be built with a proper
    #XML namelist definition file and corresponding
    #attributes/guards:
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
    #Check that a namelist can be built with a proper
    #XML namelist definition file and multiple
    #corresponding attributes/guards:
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
    #Check that a namelist with missing, required
    #XML elements/tags fails with the correct error
    #message
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
    #Check that ParamGen can properly concantenate
    #multiple XML namelist defition files into a single
    #atm_in namelist file
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

        # Get expected atm_in file:
        atm_in_output = os.path.join(_SAMPLES_DIR, "test_multi_xml_in")

        # Create the ParamGen objects:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)
        pg_ext  = AtmInParamGen.from_namelist_xml(extra_xml_fil)

        # Append the extra PG object to the other:
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
    #Check that attempting to append a ParamGen object
    #that itself was the combination of multiple XML
    #namelist defition files fails with the appropriate
    #error
    #++++++++++++++++++++++++++++++++++++++++++++++++

    def test_mutli_xml_append_multi(self):

        """
        Check that appending a ParamGen
        object that is itself a
        combination of multiple namelist
        definition file-derived ParamGen
        objects throws an error and that
        the error message is correct.
        """

        # Get XML file paths:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")
        extra_xml_fil = os.path.join(_SAMPLES_DIR, "test_extra_nml_def.xml")
        third_xml_fil = os.path.join(_SAMPLES_DIR, "test_extra_nml_same_group.xml")

        # Get expected atm_in file:
        atm_in_output = os.path.join(_SAMPLES_DIR, "test_multi_xml_in")

        # Create the ParamGen objects:
        pg_test  = AtmInParamGen.from_namelist_xml(xml_test_fil)
        pg_ext   = AtmInParamGen.from_namelist_xml(extra_xml_fil)
        pg_third = AtmInParamGen.from_namelist_xml(third_xml_fil)

        # Append the extra PG object to the other:
        pg_test.append_atm_in_pg(pg_ext)

        # Try to append the combined PG object to the third object:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_third.append_atm_in_pg(pg_test)

        # Check exception message:
        emsg = "ParamGen object being appended to another must"
        emsg += " be associated with only one namelist definition file."
        emsg += "\nInstead it is associated with the following files:\n"
        emsg += f"{xml_test_fil}\n{extra_xml_fil}"
        self.assertEqual(emsg, str(cerr.exception))

    #++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that trying to combine multiple XML namelist
    #defition files with the same namelist group
    #fails with the appropriate error
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
        emsg = f"Both\n'{xml_test_fil}'\nand\n'{extra_xml_fil}'\nhave"
        emsg += " the following conflicting namelist groups:\n"
        emsg += "bird_sounds_nl"
        self.assertEqual(emsg, str(cerr.exception))

    #++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that trying to combine multiple XML
    #namelist defition files with the same namelist
    #variable fails with the appropriate error
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
        emsg = f"Both\n'{xml_test_fil}'\nand\n'{extra_xml_fil}'\nhave"
        emsg += " the following conflicting namelist variables:\n"
        emsg += "duck_quack"
        self.assertEqual(emsg, str(cerr.exception))

    #++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that a user_nl_cam file properly modifies
    #an associated atm_in file
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

    #+++++++++++++++++++++++++++++++++++++++++++++++
    #Check that a user_nl_cam file with an un-closed
    #block comment fails with the appropriate error
    #+++++++++++++++++++++++++++++++++++++++++++++++

    def test_namelist_user_nl_unclosed_comment(self):

        """
        Check that a user_nl_cam file with an
        un-closed block comment throws an error
        and that the error message is correct.
        """

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")

        # Get "user_nl_cam" file path:
        user_nl_fil = os.path.join(_SAMPLES_DIR, "test_user_nl_bad_comment")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Attempt to append user_nl_cam file:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file(user_nl_fil)

        # Check exception message:
        emsg = f"Un-closed comment block!  Please check '{user_nl_fil}'"
        self.assertEqual(emsg, str(cerr.exception))

    #++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that a user_nl_cam file with an improperly
    #formatted namelist entry fails with the
    #appropriate error
    #+++++++++++++++++++++++++++++++++++++++++++++++

    def test_namelist_user_nl_bad_format_entry(self):

        """
        Check that a user_nl_cam file with a
        namelist entry that is missing an equals
        sign throws an error and that the error
        message is correct.
        """

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")

        # Get "user_nl_cam" file path:
        user_nl_fil = os.path.join(_SAMPLES_DIR, "test_user_nl_bad_format")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Attempt to append user_nl_cam file:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file(user_nl_fil)

        # Check exception message:
        emsg = f"Cannot parse the following line in '{user_nl_fil}' :\n'turkey_leg  22.7\n'"
        self.assertEqual(emsg, str(cerr.exception))

    #++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that a user_nl_cam file with a namelist
    #entry that is not currently present within the
    #AtmInParamGen object fails with the appropriate
    #error
    #+++++++++++++++++++++++++++++++++++++++++++++++

    def test_namelist_user_nl_undefined_entry(self):

        """
        Check that a user_nl_cam file with a
        namelist entry that has yet to be defined
        in an namelist definition file throws an
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
        emsg += f" Please double-check '{user_nl_fil}'."
        self.assertEqual(emsg, str(cerr.exception))

    #++++++++++++++++++++++++++++++++++++++++++++++++
    #Check that a user_nl_cam file with a repeated
    #namelist entry fails with the appropriate
    #error message.
    #+++++++++++++++++++++++++++++++++++++++++++++++

    def test_namelist_user_nl_double_entry(self):

        """
        Check that a user_nl_cam file with a
        namelist entry that has is included twice
        in the file throws an error and that the
        error message is correct.
        """

        # Get XML file path:
        xml_test_fil = os.path.join(_SAMPLES_DIR, "test_simple_nml_def.xml")

        # Get "user_nl_cam" file path:
        user_nl_fil = os.path.join(_SAMPLES_DIR, "test_user_nl_double_var")

        # Create the ParamGen object:
        pg_test = AtmInParamGen.from_namelist_xml(xml_test_fil)

        # Attempt to append user_nl_cam file:
        with self.assertRaises(AtmInParamGenError) as cerr:
            pg_test.append_user_nl_file(user_nl_fil)

        # Check exception message:
        emsg = f"Namelist variable 'turkey_leg' set more than once in '{user_nl_fil}'"
        emsg += "\nPlease set each variable only once."
        self.assertEqual(emsg, str(cerr.exception))

#################################################
#Run unit tests if this script is called directly
#################################################

if __name__ == "__main__":
    unittest.main()

############
#End of file
############
