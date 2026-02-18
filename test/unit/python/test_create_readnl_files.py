#! /usr/bin/env python3
#-----------------------------------------------------------------------
# Description:  Contains unit tests for testing CAM's create_readnl_files.py
#               module to turn scheme namelist XML files into
#               namelist-reading code and metadata.
#
# Assumptions:
#
# Command line arguments: none
#
# Usage: python3 "create_readnl_files_tests.py"     # run the unit tests
#-----------------------------------------------------------------------

"""Test gen_namelist_files in create_readnl_files.py"""

import filecmp
import glob
import os
import logging
import sys
import unittest
import xml.etree.ElementTree as ET

_TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_CAM_ROOT = os.path.abspath(os.path.join(_TEST_DIR, os.pardir, os.pardir, os.pardir))
_CCPP_DIR = os.path.join(_CAM_ROOT, "ccpp_framework", "scripts")
_CIME_CONFIG_DIR = os.path.join(_CAM_ROOT, "cime_config")
_XML_SAMPLES_DIR = os.path.join(_TEST_DIR, "sample_files")
_NL_SAMPLES_DIR = os.path.join(_XML_SAMPLES_DIR, "namelist_files")
_PRE_TMP_DIR = os.path.join(_TEST_DIR, "tmp")
_TMP_DIR = os.path.join(_PRE_TMP_DIR, "namelist_files")
_SRC_MOD_DIR = os.path.join(_PRE_TMP_DIR, "SourceMods")

#Check for all necessary directories:
if not os.path.exists(_CCPP_DIR):
    EMSG = "Cannot find CCPP framework directory where 'ccpp_capgen.py' should be located."
    raise ImportError(EMSG)

if not os.path.exists(_CIME_CONFIG_DIR):
    EMSG = "Cannot find cime_config directory where 'create_readnl_files.py' should be located."
    raise ImportError(EMSG)

if not os.path.exists(_XML_SAMPLES_DIR):
    raise ImportError("Cannot find sample files directory")

if not os.path.exists(_NL_SAMPLES_DIR):
    raise ImportError("Cannot find 'namelist_files' sample files directory")

#Add CCPP framework directory to python path to
#import capgen code generator:
sys.path.append(_CCPP_DIR)

#Add registry directory to python path to import
#registry and 'phys_init' code generators:
sys.path.append(_CIME_CONFIG_DIR)

# pylint: disable=wrong-import-position
from create_readnl_files import gen_namelist_files, NamelistFiles, NamelistError
from create_readnl_files import NLVar
from parse_tools import ParseInternalError  #CCPP error class
# pylint: enable=wrong-import-position

###############################################################################
def remove_files(file_list):
###############################################################################
    """Remove files in <file_list> if they exist"""
    if isinstance(file_list, str):
        file_list = [file_list]
    # end if
    for fpath in file_list:
        if os.path.exists(fpath):
            os.remove(fpath)
        # End if
    # End for

###############################################################################

class CreateReadnlFilesTest(unittest.TestCase):

    """Tests for `gen_namelist_files`."""

    @classmethod
    def setUpClass(cls):
        """Clean output directory (tmp) before running tests"""
        #Does "tmp" directory exist?  If not then create it:
        if not os.path.exists(_PRE_TMP_DIR):
            os.mkdir(_PRE_TMP_DIR)
        #Now check if "write_init_files" directory exists:
        if not os.path.exists(_TMP_DIR):
            os.mkdir(_TMP_DIR)
        #Finally check if "SourceMods" directory exists:
        if not os.path.exists(_SRC_MOD_DIR):
            os.mkdir(_SRC_MOD_DIR)

        #Clear out all files:
        remove_files(glob.iglob(os.path.join(_TMP_DIR, '*.*')))

        #Run inherited setup method:
        super().setUpClass()

    def test_nlvar_valid_xml(self):
        """
        Test that the "NLVar" class can correctly
        determine that a namelist xml variable is valid.
        """

        #Create a "good" xml namelist entry:
        good_xml_entry = ET.fromstring("""<entry id="green">
        <type>integer</type><category>banana</category><group>banana_nl</group>
        <standard_name>banana_index</standard_name><units>1</units>
        <desc>Variable to specify banana</desc>
        <values><value>2</value></values></entry>""")

        #Create NLVar object:
        nlvar_obj = NLVar(good_xml_entry)

        #Check that the object recgonizes the input as valid:
        self.assertTrue(nlvar_obj.is_valid())

    def test_nlvar_no_stdname_xml(self):
        """
        Test that the "NLVar" class can correctly
        determine that a namelist xml variable is
        missing a standard name and is thus invalid.
        """

        #Create an xml namelist entry with a missing
        #standard name:
        no_stdname_xml_entry = ET.fromstring("""<entry id="spotted">
        <type>integer</type><category>banana</category><group>banana_nl</group>
        <units>1</units><desc>Variable to specify banana</desc>
        <values><value>2</value></values></entry>""")

        #Create NLVar object:
        nlvar_obj = NLVar(no_stdname_xml_entry)

        #Check that the object recognizes the input as invalid:
        self.assertFalse(nlvar_obj.is_valid())

        #Check that the object recognizes that the standard name
        #is missing:
        self.assertEqual(nlvar_obj.missing(), 'standard_name')

    def test_nlvar_no_group_xml(self):
        """
        Test that the "NLVar" class can correctly
        determine that a namelist xml variable is
        missing a group name and is thus invalid.
        """

        #Create an xml namelist entry with a missing group:
        no_group_xml_entry = ET.fromstring("""<entry id="brown">
        <type>integer</type><category>banana</category>
        <standard_name>banana_index</standard_name><units>1</units>
        <desc>Variable to specify banana</desc>
        <values><value>2</value></values></entry>""")

        #Create NLVar object:
        nlvar_obj = NLVar(no_group_xml_entry)

        #Check that the object recognizes the input as invalid:
        self.assertFalse(nlvar_obj.is_valid())

        #Check that the object recognizes that the group name
        #is missing:
        self.assertEqual(nlvar_obj.missing(), 'group')

    def test_nlvar_no_units_xml(self):
        """
        Test that the "NLVar" class can correctly
        determine that a namelist xml variable is
        missing units and is thus invalid.
        """

        #Create an xml namelist entry with missing units:
        no_units_xml_entry = ET.fromstring("""<entry id="black">
        <type>integer</type><category>banana</category><group>banana_nl</group>
        <standard_name>banana_index</standard_name>
        <desc>Variable to specify banana</desc>
        <values><value>2</value></values></entry>""")

        #Create NLVar object:
        nlvar_obj = NLVar(no_units_xml_entry)

        #Check that the object recognizes the input as invalid:
        self.assertFalse(nlvar_obj.is_valid())

        #Check that the object recognizes that the units
        #are missing:
        self.assertEqual(nlvar_obj.missing(), 'units')

    def test_nlvar_bad_char_xml(self):
        """
        Test that the "NLVar" class can correctly
        determine that a namelist xml variable has
        a missing character length specifier, and
        is thus invalid.
        """

        #Set expected error message:
        ermsg = "Bad 'char' type for 'spotted', must specify length"

        #Create an xml namelist entry with a bad character type
        #(i.e. no length specification):
        bad_char_xml_entry = ET.fromstring("""<entry id="spotted">
        <type>char</type><category>banana</category><group>banana_nl</group>
        <standard_name>banana_index</standard_name><units>1</units>
        <desc>Variable to specify banana</desc>
        <values><value>2</value></values></entry>""")

        #Create NLVar object:
        nlvar_obj = NLVar(bad_char_xml_entry)

        #Check that the object recognizes the input as invalid:
        self.assertFalse(nlvar_obj.is_valid())

        #Check that the object recognizes that the character
        #length is missing:
        self.assertEqual(nlvar_obj.missing(), ermsg)

    def test_nlvar_bad_type_xml(self):
        """
        Test that the "NLVar" class can correctly
        determine that a namelist xml variable has
        an un-recognized variable type specifier, and
        is thus invalid.
        """

        #Set expected error message:
        ermsg = "Unknown variable type, 'orange'"

        #Create an xml namelist entry with a bad variable type:
        bad_type_xml_entry = ET.fromstring("""<entry id="mushy">
        <type>orange</type><category>banana</category><group>banana_nl</group>
        <standard_name>banana_index</standard_name><units>1</units>
        <desc>Variable to specify banana</desc>
        <values><value>2</value></values></entry>""")

        #Create NLVar object:
        nlvar_obj = NLVar(bad_type_xml_entry)

        #Check that the object recognizes the input as invalid:
        self.assertFalse(nlvar_obj.is_valid())

        #Check that the object recognizes that the variable
        #type was bad:
        self.assertEqual(nlvar_obj.missing(), ermsg)

    def test_nlvar_no_var_type_xml(self):
        """
        Test that the "NLVar" class can correctly
        throw an "ParseInternalError" message if
        the "type" XML tag is missing for the given
        namelist variable.
        """

        #Set expected error message:
        ermsg = "ERROR: No type for green"

        #Create a an xml namelist entry with a missing type tag:
        missing_type_xml_entry = ET.fromstring("""<entry id="green">
        <category>banana</category><group>banana_nl</group>
        <standard_name>banana_index</standard_name><units>1</units>
        <desc>Variable to specify banana</desc>
        <values><value>2</value></values></entry>""")

        #Expect a parsing error to be raised:
        with self.assertRaises(ParseInternalError) as perr:
            #Attempt to create NLVar object:
            nlvar_obj = NLVar(missing_type_xml_entry)
        #End with

        #Check that the raised error message matches what is expected:
        self.assertEqual(str(perr.exception), ermsg)

    def test_nlvar_multi_bad_xml(self):
        """
        Test that the "NLVar" class can correctly
        determine that a namelist xml variable has
        multiple missing required tags, and is thus
        invalid.
        """

        #Set expected error message:
        ermsg = "group, standard_name, and units"

        #Create an xml namelist entry with a bad variable type:
        multi_bad_xml_entry = ET.fromstring("""<entry id="green">
        <type>integer</type><category>banana</category>
        <desc>Variable to specify banana</desc>
        <values><value>2</value></values></entry>""")

        #Create NLVar object:
        nlvar_obj = NLVar(multi_bad_xml_entry)

        #Check that the object recognizes the input as invalid:
        self.assertFalse(nlvar_obj.is_valid())

        #Check that the object recognizes that the variable
        #type was bad:
        self.assertEqual(nlvar_obj.missing(), ermsg)

    def test_nlvar_too_many_dims_xml(self):
        """
        Test that the "NLVar" class can correctly
        determine that a namelist xml variable has
        too many array dimensions and triggers
        the array limit error.
        """

        #Set expected error message:
        ermsg = "Namelist variable 'apple_bananas' "
        ermsg += "has 11 dimensions,\n"
        ermsg += "which is more than the limit "
        ermsg += "of 8 dimensions that is "
        ermsg += "currently supported."

        #Create an xml namelist entry with too many
        #array dimensions:
        too_many_dims_xml_entry = ET.fromstring("""<entry id="apple_bananas">
        <type>integer(2,3,4,5,6,7,8,9,10,11,12)</type>
        <category>banana</category><group>banana_nl</group>
        <units>1</units><desc>Fancy Hawaii bananas</desc>
        <values><value>2</value></values></entry>""")

        #Expect "IndexError":
        with self.assertRaises(IndexError) as ixerr:
            #Try to create an "NLVar" object:
            nlvar_obj = NLVar(too_many_dims_xml_entry)

        #Check that error message matches what's expected:
        self.assertEqual(ermsg, str(ixerr.exception))

    def test_single_namelist_def(self):
        """
        Test that the 'gen_namelist_files' function
        generates the correct fortran code and metadata given
        a namelist XML definition file
        """

        # Setup runtime inputs:
        scheme_name = "banana"
        out_source_name = scheme_name + "_namelist"
        xml_file = os.path.join(_XML_SAMPLES_DIR, out_source_name + ".xml")

        # Create local logger:
        logger = logging.getLogger("test_single_namelist_def")

        # Expected output files
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        reader_mod = "cam_ccpp_scheme_namelists_single_def"
        reader_source = os.path.join(_TMP_DIR, reader_mod + ".F90")

        # Check output files
        out_source_check = os.path.join(_NL_SAMPLES_DIR,
                                        out_source_name + '.F90')
        out_meta_check = os.path.join(_NL_SAMPLES_DIR,
                                      out_source_name + '.meta')
        reader_source_check = os.path.join(_NL_SAMPLES_DIR, reader_mod + ".F90")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, reader_source])

        # Create the namelist object
        args = ['--namelist-file-arg', f"{scheme_name}:{xml_file}",
                '--namelist-read-mod', reader_mod]
        namelist_obj = gen_namelist_files(args, _TMP_DIR, logger)

        # Check return obj:
        amsg = f"Test failure: bad return type, {type(namelist_obj)}"
        self.assertTrue(isinstance(namelist_obj, NamelistFiles), msg=amsg)

        # Make sure each output file was created:
        amsg = f"{out_source} does not exist"
        self.assertTrue(os.path.exists(out_source), msg=amsg)
        amsg = f"{out_meta} does not exist"
        self.assertTrue(os.path.exists(out_meta), msg=amsg)
        amsg = f"{reader_source} does not exist"
        self.assertTrue(os.path.exists(reader_source), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{out_source} does not match {out_source_check}"
        self.assertTrue(filecmp.cmp(out_source, out_source_check,
                                    shallow=False), msg=amsg)
        amsg = f"{out_meta} does not match {out_meta_check}"
        self.assertTrue(filecmp.cmp(out_meta, out_meta_check, shallow=False),
                        msg=amsg)
        amsg = f"{reader_source} does not match {reader_source_check}"
        self.assertTrue(filecmp.cmp(reader_source, reader_source_check,
                                    shallow=False), msg=amsg)

    def test_double_namelist_def(self):
        """
        Test that the 'gen_namelist_files' function
        generates the correct fortran code and metadata given
        two namelist XML definition files
        """

        # Setup runtime inputs:
        scheme_names = ["banana", "kumquat"]
        out_source_names = [x + "_namelist" for x in scheme_names]
        xml_files = [os.path.join(_XML_SAMPLES_DIR, x + ".xml")
                     for x in out_source_names]

        # Create local logger:
        logger = logging.getLogger("test_double_namelist_def")

        # Expected output files
        out_sources = [os.path.join(_TMP_DIR, x + '.F90')
                       for x in out_source_names]
        out_metas = [os.path.join(_TMP_DIR, x + '.meta')
                     for x in out_source_names]
        reader_mod = "cam_ccpp_scheme_namelists_double_def"
        reader_source = os.path.join(_TMP_DIR, reader_mod + ".F90")

        # Check output files
        out_source_checks = [os.path.join(_NL_SAMPLES_DIR, x + '.F90')
                             for x in out_source_names]
        out_meta_checks = [os.path.join(_NL_SAMPLES_DIR, x + '.meta')
                           for x in out_source_names]
        reader_source_check = os.path.join(_NL_SAMPLES_DIR, reader_mod + ".F90")

        # Clear all temporary output files:
        remove_files(out_sources)
        remove_files(out_metas)
        remove_files(reader_source)

        # Create the namelist object
        args = []
        for index, scheme in enumerate(scheme_names):
            args.append('--namelist-file-arg')
            args.append(f"{scheme}:{xml_files[index]}")
        # end for
        args.append('--namelist-read-mod')
        args.append(reader_mod)
        with self.assertLogs(logger=logger, level='INFO') as cmp_log:
            namelist_obj = gen_namelist_files(args, _TMP_DIR, logger)
        # end with
        # Check logger
        lmsgs = [("INFO:test_double_namelist_def:Reading CAM physics "      \
                  "namelist definition file, ",
                  "test/unit/python/sample_files/banana_namelist.xml'"),
                 ("INFO:test_double_namelist_def:Writing metadata file, ",
                  "unit/python/tmp/namelist_files/banana_namelist.meta"),
                 ("INFO:test_double_namelist_def:Writing Fortran module, ",
                  "unit/python/tmp/namelist_files/banana_namelist.F90"),
                 ("INFO:test_double_namelist_def:Reading CAM physics "     \
                  "namelist definition file, ",
                  "test/unit/python/sample_files/kumquat_namelist.xml'"),
                 ("INFO:test_double_namelist_def:Writing metadata file, ",
                  "test/unit/python/tmp/namelist_files/kumquat_namelist.meta"),
                 ("INFO:test_double_namelist_def:Writing Fortran module, ",
                  "test/unit/python/tmp/namelist_files/kumquat_namelist.F90")]
        comp_lmsgs = cmp_log.output
        amsg = "Test failure: Number of log output messages, " \
               f"{len(comp_lmsgs)} does not match what is expected, " \
               f"{len(lmsgs)}."
        self.assertEqual(len(comp_lmsgs), len(lmsgs), msg=amsg)
        for index, lmsg in enumerate(lmsgs):
            comp_lmsg = comp_lmsgs[index]
            ilen = len(lmsg[0])
            elen = len(lmsg[1])
            amsg = f"Test failure: Log output message beginning, {index}, " \
                   "does not match what is expected. \n" \
                   f"Logger output is: {comp_lmsg}"
            self.assertEqual(comp_lmsg[0:ilen], lmsg[0], msg=amsg)
            amsg = f"Test failure: Log output message ending, {index}, " \
                   "does not match what is expected. \n" \
                   f"Logger output is: {comp_lmsg}"
            self.assertEqual(comp_lmsg[-elen:], lmsg[1], msg=amsg)
        # end for

        # Check return obj:
        amsg = f"Test failure: bad return type, {type(namelist_obj)}"
        self.assertTrue(isinstance(namelist_obj, NamelistFiles), msg=amsg)

        # Make sure each output file was created:
        for out_source in out_sources:
            amsg = f"{out_source} does not exist"
            self.assertTrue(os.path.exists(out_source), msg=amsg)
        # end for
        for out_meta in out_metas:
            amsg = f"{out_meta} does not exist"
            self.assertTrue(os.path.exists(out_meta), msg=amsg)
        # end if
        amsg = f"{reader_source} does not exist"
        self.assertTrue(os.path.exists(reader_source), msg=amsg)

        # For each output file, make sure it matches input file
        for index, out_source in enumerate(out_sources):
            amsg = f"{out_source} does not match {out_source_checks[index]}"
            self.assertTrue(filecmp.cmp(out_source, out_source_checks[index],
                                        shallow=False), msg=amsg)
        # end if
        for index, out_meta in enumerate(out_metas):
            amsg = f"{out_meta} does not match {out_meta_checks[index]}"
            self.assertTrue(filecmp.cmp(out_meta, out_meta_checks[index],
                                        shallow=False), msg=amsg)
        # end if
        amsg = f"{reader_source} does not match {reader_source_check}"
        self.assertTrue(filecmp.cmp(reader_source, reader_source_check,
                                    shallow=False), msg=amsg)

    def test_missing_xml_file(self):
        """
        Test that the 'gen_namelist_files' function
        fails correctly when given a missing namelist XML definition filename
        """

        # Setup runtime inputs:
        scheme_name = "no_scheme"
        out_source_name = scheme_name + "_namelist"
        xml_file = os.path.join(_XML_SAMPLES_DIR, out_source_name + ".xml")

        # Create local logger:
        logger = logging.getLogger("test_missing_xml_file")
        stream_handler = logging.StreamHandler(sys.stdout)
        logger.addHandler(stream_handler)

        # Expected output files
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        reader_mod = "cam_ccpp_scheme_namelists_single_def"
        reader_source = os.path.join(_TMP_DIR, reader_mod + ".F90")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, reader_source])

        # Create the namelist object
        args = ['--namelist-file-arg', f"{scheme_name}:{xml_file}",
                '--namelist-read-mod', reader_mod]
        with self.assertRaises(NamelistError) as verr:
            with self.assertLogs('test_missing_xml_file',
                                 level='ERROR') as cmp_log:
                _ = gen_namelist_files(args, _TMP_DIR, logger)
            # end with
        # end with
        # Check exception message
        emsg = f"Missing namelist definition file, {xml_file}"
        self.assertEqual(emsg, str(verr.exception))
        # Check log message
        emsg = "ERROR:test_missing_xml_file:"+emsg
        self.assertEqual([emsg], cmp_log.output)

        # Make sure no output file was created:
        amsg = f"{out_source} should not exist"
        self.assertFalse(os.path.exists(out_source), msg=amsg)
        amsg = f"{out_meta} should not exist"
        self.assertFalse(os.path.exists(out_meta), msg=amsg)
        amsg = f"{reader_source} should not exist"
        self.assertFalse(os.path.exists(reader_source), msg=amsg)

        # Cleanup
        logger.removeHandler(stream_handler)

    def test_bad_namelist_def(self):
        """
        Test that the 'gen_namelist_files' function
        raises an appropriate exception if bad entries are found.
        """

        # Setup runtime inputs:
        scheme_name = "rotten"
        out_source_name = scheme_name + "_namelist"
        xml_file = os.path.join(_XML_SAMPLES_DIR, out_source_name + ".xml")

        # Create local logger:
        logger = logging.getLogger("test_bad_namelist_def")

        # Expected output files
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        reader_mod = "cam_ccpp_scheme_namelists_bad_def"
        reader_source = os.path.join(_TMP_DIR, reader_mod + ".F90")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, reader_source])

        # Create the namelist object
        args = ['--namelist-file-arg', f"{scheme_name}:{xml_file}",
                '--namelist-read-mod', reader_mod]
        with self.assertRaises(NamelistError) as nerr:
            with self.assertLogs("test_bad_namelist_def",
                                 level='INFO') as cmp_log:
                _ = gen_namelist_files(args, _TMP_DIR, logger)
            # end with
        # end with

        # Check exception:
        start = "Errors processing "
        amsg = f"Test failure: bad exception message, '{nerr.exception}'"
        self.assertEqual(start, str(nerr.exception)[0:len(start)], msg=amsg)
        nerrs = str(nerr.exception).split('\n')[1:]
        expected = ["rotten_name is missing Unknown variable type, 'johnny'",
                    "nostdname is missing standard_name",
                    "nounits is missing units",
                    "badchar is missing Bad 'char' type for 'badchar', " +    \
                    "must specify length"]
        amsg = "Test failure: bad exception message, " + \
               f"Found {len(nerrs)} errors, should have been {len(expected)}."
        self.assertEqual(len(nerrs), len(expected), msg=amsg)
        if len(nerrs) == len(expected):
            for index, nexcp in enumerate(nerrs):
                amsg = "Test failure: bad exception message, " + \
                       f"Found '{nexcp}', should have been '{expected[index]}'"
                self.assertEqual(nexcp, expected[index], msg=amsg)
            # end for
        # end if

        # Check log
        log_msgs = cmp_log.output
        amsg = "Test failure: bad log message, should be a list, not a " +    \
               f"'{type(log_msgs)}'"
        self.assertTrue(isinstance(log_msgs, list), msg=amsg)
        amsg = "Test failure: bad log message, should have two entries, " +   \
               f"found {len(log_msgs)}"
        self.assertEqual(len(log_msgs), 2, msg=amsg)
        log_msgs = log_msgs[1].split('\n')[1:]
        amsg = "Test failure: bad log message, Found " + \
               f"{len(log_msgs)+1} errors, should have been {len(expected)+1}."
        self.assertEqual(len(log_msgs)+1, len(expected)+1, msg=amsg)
        if len(log_msgs) == len(expected):
            for index, msg in enumerate(log_msgs):
                amsg = "Test failure: bad log message, " + \
                       f"Found '{msg}', should have been '{expected[index]}'"
                self.assertEqual(msg, expected[index], msg=amsg)
            # end for
        # end if

        # Make sure none of the output files were created:
        amsg = f"{out_source} should not exist"
        self.assertFalse(os.path.exists(out_source), msg=amsg)
        amsg = f"{out_meta} should not exist"
        self.assertFalse(os.path.exists(out_meta), msg=amsg)
        amsg = f"{reader_source} should not exist"
        self.assertFalse(os.path.exists(reader_source), msg=amsg)

##########

if __name__ == '__main__':
    unittest.main()
