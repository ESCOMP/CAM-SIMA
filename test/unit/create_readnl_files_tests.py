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

_TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_CAM_ROOT = os.path.abspath(os.path.join(_TEST_DIR, os.pardir, os.pardir))
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

    def test_single_namelist_def(self):
        """
        Test that the 'gen_namelist_files' function
        generates the correct fortran code and metadata given
        a namelist XML definition file
        """

        # Setup runtime inputs:
        scheme_name = "rayleigh_friction"
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
        args = ['--namelist-file-arg', "{}:{}".format(scheme_name, xml_file),
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
        scheme_names = ["rayleigh_friction", "gw_drag"]
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
                  "test/unit/sample_files/rayleigh_friction_namelist.xml'"),
                 ("INFO:test_double_namelist_def:Writing metadata file, ",
                  "unit/tmp/namelist_files/rayleigh_friction_namelist.meta"),
                 ("INFO:test_double_namelist_def:Writing Fortran module, ",
                  "unit/tmp/namelist_files/rayleigh_friction_namelist.F90"),
                 ("INFO:test_double_namelist_def:Reading CAM physics "     \
                  "namelist definition file, ",
                  "test/unit/sample_files/gw_drag_namelist.xml'"),
                 ("INFO:test_double_namelist_def:Writing metadata file, ",
                  "test/unit/tmp/namelist_files/gw_drag_namelist.meta"),
                 ("INFO:test_double_namelist_def:Writing Fortran module, ",
                  "test/unit/tmp/namelist_files/gw_drag_namelist.F90")]
        comp_lmsgs = cmp_log.output
        amsg = "Test failure: Number of log output messages, " \
               f"{len(comp_lmsgs)} does not match what is expected, " \
               "{len(lmsgs)}."
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

        # Check output files
        out_source_check = os.path.join(_NL_SAMPLES_DIR,
                                        out_source_name + '.F90')
        out_meta_check = os.path.join(_NL_SAMPLES_DIR,
                                      out_source_name + '.meta')
        reader_source_check = os.path.join(_NL_SAMPLES_DIR, reader_mod + ".F90")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, reader_source])

        # Create the namelist object
        args = ['--namelist-file-arg', "{}:{}".format(scheme_name, xml_file),
                '--namelist-read-mod', reader_mod]
        with self.assertRaises(NamelistError) as verr:
            with self.assertLogs('test_missing_xml_file',
                                 level='ERROR') as cmp_log:
                namelist_obj = gen_namelist_files(args, _TMP_DIR, logger)
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

##########

if __name__ == '__main__':
    unittest.main()
