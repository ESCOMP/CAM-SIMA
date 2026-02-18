#! /usr/bin/env python3
#-----------------------------------------------------------------------
# Description:  Contains unit tests for testing CAM history configuration
#               parsing and namelist generation
#
# Assumptions:
#
# Command line arguments: none
#
# Usage: python3 test_registry.py         # run the unit tests
#-----------------------------------------------------------------------

"""Test parse_hist_config_file in hist_config.py"""

import glob
import filecmp
import logging
import os
import sys
import unittest

__TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_CAM_ROOT = os.path.abspath(os.path.join(__TEST_DIR, os.pardir, os.pardir, os.pardir))
__CIME_CONFIG_DIR = os.path.join(_CAM_ROOT, "cime_config")
_SAMPLE_FILES_DIR = os.path.join(__TEST_DIR,
                                 "sample_files", "hist_config_files")
_TMP_DIR = os.path.join(__TEST_DIR, "tmp")
_LOGGER = logging.getLogger(__name__)

if not os.path.exists(__CIME_CONFIG_DIR):
    raise ImportError(f"Cannot find '{__CIME_CONFIG_DIR}'")

if not os.path.exists(_SAMPLE_FILES_DIR):
    raise ImportError(f"Cannot find '{_SAMPLE_FILES_DIR}'")

sys.path.append(__CIME_CONFIG_DIR)

# pylint: disable=wrong-import-position
from hist_config import HistoryConfig
from hist_config import HistoryConfigError
# pylint: enable=wrong-import-position

###############################################################################
def remove_files(file_list):
###############################################################################
    """Remove files in <file_list> if they exist"""
    for fpath in file_list:
        if os.path.exists(fpath):
            os.remove(fpath)
        # End if
    # End for

class HistConfigTest(unittest.TestCase):

    """Tests for `parse_hist_config_file`."""

    @classmethod
    def setUpClass(cls):
        """Clean output directory (tmp) before running tests"""
        if not os.path.exists(_TMP_DIR):
            os.makedirs(_TMP_DIR)

        remove_files(glob.iglob(os.path.join(_TMP_DIR, '*.*')))
        super(cls, HistConfigTest).setUpClass()

    def _test_config(self, config, vol, prec, maxf, outfreq, ftype, write_nstep0, filename_spec, restart_fname_spec):
        """Check the properties of <config> against the other inputs:
        <vol>: volume
        <prec>: precision
        <maxf>: max_frames
        <outfreq>: output_frequency
        <ftype>: file_type
        <write_nstep0>: flag to write the 0th timestep
        <filename_spec>: filename template
        <restart_fname_spec>: restart filename template
        """
        self.assertEqual(config.volume, vol, msg="Bad volume")
        self.assertEqual(config.precision, prec, msg="Bad precision")
        self.assertEqual(config.max_frames, maxf, msg="Bad max frames")
        self.assertEqual(config.output_frequency, outfreq,
                         msg="Bad output frequency")
        self.assertEqual(config.file_type, ftype, msg="Bad file type")
        self.assertEqual(config.write_nstep0, write_nstep0, msg="Bad write_nstep0 flag")
        self.assertEqual(config.filename_spec, filename_spec, msg="Bad filename spec")
        self.assertEqual(config.restart_fname_spec, restart_fname_spec, msg="Bad restart filename spec")

    def test_flat_user_nl_cam(self):
        """Test history entries that would be appropriate in user_nl_cam.
        Check that the correct Fortran namelist is generated"""
        # Setup test
        in_source = os.path.join(_SAMPLE_FILES_DIR, "user_nl_cam_flat")
        out_source = os.path.join(_TMP_DIR, "atm_in")
        out_test = os.path.join(_SAMPLE_FILES_DIR, "atm_in_flat")
        remove_files([out_source])
        # Run test
        hist_log = logging.getLogger("hist_log")
        #_LOGGER.setLevel(logging.DEBUG)
        with self.assertLogs(hist_log, level='DEBUG') as cmplog:
            hist_configs = HistoryConfig(filename=in_source, logger=hist_log)
        # end with
        # Check that the first few lines of the log are as expected
        expected_logmsg = ["DEBUG:hist_log:Added average field, 'MOE' to hist volume, h1, at",
                           "DEBUG:hist_log:Added average field, 'LARRY' to hist volume, h1, at",
                           "DEBUG:hist_log:Added average field, 'CURLY' to hist volume, h1, at"]
        for index, expected_log in enumerate(expected_logmsg):
            self.assertTrue(cmplog.output[index].startswith(expected_log))
        # end for
        # Check that HistoryConfig object was created
        amsg = "Test failure: no HistConfig object created"
        self.assertTrue(isinstance(hist_configs, HistoryConfig), msg=amsg)
        clen = len(hist_configs)
        amsg = f"Test failure: Found {clen} history files, expected 3"
        self.assertEqual(clen, 3, msg=amsg)
        # Check properties of created config objects
        self.assertTrue('h1' in hist_configs, msg="'h1' not in hist_configs")
        hconfig = hist_configs['h1']
        self._test_config(hconfig, 'h1', 'REAL32', 30, (14, 'hours'), 'history', '.false.', '%c.cam.%u%f.%y-%m-%d-%s.nc', '%c.cam.r%u.%y-%m-%d-%s.nc')
        self.assertTrue('h3' in hist_configs, msg="'h3' not in hist_configs")
        hconfig = hist_configs['h3']
        self._test_config(hconfig, 'h3', 'REAL64', 24, (2, 'nsteps'), 'history', '.false.', '%c.cam.%u%f.%y-%m-%d-%s.nc', '%c.cam.r%u.%y-%m-%d-%s.nc')
        _LOGGER.setLevel(logging.DEBUG)
        # Write out the namelist file
        with open(out_source, 'w', encoding='utf-8') as nl_file:
            hist_configs.output_class_namelist(nl_file)
            for key in sorted(hist_configs.keys()):
                hist_configs[key].output_config_namelist(nl_file, logger=_LOGGER)
            # end for
        # end with
        # Make sure each output file was created
        amsg = f"{out_source} does not exist"
        self.assertTrue(os.path.exists(out_source), msg=amsg)
        # Make sure the output file is correct
        amsg = f"{out_source} does not match {out_test}"
        self.assertTrue(filecmp.cmp(out_test, out_source, shallow='.false.'),
                        msg=amsg)

    def test_multi_user_nl_cam(self):
        """Test history entries that would be appropriate in user_nl_cam that
        includes other history configuration files.
        Check that the correct Fortran namelist is generated"""
        # Setup test
        in_source = os.path.join(_SAMPLE_FILES_DIR, "user_nl_cam_multi")
        out_source = os.path.join(_TMP_DIR, "atm_in_multi")
        out_test = os.path.join(_SAMPLE_FILES_DIR, "atm_in_multi")
        remove_files([out_source])
        # Run test
        hist_configs = HistoryConfig(filename=in_source, logger=_LOGGER)
        _LOGGER.setLevel(logging.DEBUG)
        # Check return code
        amsg = "Test failure: no HistConfig object created"
        self.assertTrue(isinstance(hist_configs, HistoryConfig), msg=amsg)
        clen = len(hist_configs)
        amsg = f"Test failure: Found {clen} history files, expected 2"
        self.assertEqual(clen, 2, msg=amsg)
        # Check properties of created config objects
        self.assertTrue('h0' in hist_configs, msg="'h0' not in hist_configs")
        hconfig = hist_configs['h0']
        self._test_config(hconfig, 'h0', 'REAL32', 1, (1, 'nmonths'), 'history', '.false.', '%c.cam.%u%f.%y-%m-%d-%s.nc', '%c.cam.r%u.%y-%m-%d-%s.nc')
        self.assertTrue('h3' in hist_configs, msg="'h3' not in hist_configs")
        hconfig = hist_configs['h3']
        self._test_config(hconfig, 'h3', 'REAL64', 24, (2, 'nsteps'), 'history', '.true.', 'test_fname_%y.nc', '%c.cam.r%u.%y-%m-%d-%s.nc')
        # Write out the namelist file
        with open(out_source, 'w', encoding='utf-8') as nl_file:
            hist_configs.output_class_namelist(nl_file)
            for key in sorted(hist_configs.keys()):
                hist_configs[key].output_config_namelist(nl_file, logger=_LOGGER)
            # end for
        # end with
        # Make sure each output file was created
        amsg = f"{out_source} does not exist"
        self.assertTrue(os.path.exists(out_source), msg=amsg)
        # Make sure the output file is correct
        amsg = f"{out_source} does not match {out_test}"
        self.assertTrue(filecmp.cmp(out_test, out_source, shallow='.false.'),
                        msg=amsg)

    def test_bad_user_nl_cam(self):
        """Test invalid history entries; confirm correct errors are thrown"""
        # Setup test
        in_source = os.path.join(_SAMPLE_FILES_DIR, "user_nl_cam_multi")
        modified_in_source = os.path.join(_TMP_DIR, "user_nl_cam_multi_bad")
        out_source = os.path.join(_TMP_DIR, "atm_in_multi")
        out_test = os.path.join(_SAMPLE_FILES_DIR, "atm_in_multi")
        remove_files([out_source])

        # Open good user_nl_cam from previous test
        with open(in_source, "r", encoding="utf-8") as old_file:
            # Read in file:
            file_lines = old_file.readlines()
            # Edit to add bad lines
            file_lines[8] = ""
            file_lines[9] = "hist_remove_fields;h0:\n"
            file_lines[10] = "hist_output_frequency;h0: 1+nmonths\n"
            file_lines[11] = "hist_precision;h0: REAL34\n"
            file_lines[13] = "hist_add_inst_fields;h3: T&U&V\n"
            file_lines[16] = "hist_max_frames;h3: -24\n"
            file_lines[17] = "hist_write_nstep0;h3: treu\n"
        # end with

        # Create a new modified version of the file with the bad entries
        with open(modified_in_source, "w", encoding="utf-8") as new_file:
            # Write lines to new file
            new_file.writelines(file_lines)
        # end with

        # Run test
        with self.assertRaises(HistoryConfigError) as err:
            hist_configs = HistoryConfig(filename=modified_in_source, logger=_LOGGER)
        # end with

        exception_split = str(err.exception).split('\n')
        errmsg_expected = ["No identifiers found, at",
                           "period (\"1+nmonths\") must be one of nsteps, nstep, nseconds, nsecond, nminutes, nminute, nhours, nhour, ndays, nday, nmonths, nmonth, nyears, nyear, steps, seconds, minutes, hours, days, months, years, at",
                           "precision must be one of REAL32, REAL64, at",
                           "Found invalid identifiers",
                           "T&U&V, at",
                           "Attempt to set max frames to '-24', must be a positive integer, at",
                           "hist_write_nstep0 must be one of .false., .true., f, false, t, true, at"]

        # Check error messages are as expected
        for index, errmsg in enumerate(exception_split):
            self.assertTrue(errmsg.strip().startswith(errmsg_expected[index]))
        # end for


##############################################################################

if __name__ == '__main__':
    unittest.main()
