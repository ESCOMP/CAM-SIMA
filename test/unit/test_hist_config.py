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
_CAM_ROOT = os.path.abspath(os.path.join(__TEST_DIR, os.pardir, os.pardir))
__CIME_CONFIG_DIR = os.path.join(_CAM_ROOT, "cime_config")
_SAMPLE_FILES_DIR = os.path.join(__TEST_DIR,
                                 "sample_files", "hist_config_files")
_TMP_DIR = os.path.join(__TEST_DIR, "tmp")
_LOGGER = logging.getLogger(__name__)

if not os.path.exists(__CIME_CONFIG_DIR):
    raise ImportError("Cannot find <root>/cime_config")

if not os.path.exists(_SAMPLE_FILES_DIR):
    raise ImportError("Cannot find sample files directory")

sys.path.append(__CIME_CONFIG_DIR)

# pylint: disable=wrong-import-position
from hist_config import HistoryConfig
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

    def _test_config(self, config, vol, prec, maxf, outfreq, ftype):
        """Check the properties of <config> against the other inputs:
        <vol>: volume
        <prec>: precision
        <maxf>: max_frames
        <outfreq>: output_frequency
        <ftype>: file_type"""
        self.assertEqual(config.volume, vol, msg="Bad volume")
        self.assertEqual(config.precision, prec, msg="Bad precision")
        self.assertEqual(config.max_frames, maxf, msg="Bad max frames")
        self.assertEqual(config.output_frequency, outfreq,
                         msg="Bad output frequency")
        self.assertEqual(config.file_type, ftype, msg="Bad file type")

    def test_flat_user_nl_cam(self):
        """Test history entries that would be appropriate in user_nl_cam.
        Check that the correct Fortran namelist is generated"""
        # Setup test
        in_source = os.path.join(_SAMPLE_FILES_DIR, "user_nl_cam_flat")
        out_source = os.path.join(_TMP_DIR, "atm_in")
        out_test = os.path.join(_SAMPLE_FILES_DIR, "atm_in_flat")
        remove_files([out_source])
        # Run test
        _LOGGER.setLevel(logging.DEBUG)
        hist_configs = HistoryConfig(filename=in_source, logger=_LOGGER)
        # Check that HistoryConfig object was created
        amsg = "Test failure: no HistConfig object created"
        self.assertTrue(isinstance(hist_configs, HistoryConfig), msg=amsg)
        clen = len(hist_configs)
        amsg = f"Test failure: Found {clen} history files, expected 3"
        self.assertEqual(clen, 3, msg=amsg)
        # Check properties of created config objects
        self.assertTrue('h1' in hist_configs, msg="'h1' not in hist_configs")
        hconfig = hist_configs['h1']
        self._test_config(hconfig, 'h1', 'REAL32', 30, (14, 'hours'), 'history')
        self.assertTrue('h3' in hist_configs, msg="'h3' not in hist_configs")
        hconfig = hist_configs['h3']
        self._test_config(hconfig, 'h3', 'REAL64', 24, (2, 'nsteps'), 'history')
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
        self.assertTrue(filecmp.cmp(out_test, out_source, shallow=False),
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
        self._test_config(hconfig, 'h0', 'REAL32', 1, (1, 'monthly'), 'history')
        self.assertTrue('h3' in hist_configs, msg="'h3' not in hist_configs")
        hconfig = hist_configs['h3']
        self._test_config(hconfig, 'h3', 'REAL64', 24, (2, 'nsteps'), 'history')
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
        self.assertTrue(filecmp.cmp(out_test, out_source, shallow=False),
                        msg=amsg)

##############################################################################

if __name__ == '__main__':
    unittest.main()
