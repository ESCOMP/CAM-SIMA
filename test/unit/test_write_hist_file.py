#! /usr/bin/env python3
#-----------------------------------------------------------------------
# Description:  Contains unit tests for testing CAM "physics_history" code
#               generation using the registry and CCPP physics suites.
#
# Assumptions:
#
# Command line arguments: none
#
# Usage: python "test_write_hist_file.py"         # run the unit tests
#-----------------------------------------------------------------------

"""Test write_init_files in write_hist_file.py"""

import sys
import os
import glob
import unittest
import filecmp
import logging

__TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_CAM_ROOT = os.path.abspath(os.path.join(__TEST_DIR, os.pardir, os.pardir))
__CCPP_DIR = os.path.join(_CAM_ROOT, "ccpp_framework", "scripts")
__REGISTRY_DIR = os.path.join(_CAM_ROOT, "src", "data")
_REG_SAMPLES_DIR = os.path.join(__TEST_DIR, "sample_files")
_HIST_SAMPLES_DIR = os.path.join(_REG_SAMPLES_DIR, "write_hist_file")
_INIT_SAMPLES_DIR = os.path.join(_REG_SAMPLES_DIR, "write_init_files")
_PRE_TMP_DIR = os.path.join(__TEST_DIR, "tmp")
_TMP_DIR = os.path.join(_PRE_TMP_DIR, "write_hist_file")
_SRC_MOD_DIR = os.path.join(_PRE_TMP_DIR, "SourceMods")
_INC_SEARCH_DIRS = [_SRC_MOD_DIR, __REGISTRY_DIR]

__FILE_OPEN = (lambda x: open(x, 'r', encoding='utf-8'))

#Check for all necessary directories:
if not os.path.exists(__CCPP_DIR):
    EMSG = "Cannot find CCPP framework directory where 'ccpp_capgen.py' should be located."
    raise ImportError(EMSG)

if not os.path.exists(__REGISTRY_DIR):
    EMSG = "Cannot find registry directory where 'write_hist_files.py' should be located."
    raise ImportError(EMSG)

if not os.path.exists(_REG_SAMPLES_DIR):
    raise ImportError("Cannot find sample files directory")

if not os.path.exists(_INIT_SAMPLES_DIR):
    raise ImportError("Cannot find 'write_init_files' sample files directory")

if not os.path.exists(_HIST_SAMPLES_DIR):
    raise ImportError("Cannot find 'write_hist_file' sample files directory")

#Add CCPP framework directory to python path to
#import capgen code generator:
sys.path.append(__CCPP_DIR)

#Add registry directory to python path to import
#registry and 'phys_init' code generators:
sys.path.append(__REGISTRY_DIR)

# pylint: disable=wrong-import-position
from ccpp_capgen import capgen
from framework_env import CCPPFrameworkEnv
from generate_registry_data import gen_registry
import write_hist_file as write_hist
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

###############################################################################
def find_file(filename, search_dirs):
###############################################################################
    """Look for <filename> in <path_list>.
       Return the found path and the match directory (from <path_list>).
    """
    match_file = None
    for sdir in search_dirs:
        test_path = os.path.join(sdir, filename)
        if os.path.exists(test_path):
            match_file = test_path
            break
        # End if
    # End for
    return match_file

###############################################################################

class WriteHistTest(unittest.TestCase):

    """Tests for `write_hist_files`."""

    @classmethod
    def setUpClass(cls):
        """Clean output directory (tmp) before running tests"""
        # Does "tmp" directory exist?  If not then create it:
        if not os.path.exists(_PRE_TMP_DIR):
            os.mkdir(_PRE_TMP_DIR)
        # end if
        # Now check if "write_init_files" directory exists:
        if not os.path.exists(_TMP_DIR):
            os.mkdir(_TMP_DIR)
        # end if
        # Finally check if "SourceMods" directory exists:
        if not os.path.exists(_SRC_MOD_DIR):
            os.mkdir(_SRC_MOD_DIR)
        # end if

        # Clear out all files:
        remove_files(glob.iglob(os.path.join(_TMP_DIR, '*.*')))

        # Run inherited setup method:
        super(cls, WriteHistTest).setUpClass()

    def test_simple_reg_write_hist(self):
        """
        Test that the 'write_hist_files' function
        generates the correct Fortran code given
        a simple registry and CCPP physics suite with
        only regular variables.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "simple_reg.xml")
        out_source_name = "physics_types_simple"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        out_hist_name = "physics_history_simple.F90"
        out_hist = os.path.join(_TMP_DIR, out_hist_name)

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust_cnst.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_cnst.xml")

        host_files = [model_host, out_meta]

        # Setup comparison files
        in_hist = os.path.join(_HIST_SAMPLES_DIR, out_hist_name)

        # Create local logger:
        logger = logging.getLogger("write_hist_file_simple")

        # Clear all temporary output files:
        remove_files([out_source, cap_datafile, out_meta, out_hist])

        # Generate registry files:
        _, _, _, diag_names = gen_registry(filename, 'se', _TMP_DIR, 3,
                         _SRC_MOD_DIR, _CAM_ROOT,
                         loglevel=logging.ERROR,
                         error_on_no_validate=True)

        # Generate CCPP capgen files:
        kind_types = ['kind_phys=REAL64']
        run_env = CCPPFrameworkEnv(logger, host_files=host_files,
                                   scheme_files=scheme_files, suites=sdf,
                                   preproc_directives='',
                                   generate_docfiles=False,
                                   host_name='cam', kind_types=kind_types,
                                   use_error_obj=False,
                                   force_overwrite=True,
                                   output_root=_TMP_DIR,
                                   ccpp_datafile=cap_datafile)

        cap_database = capgen(run_env, return_db=True)

        # Generate physics initialization files:
        retmsg = write_hist.write_hist_file(cap_database, diag_names, _TMP_DIR,
                                             find_file, _INC_SEARCH_DIRS,
                                             3, logger,
                                             phys_hist_filename=out_hist_name)

        # Check return message:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{out_hist} does not exist"
        self.assertTrue(os.path.exists(out_hist), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{in_hist} does not match {out_hist}"
        self.assertTrue(filecmp.cmp(in_hist, out_hist,
                                    shallow=False), msg=amsg)

    def test_no_reqvar_write_hist(self):
        """
        Test that the 'write_hist_file' function
        generates the correct Fortran code given
        a CCPP physics suite with no required
        variables from the registry.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "no_req_var_reg.xml")
        out_source_name = "physics_types_no_req_var"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        out_hist_name = "physics_history_no_req_var.F90"
        out_hist = os.path.join(_TMP_DIR, out_hist_name)

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust_noreq.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_no_req_var.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        in_hist = os.path.join(_HIST_SAMPLES_DIR, out_hist_name)

        # Create local logger:
        logger = logging.getLogger("write_hist_file_noreq")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      out_hist])

        # Generate registry files:
        _, _, _, diag_names = gen_registry(filename, 'se', _TMP_DIR, 3,
                         _SRC_MOD_DIR, _CAM_ROOT,
                         loglevel=logging.ERROR,
                         error_on_no_validate=True)

        # Generate CCPP capgen files:
        kind_types = ['kind_phys=REAL64']
        run_env = CCPPFrameworkEnv(logger, host_files=host_files,
                                   scheme_files=scheme_files, suites=sdf,
                                   preproc_directives='',
                                   generate_docfiles=False,
                                   host_name='cam', kind_types=kind_types,
                                   use_error_obj=False,
                                   force_overwrite=True,
                                   output_root=_TMP_DIR,
                                   ccpp_datafile=cap_datafile)

        cap_database = capgen(run_env, return_db=True)

        # Generate physics initialization files:
        retmsg = write_hist.write_hist_file(cap_database, diag_names, _TMP_DIR,
                                             find_file, _INC_SEARCH_DIRS,
                                             3, logger,
                                             phys_hist_filename=out_hist_name)

        # Check return message:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{out_hist} does not exist"
        self.assertTrue(os.path.exists(out_hist), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{out_hist} does not match {out_hist}"
        self.assertTrue(filecmp.cmp(in_hist, out_hist,
                                    shallow=False), msg=amsg)


    def test_ddt_reg_write_init(self):
        """
        Test that the 'write_hist_file' function
        generates the correct Fortran code given
        a registry which contains variables and
        a DDT.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "ddt_reg.xml")
        out_source_name = "physics_types_ddt"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        out_hist_name = 'physics_history_ddt.F90'
        out_hist = os.path.join(_TMP_DIR, out_hist_name)

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_ddt.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        in_hist = os.path.join(_HIST_SAMPLES_DIR, out_hist_name)

        # Create local logger:
        logger = logging.getLogger("write_hist_file_ddt")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, out_hist])

        # Generate registry files:
        _, _, _, diag_names = gen_registry(filename, 'se', _TMP_DIR, 3,
                         _SRC_MOD_DIR, _CAM_ROOT,
                         loglevel=logging.ERROR,
                         error_on_no_validate=True)

        # Generate CCPP capgen files:
        kind_types=['kind_phys=REAL64']
        run_env = CCPPFrameworkEnv(logger, host_files=host_files,
                                   scheme_files=scheme_files, suites=sdf,
                                   preproc_directives='',
                                   generate_docfiles=False,
                                   host_name='cam', kind_types=kind_types,
                                   use_error_obj=False,
                                   force_overwrite=True,
                                   output_root=_TMP_DIR,
                                   ccpp_datafile=cap_datafile)
        cap_database = capgen(run_env, return_db=True)

        # Generate physics initialization files:
        retmsg = write_hist.write_hist_file(cap_database, diag_names, _TMP_DIR,
                                             find_file, _INC_SEARCH_DIRS,
                                             3, logger,
                                             phys_hist_filename=out_hist_name)

        # Check return code:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{out_hist} does not exist"
        self.assertTrue(os.path.exists(out_hist), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{out_hist} does not match {in_hist}"
        self.assertTrue(filecmp.cmp(in_hist, out_hist,
                                    shallow=False), msg=amsg)

    def test_ddt2_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct Fortran code given
        a registry that contains variables and
        a DDT, which itself contains another DDT.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "ddt2_reg.xml")
        out_source_name = "physics_types_ddt2"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        out_hist_name = "physics_history_ddt2.F90"
        out_hist = os.path.join(_TMP_DIR, out_hist_name)

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_ddt2.xml")

        host_files = [model_host, out_meta]

        # Comparison files
        in_hist = os.path.join(_HIST_SAMPLES_DIR, out_hist_name)

        # Create local logger:
        logger = logging.getLogger("write_hist_file_ddt2")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, out_hist])

        # Generate registry files:
        _, files, _, diag_names = gen_registry(filename, 'se', _TMP_DIR, 3,
                                   _SRC_MOD_DIR, _CAM_ROOT,
                                   loglevel=logging.ERROR,
                                   error_on_no_validate=True)

        # Generate CCPP capgen files:
        kind_types=['kind_phys=REAL64']
        run_env = CCPPFrameworkEnv(logger, host_files=host_files,
                                   scheme_files=scheme_files, suites=sdf,
                                   preproc_directives='',
                                   generate_docfiles=False,
                                   host_name='cam', kind_types=kind_types,
                                   use_error_obj=False,
                                   force_overwrite=True,
                                   output_root=_TMP_DIR,
                                   ccpp_datafile=cap_datafile)
        cap_database = capgen(run_env, return_db=True)

        # Generate physics initialization files:
        retmsg = write_hist.write_hist_file(cap_database, diag_names, _TMP_DIR,
                                             find_file, _INC_SEARCH_DIRS,
                                             3, logger,
                                             phys_hist_filename=out_hist_name)

        # Check return code:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{out_hist} does not exist"
        self.assertTrue(os.path.exists(out_hist), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{out_hist} does not match {in_hist}"
        self.assertTrue(filecmp.cmp(out_hist, in_hist,
                                    shallow=False), msg=amsg)

    def test_ddt_array_reg_write_init(self):
        """
        Test that the 'write_hist_files' function
        generates the correct Fortran code given
        a registry which contains Array variables
        and a DDT.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "ddt_array_reg.xml")
        out_source_name = "physics_types_ddt_array"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        out_hist_name = "physics_history_ddt_array.F90"
        out_hist = os.path.join(_TMP_DIR, out_hist_name)

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_ddt_array.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        in_hist = os.path.join(_HIST_SAMPLES_DIR, out_hist_name)

        # Create local logger:
        logger = logging.getLogger("write_hist_file_ddt_array")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, out_hist])

        # Generate registry files:
        _, _, _, diag_names = gen_registry(filename, 'se', _TMP_DIR, 3,
                         _SRC_MOD_DIR, _CAM_ROOT,
                         loglevel=logging.ERROR,
                         error_on_no_validate=True)

        # Generate CCPP capgen files:
        kind_types=['kind_phys=REAL64']
        run_env = CCPPFrameworkEnv(logger, host_files=host_files,
                                   scheme_files=scheme_files, suites=sdf,
                                   preproc_directives='',
                                   generate_docfiles=False,
                                   host_name='cam', kind_types=kind_types,
                                   use_error_obj=False,
                                   force_overwrite=True,
                                   output_root=_TMP_DIR,
                                   ccpp_datafile=cap_datafile)
        cap_database = capgen(run_env, return_db=True)

        # Generate physics initialization files:
        retmsg = write_hist.write_hist_file(cap_database, diag_names, _TMP_DIR,
                                             find_file, _INC_SEARCH_DIRS,
                                             3, logger,
                                             phys_hist_filename=out_hist_name)

        # Check return code:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{out_hist} does not exist"
        self.assertTrue(os.path.exists(out_hist), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{out_hist} does not match {in_hist}"
        self.assertTrue(filecmp.cmp(out_hist, in_hist,
                                    shallow=False), msg=amsg)

##########

if __name__ == '__main__':
    unittest.main()
