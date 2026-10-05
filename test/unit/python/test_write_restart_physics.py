#! /usr/bin/env python3
#-----------------------------------------------------------------------
# Description:  Contains unit tests for testing CAM "phys_restart" code
#               generation using the registry and CCPP physics suites.
#
# Assumptions:
#
# Command line arguments: none
#
# Usage: python "test_write_restart_physics.py"         # run the unit tests
#-----------------------------------------------------------------------

"""Test write_restart_physics in write_restart_physics.py"""

import sys
import os
import glob
import unittest
import filecmp
import logging

__TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_CAM_ROOT = os.path.abspath(os.path.join(__TEST_DIR, os.pardir, os.pardir, os.pardir))
__CCPP_DIR = os.path.join(_CAM_ROOT, "ccpp_framework", "scripts")
__REGISTRY_DIR = os.path.join(_CAM_ROOT, "src", "data")
_REG_SAMPLES_DIR = os.path.join(__TEST_DIR, "sample_files")
_RESTART_SAMPLES_DIR = os.path.join(_REG_SAMPLES_DIR, "write_restart_physics")
_SHARED_DIR = os.path.join(_REG_SAMPLES_DIR, "shared")
_PRE_TMP_DIR = os.path.join(__TEST_DIR, "tmp")
_TMP_DIR = os.path.join(_PRE_TMP_DIR, "write_restart_physics")
_SRC_MOD_DIR = os.path.join(_PRE_TMP_DIR, "SourceMods")
_INC_SEARCH_DIRS = [_SRC_MOD_DIR, __REGISTRY_DIR]

__FILE_OPEN = lambda x: open(x, 'r', encoding='utf-8')

#Check for all necessary directories:
if not os.path.exists(__CCPP_DIR):
    EMSG = "Cannot find CCPP framework directory where 'ccpp_capgen.py' should be located."
    raise ImportError(EMSG)

if not os.path.exists(__REGISTRY_DIR):
    EMSG = "Cannot find registry directory where 'write_restart_physics.py' should be located."
    raise ImportError(EMSG)

if not os.path.exists(_REG_SAMPLES_DIR):
    raise ImportError("Cannot find sample files directory")

if not os.path.exists(_SHARED_DIR):
    raise ImportError("Cannot find 'write_restart_physics' sample files directory")

if not os.path.exists(_SHARED_DIR):
    raise ImportError("Cannot find 'write_restart_physics' sample files directory")

#Add CCPP framework directory to python path to
#import capgen code generator:
sys.path.append(__CCPP_DIR)

#Add registry directory to python path to import
#registry and 'phys_restart' code generators:
sys.path.append(__REGISTRY_DIR)

# pylint: disable=wrong-import-position
from ccpp_capgen import capgen
from framework_env import CCPPFrameworkEnv
from generate_registry_data import gen_registry
import write_restart_physics as write_restart
from parse_source import CCPPError
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

class WriteInitTest(unittest.TestCase):

    """Tests for `write_restart_physics`."""

    @classmethod
    def setUpClass(cls):
        """Clean output directory (tmp) before running tests"""
        # Does "tmp" directory exist?  If not then create it:
        if not os.path.exists(_PRE_TMP_DIR):
            os.mkdir(_PRE_TMP_DIR)
        # end if
        # Now check if "write_restart_physics" directory exists:
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
        super(cls, WriteInitTest).setUpClass()

    def test_simple_reg_write_restart(self):
        """
        Test that the 'write_restart_physics' function
        generates the correct Fortran code given
        a simple registry and CCPP physics suite with
        only regular variables.
        """

        # Setup registry inputs:
        filename = os.path.join(_SHARED_DIR, "simple_reg.xml")
        out_source_name = "physics_types_simple"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_SHARED_DIR,"simple_host.meta")
        sdf = os.path.join(_SHARED_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_SHARED_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_simple.xml")

        host_files = [model_host, out_meta]

        # Setup write_restart_physics inputs:
        rest_name = "restart_physics_simple.F90"
        check_restart_out = os.path.join(_TMP_DIR, rest_name)
        # Setup comparison files
        check_restart_in = os.path.join(_RESTART_SAMPLES_DIR, rest_name)

        # Create local logger:
        logger = logging.getLogger("write_restart_physics_simple")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_restart_out])

        # Generate registry files:
        _, _, _, _, restart_vars, _ = gen_registry(filename, 'se', _TMP_DIR, 3,
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

        # Generate physics restart file:
        retmsg = write_restart.write_restart_physics(cap_database, {}, restart_vars, _TMP_DIR,
                                             3, logger,
                                             phys_restart_filename=rest_name)

        # Check return message:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{check_restart_out} does not exist"
        self.assertTrue(os.path.exists(check_restart_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{check_restart_out} does not match {check_restart_in}"
        self.assertTrue(filecmp.cmp(check_restart_in, check_restart_out,
                                    shallow=False), msg=amsg)

    def test_no_reqvar_write_restart(self):
        """
        Test that the 'write_restart_physics' function
        generates the correct Fortran code given
        a simple registry with no required restart vars.
        """
        # Setup registry inputs:
        filename = os.path.join(_SHARED_DIR, "no_req_var_reg.xml")
        out_source_name = "physics_types_no_req_var"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_SHARED_DIR,"simple_host.meta")
        sdf = os.path.join(_SHARED_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_SHARED_DIR, "temp_adjust_noreq.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_no_req_var.xml")

        host_files = [model_host, out_meta]

        # Setup write_restart_physics inputs:
        rest_name = "restart_physics_no_required.F90"
        check_restart_out = os.path.join(_TMP_DIR, rest_name)
        # Setup comparison files
        check_restart_in = os.path.join(_RESTART_SAMPLES_DIR, rest_name)

        # Create local logger:
        logger = logging.getLogger("write_restart_physics_no_req_var")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_restart_out])

        # Generate registry files:
        _, _, _, constituents, restart_vars, _ = gen_registry(filename, 'se', _TMP_DIR, 3,
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

        # Generate physics restart file:
        retmsg = write_restart.write_restart_physics(cap_database, constituents, restart_vars, _TMP_DIR,
                                             3, logger,
                                             phys_restart_filename=rest_name)

        # Check return message:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{check_restart_out} does not exist"
        self.assertTrue(os.path.exists(check_restart_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{check_restart_out} does not match {check_restart_in}"
        self.assertTrue(filecmp.cmp(check_restart_in, check_restart_out,
                                    shallow=False), msg=amsg)


    def test_no_dim_var_write_restart(self):
        """
        Test that the 'write_restart_physics' function
        correctly determines that a variable that
        has been labeled a "restart" variable
        has no dimensions
        """
        # Setup registry inputs:
        filename = os.path.join(_RESTART_SAMPLES_DIR, "no_dim_reg.xml")
        out_source_name = "physics_types_no_horiz"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_SHARED_DIR,"simple_host.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_no_horiz.xml")

        # Setup write_restart_physics inputs:
        rest_name = "restart_physics_no_required.F90"
        check_restart_out = os.path.join(_TMP_DIR, rest_name)

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_restart_out])

        # Attempt to generate registry files
        with self.assertRaises(CCPPError) as cerr:
            _ = gen_registry(filename, 'se', _TMP_DIR, 3,
                             _SRC_MOD_DIR, _CAM_ROOT,
                             loglevel=logging.ERROR,
                             error_on_no_validate=True)

        emsg = "Variable 'eddy_len' cannot be a restart variable without any dimensions."
        self.assertEqual(emsg, str(cerr.exception))

    def test_ddt2_reg_write_restart(self):
        """
        Test that the 'write_restart_physics' function
        generates the correct Fortran code given
        a registry which contains restart variables
        that are nested DDT members
        """

        # Setup registry inputs:
        filename = os.path.join(_SHARED_DIR, "ddt2_reg.xml")
        out_source_name = "physics_types_ddt2"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_SHARED_DIR,"simple_host.meta")
        sdf = os.path.join(_SHARED_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_SHARED_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_ddt2.xml")
        host_files = [model_host, out_meta]

        # Setup write_restart_physics inputs:
        rest_name = "restart_physics_ddt2.F90"
        check_restart_out = os.path.join(_TMP_DIR, rest_name)
        # Setup comparison files
        check_restart_in = os.path.join(_RESTART_SAMPLES_DIR, rest_name)

        # Create local logger:
        logger = logging.getLogger("write_restart_physics_ddt2")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_restart_out])

        # Generate registry files:
        _, _, _, constituents, restart_vars, _ = gen_registry(filename, 'se', _TMP_DIR, 3,
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

        # Generate physics restart file:
        retmsg = write_restart.write_restart_physics(cap_database, constituents, restart_vars, _TMP_DIR,
                                             3, logger,
                                             phys_restart_filename=rest_name)

        # Check return message:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{check_restart_out} does not exist"
        self.assertTrue(os.path.exists(check_restart_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{check_restart_out} does not match {check_restart_in}"
        self.assertTrue(filecmp.cmp(check_restart_in, check_restart_out,
                                    shallow=False), msg=amsg)

    def test_ddt_reg_write_restart(self):
        """
        Test that the 'write_restart_physics' function
        generates the correct Fortran code given
        a registry which contains restart variables
        that are DDT members
        """

        # Setup registry inputs:
        filename = os.path.join(_SHARED_DIR, "ddt_reg.xml")
        out_source_name = "physics_types_ddt"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_SHARED_DIR,"simple_host.meta")
        sdf = os.path.join(_SHARED_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_SHARED_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_ddt.xml")
        host_files = [model_host, out_meta]

        # Setup write_restart_physics inputs:
        rest_name = "restart_physics_ddt.F90"
        check_restart_out = os.path.join(_TMP_DIR, rest_name)
        # Setup comparison files
        check_restart_in = os.path.join(_RESTART_SAMPLES_DIR, rest_name)

        # Create local logger:
        logger = logging.getLogger("write_restart_physics_ddt")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_restart_out])

        # Generate registry files:
        _, _, _, constituents, restart_vars, _ = gen_registry(filename, 'se', _TMP_DIR, 3,
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

        # Generate physics restart file:
        retmsg = write_restart.write_restart_physics(cap_database, constituents, restart_vars, _TMP_DIR,
                                             3, logger,
                                             phys_restart_filename=rest_name)

        # Check return message:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{check_restart_out} does not exist"
        self.assertTrue(os.path.exists(check_restart_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{check_restart_out} does not match {check_restart_in}"
        self.assertTrue(filecmp.cmp(check_restart_in, check_restart_out,
                                    shallow=False), msg=amsg)

    def test_simple_constituent_dimensioned_var_write_restart(self):
        """
        Test that the 'write_restart_physics' function
        generates the correct Fortran code given
        a simple registry and CCPP physics suite with
        a restart variable with a horizontal dimension and
        the number of constituents as the second dimension.
        """
        # Setup registry inputs:
        filename = os.path.join(_SHARED_DIR, "simple_reg_constituent_dim.xml")
        out_source_name = "physics_types_simple_constituent_dim"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_SHARED_DIR,"simple_host.meta")
        sdf = os.path.join(_SHARED_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_SHARED_DIR, "temp_adjust_constituent_dim.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_constituent_dim.xml")

        host_files = [model_host, out_meta]

        # Setup write_restart_physics inputs:
        rest_name = "restart_physics_const_dim.F90"
        check_restart_out = os.path.join(_TMP_DIR, rest_name)
        # Setup comparison files
        check_restart_in = os.path.join(_RESTART_SAMPLES_DIR, rest_name)

        # Create local logger:
        logger = logging.getLogger("write_restart_physics_const_dim")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_restart_out])

        # Generate registry files:
        _, _, _, constituents, restart_vars, _ = gen_registry(filename, 'se', _TMP_DIR, 3,
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

        # Generate physics restart file:
        retmsg = write_restart.write_restart_physics(cap_database, constituents, restart_vars, _TMP_DIR,
                                             3, logger,
                                             phys_restart_filename=rest_name)

        # Check return message:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{check_restart_out} does not exist"
        self.assertTrue(os.path.exists(check_restart_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{check_restart_out} does not match {check_restart_in}"
        self.assertTrue(filecmp.cmp(check_restart_in, check_restart_out,
                                    shallow=False), msg=amsg)

if __name__ == '__main__':
    unittest.main()
