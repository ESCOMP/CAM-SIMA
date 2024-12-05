#! /usr/bin/env python3
#-----------------------------------------------------------------------
# Description:  Contains unit tests for testing CAM "phys_init" code
#               generation using the registry and CCPP physics suites.
#
# Assumptions:
#
# Command line arguments: none
#
# Usage: python "write_init_unit_tests.py"         # run the unit tests
#-----------------------------------------------------------------------

"""Test write_init_files in write_init_files.py"""

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
_INIT_SAMPLES_DIR = os.path.join(_REG_SAMPLES_DIR, "write_init_files")
_PRE_TMP_DIR = os.path.join(__TEST_DIR, "tmp")
_TMP_DIR = os.path.join(_PRE_TMP_DIR, "write_init_files")
_SRC_MOD_DIR = os.path.join(_PRE_TMP_DIR, "SourceMods")
_INC_SEARCH_DIRS = [_SRC_MOD_DIR, __REGISTRY_DIR]

__FILE_OPEN = (lambda x: open(x, 'r', encoding='utf-8'))

#Check for all necessary directories:
if not os.path.exists(__CCPP_DIR):
    EMSG = "Cannot find CCPP framework directory where 'ccpp_capgen.py' should be located."
    raise ImportError(EMSG)

if not os.path.exists(__REGISTRY_DIR):
    EMSG = "Cannot find registry directory where 'write_init_files.py' should be located."
    raise ImportError(EMSG)

if not os.path.exists(_REG_SAMPLES_DIR):
    raise ImportError("Cannot find sample files directory")

if not os.path.exists(_INIT_SAMPLES_DIR):
    raise ImportError("Cannot find 'write_init_files' sample files directory")

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
import write_init_files as write_init
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

    """Tests for `write_init_files`."""

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
        super(cls, WriteInitTest).setUpClass()

    def test_simple_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct Fortran code given
        a simple registry and CCPP physics suite with
        only regular variables.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "simple_reg.xml")
        out_source_name = "physics_types_simple"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_simple.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        vic_name = "phys_vars_init_check_simple.F90"
        pi_name = "physics_inputs_simple.F90"
        check_init_out = os.path.join(_TMP_DIR, vic_name)
        phys_input_out = os.path.join(_TMP_DIR, pi_name)
        # Setup comparison files
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, vic_name)
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, pi_name)

        # Create local logger:
        logger = logging.getLogger("write_init_files_simple")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_init_out, phys_input_out])

        # Generate registry files:
        _ = gen_registry(filename, 'se', _TMP_DIR, 3,
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
        retmsg = write_init.write_init_files(cap_database, {}, [], _TMP_DIR,
                                             find_file, _INC_SEARCH_DIRS,
                                             3, logger,
                                             phys_check_filename=vic_name,
                                             phys_input_filename=pi_name)

        # Check return message:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{check_init_out} does not exist"
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = f"{phys_input_out} does not exist"
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{check_init_out} does not match {check_init_in}"
        self.assertTrue(filecmp.cmp(check_init_in, check_init_out,
                                    shallow=False), msg=amsg)
        amsg = f"{phys_input_out} does not match {phys_input_in}"
        self.assertTrue(filecmp.cmp(phys_input_in, phys_input_out,
                                    shallow=False), msg=amsg)

    def test_simple_reg_constituent_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct Fortran code given
        a simple registry and CCPP physics suite with
        only regular variables plus a registered constituent.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "simple_reg.xml")
        out_source_name = "physics_types_simple"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust_cnst.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_cnst.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        vic_name = "phys_vars_init_check_cnst.F90"
        pi_name = "physics_inputs_cnst.F90"
        check_init_out = os.path.join(_TMP_DIR, vic_name)
        phys_input_out = os.path.join(_TMP_DIR, pi_name)
        # Setup comparison files
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, vic_name)
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, pi_name)

        # Create local logger:
        logger = logging.getLogger("write_init_files_constituent")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_init_out, phys_input_out])

        # Generate registry files:
        _, _, ic_names, constituents = gen_registry(filename, 'se', _TMP_DIR, 3,
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
        retmsg = write_init.write_init_files(cap_database, ic_names, constituents, _TMP_DIR,
                                             find_file, _INC_SEARCH_DIRS,
                                             3, logger,
                                             phys_check_filename=vic_name,
                                             phys_input_filename=pi_name)

        # Check return message:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{check_init_out} does not exist"
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = f"{phys_input_out} does not exist"
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{check_init_out} does not match {check_init_in}"
        self.assertTrue(filecmp.cmp(check_init_in, check_init_out,
                                    shallow=False), msg=amsg)
        amsg = f"{phys_input_out} does not match {phys_input_in}"
        self.assertTrue(filecmp.cmp(phys_input_in, phys_input_out,
                                    shallow=False), msg=amsg)

    def test_no_reqvar_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct Fortran code given
        a CCPP physics suite with no required
        variables from the registry.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "no_req_var_reg.xml")
        out_source_name = "physics_types_no_req_var"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust_noreq.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_no_req_var.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        vic_name = "phys_vars_init_check_noreq.F90"
        pi_name = "physics_inputs_noreq.F90"
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, vic_name)
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, pi_name)
        check_init_out = os.path.join(_TMP_DIR, vic_name)
        phys_input_out = os.path.join(_TMP_DIR, pi_name)

        # Create local logger:
        logger = logging.getLogger("write_init_files_noreq")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_init_out, phys_input_out])

        # Generate registry files:
        _ = gen_registry(filename, 'se', _TMP_DIR, 3,
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
        retmsg = write_init.write_init_files(cap_database, {}, [], _TMP_DIR,
                                             find_file, _INC_SEARCH_DIRS,
                                             3, logger,
                                             phys_check_filename=vic_name,
                                             phys_input_filename=pi_name)

        # Check return message:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{check_init_out} does not exist"
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = f"{phys_input_out} does not exist"
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{check_init_out} does not match {check_init_in}"
        self.assertTrue(filecmp.cmp(check_init_in, check_init_out,
                                    shallow=False), msg=amsg)
        amsg = f"{phys_input_out} does not match {phys_input_in}"
        self.assertTrue(filecmp.cmp(phys_input_in, phys_input_out,
                                    shallow=False), msg=amsg)

    def test_protected_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct Fortran code given
        a simple registry and CCPP physics suite with
        regular and protected variables.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "protected_reg.xml")
        out_source_name = "physics_types_protected"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_protected.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        vic_name = "phys_vars_init_check_protect.F90"
        pi_name = "physics_inputs_protect.F90"
        check_init_out = os.path.join(_TMP_DIR, vic_name)
        phys_input_out = os.path.join(_TMP_DIR, pi_name)
        # Setup the comparison files
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, vic_name)
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, pi_name)

        # Create local logger:
        logger = logging.getLogger("write_init_files_protect")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_init_out, phys_input_out])

        # Generate registry files:
        _, files, _, _ = gen_registry(filename, 'se', _TMP_DIR, 3,
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
        retmsg = write_init.write_init_files(cap_database, {}, [], _TMP_DIR,
                                             find_file, _INC_SEARCH_DIRS,
                                             3, logger,
                                             phys_check_filename=vic_name,
                                             phys_input_filename=pi_name)

        # Check return message:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{check_init_out} does not exist"
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = f"{phys_input_out} does not exist"
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{check_init_out} does not match {check_init_in}"
        self.assertTrue(filecmp.cmp(check_init_in, check_init_out,
                                    shallow=False), msg=amsg)
        amsg = f"{phys_input_out} does not match {phys_input_in}"
        self.assertTrue(filecmp.cmp(phys_input_in, phys_input_out,
                                    shallow=False), msg=amsg)

    def test_host_input_var_write_init(self):
        """
        Test that the 'write_init_files' function
        correctly determines that a CCPP-required
        variable is provided by the host model "host" table,
        and generates the correct files (which assume that all such
        variables are initialized since they are passed as arguments).
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "host_var_reg.xml")
        out_source_name = "physics_types_host_var"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"host_var_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_host_var.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        vic_name = "phys_vars_init_check_host_var.F90"
        check_init_out = os.path.join(_TMP_DIR, vic_name)
        pi_name = "physics_inputs_host_var.F90"
        phys_input_out = os.path.join(_TMP_DIR, pi_name)
        # Set up the compare names
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, vic_name)
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, pi_name)

        # Create local logger:
        logger = logging.getLogger("write_init_files_missing")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_init_out, phys_input_out])

        # Generate registry files:
        _ = gen_registry(filename, 'se', _TMP_DIR, 3,
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
        retmsg = write_init.write_init_files(cap_database, {}, [],  _TMP_DIR,
                                             find_file, _INC_SEARCH_DIRS,
                                             3, logger,
                                             phys_check_filename=vic_name,
                                             phys_input_filename=pi_name)

        # Check return message
        rmsg = ""
        self.assertEqual(rmsg, retmsg)

        # Make sure each output file was created:
        amsg = f"{check_init_out} does not exist"
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = f"{phys_input_out} does not exist"
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{check_init_out} does not match {check_init_in}"
        self.assertTrue(filecmp.cmp(check_init_in, check_init_out,
                                    shallow=False), msg=amsg)
        amsg = f"{phys_input_out} does not match {phys_input_in}"
        self.assertTrue(filecmp.cmp(phys_input_in, phys_input_out,
                                    shallow=False), msg=amsg)

    def test_no_horiz_var_write_init(self):
        """
        Test that the 'write_init_files' function
        correctly determines that a variable that
        could be called from "read_field" has no
        dimension labeled "horizontal_dimension"
        and create the correct endrun calls in
        case this variable is not initialized.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "no_horiz_dim_reg.xml")
        out_source_name = "physics_types_no_horiz"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR,
                                    "temp_adjust_no_horiz.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_no_horiz.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        vic_name = "phys_vars_init_check_no_horiz.F90"
        pi_name = "physics_inputs_no_horiz.F90"
        check_init_out = os.path.join(_TMP_DIR, vic_name)
        phys_input_out = os.path.join(_TMP_DIR, pi_name)
        # Setup comparison files
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, vic_name)
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, pi_name)

        # Create local logger:
        logger = logging.getLogger("write_init_files_no_horiz")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, check_init_out, phys_input_out])

        # Generate registry files:
        _, files, _, _ = gen_registry(filename, 'se', _TMP_DIR, 3,
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

        # Run test
        _ = write_init.write_init_files(cap_database, {}, [], _TMP_DIR,
                                        find_file, _INC_SEARCH_DIRS,
                                        3, logger,
                                        phys_check_filename=vic_name,
                                        phys_input_filename=pi_name)

        # For each output file, make sure it matches input file
        amsg = f"{check_init_out} does not match {check_init_in}"
        self.assertTrue(filecmp.cmp(check_init_out, check_init_in,
                                    shallow=False), msg=amsg)
        amsg = f"{phys_input_out} does not match {phys_input_in}"
        self.assertTrue(filecmp.cmp(phys_input_out, phys_input_in,
                                    shallow=False), msg=amsg)

    def test_scalar_var_write_init(self):
        """
        Test that the 'write_init_files' function
        correctly determines that a variable that
        could be called from "read_field" is a
        scalar (which read_field can't handle),
        produces the correct endrun message.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "scalar_var_reg.xml")
        out_source_name = "physics_types_scalar_var"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR,
                                    "temp_adjust_scalar.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_scalar.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        vic_name = "phys_vars_init_check_scalar.F90"
        pi_name = "physics_inputs_scalar.F90"
        check_init_out = os.path.join(_TMP_DIR, vic_name)
        phys_input_out = os.path.join(_TMP_DIR, pi_name)
        # Comparison files
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, vic_name)
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, pi_name)

        # Create local logger:
        logger = logging.getLogger("write_init_files_scalar")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_init_out, phys_input_out])

        # Generate registry files:
        _, files, _, _ = gen_registry(filename, 'se', _TMP_DIR, 3,
                                   _SRC_MOD_DIR, _CAM_ROOT,
                                   loglevel=logging.ERROR,
                                   error_on_no_validate=True)

        # Generate CCPP capgen files:
        kind_types = ["kind_phys={REAL64}"]
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

        # Run test
        _ = write_init.write_init_files(cap_database, {}, [], _TMP_DIR,
                                        find_file, _INC_SEARCH_DIRS,
                                        3, logger,
                                        phys_check_filename=vic_name,
                                        phys_input_filename=pi_name)

        # For each output file, make sure it matches input file
        amsg = f"{check_init_out} does not match {check_init_in}"
        self.assertTrue(filecmp.cmp(check_init_out, check_init_in,
                                    shallow=False), msg=amsg)
        amsg = f"{phys_input_out} does not match {phys_input_in}"
        self.assertTrue(filecmp.cmp(phys_input_out, phys_input_in,
                                    shallow=False), msg=amsg)

    def test_4d_var_write_init(self):
        """
        Test that the 'write_init_files' function
        correctly determines that a variable that
        could be called from "read_field" has more
        than two dimensions (which read_field can't handle),
        and creates the correct endrun message.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "var_4D_reg.xml")
        out_source_name = "physics_types_4D"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust_4D.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_4D.xml")

        host_files = [model_host, out_meta]


        # Setup write_init_files inputs:
        vic_name = "phys_vars_init_check_4D.F90"
        inputs_fname = "physics_inputs_4D.F90"
        check_init_out = os.path.join(_TMP_DIR, vic_name)
        phys_input_out = os.path.join(_TMP_DIR, inputs_fname)
        # Comparison files:
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, vic_name)
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, inputs_fname)

        # Create local logger:
        logger = logging.getLogger("write_init_files_4D")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_init_out, phys_input_out])

        # Generate registry files:
        _, files, _, _ = gen_registry(filename, 'se', _TMP_DIR, 3,
                                   _SRC_MOD_DIR, _CAM_ROOT,
                                   loglevel=logging.ERROR,
                                   error_on_no_validate=True)

        # Generate CCPP capgen files:
        kind_types = ["kind_phys=REAL64"]
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

        # Run test
        retmsg = write_init.write_init_files(cap_database, {}, [], _TMP_DIR,
                                             find_file, _INC_SEARCH_DIRS,
                                             3, logger,
                                             phys_check_filename=vic_name,
                                             phys_input_filename=inputs_fname)

        # Check return message
        rmsg = ""
        self.assertEqual(rmsg, retmsg)

        # For each output file, make sure it matches input file
        amsg = f"{check_init_out} does not match {check_init_in}"
        self.assertTrue(filecmp.cmp(check_init_out, check_init_in,
                                    shallow=False), msg=amsg)
        amsg = f"{phys_input_out} does not match {phys_input_in}"
        self.assertTrue(filecmp.cmp(phys_input_out, phys_input_in,
                                    shallow=False), msg=amsg)

    def test_ddt_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct Fortran code given
        a registry which contains variables and
        a DDT.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "ddt_reg.xml")
        out_source_name = "physics_types_ddt"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_ddt.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        vic_name = "phys_vars_init_check_ddt.F90"
        pi_name = "physics_inputs_ddt.F90"
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, vic_name)
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, pi_name)
        check_init_out = os.path.join(_TMP_DIR, vic_name)
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_ddt.F90")

        # Create local logger:
        logger = logging.getLogger("write_init_files_ddt")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, check_init_out, phys_input_out])

        # Generate registry files:
        _ = gen_registry(filename, 'se', _TMP_DIR, 3,
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
        retmsg = write_init.write_init_files(cap_database, {}, [], _TMP_DIR,
                                             find_file, _INC_SEARCH_DIRS,
                                             3, logger,
                                             phys_check_filename=vic_name,
                                             phys_input_filename=pi_name)

        # Check return code:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{check_init_out} does not exist"
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = f"{phys_input_out} does not exist"
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{check_init_out} does not match {check_init_in}"
        self.assertTrue(filecmp.cmp(check_init_in, check_init_out,
                                    shallow=False), msg=amsg)
        amsg = f"{phys_input_out} does not match {phys_input_in}"
        self.assertTrue(filecmp.cmp(phys_input_in, phys_input_out,
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

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_ddt2.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files outputs:
        vic_name = "phys_vars_init_check_ddt2.F90"
        pi_name = "physics_inputs_ddt2.F90"
        check_init_out = os.path.join(_TMP_DIR, vic_name)
        phys_input_out = os.path.join(_TMP_DIR, pi_name)
        # Comparison files
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, vic_name)
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, pi_name)

        # Create local logger:
        logger = logging.getLogger("write_init_files_ddt2")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_init_out, phys_input_out])

        # Generate registry files:
        _, files, _, _ = gen_registry(filename, 'se', _TMP_DIR, 3,
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
        retmsg = write_init.write_init_files(cap_database, {}, [],  _TMP_DIR,
                                             find_file, _INC_SEARCH_DIRS,
                                             3, logger,
                                             phys_check_filename=vic_name,
                                             phys_input_filename=pi_name)

        # Check return code:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{check_init_out} does not exist"
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = f"{phys_input_out} does not exist"
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{check_init_out} does not match {check_init_in}"
        self.assertTrue(filecmp.cmp(check_init_out, check_init_in,
                                    shallow=False), msg=amsg)
        amsg = f"{phys_input_out} does not match {phys_input_in}"
        self.assertTrue(filecmp.cmp(phys_input_out, phys_input_in,
                                    shallow=False), msg=amsg)

    def test_ddt_array_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct Fortran code given
        a registry which contains Array variables
        and a DDT.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "ddt_array_reg.xml")
        out_source_name = "physics_types_ddt_array"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_ddt_array.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        vic_name = "phys_vars_init_check_ddt_array.F90"
        pi_name = "physics_inputs_ddt_array.F90"
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, vic_name)
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, pi_name)
        check_init_out = os.path.join(_TMP_DIR, vic_name)
        phys_input_out = os.path.join(_TMP_DIR, pi_name)

        # Create local logger:
        logger = logging.getLogger("write_init_files_ddt_array")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_init_out, phys_input_out])

        # Generate registry files:
        _, _, ic_names, _ = gen_registry(filename, 'se', _TMP_DIR, 3,
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
        retmsg = write_init.write_init_files(cap_database, ic_names, [], _TMP_DIR,
                                             find_file, _INC_SEARCH_DIRS,
                                             3, logger,
                                             phys_check_filename=vic_name,
                                             phys_input_filename=pi_name)

        # Check return code:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{check_init_out} does not exist"
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = f"{phys_input_out} does not exist"
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{check_init_out} does not match {check_init_in}"
        self.assertTrue(filecmp.cmp(check_init_out, check_init_in,
                                    shallow=False), msg=amsg)
        amsg = f"{phys_input_out} does not match {phys_input_in}"
        self.assertTrue(filecmp.cmp(phys_input_out, phys_input_in,
                                    shallow=False), msg=amsg)

    def test_meta_file_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct Fortran code given
        a registry with a metadata file tag.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "mf_reg.xml")
        out_source_name = "physics_types_mf"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        model_mf_file = os.path.join(_INIT_SAMPLES_DIR,"ref_theta.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_mf.xml")

        host_files = [model_host, model_mf_file, out_meta]

        # Setup write_init_files inputs:
        vic_name = "phys_vars_init_check_mf.F90"
        pi_name = "physics_inputs_mf.F90"
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, vic_name)
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, pi_name)
        check_init_out = os.path.join(_TMP_DIR, vic_name)
        phys_input_out = os.path.join(_TMP_DIR, pi_name)

        # Create local logger:
        logger = logging.getLogger("write_init_files_mf")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_init_out, phys_input_out])

        # Generate registry files:
        _ = gen_registry(filename, 'se', _TMP_DIR, 3,
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
        retmsg = write_init.write_init_files(cap_database, {}, [], _TMP_DIR,
                                             find_file, _INC_SEARCH_DIRS,
                                             3, logger,
                                             phys_check_filename=vic_name,
                                             phys_input_filename=pi_name)

        # Check return code:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{check_init_out} does not exist"
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = f"{phys_input_out} does not exist"
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{check_init_out} does not match {check_init_in}"
        self.assertTrue(filecmp.cmp(check_init_out, check_init_in,
                                    shallow=False), msg=amsg)
        amsg = f"{phys_input_out} does not match {phys_input_in}"
        self.assertTrue(filecmp.cmp(phys_input_out, phys_input_in,
                                    shallow=False), msg=amsg)

    def test_parameter_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct Fortran code given
        a simple registry with a variable that is
        allocated as a parameter.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "param_reg.xml")
        out_source_name = "physics_types_param"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust_param.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_param.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        vic_name = "phys_vars_init_check_param.F90"
        pi_name = "physics_inputs_param.F90"
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, vic_name)
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, pi_name)
        check_init_out = os.path.join(_TMP_DIR, vic_name)
        phys_input_out = os.path.join(_TMP_DIR, pi_name)

        # Create local logger:
        logger = logging.getLogger("write_init_files_param")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_init_out, phys_input_out])

        # Generate registry files:
        _ = gen_registry(filename, 'se', _TMP_DIR, 3,
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
        retmsg = write_init.write_init_files(cap_database, {}, [], _TMP_DIR,
                                             find_file, _INC_SEARCH_DIRS,
                                             3, logger,
                                             phys_check_filename=vic_name,
                                             phys_input_filename=pi_name)

        # Check return code:
        amsg = f"Test failure: retmsg={retmsg}"
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = f"{check_init_out} does not exist"
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = f"{phys_input_out} does not exist"
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{check_init_out} does not match {check_init_in}"
        self.assertTrue(filecmp.cmp(check_init_out, check_init_in,
                                    shallow=False), msg=amsg)
        amsg = f"{phys_input_out} does not match {phys_input_in}"
        self.assertTrue(filecmp.cmp(phys_input_out, phys_input_in,
                                    shallow=False), msg=amsg)

    def test_bad_vertical_dimension(self):
        """
        Test that the 'write_init_files' function
        correctly determines that a variable that
        could be called from "read_field" has two dimensions
        but the second is not a vertical dimension (which read_field can't
        handle) and exits with both the correct return
        message, and with no Fortran files generated.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "var_bad_vertdim.xml")
        out_source_name = "physics_types_bad_vertdim"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"suite_simple.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust_bvd.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_bad_vertdim.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        vic_name = "phys_vars_init_check_bvd.F90"
        pi_name = "physics_inputs_bvd.F90"
        check_init_out = os.path.join(_TMP_DIR, vic_name)
        phys_input_out = os.path.join(_TMP_DIR, pi_name)
        # Comparison files:
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, vic_name)
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, pi_name)

        # Create local logger:
        logger = logging.getLogger("write_init_files_bvd")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile,
                      check_init_out, phys_input_out])

        # Generate registry files:
        _ = gen_registry(filename, 'se', _TMP_DIR, 3,
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

        # Run test
        retmsg = write_init.write_init_files(cap_database, {}, [], _TMP_DIR,
                                             find_file, _INC_SEARCH_DIRS,
                                             3, logger,
                                             phys_check_filename=vic_name,
                                             phys_input_filename=pi_name)

        # Check exception message
        emsg = ""
        self.assertEqual(emsg, retmsg)

        # For each output file, make sure it matches input file
        amsg = f"{check_init_out} does not match {check_init_in}"
        self.assertTrue(filecmp.cmp(check_init_out, check_init_in,
                                    shallow=False), msg=amsg)
        amsg = f"{phys_input_out} does not match {phys_input_in}"
        self.assertTrue(filecmp.cmp(phys_input_out, phys_input_in,
                                    shallow=False), msg=amsg)

##########

if __name__ == '__main__':
    unittest.main()
