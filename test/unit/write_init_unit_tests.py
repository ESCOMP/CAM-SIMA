#! /usr/bin/env python
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

# Find python version
PY3 = sys.version_info[0] > 2
if PY3:
    __FILE_OPEN = (lambda x: open(x, 'r', encoding='utf-8'))
else:
    __FILE_OPEN = (lambda x: open(x, 'r'))
# End if

#Check for all necessary directories:
if not os.path.exists(__CCPP_DIR):
    emsg = "Cannot find CCPP framework directory where 'ccpp_capgen.py' should be located."
    raise ImportError(emsg)

if not os.path.exists(__REGISTRY_DIR):
    emsg = "Cannot find registry directory where 'write_init_files.py' should be located."
    raise ImportError(emsg)

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

class WriteInitTest(unittest.TestCase):

    """Tests for `write_init_files`."""

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
        super(cls, WriteInitTest).setUpClass()

    def test_simple_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct fortran code given
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
        sdf = os.path.join(_INIT_SAMPLES_DIR,"simple_suite.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_simple.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_simple.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_simple.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_simple.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_simple.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_simple")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 3,
                                      _SRC_MOD_DIR, _CAM_ROOT,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate CCPP capgen files:
        capgen(host_files, scheme_files, sdf, cap_datafile,'',
               False, False, _TMP_DIR, 'cam', 'REAL64', logger)

        # Generate physics initialization files:
        retmsg = write_init.write_init_files(files, _TMP_DIR, 3,
                                             cap_datafile, logger,
                                             phys_check_filename="phys_vars_init_check_simple.F90",
                                             phys_input_filename="physics_inputs_simple.F90")

        # Check return message:
        amsg = "Test failure: retmsg={}".format(retmsg)
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = "{} does not exist".format(check_init_out)
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = "{} does not exist".format(phys_input_out)
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = "{} does not match {}".format(check_init_in, check_init_out)
        self.assertTrue(filecmp.cmp(check_init_in, check_init_out, shallow=False), \
                        msg=amsg)
        amsg = "{} does not match {}".format(phys_input_in, phys_input_out)
        self.assertTrue(filecmp.cmp(phys_input_in, phys_input_out, shallow=False), \
                        msg=amsg)

    def test_no_reqvar_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct fortran code given
        a CCPP physics suite with no required
        variables from the registry.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "simple_reg.xml")
        out_source_name = "physics_types_simple"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"simple_suite.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust_noreq.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_simple.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_noreq.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_noreq.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_noreq.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_noreq.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_noreq")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 3,
                                      _SRC_MOD_DIR, _CAM_ROOT,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate CCPP capgen files:
        capgen(host_files, scheme_files, sdf, cap_datafile,'',
               False, False, _TMP_DIR, 'cam', 'REAL64', logger)

        # Generate physics initialization files:
        retmsg = write_init.write_init_files(files, _TMP_DIR, 3,
                                             cap_datafile, logger,
                                             phys_check_filename="phys_vars_init_check_noreq.F90",
                                             phys_input_filename="physics_inputs_noreq.F90")

        # Check return message:
        amsg = "Test failure: retmsg={}".format(retmsg)
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = "{} does not exist".format(check_init_out)
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = "{} does not exist".format(phys_input_out)
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        #amsg = "{} does not match {}".format(check_init_in, check_init_out)
        #self.assertTrue(filecmp.cmp(check_init_in, check_init_out, shallow=False), \
        #                msg=amsg)
        #amsg = "{} does not match {}".format(phys_input_in, phys_input_out)
        #self.assertTrue(filecmp.cmp(phys_input_in, phys_input_out, shallow=False), \
        #                msg=amsg)

    def test_protected_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct fortran code given
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
        sdf = os.path.join(_INIT_SAMPLES_DIR,"simple_suite.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_simple.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_protect.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_protect.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_protect.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_protect.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_protect")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 3,
                                      _SRC_MOD_DIR, _CAM_ROOT,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate CCPP capgen files:
        capgen(host_files, scheme_files, sdf, cap_datafile,'',
               False, False, _TMP_DIR, 'cam', 'REAL64', logger)

        # Generate physics initialization files:
        retmsg = write_init.write_init_files(files, _TMP_DIR, 3,
                                              cap_datafile, logger,
                                              phys_check_filename="phys_vars_init_check_protect.F90",
                                              phys_input_filename="physics_inputs_protect.F90")

        # Check return message:
        amsg = "Test failure: retmsg={}".format(retmsg)
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = "{} does not exist".format(check_init_out)
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = "{} does not exist".format(phys_input_out)
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = "{} does not match {}".format(check_init_in, check_init_out)
        self.assertTrue(filecmp.cmp(check_init_in, check_init_out, shallow=False), \
                        msg=amsg)
        amsg = "{} does not match {}".format(phys_input_in, phys_input_out)
        self.assertTrue(filecmp.cmp(phys_input_in, phys_input_out, shallow=False), \
                        msg=amsg)

    def test_missing_var_write_init(self):
        """
        Test that the 'write_init_files' function
        correctly determines that a CCPP-required
        variable is missing from the host model,
        and exits with both the correct return
        message, and with no fortran files generated.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "missing_var_reg.xml")
        out_source_name = "physics_types_simple"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"missing_var_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"simple_suite.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_simple.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_simple.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_simple.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_simple.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_simple.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_missing")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 3,
                                      _SRC_MOD_DIR, _CAM_ROOT,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate CCPP capgen files:
        capgen(host_files, scheme_files, sdf, cap_datafile,'',
               False, False, _TMP_DIR, 'cam', 'REAL64', logger)

        # Generate physics initialization files:
        # Note: "assertLogs" method doesn't exist in python 2:
        if sys.version_info[0] > 2:
            with self.assertLogs('write_init_files_missing', level='ERROR') as cmp_log:
                retmsg = write_init.write_init_files(files, _TMP_DIR, 3,
                                                     cap_datafile, logger,
                                                     phys_check_filename="phys_vars_init_check_missing.F90",
                                                     phys_input_filename="physics_inputs_missing.F90")
        else:
            #If using Python 2, then set-up log handler to avoid warning message:
            handler = logging.StreamHandler(stream=sys.stderr)
            logger.handlers = [handler]
            logger.setLevel(logging.FATAL)
            retmsg = write_init.write_init_files(files, _TMP_DIR, 3,
                                                 cap_datafile, logger,
                                                 phys_check_filename="phys_vars_init_check_missing.F90",
                                                 phys_input_filename="physics_inputs_missing.F90")

        #Check logger (if python 3 or greater):
        if sys.version_info[0] > 2:
            amsg = "Test failure:  Logger output doesn't match what is expected." \
                   "Logger output is: \n{}".format(cmp_log.output)
            lmsg = "ERROR:write_init_files_missing:Required CCPP physics suite variables missing " \
                   "from registered host model variable list:\n " \
                   "potential_temperature"
            self.assertEqual(cmp_log.output, [lmsg], msg=amsg)

        # Check return message:
        amsg = "Test failure: retmsg={}".format(retmsg)
        self.assertEqual(retmsg,
                         "Required CCPPP physics variables missing from host model.",
                         msg=amsg)

        # Make sure no output file was created:
        amsg = "{} should not exist".format(check_init_out)
        self.assertFalse(os.path.exists(check_init_out), msg=amsg)
        amsg = "{} should not exist".format(phys_input_out)
        self.assertFalse(os.path.exists(phys_input_out), msg=amsg)

    def test_dual_standard_name_write_init(self):
        """
        Test that the 'write_init_files' function
        correctly determines that more than one
        variable has the same standard name,
        and exits with both the correct return
        message, and with no fortran files generated.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "two_stdnames_reg.xml")
        out_source_name = "physics_types_mf"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"simple_suite.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_simple.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_mf.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_mf.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_two.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_two.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_two_stdnames")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 3,
                                      _SRC_MOD_DIR, _CAM_ROOT,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate CCPP capgen files:
        capgen(host_files, scheme_files, sdf, cap_datafile,'',
               False, False, _TMP_DIR, 'cam', 'REAL64', logger)

        # Run test
        with self.assertRaises(ValueError) as verr:
            retmsg = write_init.write_init_files(files, _TMP_DIR, 3,
                                                     cap_datafile, logger,
                                                     phys_check_filename="phys_vars_init_check_two.F90",
                                                     phys_input_filename="physics_inputs_two.F90")

        # Check exception message
        emsg = "Multiple registered variables have the" \
               " standard name 'potential_temperature'.\n" \
               "There can only be one registered variable per" \
               " standard name.\nThe meta files containing the" \
               " conflicting variables are:\nref_two\nphysics_types_mf"

        # Make sure no output file was created:
        amsg = "{} should not exist".format(check_init_out)
        self.assertFalse(os.path.exists(check_init_out), msg=amsg)
        amsg = "{} should not exist".format(phys_input_out)
        self.assertFalse(os.path.exists(phys_input_out), msg=amsg)

    def test_no_horiz_var_write_init(self):
        """
        Test that the 'write_init_files' function
        correctly determines that a variable that
        could be called from "read_field" has no
        dimension labeled "horizontal_dimension"
        and exits with both the correct return
        message, and with no fortran files generated.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "no_horiz_dim_reg.xml")
        out_source_name = "physics_types_no_horiz"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"simple_suite.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust_no_horiz.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_simple.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_simple.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_simple.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_no_horiz.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_no_horiz.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_no_horiz")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 3,
                                      _SRC_MOD_DIR, _CAM_ROOT,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate CCPP capgen files:
        capgen(host_files, scheme_files, sdf, cap_datafile,'',
               False, False, _TMP_DIR, 'cam', 'REAL64', logger)

        # Run test
        with self.assertRaises(ValueError) as verr:
            retmsg = write_init.write_init_files(files, _TMP_DIR, 3,
                                                     cap_datafile, logger,
                                                     phys_check_filename="phys_vars_init_check_no_horiz.F90",
                                                     phys_input_filename="physics_inputs_no_horiz.F90")

        # Check exception message
        emsg = "Variable 'sea_level_pressure' needs at least one registered dimension" \
               " to be 'horizontal_dimension' in order to be read from a file using 'read_fied'.\n" \
               "Instead variable has dimensions of: ['vertical_layer_dimension']"
        self.assertEqual(emsg, str(verr.exception))

        # Make sure no output file was created:
        amsg = "{} should not exist".format(check_init_out)
        self.assertFalse(os.path.exists(check_init_out), msg=amsg)
        amsg = "{} should not exist".format(phys_input_out)
        self.assertFalse(os.path.exists(phys_input_out), msg=amsg)

    def test_scalar_var_write_init(self):
        """
        Test that the 'write_init_files' function
        correctly determines that a variable that
        could be called from "read_field" is a
        scalar (which read_field can't handle)
        and exits with both the correct return
        message, and with no fortran files generated.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "scalar_var_reg.xml")
        out_source_name = "physics_types_simple"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"simple_suite.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust_scalar.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_simple.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_simple.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_simple.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_scalar.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_scalar.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_scalar")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 3,
                                      _SRC_MOD_DIR, _CAM_ROOT,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate CCPP capgen files:
        capgen(host_files, scheme_files, sdf, cap_datafile,'',
               False, False, _TMP_DIR, 'cam', 'REAL64', logger)

        # Run test
        with self.assertRaises(ValueError) as verr:
            retmsg = write_init.write_init_files(files, _TMP_DIR, 3,
                                                     cap_datafile, logger,
                                                     phys_check_filename="phys_vars_init_check_scalar.F90",
                                                     phys_input_filename="physics_inputs_scalar.F90")

        # Check exception message
        emsg = "Variable 'sea_level_pressure' needs at least one dimension in order" \
               " to be read from a file using 'read_field'."
        self.assertEqual(emsg, str(verr.exception))

        # Make sure no output file was created:
        amsg = "{} should not exist".format(check_init_out)
        self.assertFalse(os.path.exists(check_init_out), msg=amsg)
        amsg = "{} should not exist".format(phys_input_out)
        self.assertFalse(os.path.exists(phys_input_out), msg=amsg)

    def test_4D_var_write_init(self):
        """
        Test that the 'write_init_files' function
        correctly determines that a variable that
        could be called from "read_field" has more
        than two dimensions (which read_field can't handle)
        and exits with both the correct return
        message, and with no fortran files generated.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "var_4D_reg.xml")
        out_source_name = "physics_types_simple"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"simple_suite.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust_4D.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_simple.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_simple.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_simple.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_4D.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_4D.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_4D")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 3,
                                      _SRC_MOD_DIR, _CAM_ROOT,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate CCPP capgen files:
        capgen(host_files, scheme_files, sdf, cap_datafile,'',
               False, False, _TMP_DIR, 'cam', 'REAL64', logger)

        # Run test
        with self.assertRaises(ValueError) as verr:
            retmsg = write_init.write_init_files(files, _TMP_DIR, 3,
                                                     cap_datafile, logger,
                                                     phys_check_filename="phys_vars_init_check_4D.F90",
                                                     phys_input_filename="physics_inputs_4D.F90")

        # Check exception message
        emsg = "variable 'sea_level_pressure' has more than two dimensions, but" \
               "'read_field' can only manage up to two dimensions" \
               "when reading a variable from a file."
        self.assertEqual(emsg, str(verr.exception))

        # Make sure no output file was created:
        amsg = "{} should not exist".format(check_init_out)
        self.assertFalse(os.path.exists(check_init_out), msg=amsg)
        amsg = "{} should not exist".format(phys_input_out)
        self.assertFalse(os.path.exists(phys_input_out), msg=amsg)

    @unittest.skip("Re-implement once temporary IC/local name substitution kludge has been removed.")
    def test_missing_ic_names_write_init(self):
        """
        Test that the 'write_init_files' function
        correctly determines that no IC names are
        present in the host model registry,
        and exits with both the correct return code,
        and with no fortran files generated.
        """

        # Setup registry inputs:
        filename = os.path.join(_INIT_SAMPLES_DIR, "missing_ICs_reg.xml")
        out_source_name = "physics_types_simple"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup capgen inputs:
        model_host = os.path.join(_INIT_SAMPLES_DIR,"simple_host.meta")
        sdf = os.path.join(_INIT_SAMPLES_DIR,"simple_suite.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_simple.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_simple.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_simple.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_simple.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_simple.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_IC_names_missing")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 3,
                                      _SRC_MOD_DIR, _CAM_ROOT,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate CCPP capgen files:
        capgen(host_files, scheme_files, sdf, cap_datafile,'',
               False, False, _TMP_DIR, 'cam', 'REAL64', logger)

        # Generate physics initialization files:
        # Note: "assertLogs" method doesn't exist in python 2:
        if sys.version_info[0] > 2:
            with self.assertLogs('write_init_IC_names_missing', level='ERROR') as cmp_log:
                retmsg = write_init.write_init_files(files, _TMP_DIR, 3,
                                                     cap_datafile, logger,
                                                     phys_check_filename="phys_vars_init_check_missing.F90",
                                                     phys_input_filename="physics_inputs_missing.F90")
        else:
            #If using Python 2, then set-up log handler to avoid warning message:
            handler = logging.StreamHandler(stream=sys.stderr)
            logger.handlers = [handler]
            logger.setLevel(logging.FATAL)
            retmsg = write_init.write_init_files(files, _TMP_DIR, 3,
                                                 cap_datafile, logger,
                                                 phys_check_filename="phys_vars_init_check_missing.F90",
                                                 phys_input_filename="physics_inputs_missing.F90")

        #Check logger (if python 3 or greater):
        if sys.version_info[0] > 2:
            amsg = "Test failure:  Logger output doesn't match what is expected." \
                   "Logger output is: \n{}".format(cmp_log.output)
            lmsg = "ERROR:write_init_IC_names_missing:No '<ic_file_input_names>' tags exist in registry.xml" \
                   ", so no input variable name array will be created."
            self.assertEqual(cmp_log.output, [lmsg], msg=amsg)

        # Check return code:
        amsg = "Test failure: retmsg={}".format(retmsg)
        self.assertEqual(retmsg,"No input names (<ic_file_input_names>) present in registry.", msg=amsg)

        # Make sure no output file was created:
        amsg = "{} should not exist".format(check_init_out)
        self.assertFalse(os.path.exists(check_init_out), msg=amsg)
        amsg = "{} should not exist".format(phys_input_out)
        self.assertFalse(os.path.exists(phys_input_out), msg=amsg)

    def test_ddt_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct fortran code given
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
        sdf = os.path.join(_INIT_SAMPLES_DIR,"simple_suite.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_simple.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_ddt.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_ddt.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_ddt.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_ddt.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_ddt")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 3,
                                      _SRC_MOD_DIR, _CAM_ROOT,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate CCPP capgen files:
        capgen(host_files, scheme_files, sdf, cap_datafile,'',
               False, False, _TMP_DIR, 'cam', 'REAL64', logger)

        # Generate physics initialization files:
        retmsg = write_init.write_init_files(files, _TMP_DIR, 3,
                                             cap_datafile, logger,
                                             phys_check_filename="phys_vars_init_check_ddt.F90",
                                             phys_input_filename="physics_inputs_ddt.F90")

        # Check return code:
        amsg = "Test failure: retmsg={}".format(retmsg)
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = "{} does not exist".format(check_init_out)
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = "{} does not exist".format(phys_input_out)
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = "{} does not match {}".format(check_init_in, check_init_out)
        self.assertTrue(filecmp.cmp(check_init_in, check_init_out, shallow=False), \
                        msg=amsg)
        amsg = "{} does not match {}".format(phys_input_in, phys_input_out)
        self.assertTrue(filecmp.cmp(phys_input_in, phys_input_out, shallow=False), \
                        msg=amsg)

    def test_ddt2_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct fortran code given
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
        sdf = os.path.join(_INIT_SAMPLES_DIR,"simple_suite.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_simple.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_ddt2.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_ddt2.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_ddt2.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_ddt2.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_ddt2")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 3,
                                      _SRC_MOD_DIR, _CAM_ROOT,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate CCPP capgen files:
        capgen(host_files, scheme_files, sdf, cap_datafile,'',
               False, False, _TMP_DIR, 'cam', 'REAL64', logger)

        # Generate physics initialization files:
        retmsg = write_init.write_init_files(files, _TMP_DIR, 3,
                                             cap_datafile, logger,
                                             phys_check_filename="phys_vars_init_check_ddt2.F90",
                                             phys_input_filename="physics_inputs_ddt2.F90")

        # Check return code:
        amsg = "Test failure: retmsg={}".format(retmsg)
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = "{} does not exist".format(check_init_out)
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = "{} does not exist".format(phys_input_out)
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = "{} does not match {}".format(check_init_in, check_init_out)
        self.assertTrue(filecmp.cmp(check_init_in, check_init_out, shallow=False), \
                        msg=amsg)
        amsg = "{} does not match {}".format(phys_input_in, phys_input_out)
        self.assertTrue(filecmp.cmp(phys_input_in, phys_input_out, shallow=False), \
                        msg=amsg)

    def test_ddt_array_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct fortran code given
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
        sdf = os.path.join(_INIT_SAMPLES_DIR,"simple_suite.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_simple.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_ddt_array.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_ddt_array.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_ddt_array.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_ddt_array.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_ddt_array")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 3,
                                      _SRC_MOD_DIR, _CAM_ROOT,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate CCPP capgen files:
        capgen(host_files, scheme_files, sdf, cap_datafile,'',
               False, False, _TMP_DIR, 'cam', 'REAL64', logger)

        # Generate physics initialization files:
        retmsg = write_init.write_init_files(files, _TMP_DIR, 3,
                                             cap_datafile, logger,
                                             phys_check_filename="phys_vars_init_check_ddt_array.F90",
                                             phys_input_filename="physics_inputs_ddt_array.F90")

        # Check return code:
        amsg = "Test failure: retmsg={}".format(retmsg)
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = "{} does not exist".format(check_init_out)
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = "{} does not exist".format(phys_input_out)
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = "{} does not match {}".format(check_init_in, check_init_out)
        self.assertTrue(filecmp.cmp(check_init_in, check_init_out, shallow=False), \
                        msg=amsg)
        amsg = "{} does not match {}".format(phys_input_in, phys_input_out)
        self.assertTrue(filecmp.cmp(phys_input_in, phys_input_out, shallow=False), \
                        msg=amsg)

    def test_meta_file_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct fortran code given
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
        sdf = os.path.join(_INIT_SAMPLES_DIR,"simple_suite.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_simple.xml")

        host_files = [model_host, model_mf_file, out_meta]

        # Setup write_init_files inputs:
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_mf.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_mf.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_mf.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_mf.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_mf")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 3,
                                      _SRC_MOD_DIR, _CAM_ROOT,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate CCPP capgen files:
        capgen(host_files, scheme_files, sdf, cap_datafile,'',
               False, False, _TMP_DIR, 'cam', 'REAL64', logger)

        # Generate physics initialization files:
        retmsg = write_init.write_init_files(files, _TMP_DIR, 3,
                                             cap_datafile, logger,
                                             phys_check_filename="phys_vars_init_check_mf.F90",
                                             phys_input_filename="physics_inputs_mf.F90")

        # Check return code:
        amsg = "Test failure: retmsg={}".format(retmsg)
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = "{} does not exist".format(check_init_out)
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = "{} does not exist".format(phys_input_out)
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = "{} does not match {}".format(check_init_in, check_init_out)
        self.assertTrue(filecmp.cmp(check_init_in, check_init_out, shallow=False), \
                        msg=amsg)
        amsg = "{} does not match {}".format(phys_input_in, phys_input_out)
        self.assertTrue(filecmp.cmp(phys_input_in, phys_input_out, shallow=False), \
                        msg=amsg)

    def test_parameter_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct fortran code given
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
        sdf = os.path.join(_INIT_SAMPLES_DIR,"simple_suite.xml")
        scheme_files = os.path.join(_INIT_SAMPLES_DIR, "temp_adjust_param.meta")
        cap_datafile = os.path.join(_TMP_DIR, "datatable_simple.xml")

        host_files = [model_host, out_meta]

        # Setup write_init_files inputs:
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_param.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_param.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_param.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_param.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_param")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, cap_datafile, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 3,
                                      _SRC_MOD_DIR, _CAM_ROOT,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate CCPP capgen files:
        capgen(host_files, scheme_files, sdf, cap_datafile,'',
               False, False, _TMP_DIR, 'cam', 'REAL64', logger)

        # Generate physics initialization files:
        retmsg = write_init.write_init_files(files, _TMP_DIR, 3,
                                             cap_datafile, logger,
                                             phys_check_filename="phys_vars_init_check_param.F90",
                                             phys_input_filename="physics_inputs_param.F90")

        # Check return code:
        amsg = "Test failure: retmsg={}".format(retmsg)
        self.assertEqual(retmsg, '', msg=amsg)

        # Make sure each output file was created:
        amsg = "{} does not exist".format(check_init_out)
        self.assertTrue(os.path.exists(check_init_out), msg=amsg)
        amsg = "{} does not exist".format(phys_input_out)
        self.assertTrue(os.path.exists(phys_input_out), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = "{} does not match {}".format(check_init_in, check_init_out)
        self.assertTrue(filecmp.cmp(check_init_in, check_init_out, shallow=False), \
                        msg=amsg)
        amsg = "{} does not match {}".format(phys_input_in, phys_input_out)
        self.assertTrue(filecmp.cmp(phys_input_in, phys_input_out, shallow=False), \
                        msg=amsg)

##########

if __name__ == '__main__':
    unittest.main()
