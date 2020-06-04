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
import xml.etree.ElementTree as ET

__TEST_DIR = os.path.dirname(os.path.abspath(__file__))
__CAM_ROOT = os.path.abspath(os.path.join(__TEST_DIR, os.pardir, os.pardir))
__REGISTRY_DIR = os.path.join(__CAM_ROOT, "src", "data")
_REG_SAMPLES_DIR = os.path.join(__TEST_DIR, "sample_files")
_INIT_SAMPLES_DIR = os.path.join(_REG_SAMPLES_DIR, "write_init_files")
_PRE_TMP_DIR = os.path.join(__TEST_DIR, "tmp")
_TMP_DIR = os.path.join(_PRE_TMP_DIR, "write_init_files")

# Find python version
PY3 = sys.version_info[0] > 2
if PY3:
    __FILE_OPEN = (lambda x: open(x, 'r', encoding='utf-8'))
else:
    __FILE_OPEN = (lambda x: open(x, 'r'))
# End if

#Check for all necessary directories:
if not os.path.exists(__REGISTRY_DIR):
    emsg = "Cannot find registry directory where 'write_init_files.py' should be located."
    raise ImportError(emsg)

if not os.path.exists(_REG_SAMPLES_DIR):
    raise ImportError("Cannot find sample files directory")

if not os.path.exists(_INIT_SAMPLES_DIR):
    raise ImportError("Cannot find 'write_init_files' sample files directory")

#Add registry directory to python path to import
#registry and 'phys_init' code generators:
sys.path.append(__REGISTRY_DIR)

# pylint: disable=wrong-import-position
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
def read_xml_file(filename):
###############################################################################
    """Read XML file, <filename>, and return the XML tree and root
    As this is a test script, errors will throw exceptions."""
    with __FILE_OPEN(filename) as file_:
        tree = ET.parse(file_)
        root = tree.getroot()
    # End with
    return tree, root

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
        # End if
        remove_files(glob.iglob(os.path.join(_TMP_DIR, '*')))

        #Run inherited setup method:
        super(cls, WriteInitTest).setUpClass()

    def test_good_complete_registry(self):
        """
        Test that a good registry with variables, meta-data files,
        DDTs, Arrays, and parameters validates, i.e. try and test
        everything at once.

        Specifically, check that generate_registry_data.py generates
        good Fortran and metadata files, which is needed for the
        write_init_files tests.

        If this test fails, then don't run any other tests, and
        tell user to run the registry unit tests (test_registry.py)
        first.
        """

        #Create test result handler:
        result = unittest.TestResult()

        # Setup test
        filename = os.path.join(_INIT_SAMPLES_DIR, "reg_good_complete.xml")
        out_source_name = "physics_types_complete"
        in_source = os.path.join(_INIT_SAMPLES_DIR, out_source_name + '.F90')
        in_meta = os.path.join(_INIT_SAMPLES_DIR, out_source_name + '.meta')
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])

        # Run test
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 2,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Check return code
        amsg = "Test failure: retcode={}".format(retcode)
        self.assertEqual(retcode, 0, msg=amsg)
        flen = len(files)
        amsg = "Test failure: Found {} files, expected 1".format(flen)
        self.assertEqual(flen, 2, msg=amsg)

        # Make sure each output file was created
        amsg = "{} does not exist".format(out_meta)
        self.assertTrue(os.path.exists(out_meta), msg=amsg)
        amsg = "{} does not exist".format(out_source)
        self.assertTrue(os.path.exists(out_source), msg=amsg)

        # For each output file, make sure it matches input file
        #amsg = "{} does not match {}".format(in_meta, out_meta)
        #self.assertTrue(filecmp.cmp(in_meta, out_meta, shallow=False), msg=amsg)
        #amsg = "{} does not match {}".format(in_source, out_source)
        #self.assertTrue(filecmp.cmp(in_source, out_source, shallow=False),
        #                msg=amsg)

        # Check that the metadata file has the correct number of variables
        mfile = files[1]
        mvars = mfile.variable_list()
        num_vars = len(mvars)
        amsg = "Expected 14 metadata variables, found {}".format(num_vars)
        self.assertEqual(num_vars, 14, msg=amsg)

        #Check if any tests failed:
        if not result.wasSuccessful():
            #If so, then print message to screen, and stop all additional tests:
            msg = "Registry generation failed!\nPlease run 'test_registry.py' "\
                  "first, and make sure all tests pass, before running this test suite."
            self.assertTrue(False, msg=msg) #Use "assert" to print message.
            self.stop()

    def test_simple_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct fortran code given
        a simple registry and CCPP datatable with
        only variables.
        """

        # Setup registry inputs:
        filename = os.path.join(_REG_SAMPLES_DIR, "reg_good_simple.xml")
        out_source_name = "physics_types_simple"
        in_source = os.path.join(_REG_SAMPLES_DIR, out_source_name + '.F90')
        in_meta = os.path.join(_REG_SAMPLES_DIR, out_source_name + '.meta')
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup write_init_files inputs:
        cap_datafile = os.path.join(_INIT_SAMPLES_DIR, "ccpp_datatable_simple.xml")
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_simple.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_simple.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_simple.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_simple.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_simple")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 2,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate physics initialization files:
        retcode = write_init.write_init_files(files, _TMP_DIR, 2,
                                              cap_datafile, logger,
                                              phys_check_filename="phys_vars_init_check_simple.F90",
                                              phys_input_filename="physics_inputs_simple.F90")

        # Check return code:
        amsg = "Test failure: retcode={}".format(retcode)
        self.assertEqual(retcode, 0, msg=amsg)

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
        and exits with both the correct return code,
        and with no fortran files generated.
        """

        # Setup registry inputs:
        filename = os.path.join(_REG_SAMPLES_DIR, "reg_good_simple.xml")
        out_source_name = "physics_types_simple"
        in_source = os.path.join(_REG_SAMPLES_DIR, out_source_name + '.F90')
        in_meta = os.path.join(_REG_SAMPLES_DIR, out_source_name + '.meta')
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup write_init_files inputs:
        cap_datafile = os.path.join(_INIT_SAMPLES_DIR, "ccpp_datatable_missing.xml")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_missing.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_missing.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_missing")

        # Clear all temporary output files:
        remove_files([check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 2,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate physics initialization files:
        # Note: "assertLogs" method doesn't exist in python 2:
        if sys.version_info[0] > 2:
            with self.assertLogs('write_init_files_missing', level='ERROR') as cmp_log:
                retcode = write_init.write_init_files(files, _TMP_DIR, 2,
                                                      cap_datafile, logger,
                                                      phys_check_filename="phys_vars_init_check_missing.F90",
                                                      phys_input_filename="physics_inputs_missing.F90")
        else:
            #If using Python 2, then set-up log handler to avoid warning message:
            handler = logging.StreamHandler(stream=sys.stderr)
            logger.handlers = [handler]
            logger.setLevel(logging.FATAL)
            #Now run write_init_files:
            retcode = write_init.write_init_files(files, _TMP_DIR, 2,
                                                      cap_datafile, logger,
                                                      phys_check_filename="phys_vars_init_check_missing.F90",
                                                      phys_input_filename="physics_inputs_missing.F90")

        #Check logger:
        if sys.version_info[0] > 2:
            amsg = "Test failure:  Logger output doesn't match what is expected." \
                   "Logger output is: \n{}".format(cmp_log.output)
            lmsg = "ERROR:write_init_files_missing:Required CCPP physics suite variables missing " \
                   "from registered host model variable list:\n " \
                   "missing_required_var"
            self.assertEqual(cmp_log.output, [lmsg], msg=amsg)

        # Make sure each output file was created:
        amsg = "{} should not exist".format(check_init_out)
        self.assertFalse(os.path.exists(check_init_out), msg=amsg)
        amsg = "{} should not exist".format(phys_input_out)
        self.assertFalse(os.path.exists(phys_input_out), msg=amsg)

    def test_ddt2_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct fortran code given
        a registry and CCPP datatable which
        contain variables and a DDT.
        """

        # Setup registry inputs:
        filename = os.path.join(_REG_SAMPLES_DIR, "reg_good_ddt2.xml")
        out_source_name = "physics_types_ddt2"
        in_source = os.path.join(_REG_SAMPLES_DIR, out_source_name + '.F90')
        in_meta = os.path.join(_REG_SAMPLES_DIR, out_source_name + '.meta')
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup write_init_files inputs:
        cap_datafile = os.path.join(_INIT_SAMPLES_DIR, "ccpp_datatable_ddt2.xml")
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_ddt2.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_ddt2.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_ddt2.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_ddt2.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_ddt2")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 2,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate physics initialization files:
        retcode = write_init.write_init_files(files, _TMP_DIR, 2,
                                              cap_datafile, logger,
                                              phys_check_filename="phys_vars_init_check_ddt2.F90",
                                              phys_input_filename="physics_inputs_ddt2.F90")

        # Check return code:
        amsg = "Test failure: retcode={}".format(retcode)
        self.assertEqual(retcode, 0, msg=amsg)

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
        a registry and CCPP datatable which
        contain Array variables and a DDT.
        """

        # Setup registry inputs:
        filename = os.path.join(_REG_SAMPLES_DIR, "reg_good_ddt_array.xml")
        out_source_name = "physics_types_ddt_array"
        in_source = os.path.join(_REG_SAMPLES_DIR, out_source_name + '.F90')
        in_meta = os.path.join(_REG_SAMPLES_DIR, out_source_name + '.meta')
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup write_init_files inputs:
        cap_datafile = os.path.join(_INIT_SAMPLES_DIR, "ccpp_datatable_ddt_array.xml")
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_ddt_array.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_ddt_array.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_ddt_array.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_ddt_array.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_ddt_array")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 2,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate physics initialization files:
        retcode = write_init.write_init_files(files, _TMP_DIR, 2,
                                              cap_datafile, logger,
                                              phys_check_filename="phys_vars_init_check_ddt_array.F90",
                                              phys_input_filename="physics_inputs_ddt_array.F90")

        # Check return code:
        amsg = "Test failure: retcode={}".format(retcode)
        self.assertEqual(retcode, 0, msg=amsg)

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


    #########################################
    #TEST INIT FILE GENERATION WITH META-DATA
    #########################################

    def test_meta_file_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct fortran code given
        a registry with a metadata file tag.
        """

        # Setup registry inputs:
        filename = os.path.join(_REG_SAMPLES_DIR, "reg_good_mf.xml")
        out_source_name = "physics_types_ddt"
        in_source = os.path.join(_REG_SAMPLES_DIR, out_source_name + '.F90')
        in_meta = os.path.join(_REG_SAMPLES_DIR, out_source_name + '.meta')
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')

        # Setup write_init_files inputs:
        cap_datafile = os.path.join(_INIT_SAMPLES_DIR, "ccpp_datatable_mf.xml")
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_mf.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_mf.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_mf.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_mf.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_mf")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 2,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate physics initialization files:
        retcode = write_init.write_init_files(files, _TMP_DIR, 2,
                                              cap_datafile, logger,
                                              phys_check_filename="phys_vars_init_check_mf.F90",
                                              phys_input_filename="physics_inputs_mf.F90")

        # Check return code:
        amsg = "Test failure: retcode={}".format(retcode)
        self.assertEqual(retcode, 0, msg=amsg)

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

    #########################################
    #TEST INIT FILE GENERATION WITH PARAMETER
    #########################################

    def test_parameter_reg_write_init(self):
        """
        Test that the 'write_init_files' function
        generates the correct fortran code given
        a simple registry and CCPP datatable with
        a variable that is allocated as a parameter.
        """

        # Setup registry inputs:
        infilename = os.path.join(_REG_SAMPLES_DIR, "reg_good_simple.xml")
        filename = os.path.join(_TMP_DIR, "reg_parameter.xml")
        out_source_name = "physics_types_parameter"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])
        tree, root = read_xml_file(infilename)
        # Change output filename and add a parameter with an initial value
        for obj in root:
            oname = obj.get('name')
            if (obj.tag == 'file') and (oname == 'physics_types_simple'):
                # Reset the filename
                obj.set('name', out_source_name)
                # Add a new variable with an unknown dimension
                new_var = ET.SubElement(obj, "variable")
                new_var.set("local_name", "pver")
                new_var.set("standard_name", "vertical_layer_dimension")
                new_var.set("units", "count")
                new_var.set("type", "integer")
                new_var.set("allocatable", "parameter")
                dims_elem = ET.SubElement(new_var, "initial_value")
                dims_elem.text = '42'
                break
            # End if
        # End for
        tree.write(filename)

        # Setup write_init_files inputs:
        cap_datafile = os.path.join(_INIT_SAMPLES_DIR, "ccpp_datatable_parameter.xml")
        check_init_in = os.path.join(_INIT_SAMPLES_DIR, "phys_vars_init_check_parameter.F90")
        phys_input_in = os.path.join(_INIT_SAMPLES_DIR, "physics_inputs_parameter.F90")
        check_init_out = os.path.join(_TMP_DIR, "phys_vars_init_check_parameter.F90")
        phys_input_out = os.path.join(_TMP_DIR, "physics_inputs_parameter.F90")

        #Create local logger:
        logger = logging.getLogger("write_init_files_parameter")

        # Clear all temporary output files:
        remove_files([out_source, out_meta, check_init_out, phys_input_out])

        # Generate registry files:
        retcode, files = gen_registry(filename, 'se', {}, _TMP_DIR, 2,
                                      loglevel=logging.ERROR,
                                      error_on_no_validate=True)

        # Generate physics initialization files:
        retcode = write_init.write_init_files(files, _TMP_DIR, 2,
                                              cap_datafile, logger,
                                              phys_check_filename="phys_vars_init_check_parameter.F90",
                                              phys_input_filename="physics_inputs_parameter.F90")

        # Check return code:
        amsg = "Test failure: retcode={}".format(retcode)
        self.assertEqual(retcode, 0, msg=amsg)

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
