#! /usr/bin/env python3
#-----------------------------------------------------------------------
# Description:  Contains unit tests for testing CAM registry validation
#               and code generation
#
# Assumptions:
#
# Command line arguments: none
#
# Usage: python test_registry.py         # run the unit tests
#-----------------------------------------------------------------------

"""Test gen_registry in generate_registry_data.py"""

import sys
import os
import glob
import unittest
import filecmp
import logging
import shutil
import xml.etree.ElementTree as ET

__TEST_DIR = os.path.dirname(os.path.abspath(__file__))
_CAM_ROOT = os.path.abspath(os.path.join(__TEST_DIR, os.pardir, os.pardir, os.pardir))
__REGISTRY_DIR = os.path.join(_CAM_ROOT, "src", "data")
_SAMPLE_FILES_DIR = os.path.join(__TEST_DIR, "sample_files")
_TMP_DIR = os.path.join(__TEST_DIR, "tmp")
_SRC_MOD_DIR = os.path.join(_TMP_DIR, "SourceMods")

__FILE_OPEN = (lambda x: open(x, 'r', encoding='utf-8'))

if not os.path.exists(__REGISTRY_DIR):
    raise ImportError("Cannot find registry directory")

if not os.path.exists(_SAMPLE_FILES_DIR):
    raise ImportError("Cannot find sample files directory")

sys.path.append(__REGISTRY_DIR)

# pylint: disable=wrong-import-position
from generate_registry_data import gen_registry
from generate_registry_data import metadata_file_to_files, TypeRegistry
from framework_env import CCPPFrameworkEnv
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
def read_xml_file(filename):
###############################################################################
    """Read XML file, <filename>, and return the XML tree and root
    As this is a test script, errors will throw exceptions."""
    with __FILE_OPEN(filename) as file_:
        tree = ET.parse(file_)
        root = tree.getroot()
    # End with
    return tree, root

class RegistryTest(unittest.TestCase):

    """Tests for `gen_registry`."""

    @classmethod
    def setUpClass(cls):
        """Clean output directory (tmp) before running tests"""
        if not os.path.exists(_TMP_DIR):
            os.makedirs(_TMP_DIR)
        if not os.path.exists(_SRC_MOD_DIR):
            os.makedirs(_SRC_MOD_DIR)

        remove_files(glob.iglob(os.path.join(_TMP_DIR, '*.*')))
        remove_files(glob.iglob(os.path.join(_SRC_MOD_DIR, '*.*')))
        super(cls, RegistryTest).setUpClass()

    def test_good_simple_registry(self):
        """Test that a good registry with only variables validates.
        Check that generate_registry_data.py generates good
        Fortran and metadata files. Expectation is that the initial
        value for ncol will be set to the default of 0 because there is no
        dycore match"""
        # Setup test
        filename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_simple.xml")
        out_source_name = "physics_types_simple"
        in_source = os.path.join(_SAMPLE_FILES_DIR, out_source_name + '.F90')
        in_meta = os.path.join(_SAMPLE_FILES_DIR, out_source_name + '.meta')
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])
        # Run test
        retcode, files, _, _, _ = gen_registry(filename, 'fv', _TMP_DIR, 2,
                                               _SRC_MOD_DIR, _CAM_ROOT,
                                               loglevel=logging.ERROR,
                                               error_on_no_validate=True)
        # Check return code
        amsg = f"Test failure: retcode={retcode}"
        self.assertEqual(retcode, 0, msg=amsg)
        flen = len(files)
        amsg = f"Test failure: Found {flen} files, expected 1"
        self.assertEqual(flen, 1, msg=amsg)
        # Make sure each output file was created
        amsg = f"{out_meta} does not exist"
        self.assertTrue(os.path.exists(out_meta), msg=amsg)
        amsg = f"{out_source} does not exist"
        self.assertTrue(os.path.exists(out_source), msg=amsg)
        # For each output file, make sure it matches input file
        amsg = f"{out_meta} does not match {in_meta}"
        self.assertTrue(filecmp.cmp(out_meta, in_meta, shallow=False), msg=amsg)
        amsg = f"{out_source} does not match {in_source}"
        self.assertTrue(filecmp.cmp(out_source, in_source, shallow=False),
                        msg=amsg)

    def test_bad_registry_xml(self):
        """Test that the full error messages from xmllint is returned"""
        # Setup test
        filename = os.path.join(_SAMPLE_FILES_DIR, "reg_bad_xml.xml")
        out_name = "physics_types_bad"
        # Try to generate the registry
        with self.assertRaises(CCPPError) as cerr:
            retcode, files, _ = gen_registry(filename, 'se', _TMP_DIR, 2,
                                             _SRC_MOD_DIR, _CAM_ROOT,
                                             loglevel=logging.ERROR,
                                             error_on_no_validate=True)
        # end with
        expected_error = "reg_bad_xml.xml:32: element ic_file_input_name: Schemas validity error : Element 'ic_file_input_name': This element is not expected. Expected is one of ( initial_value, ic_file_input_names )."
        split_exception = str(cerr.exception).split('\n')
        amsg = f"Test failure: exception raised is {len(split_exception)} lines long and is expected to be 4"
        self.assertEqual(len(split_exception), 4, msg=amsg)
        # Check that the full xmllint message was returned
        self.assertTrue(split_exception[2].endswith(expected_error))

    def test_good_ddt_registry(self):
        """Test code and metadata generation from a good registry with a DDT.
        Check that generate_registry_data.py generates good
        Fortran and metadata files.
        Check that the DDT contains the proper information
        depending on dycore"""
        # Setup test
        filename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_ddt.xml")
        out_name = "physics_types_ddt"
        for dycore in ['fv', 'eul', 'se']:
            out_source_name = out_name + '_' + dycore + '.F90'
            out_meta_name = out_name + '_' + dycore + '.meta'
            in_source = os.path.join(_SAMPLE_FILES_DIR, out_source_name)
            in_meta = os.path.join(_SAMPLE_FILES_DIR, out_meta_name)
            gen_source = os.path.join(_TMP_DIR, out_name + '.F90')
            gen_meta = os.path.join(_TMP_DIR, out_name + '.meta')
            out_source = os.path.join(_TMP_DIR, out_source_name)
            out_meta = os.path.join(_TMP_DIR, out_meta_name)
            remove_files([out_source, out_meta])
            # Run dycore
            retcode, files, _, _, _ = gen_registry(filename, dycore, _TMP_DIR, 2,
                                                   _SRC_MOD_DIR, _CAM_ROOT,
                                                   loglevel=logging.ERROR,
                                                   error_on_no_validate=True)
            # Check return code
            amsg = f"Test failure for dycore = {dycore}, retcode={retcode}"
            self.assertEqual(retcode, 0, msg=amsg)
            flen = len(files)
            amsg = f"Test failure for {dycore} dycore: Found {flen} files, expected 1"
            self.assertEqual(flen, 1, msg=amsg)
            # Make sure each output file was created
            if os.path.exists(gen_meta):
                os.rename(gen_meta, out_meta)
            # End if
            if os.path.exists(gen_source):
                os.rename(gen_source, out_source)
            # End if
            amsg = f"{out_meta} does not exist"
            self.assertTrue(os.path.exists(out_meta), msg=amsg)
            amsg = f"{out_source} does not exist"
            self.assertTrue(os.path.exists(out_source), msg=amsg)
            # For each output file, make sure it matches input file
            amsg = f"{out_meta} does not match {in_meta}"
            self.assertTrue(filecmp.cmp(out_meta, in_meta,
                                        shallow=False), msg=amsg)
            amsg = f"{out_source} does not match {in_source}"
            self.assertTrue(filecmp.cmp(out_source, in_source,
                                        shallow=False), msg=amsg)
        # End for
    def test_good_ddt_registry2(self):
        """Test code and metadata generation from a good registry with DDTs
        with extends and bindC attributes.
        Check that generate_registry_data.py generates good
        Fortran and metadata files.
        Check that the DDT contains the proper information
        depending on dycore"""
        # Setup test
        filename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_ddt2.xml")
        out_name = "physics_types_ddt2"
        out_source_name = out_name + '.F90'
        out_meta_name = out_name + '.meta'
        in_source = os.path.join(_SAMPLE_FILES_DIR, out_source_name)
        in_meta = os.path.join(_SAMPLE_FILES_DIR, out_meta_name)
        out_source = os.path.join(_TMP_DIR, out_source_name)
        out_meta = os.path.join(_TMP_DIR, out_meta_name)
        remove_files([out_source, out_meta])
        # Run dycore
        retcode, files, _, _, _ = gen_registry(filename, 'se', _TMP_DIR, 2,
                                               _SRC_MOD_DIR, _CAM_ROOT,
                                               loglevel=logging.ERROR,
                                               error_on_no_validate=True)
        # Check return code
        amsg = f"Test failure: retcode={retcode}"
        self.assertEqual(retcode, 0, msg=amsg)
        flen = len(files)
        amsg = f"Test failure: Found {flen} files, expected 1"
        self.assertEqual(flen, 1, msg=amsg)
        amsg = f"{out_meta} does not exist"
        self.assertTrue(os.path.exists(out_meta), msg=amsg)
        amsg = f"{out_source} does not exist"
        self.assertTrue(os.path.exists(out_source), msg=amsg)
        # For each output file, make sure it matches input file
        amsg = f"{out_meta} does not match {in_meta}"
        self.assertTrue(filecmp.cmp(out_meta, in_meta,
                                    shallow=False), msg=amsg)
        amsg = f"{out_source} does not match {in_source}"
        self.assertTrue(filecmp.cmp(out_source, in_source,
                                    shallow=False), msg=amsg)

    def test_good_array(self):
        """Test code and metadata generation from a good registry with DDTs
        containing an "Array" variable with multiple internal Array elements
        Check that generate_registry_data.py generates good Fortran and
        metadata files.
        Check that the DDT contains the proper information and the
        initialization code has the correct array calls"""
        # Setup test
        filename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_ddt_array.xml")
        out_name = "physics_types_ddt_array"
        out_source_name = out_name + '.F90'
        out_meta_name = out_name + '.meta'
        in_source = os.path.join(_SAMPLE_FILES_DIR, out_source_name)
        in_meta = os.path.join(_SAMPLE_FILES_DIR, out_meta_name)
        out_source = os.path.join(_TMP_DIR, out_source_name)
        out_meta = os.path.join(_TMP_DIR, out_meta_name)
        remove_files([out_source, out_meta])
        # Run dycore
        retcode, files, _, _, _ = gen_registry(filename, 'se', _TMP_DIR, 2,
                                               _SRC_MOD_DIR, _CAM_ROOT,
                                               loglevel=logging.ERROR,
                                               error_on_no_validate=True)
        # Check return code
        amsg = f"Test failure: retcode={retcode}"
        self.assertEqual(retcode, 0, msg=amsg)
        flen = len(files)
        amsg = f"Test failure: Found {flen} files, expected 1"
        self.assertEqual(flen, 1, msg=amsg)
        amsg = f"{out_meta} does not exist"
        self.assertTrue(os.path.exists(out_meta), msg=amsg)
        amsg = f"{out_source} does not exist"
        self.assertTrue(os.path.exists(out_source), msg=amsg)
        # For each output file, make sure it matches input file
        amsg = f"{out_meta} does not match {in_meta}"
        self.assertTrue(filecmp.cmp(out_meta, in_meta,
                                    shallow=False), msg=amsg)
        amsg = f"{out_source} does not match {in_source}"
        self.assertTrue(filecmp.cmp(out_source, in_source,
                                    shallow=False), msg=amsg)

    def test_good_metadata_file_registry(self):
        """Test code and metadata generation from a good registry with a DDT
        and a metadata file.
        Check that generate_registry_data.py generates good
        Fortran and metadata files.
        Check that the DDT contains the proper information
        for the SE dycore"""
        # Setup test
        filename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_mf.xml")
        out_name = "physics_types_ddt_mf"
        in_source = os.path.join(_SAMPLE_FILES_DIR, out_name + '.F90')
        in_meta = os.path.join(_SAMPLE_FILES_DIR, out_name + '.meta')
        out_source = os.path.join(_TMP_DIR, out_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_name + '.meta')
        remove_files([out_source, out_meta])
        # generate registry
        retcode, files, _, _, _ = gen_registry(filename, 'se', _TMP_DIR, 2,
                                              _SRC_MOD_DIR, _CAM_ROOT,
                                              loglevel=logging.ERROR,
                                              error_on_no_validate=True)
        # Check return code
        amsg = f"Test failure for SE dycore, retcode={retcode}"
        self.assertEqual(retcode, 0, msg=amsg)
        flen = len(files)
        amsg = f"Test failure for SE dycore: Found {flen} files, expected 2"
        self.assertEqual(flen, 2, msg=amsg)
        amsg = f"{out_meta} does not exist"
        self.assertTrue(os.path.exists(out_meta), msg=amsg)
        amsg = f"{out_source} does not exist"
        self.assertTrue(os.path.exists(out_source), msg=amsg)
        # For each output file, make sure it matches input file
        amsg = f"{out_meta} does not match {in_meta}"
        self.assertTrue(filecmp.cmp(out_meta, in_meta,
                                    shallow=False), msg=amsg)
        amsg = f"{out_source} does not match {in_source}"
        self.assertTrue(filecmp.cmp(out_source, in_source,
                                    shallow=False), msg=amsg)
        # Check that the metadata file has the correct number of variables
        mfile = files[1]
        mvars = mfile.variable_list()
        num_vars = len(mvars)
        amsg = f"Expected 14 metadata variables, found {num_vars}"
        self.assertEqual(num_vars, 14, msg=amsg)

    def test_diff_src_root_metadata_file_registry(self):
        """
        Perform the same test as "test_good_metadata_file_registry",
        except with the meta-data file located elsewhere, and the
        "src_root" input variable set accordingly.
        """
        # Setup test
        filename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_mf.xml")
        out_name = "physics_types_ddt_mf"
        in_source = os.path.join(_SAMPLE_FILES_DIR, out_name + '.F90')
        in_meta = os.path.join(_SAMPLE_FILES_DIR, out_name + '.meta')
        out_source = os.path.join(_TMP_DIR, out_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_name + '.meta')
        remove_files([out_source, out_meta])

        # Create new directory:
        tmp_src_dir = os.path.join(_TMP_DIR, "test", "unit", "python", \
                                   "sample_files")
        if not os.path.exists(tmp_src_dir):
            os.makedirs(tmp_src_dir)

        # Copy ref_pres.meta file to new location:
        meta_file = os.path.join(_SAMPLE_FILES_DIR, "ref_pres.meta")
        shutil.copy(meta_file, tmp_src_dir)

        # Generate registry
        retcode, files, _, _, _ = gen_registry(filename, 'se', _TMP_DIR, 2,
                                               _SRC_MOD_DIR, _TMP_DIR,
                                               loglevel=logging.ERROR,
                                               error_on_no_validate=True)
        # Check return code
        amsg = f"Test failure for SE dycore, retcode={retcode}"
        self.assertEqual(retcode, 0, msg=amsg)
        flen = len(files)
        amsg = f"Test failure for SE dycore: Found {flen} files, expected 2"
        self.assertEqual(flen, 2, msg=amsg)
        amsg = f"{out_meta} does not exist"
        self.assertTrue(os.path.exists(out_meta), msg=amsg)
        amsg = f"{out_source} does not exist"
        self.assertTrue(os.path.exists(out_source), msg=amsg)
        # For each output file, make sure it matches input file
        amsg = f"{out_meta} does not match {in_meta}"
        self.assertTrue(filecmp.cmp(out_meta, in_meta,
                                    shallow=False), msg=amsg)
        amsg = f"{out_source} does not match {in_source}"
        self.assertTrue(filecmp.cmp(out_source, in_source,
                                    shallow=False), msg=amsg)
        # Check that the metadata file has the correct number of variables
        mfile = files[1]
        mvars = mfile.variable_list()
        num_vars = len(mvars)
        amsg = f"Expected 14 metadata variables, found {num_vars}"
        self.assertEqual(num_vars, 14, msg=amsg)

    def test_SourceMods_metadata_file_registry(self):
        """
        Test that a registry file present in the
        'SourceMods' directory is correctly used
        over the standard input registry file.
        """
        # Setup test
        filename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_mf.xml")
        out_name = "physics_types_ddt_mf"
        in_source = os.path.join(_SAMPLE_FILES_DIR, out_name + '.F90')
        in_meta = os.path.join(_SAMPLE_FILES_DIR, out_name + '.meta')
        out_source = os.path.join(_TMP_DIR, out_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_name + '.meta')
        remove_files([out_source, out_meta])

        # Create new directory:
        tmp_src_dir = os.path.join(_TMP_DIR, "test", "unit", \
                                   "sample_files")
        if not os.path.exists(tmp_src_dir):
            os.makedirs(tmp_src_dir)

        # Copy ref_pres_sm.meta file to new location:
        meta_file = os.path.join(_SAMPLE_FILES_DIR, "ref_pres_SourceMods.meta")
        source_mod_file = os.path.join(tmp_src_dir, "ref_pres.meta")
        if os.path.exists(source_mod_file):
            os.remove(source_mod_file)
        shutil.copy(meta_file, source_mod_file)

        # Generate registry
        retcode, files, _, _, _ = gen_registry(filename, 'se', _TMP_DIR, 2,
                                               tmp_src_dir, _CAM_ROOT,
                                               loglevel=logging.ERROR,
                                               error_on_no_validate=True)

        # Check return code
        amsg = f"Test failure for SE dycore, retcode={retcode}"
        self.assertEqual(retcode, 0, msg=amsg)
        flen = len(files)
        amsg = f"Test failure for SE dycore: Found {flen} files, expected 2"
        self.assertEqual(flen, 2, msg=amsg)
        amsg = f"{out_meta} does not exist"
        self.assertTrue(os.path.exists(out_meta), msg=amsg)
        amsg = f"{out_source} does not exist"
        self.assertTrue(os.path.exists(out_source), msg=amsg)
        # For each output file, make sure it matches input file
        amsg = f"{out_meta} does not match {in_meta}"
        self.assertTrue(filecmp.cmp(out_meta, in_meta,
                                    shallow=False), msg=amsg)
        amsg = f"{out_source} does not match {in_source}"
        self.assertTrue(filecmp.cmp(out_source, in_source,
                                    shallow=False), msg=amsg)
        # Check that the metadata file has the correct number of variables
        mfile = files[1]
        mvars = mfile.variable_list()
        num_vars = len(mvars)
        amsg = f"Expected 16 metadata variables, found {num_vars}"
        self.assertEqual(num_vars, 16, msg=amsg)

    def test_good_complete_registry(self):
        """
        Test that a good registry with variables, meta-data files,
        DDTs, Arrays, variables set to a physconst variable,
        and parameters validates, i.e. try and test
        everything at once.

        Check that generate_registry_data.py generates
        good Fortran and metadata files with all of the
        proper code features.
        """

        # Setup test
        filename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_complete.xml")
        out_source_name = "physics_types_complete"
        in_source = os.path.join(_SAMPLE_FILES_DIR, out_source_name + '.F90')
        in_meta = os.path.join(_SAMPLE_FILES_DIR, out_source_name + '.meta')
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])

        # Run test
        retcode, files, _, _, _ = gen_registry(filename, 'se', _TMP_DIR, 2,
                                               _SRC_MOD_DIR, _CAM_ROOT,
                                               loglevel=logging.ERROR,
                                               error_on_no_validate=True)

                # Check return code
        amsg = f"Test failure: retcode={retcode}"
        self.assertEqual(retcode, 0, msg=amsg)
        flen = len(files)
        amsg = f"Test failure: Found {flen} files, expected 2"
        self.assertEqual(flen, 4, msg=amsg)

        # Make sure each output file was created
        amsg = f"{out_meta} does not exist"
        self.assertTrue(os.path.exists(out_meta), msg=amsg)
        amsg = f"{out_source} does not exist"
        self.assertTrue(os.path.exists(out_source), msg=amsg)

        # For each output file, make sure it matches input file
        amsg = f"{out_meta} does not match {in_meta}"
        self.assertTrue(filecmp.cmp(out_meta, in_meta, shallow=False), msg=amsg)
        amsg = f"{out_source} does not match {in_source}"
        self.assertTrue(filecmp.cmp(out_source, in_source, shallow=False),
                        msg=amsg)

        # Check that the metadata file has the correct number of variables
        mfile = files[1]
        mvars = mfile.variable_list()
        num_vars = len(mvars)
        amsg = f"Expected 14 metadata variables, found {num_vars}"
        self.assertEqual(num_vars, 14, msg=amsg)

    def test_no_metadata_file_registry(self):
        """Test code and metadata generation from a good registry with
        a non-existent metadata file.
        Check that generate_registry_data.py raises the correct error."""
        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_mf.xml")
        filename = os.path.join(_TMP_DIR, "reg_no_mf.xml")
        out_source_name = "physics_types_no_mf"
        out_source = out_source_name + '.F90'
        out_meta_name = out_source_name + '.meta'
        out_source = os.path.join(_TMP_DIR, out_source_name)
        out_meta = os.path.join(_TMP_DIR, out_meta_name)
        remove_files([out_source, out_meta])
        tree, root = read_xml_file(infilename)
        # Change output filename
        for obj in root:
            oname = obj.get('name')
            if (obj.tag == 'file') and (oname == 'physics_types_ddt'):
                obj.set('name', out_source_name)
            elif obj.tag == 'metadata_file':
                bad_filename = os.path.join(_SAMPLE_FILES_DIR,
                                            'bad_metadata_filename.meta')
                obj.text = bad_filename
            # End if
        # End for
        tree.write(filename)

        # Run test
        with self.assertRaises(ValueError) as verr:
            _ = gen_registry(filename, 'se', _TMP_DIR, 2,
                             _SRC_MOD_DIR, _CAM_ROOT,
                             loglevel=logging.ERROR,
                             error_on_no_validate=True)
        # Check exception message
        bad_meta = os.path.abspath(bad_filename)
        emsg = (f"Metadata file, '{bad_meta}', does not exist")
        self.assertEqual(emsg, str(verr.exception).split('\n', maxsplit=1)[0])
        # Make sure no output files were created
        self.assertFalse(os.path.exists(out_meta))
        self.assertFalse(os.path.exists(out_source))

    def test_parameter(self):
        """Test a registry with a parameter.
        Check that it validates and generates Fortran or metadata files
        """
        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_simple.xml")
        filename = os.path.join(_TMP_DIR, "reg_parameter.xml")
        out_source_name = "physics_types_parameter"
        in_source = os.path.join(_SAMPLE_FILES_DIR, out_source_name + '.F90')
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        in_meta = os.path.join(_SAMPLE_FILES_DIR, out_source_name + '.meta')
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
        # Run test
        retcode, files, _, _, _ = gen_registry(filename, 'eul', _TMP_DIR, 2,
                                               _SRC_MOD_DIR, _CAM_ROOT,
                                               loglevel=logging.ERROR,
                                               error_on_no_validate=True)
        # Check return code
        amsg = f"Test failure: retcode={retcode}"
        self.assertEqual(retcode, 0, msg=amsg)
        flen = len(files)
        amsg = f"Test failure: Found {flen} files, expected 1"
        self.assertEqual(flen, 1, msg=amsg)
        # Make sure each output file was created
        self.assertTrue(os.path.exists(out_meta))
        self.assertTrue(os.path.exists(out_source))
        # For each output file, make sure it matches input file
        amsg = f"{out_meta} does not match {in_meta}"
        self.assertTrue(filecmp.cmp(out_meta, in_meta, shallow=False), msg=amsg)
        amsg = f"{out_source} does not match {in_source}"
        self.assertTrue(filecmp.cmp(out_source, in_source, shallow=False),
                        msg=amsg)

    def test_bad_registry_version(self):
        """Test a registry with a bad version number.
        Check that it does not validate and does not generate any
        Fortran or metadata files"""
        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_simple.xml")
        filename = os.path.join(_TMP_DIR, "reg_bad_version.xml")
        out_source_name = "physics_types_bad_ver"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])
        tree, root = read_xml_file(infilename)
        # Write bad version number
        root.set('version', '1.1')
        # Change output filename
        for obj in root:
            oname = obj.get('name')
            if (obj.tag == 'file') and (oname == 'physics_types_simple'):
                obj.set('name', out_source_name)
                break
            # End if
        # End for
        tree.write(filename)

        # Run test
        with self.assertRaises(ValueError) as verr:
            _ = gen_registry(filename, 'fv', _TMP_DIR, 2,
                             _SRC_MOD_DIR, _CAM_ROOT,
                             loglevel=logging.ERROR,
                             error_on_no_validate=True)
        # Check exception message
        emsg = (f"Invalid registry file, {filename}")
        self.assertEqual(emsg, str(verr.exception).split('\n', maxsplit=1)[0])
        # Make sure no output files were created
        self.assertFalse(os.path.exists(out_meta))
        self.assertFalse(os.path.exists(out_source))

    def test_registry_no_clear_initial_value(self):
        """
        Check that generate_registry_data.py throws the correct
        error message when there is no clear "winner"
        between the initial_value entries
        """

        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_simple.xml")
        tree, root = read_xml_file(infilename)

        # Open base XML file:
        base_tree = ET.parse(infilename)
        base_root = base_tree.getroot()

        # Write new namelist entry with "bad" value:
        initial_value_tie_string = \
        """
        <variable local_name="myvar" standard_name="variable_with_unclear_initial_value"
                  units="count" type="integer" access="protected">
           <long_name>Number of horizontal columns</long_name>
           <initial_value>2</initial_value>
           <initial_value dyn="SE">0</initial_value>
           <initial_value dyn="SE">1</initial_value>
        </variable>
        """

        initial_value_tie_entry = ET.fromstring(initial_value_tie_string)

        # Add new namelist entry back to original namelist XML tree:
        base_root[0].append(initial_value_tie_entry)

        # Write out new, temporary XML namelist file for testing:
        xml_test_fil = os.path.join(_TMP_DIR, "test_registry_initial_value_tie.xml")
        base_tree.write(xml_test_fil, encoding="utf-8", xml_declaration=True)
        filename = os.path.join(_TMP_DIR, "test_registry_initial_value_tie.xml")

        # Attempt to generate registry:
        with self.assertRaises(CCPPError) as cerr:
            retcode, files, _ = gen_registry(filename, 'se', _TMP_DIR, 2,
                                             _SRC_MOD_DIR, _CAM_ROOT,
                                             loglevel=logging.ERROR,
                                             error_on_no_validate=True)

        # Check exception message
        emsg = "Unclear which initial_value to use for myvar. "
        emsg += "There are at least two configurations with 1 matching attributes"

        self.assertEqual(emsg, str(cerr.exception))

    def test_missing_standard_name(self):
        """Test a registry with a missing standard name.
        Check that it does not validate and does not generate any
        Fortran or metadata files"""
        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_simple.xml")
        filename = os.path.join(_TMP_DIR, "reg_no_std_name.xml")
        out_source_name = "physics_types_no_std_name"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])
        tree, root = read_xml_file(infilename)
        # Change output filename and remove a standard name
        for obj in root:
            oname = obj.get('name')
            if (obj.tag == 'file') and (oname == 'physics_types_simple'):
                # Reset the filename
                obj.set('name', out_source_name)
                # Find and remove the standard name for latitude
                for var in obj:
                    lname = var.get('local_name')
                    if (var.tag == 'variable') and (lname == 'latitude'):
                        del var.attrib['standard_name']
                        break
                    # End if
                # End for
                break
            # End if
        # End for
        tree.write(filename)

        # Run test
        with self.assertRaises(ValueError) as verr:
            _ = gen_registry(filename, 'fv', _TMP_DIR, 2,
                             _SRC_MOD_DIR, _CAM_ROOT,
                             loglevel=logging.ERROR,
                             error_on_no_validate=True)
        # Check exception message
        emsg = (f"Invalid registry file, {filename}")
        self.assertEqual(emsg, str(verr.exception).split('\n', maxsplit=1)[0])
        # Make sure no output files were created
        self.assertFalse(os.path.exists(out_meta))
        self.assertFalse(os.path.exists(out_source))

    def test_bad_dimensions(self):
        """Test a registry with a variable with bad dimensions.
        Check that it does not validate and does not generate any
        Fortran or metadata files"""
        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_simple.xml")
        filename = os.path.join(_TMP_DIR, "reg_bad_dimensions.xml")
        out_source_name = "physics_types_bad_dimensions"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])
        tree, root = read_xml_file(infilename)
        # Change output filename and add a new variable with bad dimensions
        for obj in root:
            oname = obj.get('name')
            if (obj.tag == 'file') and (oname == 'physics_types_simple'):
                # Reset the filename
                obj.set('name', out_source_name)
                # Add a new variable with bad dimensions
                new_var = ET.SubElement(obj, "variable")
                new_var.set("local_name", "u")
                new_var.set("standard_name", "east_wind")
                new_var.set("units", "m s-1")
                new_var.set("type", "real")
                new_var.set("kind", "kind_phys")
                dims_elem = ET.SubElement(new_var, "dimensions")
                dims_elem.text = 'ccpp_constant_one:horizontal_dimension:two'
                break
            # End if
        # End for
        tree.write(filename)
        # Run test
        with self.assertRaises(ValueError) as verr:
            _ = gen_registry(filename, 'eul', _TMP_DIR, 2,
                             _SRC_MOD_DIR, _CAM_ROOT,
                             loglevel=logging.ERROR,
                             error_on_no_validate=True)
        # Check exception message
        emsg = (f"Invalid registry file, {filename}")
        self.assertEqual(emsg, str(verr.exception).split('\n', maxsplit=1)[0])
        # Make sure no output files were created
        self.assertFalse(os.path.exists(out_meta))
        self.assertFalse(os.path.exists(out_source))

    def test_unknown_dimensions(self):
        """Test a registry with a variable with an unknown dimension.
        Check that it does not validate and does not generate any
        Fortran or metadata files"""
        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_simple.xml")
        filename = os.path.join(_TMP_DIR, "reg_unknown_dimension.xml")
        out_source_name = "physics_types_unknown_dimension"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])
        tree, root = read_xml_file(infilename)
        # Change output filename and add a variable with an unknown dimension
        for obj in root:
            oname = obj.get('name')
            if (obj.tag == 'file') and (oname == 'physics_types_simple'):
                # Reset the filename
                obj.set('name', out_source_name)
                # Add a new variable with an unknown dimension
                new_var = ET.SubElement(obj, "variable")
                new_var.set("local_name", "u")
                new_var.set("standard_name", "east_wind")
                new_var.set("units", "m s-1")
                new_var.set("type", "real")
                new_var.set("kind", "kind_phys")
                new_var.set("allocatable", "target")
                dims_elem = ET.SubElement(new_var, "dimensions")
                dims_elem.text = 'horizontal_dimension vertical_dimension'
                break
            # End if
        # End for
        tree.write(filename)
        # Run test

        with self.assertRaises(ValueError) as verr:
            _ = gen_registry(filename, 'eul', _TMP_DIR, 2,
                             _SRC_MOD_DIR, _CAM_ROOT,
                             loglevel=logging.ERROR,
                             error_on_no_validate=True)
        # End with
        # Check exception message
        emsg = "Dimension, 'vertical_dimension', not found for 'u'"
        self.assertEqual(emsg, str(verr.exception))
        # Make sure no output files were created
        self.assertFalse(os.path.exists(out_meta))
        self.assertFalse(os.path.exists(out_source))

    def test_no_init_value(self):
        """Test a registry with a parameter with no initial value.
        Check that it does not validate and does not generate any
        Fortran or metadata files"""
        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_simple.xml")
        filename = os.path.join(_TMP_DIR, "reg_no_init_value.xml")
        out_source_name = "physics_types_no_init_value"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])
        tree, root = read_xml_file(infilename)
        # Change output filename and add a parameter with no initial value
        for obj in root:
            oname = obj.get('name')
            if (obj.tag == 'file') and (oname == 'physics_types_simple'):
                # Reset the filename
                obj.set('name', out_source_name)
                # Add a new variable with an unknown dimension
                new_var = ET.SubElement(obj, "variable")
                new_var.set("local_name", "u")
                new_var.set("standard_name", "east_wind")
                new_var.set("units", "m s-1")
                new_var.set("type", "real")
                new_var.set("kind", "kind_phys")
                new_var.set("allocatable", "parameter")
                dims_elem = ET.SubElement(new_var, "dimensions")
                dims_elem.text = 'horizontal_dimension'
                break
            # End if
        # End for
        tree.write(filename)
        # Run test
        with self.assertRaises(ValueError) as verr:
            _ = gen_registry(filename, 'eul', _TMP_DIR, 2,
                             _SRC_MOD_DIR, _CAM_ROOT,
                             loglevel=logging.ERROR,
                             error_on_no_validate=True)
        # End with
        # Check exception message
        emsg = "parameter, 'u', does not have an initial value"
        self.assertEqual(emsg, str(verr.exception))
        # Make sure no output files were created
        self.assertFalse(os.path.exists(out_meta))
        self.assertFalse(os.path.exists(out_source))

    def test_duplicate_type(self):
        """Test a registry with a duplicate DDT type.
        Check that it raises an exception and does not generate any
        Fortran or metadata files"""
        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_ddt.xml")
        filename = os.path.join(_TMP_DIR, "reg_dup_ddt.xml")
        out_source_name = "physics_types_dup_ddt"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])
        tree, root = read_xml_file(infilename)
        # Change output filename
        for obj in root:
            oname = obj.get('name')
            if (obj.tag == 'file') and (oname == 'physics_types_ddt'):
                obj.set('name', out_source_name)
                for var in obj:
                    dtype = var.get('type')
                    if (var.tag == 'ddt') and (dtype == "physics_state"):
                        # Add a second DDT
                        new_ddt = ET.SubElement(obj, "ddt")
                        new_ddt.set("type", dtype)
                        data_elem = ET.SubElement(new_ddt, "data")
                        data_elem.set("dyn", "EUL")
                        data_elem.text = 'latitude'
                        data_elem = ET.SubElement(new_ddt, "data")
                        data_elem.set("dyn", "EUL")
                        data_elem.text = 'longitude'
                        break
                    # End if
                # End for
                break
            # End if
        # End for
        tree.write(filename)
        # Run test
        vmsg = 'Failed to flag a duplicate DDT type'
        with self.assertRaises(ValueError, msg=vmsg) as verr:
            _ = gen_registry(filename, 'eul', _TMP_DIR, 2,
                             _SRC_MOD_DIR, _CAM_ROOT,
                             loglevel=logging.ERROR,
                             error_on_no_validate=True)
        # End with
        # Check exception message
        emsg = 'Duplicate DDT entry, physics_state'
        self.assertEqual(emsg, str(verr.exception))
        # Make sure no output files were created
        self.assertFalse(os.path.exists(out_meta))
        self.assertFalse(os.path.exists(out_source))

    def test_ddt_with_kind(self):
        """Test a registry with a DDT variable that has a kind attribute.
        Check that it raises an exception and does not generate any
        Fortran or metadata files"""
        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_ddt.xml")
        filename = os.path.join(_TMP_DIR, "reg_ddt_with_kind.xml")
        out_source_name = "physics_types_ddt_with_kind"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])
        tree, root = read_xml_file(infilename)
        # Change output filename
        for obj in root:
            oname = obj.get('name')
            if (obj.tag == 'file') and (oname == 'physics_types_ddt'):
                obj.set('name', out_source_name)
                for var in obj:
                    lname = var.get('local_name')
                    if (var.tag == 'variable') and (lname == "phys_state"):
                        var.set('kind', 'ddt')
                        break
                    # End if
                # End for
                break
            # End if
        # End for
        tree.write(filename)
        # Run test
        with self.assertRaises(ValueError) as verr:
            _ = gen_registry(filename, 'eul', _TMP_DIR, 2,
                             _SRC_MOD_DIR, _CAM_ROOT,
                             loglevel=logging.ERROR,
                             error_on_no_validate=True)
        # End with
        # Check exception message
        emsg = "kind attribute illegal for DDT type physics_state"
        self.assertEqual(emsg, str(verr.exception))
        # Make sure no output files were created
        self.assertFalse(os.path.exists(out_meta))
        self.assertFalse(os.path.exists(out_source))

    def test_ddt_with_unknown_type(self):
        """Test a registry with a DDT variable of unknown type.
        Check that it raises an exception and does not generate any
        Fortran or metadata files"""
        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_ddt.xml")
        filename = os.path.join(_TMP_DIR, "reg_ddt_var_unknown.xml")
        out_source_name = "physics_types_ddt_var_unknown_type"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])
        tree, root = read_xml_file(infilename)
        # Change output filename
        for obj in root:
            oname = obj.get('name')
            if (obj.tag == 'file') and (oname == 'physics_types_ddt'):
                obj.set('name', out_source_name)
                for var in obj:
                    lname = var.get('local_name')
                    if (var.tag == 'variable') and (lname == "phys_state"):
                        var.set('type', 'physics_tend')
                        break
                    # End if
                # End for
                break
            # End if
        # End for
        tree.write(filename)
        # Run test
        with self.assertRaises(ValueError) as verr:
            _ = gen_registry(filename, 'eul', _TMP_DIR, 2,
                             _SRC_MOD_DIR, _CAM_ROOT,
                             loglevel=logging.ERROR,
                             error_on_no_validate=True)
        # End with
        # Check exception message
        emsg = "phys_state is an unknown Variable type, physics_tend"
        self.assertEqual(emsg, str(verr.exception))
        # Make sure no output files were created
        self.assertFalse(os.path.exists(out_meta))
        self.assertFalse(os.path.exists(out_source))

    def test_ddt_with_unknown_extends(self):
        """Test a registry with a DDT which extends an unknown type
        attributes.
        Check that it raises an exception and does not generate any
        Fortran or metadata files"""
        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_ddt.xml")
        filename = os.path.join(_TMP_DIR, "reg_ddt_unknown_extends.xml")
        out_source_name = "physics_types_ddt_unknown_extends"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])
        tree, root = read_xml_file(infilename)
        # Change output filename
        for obj in root:
            oname = obj.get('name')
            if (obj.tag == 'file') and (oname == 'physics_types_ddt'):
                obj.set('name', out_source_name)
                for var in obj:
                    ltype = var.get('type')
                    if (var.tag == 'ddt') and (ltype == "physics_state"):
                        var.set('extends', 'physics_tend')
                        break
                    # End if
                # End for
                break
            # End if
        # End for
        tree.write(filename)
        # Run test
        with self.assertRaises(ValueError) as verr:
            _ = gen_registry(filename, 'eul', _TMP_DIR, 2,
                             _SRC_MOD_DIR, _CAM_ROOT,
                             loglevel=logging.ERROR,
                             error_on_no_validate=True)
        # End with
        # Check exception message
        emsg = ("DDT, 'physics_state', extends type 'physics_tend', "
                "however, this type is not known")
        self.assertEqual(emsg, str(verr.exception))
        # Make sure no output files were created
        self.assertFalse(os.path.exists(out_meta))
        self.assertFalse(os.path.exists(out_source))

    def test_ddt_with_incompatible_attr(self):
        """Test a registry with a DDT with both the extends and bindC
        attributes.
        Check that it raises an exception and does not generate any
        Fortran or metadata files"""
        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_ddt2.xml")
        filename = os.path.join(_TMP_DIR, "reg_ddt_incompatible_attributes.xml")
        out_source_name = "physics_types_ddt_incompatible_attributes"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])
        tree, root = read_xml_file(infilename)
        # Change output filename
        found_file = False
        found_ddt = False
        for obj in root:
            oname = obj.get('name')
            if (obj.tag == 'file') and (oname == 'physics_types_ddt2'):
                obj.set('name', out_source_name)
                found_file = True
                for var in obj:
                    ltype = var.get('type')
                    if (var.tag == 'ddt') and (ltype == "physics_state"):
                        var.set('bindC', "true")
                        found_ddt = True
                        break
                    # End if
                # End for
                break
            # End if
        # End for
        self.assertTrue(found_file)
        self.assertTrue(found_ddt)
        tree.write(filename)
        # Run test
        with self.assertRaises(ValueError) as verr:
            _ = gen_registry(filename, 'eul', _TMP_DIR, 2,
                             _SRC_MOD_DIR, _CAM_ROOT,
                             loglevel=logging.ERROR,
                             error_on_no_validate=True)
        # End with
        # Check exception message
        emsg = ("DDT, 'physics_state', cannot have both 'extends' and "
                "'bindC' attributes")
        self.assertEqual(emsg, str(verr.exception))
        # Make sure no output files were created
        self.assertFalse(os.path.exists(out_meta))
        self.assertFalse(os.path.exists(out_source))

    def test_ddt_with_unknown_variable(self):
        """Test a registry with a DDT with both the extends and bindC
        attributes.
        Check that it raises an exception and does not generate any
        Fortran or metadata files"""
        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_ddt.xml")
        filename = os.path.join(_TMP_DIR, "reg_ddt_unknown_variable.xml")
        out_source_name = "physics_types_ddt_unknown_variable"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])
        tree, root = read_xml_file(infilename)
        # Change output filename
        for obj in root:
            oname = obj.get('name')
            if (obj.tag == 'file') and (oname == 'physics_types_ddt'):
                obj.set('name', out_source_name)
                for var in obj:
                    ltype = var.get('type')
                    if (var.tag == 'ddt') and (ltype == "physics_state"):
                        data_elem = ET.SubElement(var, "data")
                        data_elem.text = 'ice_cream'
                        break
                    # End if
                # End for
                break
            # End if
        # End for
        tree.write(filename)
        # Run test
        with self.assertRaises(ValueError) as verr:
            _ = gen_registry(filename, 'eul',_TMP_DIR, 2,
                             _SRC_MOD_DIR, _CAM_ROOT,
                             loglevel=logging.ERROR,
                             error_on_no_validate=True)
        # End with
        # Check exception message
        emsg = ("Variable, 'ice_cream', not found for DDT, 'physics_state', "
                "in 'physics_types_ddt_unknown_variable'")
        self.assertEqual(emsg, str(verr.exception))
        # Make sure no output files were created
        self.assertFalse(os.path.exists(out_meta))
        self.assertFalse(os.path.exists(out_source))

    def test_duplicate_local_name(self):
        """Test a registry with a duplicate local name.
        Check that it raises an exception and does not generate any
        Fortran or metadata files"""
        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_simple.xml")
        filename = os.path.join(_TMP_DIR, "reg_duplicate_local_name.xml")
        out_source_name = "physics_types_duplicate_local_name"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])
        tree, root = read_xml_file(infilename)
        # Change output filename
        for obj in root:
            oname = obj.get('name')
            if (obj.tag == 'file') and (oname == 'physics_types_simple'):
                obj.set('name', out_source_name)
                new_var = ET.SubElement(obj, "variable")
                new_var.set("local_name", "latitude")
                new_var.set("standard_name", "east_wind")
                new_var.set("units", "radians")
                new_var.set("type", "real")
                new_var.set("kind", "kind_phys")
                dims_elem = ET.SubElement(new_var, "dimensions")
                dims_elem.text = 'horizontal_dimension'
                break
            # End if
        # End for
        tree.write(filename)
        # Run test
        with self.assertRaises(ValueError) as verr:
            _ = gen_registry(filename, 'eul', _TMP_DIR, 2,
                             _SRC_MOD_DIR, _CAM_ROOT,
                             loglevel=logging.ERROR,
                             error_on_no_validate=True)
        # End with
        # Check exception message
        emsg = "duplicate variable local_name, 'latitude', in "
        emsg += "physics_types_duplicate_local_name, already defined "
        emsg += "with standard_name, 'latitude'"
        self.assertEqual(emsg, str(verr.exception))
        # Make sure no output files were created
        self.assertFalse(os.path.exists(out_meta))
        self.assertFalse(os.path.exists(out_source))

    def test_duplicate_standard_name(self):
        """Test a registry with a duplicate standard name.
        Check that it raises an exception and does not generate any
        Fortran or metadata files"""
        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_simple.xml")
        filename = os.path.join(_TMP_DIR, "reg_duplicate_standard_name.xml")
        out_source_name = "physics_types_duplicate_standard_name"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])
        tree, root = read_xml_file(infilename)
        # Change output filename
        for obj in root:
            oname = obj.get('name')
            if (obj.tag == 'file') and (oname == 'physics_types_simple'):
                obj.set('name', out_source_name)
                new_var = ET.SubElement(obj, "variable")
                new_var.set("local_name", "french_fries")
                new_var.set("standard_name", "latitude")
                new_var.set("units", "radians")
                new_var.set("type", "real")
                new_var.set("kind", "kind_phys")
                dims_elem = ET.SubElement(new_var, "dimensions")
                dims_elem.text = 'horizontal_dimension'
                break
            # End if
        # End for
        tree.write(filename)

        # Run test
        with self.assertRaises(ValueError) as verr:
            _ = gen_registry(filename, 'eul', _TMP_DIR, 2,
                             _SRC_MOD_DIR, _CAM_ROOT,
                             loglevel=logging.ERROR,
                             error_on_no_validate=True)
        # End with
        # Check exception message
        emsg = "duplicate variable standard_name, 'latitude' from "
        emsg += "'french_fries' in 'physics_types_duplicate_standard_name'"
        emsg += ", already defined with local_name, 'latitude'"
        self.assertEqual(emsg, str(verr.exception))
        # Make sure no output files were created
        self.assertFalse(os.path.exists(out_meta))
        self.assertFalse(os.path.exists(out_source))

    def test_missing_use_statement(self):
        """Test a registry with a missing use statement needed for initialization.
        Check that it raises an exception and does not generate any
        Fortran or metadata files"""
        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR, "reg_good_complete.xml")
        filename = os.path.join(_TMP_DIR, "reg_missing_use.xml")
        out_source_name = "physics_types_missing_use"
        out_source = os.path.join(_TMP_DIR, out_source_name + '.F90')
        out_meta = os.path.join(_TMP_DIR, out_source_name + '.meta')
        remove_files([out_source, out_meta])
        tree, root = read_xml_file(infilename)
        # Add a new variable that uses a non-"used" physconst variable
        for obj in root:
            oname = obj.get('name')
            if (obj.tag == 'file') and (oname == 'physics_types_complete'):
                obj.set('name', out_source_name)
                new_var = ET.SubElement(obj, "variable")
                new_var.set("local_name", "french_fries")
                new_var.set("standard_name", "french_fried_potaters")
                new_var.set("units", "radians")
                new_var.set("type", "real")
                new_var.set("kind", "kind_phys")
                new_var.set("allocatable", "allocatable")
                dims_elem = ET.SubElement(new_var, "dimensions")
                dims_elem.text = 'horizontal_dimension'
                initial_elem = ET.SubElement(new_var, "initial_value")
                initial_elem.text = 'zvir'
                break
            # End if
        # End for
        tree.write(filename)
        # Run test
        with self.assertRaises(ValueError) as verr:
            _ = gen_registry(filename, 'eul', _TMP_DIR, 2,
                             _SRC_MOD_DIR, _CAM_ROOT,
                             loglevel=logging.ERROR,
                             error_on_no_validate=True)
        # End with
        # Check exception message
        emsg = "Initial value 'zvir' is not a physconst variable or does not have necessary use statement"
        self.assertEqual(emsg, str(verr.exception))
        # Make sure the output meta data file matches and no source data file has been generated
        self.assertFalse(os.path.exists(out_meta))
        self.assertFalse(os.path.exists(out_source))

    def test_bad_metadata_file_dup_section(self):
        """Test response to bad metadata file with a duplicate section.
        Check that the correct error is raised."""
        # Setup test
        infilename = os.path.join(_SAMPLE_FILES_DIR,
                                  "phys_types_dup_section.meta")
        # Create fake CCPPFrameworkEnv object to contain the logger
        run_env = CCPPFrameworkEnv(logging.getLogger("badmf"), host_files='',
                                   scheme_files='', suites='')

        # Run test
        with self.assertRaises(ValueError) as verr:
            metadata_file_to_files(infilename, TypeRegistry(), 'eul', run_env)
        # Check exception message
        emsg = "module, 'physics_types_simple', table already contains "
        emsg += f"'physics_types_simple', at {infilename}:36"
        self.assertEqual(emsg, str(verr.exception).split('\n', maxsplit=1)[0])

    def test_bad_metadata_file_no_table(self):
        """Test response to bad metadata file with no table.
        Check that the correct error is raised."""
        # Setup test
        table_name = "phys_types_no_table.meta"
        infilename = os.path.join(_SAMPLE_FILES_DIR, table_name)

        # Create fake CCPPFrameworkEnv object to contain the logger
        run_env = CCPPFrameworkEnv(logging.getLogger("badmf"), host_files='',
                                   scheme_files='', suites='')

        # Run test
        with self.assertRaises(ValueError) as verr:
            metadata_file_to_files(infilename, TypeRegistry(), 'eul', run_env)
        # Check exception message
        emsg = "Missing metadata section ([ccpp-arg-table]) for physics_types_simple"
        self.assertEqual(emsg, str(verr.exception).split('\n', maxsplit=1)[0])

if __name__ == '__main__':
    unittest.main()
