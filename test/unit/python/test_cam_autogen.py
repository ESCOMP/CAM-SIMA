"""
Python unit testing collection for the various
cam_autogen.py functions, including their
error-handling processes.  Please note that
these tests will only work with Python 3.7
or later.

To run these unit tests, simply type:

python test_cam_autogen.py

or (for more verbose output):

python test_cam_autogen.py -v

which will currently run 14 tests, all of which should pass.
"""

#----------------------------------------
#Import required python libraries/modules:
#----------------------------------------
import os
import os.path
import sys
import shutil
import glob
import filecmp
from collections.abc import Iterable

#Python unit-testing library:
import unittest

#Add directory to python path:
_CURRDIR = os.path.abspath(os.path.dirname(__file__))
_CAM_ROOT_DIR = os.path.join(_CURRDIR, os.pardir, os.pardir)
_CAM_CONF_DIR = os.path.abspath(os.path.join(_CAM_ROOT_DIR, "cime_config"))
_CCPP_DIR = os.path.join(_CAM_ROOT_DIR, "ccpp_framework", "scripts")

#Check for all necessary directories:
if not os.path.exists(_CAM_CONF_DIR):
    _EMSG = f"ERROR: Cannot find required '{_CAM_CONF_DIR}' directory"
    raise ImportError(_EMSG)
#End if
if not os.path.exists(_CCPP_DIR):
    _EMSG = f"ERROR: Cannot find CCPP-framework routines in '{_CCPP_DIR}'"
    raise ImportError(_EMSG)
#End if

#Add "cime_config" directory to python path:
sys.path.append(_CAM_CONF_DIR)

#Add "ccpp_framework" directory to python path:
sys.path.append(_CCPP_DIR)

#Import CAM autogen functions:
# pylint: disable=wrong-import-position
from cam_autogen import CamAutoGenError
from cam_autogen import _update_file, _find_schemes_in_sdf
from cam_autogen import _find_metadata_files, generate_registry
from cam_autogen import generate_physics_suites, generate_init_routines

#Import necessary CCPP framework functions:
from parse_tools import read_xml_file
from metadata_table import find_scheme_names

# pylint: enable=wrong-import-position

#+++++++++++++++++++++++++++++++++++++++++++
#Create "fake" CIME case to test cam_autogen
#+++++++++++++++++++++++++++++++++++++++++++

class FakeBuildCache:

    """
    Fake BuildCache class with methods
    needed to properly test cam_autogen
    functions.
    """

    # pylint: disable=unused-argument
    # pylint: disable=no-self-use
    def __init__(self):
        pass

    def update_registry(self, gen_reg_file, registry_files,
                        dycore):

        """Fake version of 'update_regsitry' method"""

    def registry_mismatch(self, gen_reg_file, registry_files,
                          dycore):

        """
        Fake version of 'registry_mismatch' method.
        It always return False, in order to avoid running the
        actual generation routines when performing doctests.
        """
        return False

    def update_ccpp(self, sdfs, scheme_files, xml_files,
                    preproc_defs, kind_phys):

        """Fake version of 'update_ccpp' method"""

    def ccpp_mismatch(self, sdfs, scheme_files, host_files,
                      preproc_defs, kind_phys):

        """
        Fake version of 'ccpp_mismatch' method.
        It always return False, in order to avoid running the
        actual generation routines when performing doctests.
        """
        return False

    def xml_nl_mismatch(self, create_nl_file, xml_files):

        """
        Fake version of 'xml_nl_mismatch' method.
        It always return False, in order to avoid running the
        actual generation routines when performing doctests.
        """
        return False

    def update_init_gen(self, input_file):

        """Fake version of 'update_init_gen' method."""

    def init_write_mismatch(self, input_file):

        """
        Fake version of 'init_write_mismatch' method.
        It always return False, in order to avoid running the
        actual generation routines when performing doctests.
        """
        return False

    def reg_file_list(self):

        """Fake version of 'reg_file_list' property."""

        return []

    def scheme_nl_metadata(self):

        """Fake version of 'scheme_nl_metadata' property."""

        return []

    def scheme_nl_groups(self):

        """Fake version of 'scheme_nl_groups' property."""

        return []

    def ic_names(self):

        """Fake version of 'ic_names' property."""

        return {}

    def constituents(self):
        """Fake version of 'constituents' property."""
        return []

    # pylint: enable=no-self-use
    # pylint: enable=unused-argument

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Main cam_autogen testing routine, used when script is run directly
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

class CamAutoGenTestRoutine(unittest.TestCase):

    """
    Runs all CAM code autogeneration (cam_autogen) tests,
    to ensure that the scripts and error-handling methods
    are running properly.
    """

    #Set-up unit tests:
    def setUp(self):

        """Initalize Config_CAM object being tested."""

        #create fake build cache:
        test_cache = FakeBuildCache()

        #Add CAM build cache object to unittest object list:
        self.test_cache = test_cache

        #Set needed paths:
        test_suite_path = os.path.join(_CURRDIR, "sample_files")

        test_tmp_dir = os.path.join(_CURRDIR, "tmp")
        test_bldroot = os.path.join(test_tmp_dir, "test_bldroot")
        test_reg_dir = os.path.join(test_bldroot, "cam_registry")
        test_src_mods_dir = os.path.join(test_tmp_dir, "SourceMods")

        # Does "tmp" directory exist?  If not then create it:
        if not os.path.exists(test_tmp_dir):
            os.mkdir(test_tmp_dir)
        #End if

        # Remove old test directories if they exist:
        if os.path.exists(test_bldroot):
            shutil.rmtree(test_bldroot)
        #End if

        # Create source code directories needed in order
        # to avoid running the actual code generators
        # while testing:
        os.mkdir(test_bldroot)
        os.mkdir(test_reg_dir)
        os.mkdir(os.path.join(test_bldroot, "ccpp"))
        os.mkdir(os.path.join(test_bldroot, "phys_init"))

        # Remove old test SourceMods test directory,
        # and crate a new empty directory:
        if os.path.exists(test_src_mods_dir):
            shutil.rmtree(test_src_mods_dir)
        #End if
        os.mkdir(test_src_mods_dir)

        # Add paths to self:
        self.test_suite_path   = test_suite_path
        self.test_bldroot      = test_bldroot
        self.test_reg_dir      = test_reg_dir
        self.test_src_mods_dir = test_src_mods_dir

        # Add generated fortran scoping indent value to self:
        self.fort_indent = 3

    #+++++++++++++++++++++++++++++++++++++++++++++++++
    #Tests for "_update_file" autogen function
    #+++++++++++++++++++++++++++++++++++++++++++++++++

    def test_update_file_no_exist(self):

        """Check that "_update_file" correctly copies over a missing file."""

        #Set path to original file:
        orig_file = os.path.join(self.test_suite_path, "physics_types_simple.F90")

        #Set path to new (copied) file:
        new_file = os.path.join(self.test_bldroot, "physics_types_simple.F90")

        #Run function:
        _update_file("physics_types_simple.F90", orig_file, self.test_bldroot)

        #Check that the new file exists:
        self.assertTrue(os.path.exists(new_file))

        #Check that the new file matches the original:
        self.assertTrue(filecmp.cmp(new_file, orig_file, shallow=False))

        #Remove "new" file from bld directory:
        if os.path.exists(new_file):
            os.remove(new_file)
        #End if

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_update_file_match(self):

        """Check that "_update_file correctly does nothing if the two files match."""

        #Set path to original file:
        orig_file = os.path.join(self.test_suite_path, "physics_types_simple.F90")

        #Set path to new (copied) file:
        new_file = os.path.join(self.test_bldroot, "physics_types_simple.F90")

        #Copy over original file:
        shutil.copy2(orig_file, new_file)

        #Run function:
        _update_file("physics_types_simple.F90", orig_file, self.test_bldroot)

        #Check that the new file exists:
        self.assertTrue(os.path.exists(new_file))

        #Check that the new file matches the original:
        self.assertTrue(filecmp.cmp(new_file, orig_file, shallow=False))

        #Remove "new" file from bld directory:
        if os.path.exists(new_file):
            os.remove(new_file)
        #End if

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_update_file_mismatch(self):

        """Check that "_update_file" correctly replaces a mis-matched file."""

        #Set path to original file:
        orig_file = os.path.join(self.test_suite_path, "physics_types_simple.F90")

        #Set path to "bad" file:
        bad_file = os.path.join(self.test_suite_path, "physics_types_ddt2.F90")

        #Set path to new (copied) file:
        new_file = os.path.join(self.test_bldroot, "physics_types_simple.F90")

        #Copy over a different file but label it as the same file:
        shutil.copy2(bad_file, new_file)

        #Run function:
        _update_file("physics_types_simple.F90", orig_file, self.test_bldroot)

        #Check that the new file exists:
        self.assertTrue(os.path.exists(new_file))

        #Check that the new file matches the original:
        self.assertTrue(filecmp.cmp(new_file, orig_file, shallow=False))

        #Remove "new" file from bld directory:
        if os.path.exists(new_file):
            os.remove(new_file)
        #End if

    #+++++++++++++++++++++++++++++++++++++++++++++++++
    #Tests for "_find_schemes_in_sdf" autogen function
    #+++++++++++++++++++++++++++++++++++++++++++++++++

    def test_find_schemes_in_sdf(self):

        """Check that "_find_schemes_in_sdf" correctly finds schemes in an SDF file."""

        #Set path to sample SDF file:
        test_sdf = os.path.join(self.test_suite_path, "write_init_files", "suite_simple.xml")

        #Open XML file:
        _, test_suite = read_xml_file(test_sdf)

        #Set expected results:
        expected_results = ["temp_adjust"]

        #Run function:
        gen_results = _find_schemes_in_sdf(test_suite)

        #Check that the output list matches what is expected:
        self.assertEqual(gen_results, expected_results)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_find_schemes_no_schemes(self):

        """
        Check that "_find_schemes_in_sdf" correctly
        returns an empty list if no schemes are found.
        """

        #Set path to a non-SDF file:
        bad_sdf = os.path.join(self.test_suite_path, "banana_namelist.xml")

        #Open XML file:
        _, bad_file_root = read_xml_file(bad_sdf)

        #Run function:
        gen_results = _find_schemes_in_sdf(bad_file_root)

        #Check that the output list is empty:
        self.assertEqual(gen_results, [])

    #+++++++++++++++++++++++++++++++++++++++++++++++++
    #Tests for "_find_metadata_files" autogen function
    #+++++++++++++++++++++++++++++++++++++++++++++++++

    def test_find_metadata_files(self):

        """Check that "_find_metadata_files" works properly if given the proper inputs."""

        #Copy test files into test SourceMods directory:
        test_meta = os.path.join(self.test_suite_path, "write_init_files", "temp_adjust.meta")
        test_src = os.path.join(self.test_suite_path, "write_init_files", "temp_adjust.F90")

        shutil.copy2(test_meta, self.test_src_mods_dir)
        shutil.copy2(test_src, self.test_src_mods_dir)

        #Set expected dictionary values:
        expected_schemes = {'temp_adjust': (f'{self.test_src_mods_dir}'+os.sep+'temp_adjust.meta',
                            f'{self.test_src_mods_dir}'+os.sep+'temp_adjust.F90', None)}

        #Run function:
        test_scheme_files = _find_metadata_files([self.test_src_mods_dir], find_scheme_names)

        #Check that the output dictionary matches what is expected:
        self.assertEqual(test_scheme_files, expected_schemes)

        #Remove test files:
        test_meta_files = glob.glob(f"{self.test_src_mods_dir}"+os.sep+"temp_adjust.*")
        for test_file in test_meta_files:
            os.remove(test_file)
        #End for

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_missing_src_files(self):

        """
        Check that "_find_metadata_files" raises the appropriate error
        if there are meta files with no equivalent source (Fortran) files.
        """

        #Copy test files into test SourceMods directory:
        phys_dup_file = os.path.join(self.test_suite_path, "phys_types_dup_section.meta")
        phys_no_table = os.path.join(self.test_suite_path, "phys_types_no_table.meta")
        phys_ref_pres = os.path.join(self.test_suite_path, "ref_pres.meta")

        shutil.copy2(phys_dup_file, self.test_src_mods_dir)
        shutil.copy2(phys_no_table, self.test_src_mods_dir)
        shutil.copy2(phys_ref_pres, self.test_src_mods_dir)

        #Set expected error message:
        emsg = "ERROR: No Fortran files were found for the following meta files:"
        emsg += f"\n{self.test_src_mods_dir}"+os.sep+"phys_types_dup_section.meta"
        emsg += f"\n{self.test_src_mods_dir}"+os.sep+"phys_types_no_table.meta"
        emsg += f"\n{self.test_src_mods_dir}"+os.sep+"ref_pres.meta"

        #Expect "CamAutoGenError":
        with self.assertRaises(CamAutoGenError) as autoerr:
            #Run function:
            _ = _find_metadata_files([self.test_src_mods_dir], find_scheme_names)
        #End with

        #Check that error message matches what's expected:
        self.assertEqual(emsg, str(autoerr.exception))

        #Remove test files:
        test_meta_files = glob.glob(f"{self.test_src_mods_dir}"+os.sep+"*.meta")
        for test_file in test_meta_files:
            os.remove(test_file)
        #End for

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_two_schemes_one_namelist_xml(self):

        """
        Check that "_find_metadata_files" raises the appropriate error
        if there is a namelist meta, source, and xml file, but the
        meta and source files contain multiple schemes.
        """

        #Copy files into test SourceMods directory:
        two_schemes_meta = os.path.join(self.test_suite_path, "autogen_files",
                                        "two_scheme_banana.meta")
        two_schemes_src = os.path.join(self.test_suite_path, "autogen_files",
                                       "two_scheme_banana.F90")
        two_schemes_xml = os.path.join(self.test_suite_path, "autogen_files",
                                       "two_scheme_banana_namelist.xml")

        shutil.copy2(two_schemes_meta, self.test_src_mods_dir)
        shutil.copy2(two_schemes_src, self.test_src_mods_dir)
        shutil.copy2(two_schemes_xml, self.test_src_mods_dir)

        #Set xml path used in expected error message:
        bad_xml_file = os.path.join(self.test_src_mods_dir, "two_scheme_banana_namelist.xml")

        #Set expected error message:
        emsg = "ERROR: This XML file was associated with more than one scheme"
        emsg += f"\n{bad_xml_file}"

        #Expect "CamAutoGenError":
        with self.assertRaises(CamAutoGenError) as autoerr:
            #Run function:
            _ = _find_metadata_files([self.test_src_mods_dir], find_scheme_names)
        #End with

        #Check that error message matches what's expected:
        self.assertEqual(emsg, str(autoerr.exception))

        #---------------------
        #Now try two XML files:
        #---------------------

        #Set xml path for second bad XML file:
        bad_xml_file_copy = os.path.join(self.test_src_mods_dir, "two_scheme_copy_namelist.xml")

        #Copy files into test SourceMods directory again:
        shutil.copy2(two_schemes_meta, os.path.join(self.test_src_mods_dir, "two_scheme_copy.meta"))
        shutil.copy2(two_schemes_src, os.path.join(self.test_src_mods_dir, "two_scheme_copy.F90"))
        shutil.copy2(two_schemes_xml, bad_xml_file_copy)

        #Set new expected error message:
        emsg = "ERROR: These XML files were associated with more than one scheme"
        emsg += f"\n{bad_xml_file}\n{bad_xml_file_copy}"

        #Expect "CamAutoGenError":
        with self.assertRaises(CamAutoGenError) as autoerr:
            #Run function:
            _ = _find_metadata_files([self.test_src_mods_dir], find_scheme_names)
        #End with

        #Check that error message matches what's expected:
        self.assertEqual(emsg, str(autoerr.exception))

        #Remove test files:
        test_meta_files = glob.glob(f"{self.test_src_mods_dir}"+os.sep+"two_scheme_banana*")
        for test_file in test_meta_files:
            os.remove(test_file)
        #End for

        test_meta_files = glob.glob(f"{self.test_src_mods_dir}"+os.sep+"two_scheme_copy*")
        for test_file in test_meta_files:
            os.remove(test_file)
        #End for

    #+++++++++++++++++++++++++++++++++++++++++++++++++
    #Tests for "generate_registry" autogen function
    #+++++++++++++++++++++++++++++++++++++++++++++++++

    def test_generate_registry(self):

        """Check that "generate_registry" works properly if given the proper inputs."""

        #Set path to src/data (where the registry file is located):
        test_data_search = [os.path.join(_CAM_ROOT_DIR, "src", "data")]

        #Set expected output tuple:
        expected_results = (f'{self.test_bldroot}'+os.sep+'cam_registry', False, [], {}, [])

        #Run registry generation function:
        gen_results = generate_registry(test_data_search, self.test_cache, _CAM_ROOT_DIR,
                                        self.test_bldroot, self.test_src_mods_dir, 'se',
                                        self.fort_indent)

        #Check that the output tuple matches what is expected:
        self.assertEqual(gen_results, expected_results)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_bad_generate_registry_search_path(self):

        """
        Check that "generate_registry" fails with the correct
        error if unable to find a registry file.
        """

        #Set "bad" path for testing:
        bad_path = ["/bad/path"]

        #Set expected error message:
        emsg = f"ERROR: Unable to find CAM registry, registry.xml, in [{bad_path[0]}]"

        #Expect "CamAutoGenError":
        with self.assertRaises(CamAutoGenError) as autoerr:
            #Run function:
            _ = generate_registry(bad_path, self.test_cache, _CAM_ROOT_DIR,
                                  self.test_bldroot, self.test_src_mods_dir, 'se',
                                  self.fort_indent)
        #End with

        #Check that error message matches what's expected:
        self.assertEqual(emsg, str(autoerr.exception))

    #++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Tests for "generate_physics_suites" autogen function
    #++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_generate_physics_suites(self):

        """Check that "generate_physics_suites" works properly if given the proper inputs"""

        #Copy test files into test SourceMods directory:
        test_suite = os.path.join(self.test_suite_path, "write_init_files", "suite_simple.xml")
        test_meta  = os.path.join(self.test_suite_path, "write_init_files", "temp_adjust.meta")
        test_src  = os.path.join(self.test_suite_path, "write_init_files", "temp_adjust.F90")

        shutil.copy2(test_suite, self.test_src_mods_dir)
        shutil.copy2(test_meta, self.test_src_mods_dir)
        shutil.copy2(test_src, self.test_src_mods_dir)

        #Set expected output tuple:
        expected_results = ([f'{self.test_bldroot}'+os.sep+'ccpp_physics',
                             f'{self.test_bldroot}'+os.sep+'ccpp'], False,
                             f'{self.test_bldroot}'+os.sep+'ccpp'+os.sep+'ccpp_datatable.xml',
                             [], None)

        #Run physics suite generation function:
        gen_results = generate_physics_suites(self.test_cache, "UNSET", "cam", "simple",
                                              _CAM_ROOT_DIR, self.test_bldroot,
                                              self.test_reg_dir, [],
                                              self.test_src_mods_dir, False)

        #Due to the presence of a "dict_values" dictview object which needs
        #to be treated in a special way, the tuples will need to be iterated
        #over and each element compared directly:
        for idx, elem in enumerate(gen_results):
            #Check if a variable is iterable.  If so then convert to a list,
            #which is needed to manage the "dict_values" view object:
            if isinstance(elem, Iterable):
                #Convert to list and then compare:
                self.assertEqual(list(elem), list(expected_results[idx]))
            else:
                #Assert elements match:
                self.assertEqual(elem, expected_results[idx])
            #End if
        #End for

        #Remove extra test files:
        test_meta_files = glob.glob(f"{self.test_src_mods_dir}"+os.sep+"temp_adjust.*")
        for test_file in test_meta_files:
            os.remove(test_file)
        #End for
        os.remove(os.path.join(self.test_src_mods_dir, "suite_simple.xml"))

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_missing_sdf_generate_physics_suites(self):

        """
        Check that "generate_physics_suites" fails with
        the correct error if the specified SDF cannot be
        found.
        """

        #Set expected error message:
        emsg = "ERROR: Unable to find SDF for suite 'missing'"

        #Expect "CamAutoGenError":
        with self.assertRaises(CamAutoGenError) as autoerr:
            _ = generate_physics_suites(self.test_cache, "UNSET", "cam", "missing",
                                        _CAM_ROOT_DIR, self.test_bldroot,
                                        self.test_reg_dir, [],
                                        self.test_src_mods_dir, False)
        #End with

        #Check that error message matches what's expected:
        self.assertEqual(emsg, str(autoerr.exception))

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_missing_scheme_generate_physics_suites(self):

        """
        Check that "generate_physics_suites" fails with
        the correct error if a scheme specified by an
        SDF cannot be found.
        """

        #Set expected error message:
        emsg = "ERROR: No metadata file found for physics scheme 'temp_adjust'"

        #Copy test files into test SourceMods directory:
        test_suite = os.path.join(self.test_suite_path, "write_init_files", "suite_simple.xml")
        shutil.copy2(test_suite, self.test_src_mods_dir)

        #Expect "CamAutoGenError":
        with self.assertRaises(CamAutoGenError) as autoerr:
            _ = generate_physics_suites(self.test_cache, "UNSET", "cam", "simple",
                                        _CAM_ROOT_DIR, self.test_bldroot,
                                        self.test_reg_dir, [],
                                        self.test_src_mods_dir, False)
        #End with

        #Check that error message matches what's expected:
        self.assertEqual(emsg, str(autoerr.exception))

        #Remove extra test file:
        os.remove(os.path.join(self.test_src_mods_dir, "suite_simple.xml"))

    #+++++++++++++++++++++++++++++++++++++++++++++++++++
    #Tests for "generate_init_routines" autogen function
    #+++++++++++++++++++++++++++++++++++++++++++++++++++

    def test_generate_init_routines(self):

        """Check that "test_generate_init_routines" works properly if given the proper inputs"""

        #Set expected output path:
        expected_path = os.path.join(self.test_bldroot, "phys_init")

        #Run init routines generation function:
        gen_path = generate_init_routines(self.test_cache, self.test_bldroot, False, False,
                                          self.test_src_mods_dir, self.fort_indent, None, {}, [])

        #Check that the output path matches what is expected:
        self.assertEqual(gen_path, expected_path)

#################################################
#Run unit tests if this script is called directly
#################################################

if __name__ == "__main__":
    unittest.main()

############
#End of file
############
