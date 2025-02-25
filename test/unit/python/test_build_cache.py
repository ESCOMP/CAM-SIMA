"""
Python unit testing collection for the various
cam_build_cache.py functions, including their
error-handling processes.  Please note that
these tests will only work with Python 3.7
or later.

To run these unit tests, simply type:

python test_build_cache.py

or (for more verbose output):

python test_build_cache.py -v

which will currently run 30 tests, all of which should pass.
"""

#----------------------------------------
#Import required python libraries/modules:
#----------------------------------------
import sys
import os
import os.path
import glob
import shutil
import filecmp

#Python unit-testing library:
import unittest

#Add directory to python path:
_CWD = os.getcwd()
_CURRDIR = os.path.abspath(os.path.dirname(__file__))
_CAM_ROOT_DIR = os.path.join(_CURRDIR, os.pardir, os.pardir, os.pardir)
_CAM_CONF_DIR = os.path.abspath(os.path.join(_CAM_ROOT_DIR, "cime_config"))
_PRE_TMP_DIR = os.path.join(_CURRDIR, "tmp")
_TMP_DIR = os.path.join(_PRE_TMP_DIR, "cam_build_cache")
_SAMPLES_DIR = os.path.join(_CURRDIR, "sample_files", "build_cache_files")
_WRITE_INIT_DIR = os.path.join(_CURRDIR, "sample_files", "write_init_files")
_CCPP_FRAMEWORK = os.path.join(_CAM_ROOT_DIR, "ccpp_framework", 'scripts')

#Check for all necessary directories:
if not os.path.exists(_CAM_CONF_DIR):
    _EMSG = f"ERROR: Cannot find required '{_CAM_CONF_DIR}' directory"
    raise ImportError(_EMSG)
#End if
if not os.path.exists(_CCPP_FRAMEWORK):
    _EMSG = f"ERROR: Cannot find CCPP-framework routines in '{_CCPP_FRAMEWORK}'"
    raise ImportError(_EMSG)
#End if

#Add "cime_config" directory to python path:
sys.path.append(_CAM_CONF_DIR)

#Add "ccpp_framework" scripts directory to python path:
sys.path.append(_CCPP_FRAMEWORK)

#Import CAM Build Cache object:
# pylint: disable=wrong-import-position
from cam_build_cache import FileStatus, BuildCacheCAM

#Import CCPP Error type:
from parse_source import CCPPError
# pylint: enable=wrong-import-position

#++++++++++++++++
#Helper functions
#++++++++++++++++

def remove_files(file_list):
    """Remove files in <file_list> if they exist"""
    for fpath in file_list:
        if os.path.exists(fpath):
            os.remove(fpath)
        #End if
    #End for

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Main cam_build_cache testing routine, used when script is run directly
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

class CamBuildCacheTestRoutine(unittest.TestCase):

    """
    Runs all CAM build cache (cam_build_cache) tests,
    to ensure that the scripts and error-handling methods
    are running properly.
    """

    #++++++++++++++++++++++
    #Test environment setup
    #++++++++++++++++++++++

    @classmethod
    def setUpClass(cls):
        """Clean output directory (tmp) before running tests"""
        #Does "tmp" directory exist?  If not then create it:
        if not os.path.exists(_PRE_TMP_DIR):
            os.mkdir(_PRE_TMP_DIR)
        #end if
        #Now check if "write_init_files" directory exists:
        if not os.path.exists(_TMP_DIR):
            os.mkdir(_TMP_DIR)
        #end if

        #Clear out all files:
        remove_files(glob.iglob(os.path.join(_TMP_DIR, '*.*')))

        #Set path to test registry file, which will be used in
        #various tests below as a stand-in for all needed files:
        test_reg_file = os.path.join(_WRITE_INIT_DIR, "param_reg.xml")

        #Copy test registry file to local "tmp" directory:
        shutil.copy2(test_reg_file, os.path.join(_TMP_DIR, "test_reg.xml"))

        #Finally, need to change the current working directory in order
        #for the relative paths to work:
        os.chdir(_CURRDIR)

        #Run inherited setup method:
        super(cls, CamBuildCacheTestRoutine).setUpClass()

    #++++++++++++++++

    @classmethod
    def tearDownClass(cls):
        """
        Return to original working directory
        now that these tests are finished.
        """

        #Return to original working working directory:
        os.chdir(_CWD)

        #Run inherited teardown method:
        super(cls, CamBuildCacheTestRoutine).tearDownClass()

    #++++++++++++++++++++++++++++++++
    #FileStatus object creation tests
    #++++++++++++++++++++++++++++++++

    def test_file_status_cache_file_hash(self):

        """
        Check that the FileStatus object is
        created successfully when given the
        proper inputs, incluing a file hash.
        """

        #Set path to test registry file:
        test_reg_file = os.path.join(_WRITE_INIT_DIR, "param_reg.xml")

        #Create new FileStatus object with assigned hashs:
        test_status = FileStatus(test_reg_file, "test", file_hash="orange")

        #Check that new status object has correct properties:
        self.assertEqual(test_status.file_path, test_reg_file)
        self.assertEqual(test_status.file_hash, "orange")
        self.assertEqual(test_status.key, "param_reg.xml")

    #++++++++++++++++

    def test_file_status_cache_file_no_hash(self):

        """
        Check that the FileStatus object is
        created successfully when given the
        proper inputs, except a file hash.
        """

        #Set path to test registry file:
        test_reg_file = os.path.join(_WRITE_INIT_DIR, "param_reg.xml")

        #Set expected sha256 hash value:
        test_hash = "584c7f0992d4af811afb2069c752f279c51a1ac4"

        #Create new FileStatus object with assigned hash:
        test_status = FileStatus(test_reg_file, "test")

        #Check that new status object has correct properties:
        self.assertEqual(test_status.file_path, test_reg_file)
        self.assertEqual(test_status.file_hash, test_hash)
        self.assertEqual(test_status.key, "param_reg.xml")

    #++++++++++++++++

    def test_file_status_no_file(self):

        """
        Check that the correct error is raised when
        the FileStatus object is initialized with
        no provided hash and a non-existent file.
        """

        #Set path to non-existent file:
        missing_file = os.path.join(_WRITE_INIT_DIR, "scooby_dooby.doo")

        #Set expected error message:
        emsg = f"ERROR: 'test', '{missing_file}', does not exist"

        #Expect Value error when initializing object:
        with self.assertRaises(ValueError) as verr:
            _ = FileStatus(missing_file, "test")
        #End with

        #Check that error message matches what is expected:
        self.assertEqual(emsg, str(verr.exception))

    #++++++++++++++++

    def test_file_status_hash_mismatch(self):

        """
        Check that the "hash_mismatch" method
        works as expected.
        """

        #Set path to test registry file:
        test_reg_file = os.path.join(_WRITE_INIT_DIR, "param_reg.xml")

        #Set path to temporary copy of registry file:
        mod_reg_file = os.path.join(_TMP_DIR, "param_reg_mod.xml")

        #Create new FileStatus object:
        test_status = FileStatus(test_reg_file, "test")

        #Open test file:
        with open(test_reg_file, "r", encoding="utf-8") as test_fil:
            #Read in file:
            file_lines = test_fil.readlines()

            #Modify third line:
            file_lines[2] = '<registry name="zoiks" version="1.0">'
        #end with

        #Now create new file with modified lines:
        with open(mod_reg_file, "w", encoding="utf-8") as mod_fil:
            #Write lines to new file:
            mod_fil.writelines(file_lines)
        #End with

        #Check that hash_mismatch returns the correct result:
        self.assertTrue(test_status.hash_mismatch(mod_reg_file))

    #+++++++++++++++++++++++++++++++++++
    #BuildCacheCAM object creation tests
    #+++++++++++++++++++++++++++++++++++

    def test_build_cache_cam_no_file(self):

        """
        Check that the BuildCacheCAM object is
        created successfully when the cache
        file does not exist.
        """

        #Create non-existent cache file path:
        cache_file = os.path.join(_TMP_DIR, "empty_test_cache.xml")

        #Create new build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Check that outputs match expected defaults:
        self.assertEqual(test_cache.scheme_nl_metadata(), [])
        self.assertEqual(test_cache.scheme_nl_groups(), [])
        self.assertEqual(test_cache.reg_file_list(), [])
        self.assertEqual(test_cache.ic_names(), {})

    #++++++++++++++++

    def test_build_cache_cam_file(self):

        """
        Check that the BuildCacheCAM object is
        create successfully when an already
        existing cache file is used.
        """

        #Set expected outputs:
        nl_meta_list  = ['/not/in/kansas/wizard_nl.meta']
        nl_group_list = ['toto', 'dog']
        reg_file_list = ['/yellow/brick/road/munchkin.meta']
        ic_names_dict = {'Only_had_a': ['brain', 'heart']}

        #Set path to already-existing cache file:
        cache_file = os.path.join(_SAMPLES_DIR, "example_build_cache.xml")

        #Create new  build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Check that outputs match values listed in cache file:
        self.assertEqual(test_cache.scheme_nl_metadata(), nl_meta_list)
        self.assertEqual(test_cache.scheme_nl_groups(), nl_group_list)
        self.assertEqual(test_cache.reg_file_list(), reg_file_list)
        self.assertEqual(test_cache.ic_names(), ic_names_dict)

    #++++++++++++++++

    def test_build_cache_wrong_file_type(self):

        """
        Check that the correct error is raised when
        the BuildCacheCAM object is initialized with
        the wrong file type.
        """

        #Set path to "bad" file:
        non_xml_file = os.path.join(_SAMPLES_DIR, os.pardir, "ref_pres.meta")

        #Set expected error message:
        emsg = f"read_xml_file: Cannot read {non_xml_file}, syntax error: line 1, column 0"

        #Expect CCPP error when initializing object:
        with self.assertRaises(CCPPError) as cerr:
            _ = BuildCacheCAM(non_xml_file)
        #End with

        #Check that error message matches what is expected:
        self.assertEqual(emsg, str(cerr.exception))

    def test_build_cache_wrong_section_tag(self):

        """
        Check that the correct error is raised when
        the BuildCacheCAM object is initialized with a
        cache file that contains an unkown section tag.
        """

        #Set path to cache file with "bad" section tag:
        bad_section_tag_file = os.path.join(_SAMPLES_DIR, "bad_section_tag_build_cache.xml")

        #Set expected error message:
        emsg = "ERROR: Unknown section tag, 'Wicked_Witch'"

        #Expect Value error when initializing object:
        with self.assertRaises(ValueError) as verr:
            _ = BuildCacheCAM(bad_section_tag_file)
        #End with

        #Check that error message matches what is expected:
        self.assertEqual(emsg, str(verr.exception))

    #++++++++++++++++

    def test_build_cache_wrong_reg_tag(self):

        """
        Check that the correct error is raised when
        the BuildCacheCAM object is initialized with
        a cache file that has a bad registry tag.
        """

        #Set path to cache file with "bad" registry tag:
        bad_reg_tag_file = os.path.join(_SAMPLES_DIR, "bad_reg_tag_build_cache.xml")

        #Set expected error message:
        emsg = "ERROR: Unknown registry tag, 'Wizard'"

        #Expect Value error when initializing object:
        with self.assertRaises(ValueError) as verr:
            _ = BuildCacheCAM(bad_reg_tag_file)
        #End with

        #Check that error message matches what is expected:
        self.assertEqual(emsg, str(verr.exception))

    #++++++++++++++++

    def test_build_cache_wrong_ccpp_tag(self):

        """
        Check that the correct error is raised when
        the BuildCacheCAM object is initialized with
        a cache file that has a bad CCPP tag.
        """

        #Set path to cache file with "bad" registry tag:
        bad_ccpp_tag_file = os.path.join(_SAMPLES_DIR, "bad_ccpp_tag_build_cache.xml")

        #Set expected error message:
        emsg = "ERROR: Unknown CCPP tag, 'Good_Witch'"

        #Expect Value error when initializing object:
        with self.assertRaises(ValueError) as verr:
            _ = BuildCacheCAM(bad_ccpp_tag_file)
        #End with

        #Check that error message matches what is expected:
        self.assertEqual(emsg, str(verr.exception))

    #+++++++++++++++++++++++++
    #Registry generation tests
    #+++++++++++++++++++++++++

    def test_update_registry(self):

        """
        Check that the "update_registry"
        method successfully updates the build
        cache, using the "write" method to validate
        the change.
        """

        #Set path to already-existing cache file:
        cache_file = os.path.join(_SAMPLES_DIR, "example_build_cache.xml")

        #Set path to expected cache file:
        expected_file = os.path.join(_SAMPLES_DIR, "update_reg_build_cache.xml")

        #Set path to "new" build cache file:
        test_file = os.path.join(_TMP_DIR, "update_reg_build_cache.xml")

        #Set path to test registry file, which is used in the provided cache file,
        #making sure to use the relative path in order to exactly match the cache:
        tmp_test_reg = os.path.join("tmp", "cam_build_cache", "test_reg.xml")

        #Copy build cache file to temporary directory:
        shutil.copy2(cache_file, test_file)

        #Create new build cache object:
        test_cache = BuildCacheCAM(test_file)

        #Set non-file update_registry inputs:
        ic_names = {"Only_had_a": ["heart", "brain"]}
        dycore = "banana"

        constituents = ['cnst_1', 'cnst_2']

        vars_init_value = ["Only_had_a_brain"]

        #Update registry fields:
        test_cache.update_registry(tmp_test_reg, [tmp_test_reg],
                                   dycore, [tmp_test_reg], ic_names, constituents,
                                   vars_init_value)

        #Write updated fields to build cache file:
        test_cache.write()

        #Create assertion message for file comparison:
        amsg = f"Test file '{test_file}' does not match '{expected_file}'"

        #Check that the newly written build cache file matches
        #what is expected:
        self.assertTrue(filecmp.cmp(test_file, expected_file,
                                    shallow=False), msg=amsg)

    #++++++++++++++++

    def test_registry_mismatch_good_match(self):

        """
        Check that the 'registry_mismatch'
        function returns False when there
        is no change in the registry.
        """
        #Set path to already-existing cache file used by test_write_init_files:
        cache_file = os.path.join(_WRITE_INIT_DIR, "simple_build_cache_template.xml")

        #Set path to registry generator file listed in build_cache file.
        #Please note that in this sample file the registry XML file is listed,
        #and not a python file as would normally be the case:
        reg_file = os.path.join(_WRITE_INIT_DIR, "simple_reg.xml")

        #Create new  build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Run registry_mismatch function:
        reg_match = test_cache.registry_mismatch(reg_file, [reg_file], "none")

        #Check that function returns False:
        self.assertFalse(reg_match)

    #++++++++++++++++

    def test_registry_mismatch_diff_dycore(self):

        """
        Check that the 'registry_mismatch'
        function returns True when there is
        a change in the dycore being used.
        """

        #Set path to already-existing cache file used by test_write_init_files:
        cache_file = os.path.join(_WRITE_INIT_DIR, "simple_build_cache_template.xml")

        #Set path to registry generator file listed in build_cache file.
        #Please note that in this sample file the registry XML file is listed,
        #and not a python file as would normally be the case:
        reg_file = os.path.join(_WRITE_INIT_DIR, "simple_reg.xml")

        #Create new  build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Set "new" dycore value:
        new_dycore = "se"

        #Run registry_mismatch function:
        reg_match = test_cache.registry_mismatch(reg_file, [reg_file], new_dycore)

        #Check that function returns True:
        self.assertTrue(reg_match)

    #++++++++++++++++

    def test_registry_mismatch_diff_reg_file(self):

        """
        Check that the 'registry_mismatch'
        function returns True when there is
        a change in the registry file being used.
        """

        #Set path to already-existing cache file used by test_write_init_files:
        cache_file = os.path.join(_WRITE_INIT_DIR, "simple_build_cache_template.xml")

        #Set path to registry generator file listed in build_cache file.
        #Please note that in this sample file the registry XML file is listed,
        #and not a python file as would normally be the case:
        reg_file = os.path.join(_WRITE_INIT_DIR, "simple_reg.xml")

        #Set path to "new" registry file:
        new_file = os.path.join(_WRITE_INIT_DIR, "param_reg.xml")

        #Create new  build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Run registry_mismatch function:
        reg_match = test_cache.registry_mismatch(reg_file, [new_file], "none")

        #Check that function returns True:
        self.assertTrue(reg_match)

    #++++++++++++++++

    def test_registry_mismatch_diff_gen_file(self):

        """
        Check that the 'registry_mismatch'
        function returns True when there is a
        change in the registry generator being
        used.
        """

        #Set path to already-existing cache file used by test_write_init_files:
        cache_file = os.path.join(_WRITE_INIT_DIR, "simple_build_cache_template.xml")

        #Set path to registry generator file listed in build_cache file.
        #Please note that in this sample file the registry XML file is listed,
        #and not a python file as would normally be the case:
        reg_file = os.path.join(_WRITE_INIT_DIR, "simple_reg.xml")

        #Set path to "new" registry file:
        new_file = os.path.join(_WRITE_INIT_DIR, "param_reg.xml")

        #Create new  build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Run registry_mismatch function:
        reg_match = test_cache.registry_mismatch(new_file, [reg_file], "none")

        #Check that function returns True:
        self.assertTrue(reg_match)

    #++++++++++++++++++++++++++++++++++++
    #CCPP Physics suites generation tests
    #++++++++++++++++++++++++++++++++++++

    def test_update_ccpp(self):

        """
        Check that the "update_ccpp"
        method successfully updates the build
        cache, using the "write" method to validate
        the change.
        """

        #Set path to already-existing cache file:
        cache_file = os.path.join(_SAMPLES_DIR, "example_build_cache.xml")

        #Set path to expected cache file:
        expected_file = os.path.join(_SAMPLES_DIR, "update_ccpp_build_cache.xml")

        #Set path to "new" build cache file:
        test_file = os.path.join(_TMP_DIR, "update_ccpp_build_cache.xml")

        #Set path to test registry file, which is used in the provided cache file,
        #making sure to use the relative path in order to exactly match the cache:
        tmp_test_reg = os.path.join("tmp", "cam_build_cache", "test_reg.xml")

        #Copy build cache file to temporary directory:
        shutil.copy2(cache_file, test_file)

        #Create new build cache object:
        test_cache = BuildCacheCAM(test_file)

        #Set file-based update_ccpp inputs:
        xml_nl_file_dict = {"starfruit": tmp_test_reg}

        #Set non-file update_ccpp inputs:
        nl_meta      = ["/dragon/fruit/kumquat_namelist.meta"]
        nl_groups    = ["orange", "lemon"]
        preproc_defs = "-DBANANA"
        kind_types   = ["kind_phys=REAL32"]

        #Update ccpp fields:
        test_cache.update_ccpp([tmp_test_reg], [tmp_test_reg], [tmp_test_reg],
                               xml_nl_file_dict, nl_meta, nl_groups,
                               tmp_test_reg, preproc_defs, kind_types)

        #Write updated fields to build cache file:
        test_cache.write()

        #Create assertion message for file comparison:
        amsg = f"Test file '{test_file}' does not match '{expected_file}'"

        #Check that the newly written build cache file matches
        #what is expected:
        self.assertTrue(filecmp.cmp(test_file, expected_file,
                                    shallow=False), msg=amsg)

    #++++++++++++++++

    def test_ccpp_mismatch_good_match(self):

        """
        Check that the 'ccpp_mismatch'
        function returns False when there
        is no change to the CCPP framework
        or physics schemes.
        """

        #Set path to already-existing cache file used by test_write_init_files:
        cache_file = os.path.join(_WRITE_INIT_DIR, "simple_build_cache_template.xml")

        #Set path to Suite Definition File (SDF) listed in build_cache file:
        sdf_file = os.path.join(_WRITE_INIT_DIR, "suite_simple.xml")

        #Set path to physics scheme meta file:
        scheme_meta_file = os.path.join(_WRITE_INIT_DIR, "temp_adjust_scalar.meta")

        #Create new build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Run registry_mismatch function:
        ccpp_match = test_cache.ccpp_mismatch([sdf_file], [scheme_meta_file], [],
                                              "UNSET", ["kind_phys = REAL64"])

        #Check that function returns False:
        self.assertFalse(ccpp_match)

    #++++++++++++++++

    def test_ccpp_mismatch_diff_preproc(self):

        """
        Check that the 'ccpp_mismatch'
        function returns True when there
        is a change in the pre-processor
        definitions being used.
        """

        #Set path to already-existing cache file used by test_write_init_files:
        cache_file = os.path.join(_WRITE_INIT_DIR, "simple_build_cache_template.xml")

        #Set path to Suite Definition File (SDF) listed in build_cache file:
        sdf_file = os.path.join(_WRITE_INIT_DIR, "suite_simple.xml")

        #Set path to physics scheme meta file:
        scheme_meta_file = os.path.join(_WRITE_INIT_DIR, "temp_adjust_scalar.meta")

        #Set "new" pre-processor definition:
        preproc_def = "BANANA"

        #Create new  build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Run registry_mismatch function:
        ccpp_match = test_cache.ccpp_mismatch([sdf_file], [scheme_meta_file], [],
                                              preproc_def, ["kind_phys = REAL64"])

        #Check that function returns True:
        self.assertTrue(ccpp_match)

    #++++++++++++++++

    def test_ccpp_mismatch_diff_kind(self):

        """
        Check that the 'ccpp_mismatch'
        function returns True when there
        is a change in the kind types
        being used.
        """

        #Set path to already-existing cache file used by test_write_init_files:
        cache_file = os.path.join(_WRITE_INIT_DIR, "simple_build_cache_template.xml")

        #Set path to Suite Definition File (SDF) listed in build_cache file:
        sdf_file = os.path.join(_WRITE_INIT_DIR, "suite_simple.xml")

        #Set path to physics scheme meta file:
        scheme_meta_file = os.path.join(_WRITE_INIT_DIR, "temp_adjust_scalar.meta")

        #Set "new" physics kind value:
        new_kind_phys_def = ["kind_phys = REAL32"]

        #Create new build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Run registry_mismatch function:
        ccpp_match = test_cache.ccpp_mismatch([sdf_file], [scheme_meta_file], [],
                                              "UNSET", new_kind_phys_def)

        #Check that function returns True:
        self.assertTrue(ccpp_match)

    #++++++++++++++++

    def test_ccpp_mismatch_diff_sdf(self):

        """
        Check that the 'ccpp_mismatch'
        function returns True when there
        is a change in the SDF being
        used.
        """

        #Set path to already-existing cache file used by test_write_init_files:
        cache_file = os.path.join(_WRITE_INIT_DIR, "simple_build_cache_template.xml")

        #Set path to "new" Suite Definition File (SDF), which in this case is actually
        #just a registry file:
        new_sdf_file = os.path.join(_WRITE_INIT_DIR, "simple_reg.xml")

        #Set path to physics scheme meta file:
        scheme_meta_file = os.path.join(_WRITE_INIT_DIR, "temp_adjust_scalar.meta")

        #Create new build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Run registry_mismatch function:
        ccpp_match = test_cache.ccpp_mismatch([new_sdf_file], [scheme_meta_file], [],
                                              "UNSET", ["kind_phys = REAL64"])

        #Check that function returns True:
        self.assertTrue(ccpp_match)

    #++++++++++++++++

    def test_ccpp_mismatch_diff_scheme(self):

        """
        Check that the 'ccpp_mismatch'
        function returns True when there
        is a change in the scheme being
        used.
        """

        #Set path to already-existing cache file used by test_write_init_files:
        cache_file = os.path.join(_WRITE_INIT_DIR, "simple_build_cache_template.xml")

        #Set path to Suite Definition File (SDF) listed in build_cache file:
        sdf_file = os.path.join(_WRITE_INIT_DIR, "suite_simple.xml")

        #Set path to "new" physics scheme meta file:
        new_scheme_meta_file = os.path.join(_WRITE_INIT_DIR, "temp_adjust_param.meta")

        #Create new build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Run registry_mismatch function:
        ccpp_match = test_cache.ccpp_mismatch([sdf_file], [new_scheme_meta_file], [],
                                              "UNSET", ["kind_phys = REAL64"])

        #Check that function returns True:
        self.assertTrue(ccpp_match)

    #++++++++++++++++

    def test_ccpp_mismatch_diff_host(self):

        """
        Check that the 'ccpp_mismatch'
        function returns True when there
        is a change in the host files
        being used.
        """

        #Set path to already-existing cache file used by test_write_init_files:
        cache_file = os.path.join(_WRITE_INIT_DIR, "simple_build_cache_template.xml")

        #Set path to Suite Definition File (SDF) listed in build_cache file:
        sdf_file = os.path.join(_WRITE_INIT_DIR, "suite_simple.xml")

        #Set path to physics scheme meta file:
        scheme_meta_file = os.path.join(_WRITE_INIT_DIR, "temp_adjust_scalar.meta")

        #Set path to "new" host model meta file:
        new_host_file = os.path.join(_WRITE_INIT_DIR, "simple_host.meta")

        #Create new build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Run registry_mismatch function:
        ccpp_match = test_cache.ccpp_mismatch([sdf_file], [scheme_meta_file],
                                              [new_host_file], "UNSET",
                                              ["kind_phys = REAL64"])

        #Check that function returns True:
        self.assertTrue(ccpp_match)

    #++++++++++++++++++++++++++++++++
    #CCPP scheme namelist files tests
    #++++++++++++++++++++++++++++++++

    def test_xml_nl_good_match(self):

        """
        Check that the 'xml_nl_mismatch'
        function returns False when there
        is no change in the namelist files
        or readnl generation function being
        used.
        """

        #Set path to already-existing cache file used by "update" tests:
        cache_file = os.path.join(_SAMPLES_DIR, "update_ccpp_build_cache.xml")

        #Set path to test registry file, which is used in the provided cache file,
        #making sure to use the relative path in order to exactly match the cache:
        tmp_test_reg = os.path.join("tmp", "cam_build_cache", "test_reg.xml")

        #Create new build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Create xml namelist files dictionary:
        xml_nl_file_dict = {"starfruit": tmp_test_reg}

        #Run xml_nl_mismatch function:
        nl_match = test_cache.xml_nl_mismatch(tmp_test_reg, xml_nl_file_dict)

        #Check that function returns False:
        self.assertFalse(nl_match)

    #++++++++++++++++

    def test_xml_nl_diff_create_nl_file(self):

        """
        Check that the 'xml_nl_mismatch'
        function returns True when there
        is a change in the readnl generation
        function being used.
        """

        #Set path to already-existing cache file used by "update" tests:
        cache_file = os.path.join(_SAMPLES_DIR, "update_ccpp_build_cache.xml")

        #Set path to test registry file, which is used in the provided cache file,
        #making sure to use the relative path in order to exactly match the cache:
        tmp_test_reg = os.path.join("tmp", "cam_build_cache", "test_reg.xml")

        #Set path to "new" file being used as the namelist generator file.  Please
        #note that for simplicity this is still just another registry xml file:
        new_create_nl_file = os.path.join(_WRITE_INIT_DIR, "simple_reg.xml")

        #Create new build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Create xml namelist files dictionary:
        xml_nl_file_dict = {"starfruit": tmp_test_reg}

        #Run xml_nl_mismatch function:
        nl_match = test_cache.xml_nl_mismatch(new_create_nl_file, xml_nl_file_dict)

        #Check that function returns False:
        self.assertTrue(nl_match)

    #++++++++++++++++

    def test_xml_nl_diff_xml_file_path(self):

        """
        Check that the 'xml_nl_mismatch'
        function returns True when there
        is a difference in the namelist xml
        file path, even if the files themselves
        are the same.
        """

        #Set path to already-existing cache file used by "update" tests:
        cache_file = os.path.join(_SAMPLES_DIR, "update_ccpp_build_cache.xml")

        #Set path to test registry file, which is used in the provided cache file:
        tmp_test_reg = os.path.join("tmp", "cam_build_cache", "test_reg.xml")

        #Also set path to original registry file, which should have the same
        #file contents (and thus the same hash), but a different file path:
        test_reg_file = os.path.join(_WRITE_INIT_DIR, "param_reg.xml")

        #Create new build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Create xml namelist files dictionary, but this time
        #pointing to the original, non-cached file path:
        xml_nl_file_dict = {"starfruit": test_reg_file}

        #Run xml_nl_mismatch function:
        nl_match = test_cache.xml_nl_mismatch(tmp_test_reg, xml_nl_file_dict)

        #Check that function returns False:
        self.assertTrue(nl_match)

    #++++++++++++++++

    def test_xml_nl_diff_xml_file(self):

        """
        Check that the 'xml_nl_mismatch'
        function returns True when there
        is a difference in the namelist xml
        file, even if the file paths are the
        same.
        """

        #Set path to already-existing cache file used by "update" tests:
        cache_file = os.path.join(_SAMPLES_DIR, "update_ccpp_build_cache.xml")

        #Set path to test registry file, which is used in the provided cache file,
        #making sure to use the relative path in order to exactly match the cache:
        tmp_test_reg = os.path.join("tmp", "cam_build_cache", "test_reg.xml")

        #Set path to "new" xml file, which is different from the one provided
        #in the cache file:
        test_reg_file = os.path.join(_WRITE_INIT_DIR, "simple_reg.xml")

        #Copy "new" xml file to local "tmp" directory, so that it has the
        #same path and name as the cached file, but different  contents/hash:
        shutil.copy2(test_reg_file, os.path.join(_CURRDIR, tmp_test_reg))

        #Create new build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Create xml namelist files dictionary, but this time
        #pointing to the original, non-cached file path:
        xml_nl_file_dict = {"starfruit": tmp_test_reg}

        #Run xml_nl_mismatch function:
        nl_match = test_cache.xml_nl_mismatch(tmp_test_reg, xml_nl_file_dict)

        #Check that function returns False:
        self.assertTrue(nl_match)

        #Reset the "test_reg.xml" file to it's original values,
        #in order to avoid test failures elsewhere:
        orig_reg_file = os.path.join(_WRITE_INIT_DIR, "param_reg.xml")
        shutil.copy2(orig_reg_file, os.path.join(_CURRDIR, tmp_test_reg))

    #++++++++++++++++++++++++++++++++++++++++
    #Initialization routines generation tests
    #++++++++++++++++++++++++++++++++++++++++

    def test_update_init_gen(self):

        """
        Check that the "update_init_gen"
        method successfully updates the build
        cache, using the "write" method to validate
        the change.
        """

        #Set path to already-existing cache file:
        cache_file = os.path.join(_SAMPLES_DIR, "example_build_cache.xml")

        #Set path to expected cache file:
        expected_file = os.path.join(_SAMPLES_DIR, "update_init_gen_build_cache.xml")

        #Set path to "new" build cache file:
        test_file = os.path.join(_TMP_DIR, "update_init_gen_build_cache.xml")

        #Set path to test registry file, which will be used in the
        #"updated" build cache as a stand-in for all needed files:
        test_reg_file = os.path.join(_WRITE_INIT_DIR, "param_reg.xml")

        #Copy build cache file to temporary directory:
        shutil.copy2(cache_file, test_file)

        #Copy registry file to local "tmp" directory, which
        #removes the need to match absolute paths when
        #checking the contents of the output file:
        tmp_test_reg = os.path.join("tmp", "cam_build_cache", "test_reg.xml")
        shutil.copy2(test_reg_file, os.path.join(os.curdir, tmp_test_reg))

        #Create new build cache object:
        test_cache = BuildCacheCAM(test_file)

        #Update init routines generator:
        test_cache.update_init_gen(tmp_test_reg)

        #Write updated fields to build cache file:
        test_cache.write()

        #Create assertion message for file comparison:
        amsg = f"Test file '{test_file}' does not match '{expected_file}'"

        #Check that the newly written build cache file matches
        #what is expected:
        self.assertTrue(filecmp.cmp(test_file, expected_file,
                                    shallow=False), msg=amsg)

    #++++++++++++++++

    def test_init_write_mismatch_good_match(self):

        """
        Check that the 'init_write_mismatch'
        function returns False when there
        is no change in the init files writing
        function being used.
        """

        #Set path to already-existing cache file used by test_write_init_files:
        cache_file = os.path.join(_WRITE_INIT_DIR, "simple_build_cache_template.xml")

        #Set path to initialization files generator file listed in build_cache file.
        #Please note that in this sample file the registry XML file is listed,
        #and not a python file as would normally be the case:
        init_gen_file = os.path.join(_WRITE_INIT_DIR, "simple_reg.xml")

        #Create new  build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Run init_write_mismatch function:
        init_match = test_cache.init_write_mismatch(init_gen_file)

        #Check that function returns False:
        self.assertFalse(init_match)

    #++++++++++++++++

    def test_init_write_diff_host(self):

        """
        Check that the 'init_write_mismatch'
        function returns True when there
        is a change in the init files writing
        function being used.
        """

        #Set path to already-existing cache file used by test_write_init_files:
        cache_file = os.path.join(_WRITE_INIT_DIR, "simple_build_cache_template.xml")

        #Set path to "new" initialization files generator,
        #which is just a different XML file in this case:
        new_init_gen_file = os.path.join(_WRITE_INIT_DIR, "suite_simple.xml")

        #Create new  build cache object:
        test_cache = BuildCacheCAM(cache_file)

        #Run init_write_mismatch function:
        init_match = test_cache.init_write_mismatch(new_init_gen_file)

        #Check that function returns True:
        self.assertTrue(init_match)

#################################################
#Run unit tests if this script is called directly
#################################################

if __name__ == "__main__":
    unittest.main()

############
#End of file
############
