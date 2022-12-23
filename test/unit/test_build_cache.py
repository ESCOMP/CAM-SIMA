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

which will currently run XX tests, all of which should pass.
"""

#----------------------------------------
#Import required python libraries/modules:
#----------------------------------------
import sys
import os
import os.path

#Python unit-testing library:
import unittest

#Add directory to python path:
_CURRDIR = os.path.abspath(os.path.dirname(__file__))
_CAM_ROOT_DIR = os.path.join(_CURRDIR, os.pardir, os.pardir)
_CAM_CONF_DIR = os.path.abspath(os.path.join(_CAM_ROOT_DIR, "cime_config"))
_PRE_TMP_DIR = os.path.join(_CURRDIR, "tmp")
_TMP_DIR = os.path.join(_PRE_TMP_DIR, "cam_build_cache")
_SAMPLES_DIR = os.path.join(_CURRDIR, "sample_files", "build_cache_files")
_WRITE_INIT_DIR = os.path.join(_CURRDIR, "sample_files", "write_init_files")
_CCPP_FRAMEWORK = os.path.join(_CAM_ROOT_DIR, "ccpp_framework", 'scripts')

#Check for all necessary directories:
if not os.path.exists(_CAM_CONF_DIR):
    _EMSG = f"ERROR: Cannot find required '{_CAM_ROOT_DIR}' directory"
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
from cam_build_cache import BuildCacheCAM

#Import CCPP Error type:
from parse_source import CCPPError
# pylint: enable=wrong-import-position

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Main cam_build_cache testing routine, used when script is run directly
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

class CamBuildCacheTestRoutine(unittest.TestCase):

    """
    Runs all CAM build cache (cam_build_cache) tests,
    to ensure that the scripts and error-handling methods
    are running properly.
    """

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
        reg_match = test_cache.registry_mismatch(reg_file, [reg_file], "none", None)

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
        reg_match = test_cache.registry_mismatch(reg_file, [reg_file], new_dycore, None)

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
       reg_match = test_cache.registry_mismatch(reg_file, [new_file], "none", None)

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
        reg_match = test_cache.registry_mismatch(new_file, [reg_file], "none", None)

        #Check that function returns True:
        self.assertTrue(reg_match)

    #++++++++++++++++

    def test_registry_mismatch_diff_config(self):

        """
        Check that the 'registry_mismatch'
        function returns True when the there is
        a change in the registry config options
        being used.
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
        reg_match = test_cache.registry_mismatch(reg_file, [reg_file], "none", "banana")

        #Check that function returns True:
        self.assertTrue(reg_match)

    #++++++++++++++++++++++++++++++++++++
    #CCPP Physics suites generation tests
    #++++++++++++++++++++++++++++++++++++

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

    #++++++++++++++++++++++++++++++++++++++++
    #Initialization routines generation tests
    #++++++++++++++++++++++++++++++++++++++++

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

        #Run registry_mismatch function:
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

        #Run registry_mismatch function:
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
