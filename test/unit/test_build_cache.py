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

#Check for all necessary directories:
if not os.path.exists(_CAM_CONF_DIR):
    _EMSG = f"ERROR: Cannot find 'cime_config' directory in '{_CAM_CONF_DIR}'"
    raise ImportError(_EMSG)
#End if

#Add "cime_config" directory to python path:
sys.path.append(_CAM_CONF_DIR)

#Import CAM Build Cache object:
# pylint: disable=wrong-import-position
from cam_build_cache import BuildCacheCAM
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

    #+++++++++++++++++++++++++
    #Registry generation tests
    #+++++++++++++++++++++++++

    #++++++++++++++++++++++++++++++++++++
    #CCPP Physics suites generation tests
    #++++++++++++++++++++++++++++++++++++


    #++++++++++++++++++++++++++++++++++++++++
    #Initialization routines generation tests
    #++++++++++++++++++++++++++++++++++++++++

#################################################
#Run unit tests if this script is called directly
#################################################

if __name__ == "__main__":
    unittest.main()

############
#End of file
############
