"""
Class, methods, and supporting functions to track whether either of CAM's
specialized pre-processing file generation routines (CAM registry and CCPP)
need to be run as part of a current build.

To run doctests on this file: python cam_build_cache.py
"""

#----------------------------------------
# Import generic python libraries/modules
#----------------------------------------
import sys
import os
import hashlib
import xml.etree.ElementTree as ET

# Find and include the ccpp-framework scripts directory
# Assume we are in <CAMROOT>/cime_config and SPIN is in <CAMROOT>/ccpp_framework
__CURRDIR = os.path.abspath(os.path.dirname(__file__))
__CAMROOT = os.path.abspath(os.path.join(__CURRDIR, os.pardir))
__SPINSCRIPTS = os.path.join(__CAMROOT, "ccpp_framework", 'scripts')
if __SPINSCRIPTS not in sys.path:
    sys.path.append(__SPINSCRIPTS)
# end if

# CCPP framework imports
# pylint: disable=wrong-import-position
from parse_tools import read_xml_file
# pylint: enable=wrong-import-position

###############################################################################
def new_xml_entry(parent, tag, path, file_hash):
###############################################################################
    """Create a new file type entry in the cache"""
    new_entry = ET.SubElement(parent, tag)
    new_entry.set('file_path', path)
    new_entry.set('hash', file_hash)
    return new_entry

###############################################################################
def new_entry_from_xml(item):
###############################################################################
    """Create a new FileStatus entry from XML entry, <item>.
        ValueError is thrown in error.
    """
    path = item.get('file_path', default=None)
    file_hash = item.get('hash', default=None)
    new_entry = None
    if path and file_hash:
        new_entry = FileStatus(path, item.tag, file_hash)
    elif path:
        emsg = f"ERROR: No hash for {item.tag}, '{path}'"
        raise ValueError(emsg)
    elif file_hash:
        emsg = f"ERROR: No path for {item.tag} XML item"
        raise ValueError(emsg)
    else:
        raise ValueError(f"ERROR: Invalid {item.tag} XML item")
    # end if
    return new_entry

class FileStatus:
    """Class to hold full path and SHA hash of a file"""

    def __init__(self, file_path, description, file_hash=None):
        """Create a FileStatus object with the file's path and hash.
        It is an error for <file_path> to not exist
        unless <file_hash> is present.
        <description> is used to generate any error messages."""
        self.__path = file_path
        if file_hash:
             # Add an existing entry
            self.__hash = file_hash
        elif os.path.exists(self.__path):
            self.__hash = FileStatus.sha1sum(self.__path)
        else:
            emsg = "ERROR: {}, '{}', does not exist"
            raise ValueError(emsg.format(description, file_path))
        # end if

    def hash_mismatch(self, file_path):
        """Return True unless the hash of <file_path> matches our hash"""
        return self.file_hash != FileStatus.sha1sum(file_path)

    @property
    def file_path(self):
        """Return the full path this this file"""
        return self.__path

    @property
    def file_hash(self):
        """Return the SHA1 hash of this file"""
        return self.__hash

    @property
    def key(self):
        """Return a dictionary key for this file"""
        return FileStatus.gen_key(self.__path)

    @classmethod
    def gen_key(cls, file_path):
        """Generate the dictionary key for this <file_path>
        (currently filename without the path)"""
        return os.path.basename(file_path)

    @classmethod
    def sha1sum(cls, filename):
        """Efficient SHA hash of a file. SHA1 is good enough for this cache
        Stolen from Georg Sauthoff (https://stackoverflow.com/users/427158)
        """
        file_hash = hashlib.sha1()
        barray = bytearray(128*1024)
        mem_view = memoryview(barray)
        with open(filename, 'rb', buffering=0) as infile:
            for num_read in iter(lambda: infile.readinto(mem_view), 0):
                file_hash.update(mem_view[:num_read])
            # end for
        # end with
        return file_hash.hexdigest()

class BuildCacheCAM:
    """Class to store and check the state of all files involved in
    CAM pre-processing.
    For the CAM registry Fortran and metadata creation, these files include
    the registry XML file(s) and the generation python script.
    For the physics CCPP analysis and file generation, these files include
    the specified suite definition files (SDFs) along with all the scheme
    metadata and source code files for all schemes specified in the SDFs.
    Any host model metadata file changes will also trigger a CCPP rebuild so
    a registry-file creation implies a CCPP Framework run.

    doctests

    1.  Check that the proper error is generated when wrong file is input:

    >>> BuildCacheCAM(TEST_SCHEME)
    Traceback (most recent call last):
    ...
    parse_source.CCPPError: read_xml_file: Cannot read ...temp_adjust_scalar.meta, syntax error: line 1, column 0

    2.  Check that the proper error is generated when build_cache has an invalid tag:

    >>> BuildCacheCAM(BAD_BUILD_CACHE)
    Traceback (most recent call last):
    ...
    ValueError: ERROR: Unknown section tag, 'test'

    3.  Check that the proper error is generated when build_cache has an invalid registry tag:

    >>> BuildCacheCAM(BAD_BUILD_CACHE_REG)
    Traceback (most recent call last):
    ...
    ValueError: ERROR: Unknown registry tag, 'test'

    4.  Check that the proper error is generated when build_cache has an invalid ccpp tag:

    >>> BuildCacheCAM(BAD_BUILD_CACHE_CCPP)
    Traceback (most recent call last):
    ...
    ValueError: ERROR: Unknown CCPP tag, 'test'

    5.  Check that parsing works (no errors) when input is valid:

    >>> BuildCacheCAM(BUILD_CACHE)._BuildCacheCAM__dycore
    'none'

    """

    def __init__(self, build_cache):
        """
        Initialize all the build state from a build cache file.
        If <build_cache> does not exist, initialize to empty values.
        <build_cache> is also used to store build state when requested.
        """
        self.__build_cache = build_cache
        # Set empty values sure to trigger processing
        self.__gen_reg_file = None
        self.__gen_init_file = None
        self.__registry_files = {}
        self.__dycore = None
        self.__config = None
        self.__sdfs = {}
        self.__schemes = {}
        self.__host_files = {}
        self.__xml_files = {}
        self.__scheme_nl_metadata = []
        self.__scheme_nl_groups = None
        self.__create_nl_file = None
        self.__preproc_defs = None
        self.__kind_types = {}
        self.__reg_gen_files = []
        self.__ic_names = {}
        if os.path.exists(build_cache):
            # Initialize build cache state
            _, cache = read_xml_file(build_cache)
            for section in cache:
                if section.tag == 'registry':
                    for item in section:
                        if item.tag == 'generate_registry_file':
                            new_entry = new_entry_from_xml(item)
                            self.__gen_reg_file = new_entry
                        elif item.tag == 'generate_init_file':
                            new_entry = new_entry_from_xml(item)
                            self.__gen_init_file = new_entry
                        elif item.tag == 'registry_file':
                            new_entry = new_entry_from_xml(item)
                            self.__registry_files[new_entry.key] = new_entry
                        elif item.tag == 'dycore':
                            self.__dycore = item.text
                        elif item.tag == 'config':
                            self.__config = item.text
                        elif item.tag == 'reg_gen_file':
                            self.__reg_gen_files.append(item.text.strip())
                        elif item.tag == 'ic_name_entry':
                            stdname = item.get('standard_name')
                            if stdname not in self.__ic_names:
                                self.__ic_names[stdname] = []
                            # end if
                            self.__ic_names[stdname].append(item.text.strip())
                        else:
                            emsg = "ERROR: Unknown registry tag, '{}'"
                            raise ValueError(emsg.format(item.tag))
                        # end if
                elif section.tag == 'CCPP':
                    for item in section:
                        if item.tag == 'SDF':
                            new_entry = new_entry_from_xml(item)
                            self.__sdfs[new_entry.key] = new_entry
                        elif item.tag == 'scheme':
                            new_entry = new_entry_from_xml(item)
                            self.__schemes[new_entry.key] = new_entry
                        elif item.tag == 'host':
                            new_entry = new_entry_from_xml(item)
                            self.__host_files[new_entry.key] = new_entry
                        elif item.tag == 'create_nl_file':
                            new_entry = new_entry_from_xml(item)
                            self.__create_nl_file = new_entry
                        elif item.tag == 'xml_file':
                            new_entry = new_entry_from_xml(item)
                            self.__xml_files[new_entry.key] = new_entry
                        elif item.tag == 'scheme_namelist_meta_file':
                            new_entry = new_entry_from_xml(item)
                            self.__scheme_nl_metadata.append(new_entry)
                        elif item.tag == 'scheme_namelist_groups':
                            group_list = [x for x in
                                          item.text.strip().split(' ') if x]
                            self.__scheme_nl_groups = group_list
                        elif item.tag == 'preproc_defs':
                            self.__preproc_defs = item.text.strip()
                        elif item.tag == 'kind_type':
                            kname, ktype = item.text.strip().split('=')
                            self.__kind_types[kname.strip()] = ktype.strip()
                        else:
                            emsg = "ERROR: Unknown CCPP tag, '{}'"
                            raise ValueError(emsg.format(item.tag))
                        # end if
                else:
                    emsg = "ERROR: Unknown section tag, '{}'"
                    raise ValueError(emsg.format(section.tag))
                # end if
            # end for
        # end if (no else, we just have an almost empty object)
        # We always need a default definition for kind_phys
        if 'kind_phys' not in self.__kind_types:
            self.__kind_types['kind_phys'] = 'REAL64'
        # end if

    def update_registry(self, gen_reg_file, registry_source_files,
                        dycore, config, reg_file_list, ic_names):
        """Replace the registry cache data with input data
        """
        self.__dycore = dycore
        self.__config = config
        self.__gen_reg_file = FileStatus(gen_reg_file, 'generate_registry_file')
        self.__registry_files = {}
        for rfile in registry_source_files:
            new_entry = FileStatus(rfile, 'registry_file')
            self.__registry_files[new_entry.key] = new_entry
        # end for
        # reg_file_list contains the files generated from the registry
        self.__reg_gen_files = reg_file_list
        # ic_names are the initial condition variable names from the registry
        self.__ic_names = dict(ic_names)

    def update_ccpp(self, suite_definition_files, scheme_files, host_files,
                    xml_files, namelist_meta_files, namelist_groups,
                    create_nl_file, preproc_defs, kind_types):
        """Replace the ccpp cache data with input data
        """
        self.__preproc_defs = preproc_defs
        self.__kind_types = {}
        for kind_def in kind_types:
            name, ktype = [x.strip() for x in kind_def.split('=')]
            self.__kind_types[name] = ktype
        # end for
        self.__sdfs = {}
        for sfile in suite_definition_files:
            new_entry = FileStatus(sfile, 'SDF')
            self.__sdfs[new_entry.key] = new_entry
        # end for
        self.__schemes = {}
        for sfile in scheme_files:
            new_entry = FileStatus(sfile, 'scheme')
            self.__schemes[new_entry.key] = new_entry
        # end for
        self.__host_files = {}
        for hfile in host_files:
            new_entry = FileStatus(hfile, 'host')
            self.__host_files[new_entry.key] = new_entry
        # end for
        self.__xml_files = {}
        for sfile in xml_files.values():
            new_entry = FileStatus(sfile, 'xml_file')
            self.__xml_files[new_entry.key] = new_entry
        # end for
        self.__scheme_nl_metadata = namelist_meta_files
        self.__scheme_nl_groups = namelist_groups
        self.__create_nl_file = FileStatus(create_nl_file, 'create_nl_file')

    def update_init_gen(self, gen_init_file):
        """
        Replace the init_files writer
        (generate_registry_data.py) cache
        data with input data
        """
        self.__gen_init_file = FileStatus(gen_init_file, 'generate_init_file')

    def write(self):
        """Write out the current cache state"""
        new_cache = ET.Element("CAMBuildCache")
        # Registry
        registry = ET.SubElement(new_cache, 'registry')
        new_xml_entry(registry, 'generate_init_file',
                      self.__gen_init_file.file_path,
                      self.__gen_init_file.file_hash)
        new_xml_entry(registry, 'generate_registry_file',
                      self.__gen_reg_file.file_path,
                      self.__gen_reg_file.file_hash)
        for rfile in self.__registry_files.values():
            new_xml_entry(registry, 'registry_file',
                          rfile.file_path, rfile.file_hash)
        # end for
        dycore = ET.SubElement(registry, 'dycore')
        dycore.text = self.__dycore
        config = ET.SubElement(registry, 'config')
        config.text = self.__config
        for rgen_file in self.__reg_gen_files:
            rgen_entry = ET.SubElement(registry, 'reg_gen_file')
            rgen_entry.text = rgen_file
        # end for
        for stdname, ic_names in self.__ic_names.items():
            for ic_name in ic_names:
                ic_entry = ET.SubElement(registry, 'ic_name_entry')
                ic_entry.set('standard_name', stdname)
                ic_entry.text = ic_name
            # end for
        # end for
        # CCPP
        ccpp = ET.SubElement(new_cache, 'CCPP')
        for sfile in self.__sdfs.values():
            new_xml_entry(ccpp, 'SDF', sfile.file_path, sfile.file_hash)
        # end for
        for sfile in self.__schemes.values():
            new_xml_entry(ccpp, 'scheme', sfile.file_path, sfile.file_hash)
        # end for
        for sfile in self.__host_files.values():
            new_xml_entry(ccpp, 'host', sfile.file_path, sfile.file_hash)
        # end for
        for sfile in self.__xml_files.values():
            new_xml_entry(ccpp, 'xml_file', sfile.file_path, sfile.file_hash)
        # end for
        for sfile in self.__scheme_nl_metadata:
            new_xml_entry(ccpp, 'scheme_namelist_meta_file', sfile,
                          FileStatus.sha1sum(sfile))
        # end for
        if self.__scheme_nl_groups:
            scheme_nlgroups = ET.SubElement(ccpp, 'scheme_namelist_groups')
            scheme_nlgroups.text = " ".join(self.__scheme_nl_groups)
        #end if
        new_xml_entry(ccpp, 'create_nl_file',
                      self.__create_nl_file.file_path,
                      self.__create_nl_file.file_hash)
        preproc = ET.SubElement(ccpp, 'preproc_defs')
        preproc.text = self.__preproc_defs
        for kind_def, kind_type in self.__kind_types.items():
            kind_type = ET.SubElement(ccpp, 'kind_type')
            kind_type.text = f"{kind_def}={kind_type}"
        # end for
        new_cache_tree = ET.ElementTree(new_cache)
        new_cache_tree.write(self.__build_cache)

    def registry_mismatch(self, gen_reg_file, registry_source_files,
                          dycore, config):
        """
        Determine if the registry input data differs from the data
        stored in our cache. Return True if the data differs.

        doctests

        1.  Check that the function returns False when there are no changes:

        >>> BUILD_CACHE_CAM.registry_mismatch(REGISTRY_FILE, [REGISTRY_FILE], NULL_DYCORE, NONE_CONFIG)
        False

        2.  Check that the function returns True when the dycore has changed:

        >>> BUILD_CACHE_CAM.registry_mismatch(REGISTRY_FILE, [REGISTRY_FILE], SE_DYCORE, NONE_CONFIG)
        True

        3.  Check that the function returns True when the registry has been updated:

        >>> BUILD_CACHE_CAM.registry_mismatch(REGISTRY_FILE, [TEST_SDF], NULL_DYCORE, NONE_CONFIG)
        True

        4.  Check that the function returns True when the gen_reg_file has been updated:

        >>> BUILD_CACHE_CAM.registry_mismatch(TEST_SDF, [REGISTRY_FILE], NULL_DYCORE, NONE_CONFIG)
        True

        5.  Check that the function returns True when config changes:

        >>> BUILD_CACHE_CAM.registry_mismatch(REGISTRY_FILE, [REGISTRY_FILE], NULL_DYCORE, TEST_CHANGE)
        True

        """
        mismatch = False
        mismatch = (not self.__dycore) or (self.__dycore != dycore)
        mismatch = mismatch or (self.__config != config)
        if not mismatch:
            mismatch = self.__gen_reg_file.hash_mismatch(gen_reg_file)
        # end if
        # For registry files, we need to make sure we have 1-1 files
        # Note that this method will ignore duplicated files.
        if not mismatch:
            my_reg_keys = set(self.__registry_files.keys())
            test_reg_keys = {FileStatus.gen_key(x)
                             for x in registry_source_files}
            mismatch = (my_reg_keys != test_reg_keys)
            for ref_file in registry_source_files:
                if mismatch:
                    break
                # end if
                key = FileStatus.gen_key(ref_file)
                fstat = self.__registry_files[key]
                mismatch = fstat.hash_mismatch(ref_file)
            # end for
        # end if
        return mismatch

    def ccpp_mismatch(self, sdfs, scheme_files, host_files,
                      preproc_defs, kind_types):
        """
        Determine if the CCPP input data differs from the data stored in
        our cache. Return True if the data differs.

        doctests

        1.  Check that the function returns False when no changes were made:

        >>> BUILD_CACHE_CAM.ccpp_mismatch([TEST_SDF], [TEST_SCHEME], [], PREPROC_DEFS, KIND_PHYS)
        False

        2.  Check that the function returns True when the preproc_defs changes:

        >>> BUILD_CACHE_CAM.ccpp_mismatch([TEST_SDF], [TEST_SCHEME], [], TEST_CHANGE, KIND_PHYS)
        True

        3.  Check that the function returns True when kind_phys changes:

        >>> BUILD_CACHE_CAM.ccpp_mismatch([TEST_SDF], [TEST_SCHEME], [], PREPROC_DEFS, KPHYS_CHANGE)
        True

        4. Check that the function returns True when an SDF changes:

        >>> BUILD_CACHE_CAM.ccpp_mismatch([REGISTRY_FILE], [TEST_SCHEME], [], PREPROC_DEFS, KIND_PHYS)
        True

        5.  Check that the function returns True when a scheme changes:

        >>> BUILD_CACHE_CAM.ccpp_mismatch([TEST_SDF], [REGISTRY_FILE], [], PREPROC_DEFS, KIND_PHYS)
        True

        """
        mismatch = ((not self.__preproc_defs) or
                    (self.__preproc_defs != preproc_defs))
        if not mismatch:
            my_kind_defs = set(self.__kind_types.keys())
            test_kdefs = {z[0] : z[1] for z in
                          [[x.strip() for x in y.split('=')]
                           for y in kind_types]}
            test_kind_keys = test_kdefs.keys()
            test_kind_set = set(test_kind_keys)
            mismatch = my_kind_defs != test_kind_set
            for ref_kind in test_kind_keys:
                if mismatch:
                    break
                # end if
                mismatch = test_kdefs[ref_kind] != self.__kind_types[ref_kind]
            # end for
        # end if
        # For SDFs, we need to make sure we have 1-1 files
        # Note that this method will ignore duplicated files.
        if not mismatch:
            my_sdf_keys = set(self.__sdfs.keys())
            test_sdf_keys = {FileStatus.gen_key(x) for x in sdfs}
            mismatch = my_sdf_keys != test_sdf_keys
            for ref_file in sdfs:
                if mismatch:
                    break
                # end if
                key = FileStatus.gen_key(ref_file)
                fstat = self.__sdfs[key]
                mismatch = fstat.hash_mismatch(ref_file)
            # end for
        # end if
        # For scheme files, we need to make sure we have 1-1 files
        # Note that this method will ignore duplicated files.
        if not mismatch:
            my_scheme_keys = set(self.__schemes.keys())
            test_scheme_keys = {FileStatus.gen_key(x) for x in scheme_files}
            mismatch = (my_scheme_keys != test_scheme_keys)
            for ref_file in scheme_files:
                if mismatch:
                    break
                # end if
                key = FileStatus.gen_key(ref_file)
                fstat = self.__schemes[key]
                mismatch = fstat.hash_mismatch(ref_file)
            # end for
        # end if
        # For host files, we need to make sure we have 1-1 files
        # Note that this method will ignore duplicated files.
        if not mismatch:
            my_host_keys = set(self.__host_files.keys())
            test_host_keys = {FileStatus.gen_key(x) for x in host_files}
            mismatch = (my_host_keys != test_host_keys)
            for ref_file in host_files:
                if mismatch:
                    break
                # end if
                key = FileStatus.gen_key(ref_file)
                fstat = self.__host_files[key]
                mismatch = fstat.hash_mismatch(ref_file)
            # end for
        # end if
        return mismatch

    def xml_nl_mismatch(self, create_nl_file, xml_files):
        """
        Determine if any XML namelist file for CCPP schemes differs
        from the data stored in our cache. Return True if the data differs.
        Also return True if <create_nl_file> has changed.
        """
        mismatch = False
        if not mismatch:
            mismatch = self.__create_nl_file.hash_mismatch(create_nl_file)
        # end if
        if not mismatch:
            # For XML files, we need to make sure we have 1-1 files
            # Note that this method will ignore duplicated files.
            my_xml_keys = set(self.__xml_files.keys())
            test_xml_keys = {FileStatus.gen_key(x) for x in xml_files.values()}
            mismatch = (my_xml_keys != test_xml_keys)
            for ref_file in xml_files.values():
                if mismatch:
                    break
                # end if
                key = FileStatus.gen_key(ref_file)
                fstat = self.__xml_files[key]
                mismatch = fstat.hash_mismatch(ref_file)
            # end for
        # end if
        return mismatch

    def init_write_mismatch(self, gen_init_file):
        """
        Determine if the init_files writer (write_init_files.py)
            differs from the data stored in our cache. Return True
            if the data differs.

        doctests

        1.  Check that the function returns False when nothing has changed:

        >>> BUILD_CACHE_CAM.init_write_mismatch(REGISTRY_FILE)
        False

        2.  Check that the function returns True when the file has changed:

        >>> BUILD_CACHE_CAM.init_write_mismatch(TEST_SDF)
        True

        """

        #Initialize variable:
        mismatch = False

        #Check file hash to see if mis-match exists:
        mismatch = self.__gen_init_file.hash_mismatch(gen_init_file)

        #Return mismatch logical:
        return mismatch

    def scheme_nl_metadata(self):
        """Return the stored list of scheme namelist metadata files"""
        return [x.file_path for x in self.__scheme_nl_metadata]

    def scheme_nl_groups(self):
        """Return the stored list of scheme namelist groups"""
        if self.__scheme_nl_groups:
            return list(self.__scheme_nl_groups)
        return []

    def reg_file_list(self):
        """Return a copy of the filenames generated from the registry"""
        return list(self.__reg_gen_files)

    def ic_names(self):
        """Return a copy of the registry initial conditions dictionary"""
        return dict(self.__ic_names)

###############################################################################
# IGNORE EVERYTHING BELOW HERE UNLESS RUNNING TESTS ON CAM_BUILD_CACHE!
###############################################################################

# Call testing routine, if script is run directly
if __name__ == "__main__":

    # Import modules needed for testing:
    import doctest
    import logging
    import shutil

    _LOGGER = logging.getLogger(__name__)

    #++++++++++++++++++++++++++++++++++++++++++
    # Determine current working directory:
    TEST_AUTO_DIR = os.path.dirname(os.path.abspath(__file__))
    TEST_ATM_ROOT = os.path.abspath(os.path.join(TEST_AUTO_DIR, os.pardir))

    TEST_SOURCE_MODS_DIR = os.path.join(TEST_ATM_ROOT, "SourceMods")

    # Remove old test directories if they exist:
    if os.path.exists(TEST_SOURCE_MODS_DIR):
        shutil.rmtree(TEST_SOURCE_MODS_DIR)

    # Create variables for testing:
    NULL_DYCORE = 'none'
    SE_DYCORE = 'se'
    NONE_CONFIG = None
    KIND_PHYS = ["kind_phys = REAL64"]
    KPHYS_CHANGE = ["kind_phys = REAL32"]
    PREPROC_DEFS = "UNSET"
    TEST_CHANGE = "TEST"

    # Create "SourceMods directory:
    os.mkdir(TEST_SOURCE_MODS_DIR)

    # Set logger to fatal, to avoid log messages:
    _LOGGER.setLevel(logging.FATAL)

    # Set test CCPP suite paths:
    SUITE_TEST_PATH = os.path.join(TEST_ATM_ROOT, "test", "unit", "sample_files",
                                   "write_init_files")
    TEST_SDF = os.path.join(SUITE_TEST_PATH, "suite_simple.xml")
    TEST_SCHEME = os.path.join(SUITE_TEST_PATH, "temp_adjust_scalar.meta")
    BUILD_CACHE = os.path.join(SUITE_TEST_PATH, "simple_build_cache_template.xml")
    REGISTRY_FILE = os.path.join(SUITE_TEST_PATH, "simple_reg.xml")

    # Copy test CCPP suite into SourceMods directory:
    shutil.copy2(TEST_SDF, TEST_SOURCE_MODS_DIR)
    shutil.copy2(BUILD_CACHE, os.path.join(TEST_SOURCE_MODS_DIR, "simple_build_cache.xml"))
    shutil.copy2(BUILD_CACHE, os.path.join(TEST_SOURCE_MODS_DIR, "bad_simple_build_cache.xml"))
    shutil.copy2(BUILD_CACHE, os.path.join(TEST_SOURCE_MODS_DIR, "bad_simple_build_cache_reg.xml"))
    shutil.copy2(BUILD_CACHE, os.path.join(TEST_SOURCE_MODS_DIR, "bad_simple_build_cache_ccpp.xml"))
    shutil.copy2(REGISTRY_FILE, TEST_SOURCE_MODS_DIR)
    shutil.copy2(TEST_SCHEME, TEST_SOURCE_MODS_DIR)

    REGISTRY_FILE = os.path.join(TEST_SOURCE_MODS_DIR, "simple_reg.xml")
    BUILD_CACHE = os.path.join(TEST_SOURCE_MODS_DIR, "simple_build_cache.xml")
    BAD_BUILD_CACHE = os.path.join(TEST_SOURCE_MODS_DIR, "bad_simple_build_cache.xml")
    BAD_BUILD_CACHE_REG = os.path.join(TEST_SOURCE_MODS_DIR, "bad_simple_build_cache_reg.xml")
    BAD_BUILD_CACHE_CCPP = os.path.join(TEST_SOURCE_MODS_DIR, "bad_simple_build_cache_ccpp.xml")
    TEST_SDF = os.path.join(TEST_SOURCE_MODS_DIR, "suite_simple.xml")
    TEST_SCHEME = os.path.join(TEST_SOURCE_MODS_DIR, "temp_adjust_scalar.meta")

    # Generate test build caches from template:
    f1 = open(BUILD_CACHE, 'rt', encoding='utf-8')
    data = f1.read()
    data = data.replace("TAG1", "").replace("TAG2", "").replace("TAG3", "")
    f1.close()
    f1 = open(BUILD_CACHE, 'w', encoding='utf-8')
    f1.write(data)
    f1.close()

    f1 = open(BAD_BUILD_CACHE, 'rt', encoding='utf-8')
    data = f1.read()
    data = data.replace("TAG1", "<test />").replace("TAG2", "").replace("TAG3", "")
    f1.close()
    f1 = open(BAD_BUILD_CACHE, 'w', encoding='utf-8')
    f1.write(data)
    f1.close()

    f1 = open(BAD_BUILD_CACHE_REG, 'rt', encoding='utf-8')
    data = f1.read()
    data = data.replace("TAG1", "").replace("TAG2", "<test />").replace("TAG3", "")
    f1.close()
    f1 = open(BAD_BUILD_CACHE_REG, 'w', encoding='utf-8')
    f1.write(data)
    f1.close()

    f1 = open(BAD_BUILD_CACHE_CCPP, 'rt', encoding='utf-8')
    data = f1.read()
    data = data.replace("TAG1", "").replace("TAG2", "").replace("TAG3", "<test />")
    f1.close()
    f1 = open(BAD_BUILD_CACHE_CCPP, 'w', encoding='utf-8')
    f1.write(data)
    f1.close()

    BUILD_CACHE_CAM = BuildCacheCAM(BUILD_CACHE)

    # Run doctests:
    OPTIONS = doctest.ELLIPSIS | doctest.NORMALIZE_WHITESPACE
    TEST_SUCCESS = doctest.testmod(optionflags=OPTIONS)[0]

    # Remove testing directories:
    shutil.rmtree(TEST_SOURCE_MODS_DIR)

    # Exit script with error code matching number of failed tests:
    sys.exit(TEST_SUCCESS)

#############
# End of file
