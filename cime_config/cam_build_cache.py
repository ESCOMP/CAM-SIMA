"""
Class, methods, and supporting functions to track whether either of CAM's
specialized pre-processing file generation routines (CAM registry and CCPP)
need to be run as part of a current build.
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
        emsg = "ERROR: No hash for {}, '{}'"
        raise ValueError(item.tag, path)
    elif file_hash:
        emsg = "ERROR: No path for {} XML item"
        raise ValueError(emsg.format(item.tag))
    else:
        raise ValueError("ERROR: Invalid {} XML item".format(item.tag))
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
    """

    def __init__(self, build_cache):
        """Initialize all the build state from a build cache file.
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
        self.__preproc_defs = None
        self.__kind_phys = None
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
                        elif item.tag == 'preproc_defs':
                            self.__preproc_defs = item.text
                        elif item.tag == 'kind_phys':
                            self.__kind_phys = item.text
                        else:
                            emsg = "ERROR: Unknown CCPP tag, '{}'"
                            raise ValueError(emsg.format(item.tag))
                        # end if
                else:
                    raise ValueError(emsg)
                # end if
            # end for
        # end if (no else, we just have an empty object)

    def update_registry(self, gen_reg_file, registry_source_files,
                        dycore, config):
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

    def update_ccpp(self, suite_definition_files, scheme_files,
                    preproc_defs, kind_phys):
        """Replace the ccpp cache data with input data
        """
        self.__preproc_defs = preproc_defs
        self.__kind_phys = kind_phys
        self.__sdfs = {}
        for sfile in suite_definition_files:
            new_entry = FileStatus(sfile, 'SDF')
            self.__sdfs[new_entry.key] = new_entry
        # end for
        self.__schemes = {}
        for sfile in scheme_files:
            new_entry = FileStatus(sfile, 'scheme')
            self.__sdfs[new_entry.key] = new_entry
        # end for

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
        # CCPP
        ccpp = ET.SubElement(new_cache, 'CCPP')
        for sfile in self.__sdfs.values():
            new_xml_entry(ccpp, 'SDF', sfile.file_path, sfile.file_hash)
        # end for
        for sfile in self.__schemes.values():
            new_xml_entry(ccpp, 'scheme', sfile.file_path, sfile.file_hash)
        # end for
        preproc = ET.SubElement(ccpp, 'preproc_defs')
        preproc.text = self.__preproc_defs
        kind_phys = ET.SubElement(ccpp, 'kind_phys')
        kind_phys.text = self.__kind_phys
        new_cache_tree = ET.ElementTree(new_cache)
        new_cache_tree.write(self.__build_cache)

    def registry_mismatch(self, gen_reg_file, registry_source_files,
                          dycore, config):
        """Determine if the registry input data differs from the data
        stored in our cache. Return True if the data differs."""
        mismatch = False
        mismatch = (not self.__dycore) or (self.__dycore != dycore)
        mismatch = mismatch or (not self.__config) or (self.__config != config)
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

    def ccpp_mismatch(self, sdfs, scheme_files, preproc_defs, kind_phys):
        """Determine if the CCPP input data differs from the data stored in
        our cache. Return True if the data differs."""
        mismatch = False
        mismatch = ((not self.__preproc_defs) or
                    (self.__preproc_defs != preproc_defs))
        mismatch = (mismatch or
                    (not self.__kind_phys) or (self.__kind_phys != kind_phys))
        # For SDFs, we need to make sure we have 1-1 files
        # Note that this method will ignore duplicated files.
        if not mismatch:
            my_sdf_keys = set(self.__sdfs.keys())
            test_sdf_keys = {FileStatus.gen_key(x) for x in sdfs}
            mismatch = (my_sdf_keys != test_sdf_keys)
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
        return mismatch

    def init_write_mismatch(self, gen_init_file):
        """Determine if the init_files writer (write_init_files.py)
            differs from the data stored in our cache. Return True
            if the data differs."""

        #Initialize variable:
        mismatch = False

        #Check file hash to see if mis-match exists:
        mismatch = self.__gen_init_file.hash_mismatch(gen_init_file)

        #Return mismatch logical:
        return mismatch
