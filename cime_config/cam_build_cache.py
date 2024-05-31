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
from xml.dom import minidom #Used to pretty-print cache file

# Find and include the ccpp-framework scripts directory
# Assume we are in <CAMROOT>/cime_config and CCPP_FRAMEWORK is in <CAMROOT>/ccpp_framework
__CURRDIR = os.path.abspath(os.path.dirname(__file__))
__CAMROOT = os.path.abspath(os.path.join(__CURRDIR, os.pardir))
__CCPP_FRAMEWORK = os.path.join(__CAMROOT, "ccpp_framework", 'scripts')
if __CCPP_FRAMEWORK not in sys.path:
    sys.path.append(__CCPP_FRAMEWORK)
# end if

# CCPP framework imports
# pylint: disable=wrong-import-position
from parse_tools import read_xml_file
# pylint: enable=wrong-import-position

###############################################################################
def new_xml_entry(parent, tag, path, file_hash):
###############################################################################
    """
    Create a new file type entry in the cache

    doctest:
    >>> parent_xml = ET.Element("test")
    >>> test_xml = new_xml_entry(parent_xml, "test", "/no/where", "banana")
    >>> ET.dump(test_xml)
    <test file_path="/no/where" hash="banana" />
    """
    new_entry = ET.SubElement(parent, tag)
    new_entry.set('file_path', path)
    new_entry.set('hash', file_hash)
    return new_entry

###############################################################################
def new_entry_from_xml(item):
###############################################################################
    """Create a new FileStatus entry from XML entry, <item>.
        ValueError is thrown in error.

    doctests:

    1.  Make sure function works as expected:
    >>> parent_xml = ET.Element("test")
    >>> test_xml = new_xml_entry(parent_xml, "test", "/no/where", "banana")
    >>> new_entry_from_xml(test_xml).file_hash
    'banana'

    2.  Make sure function throws an error if no hash entry is found:
    >>> test_xml = ET.Element("test")
    >>> test_xml.set("file_path", "/no/where")
    >>> new_entry_from_xml(test_xml) # doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    ValueError: ERROR: No hash for 'test', '/no/where'

    3.  Make sure function throws an error if no file path entry is found:
    >>> test_xml = ET.Element("test")
    >>> test_xml.set("hash", "banana")
    >>> new_entry_from_xml(test_xml) # doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    ValueError: ERROR: No path for 'test' XML item

    4.  Make sure function throws an error if neither a file path or hash is found:
    >>> test_xml = ET.Element("test")
    >>> new_entry_from_xml(test_xml) # doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    ValueError: ERROR: Invalid 'test' XML item

    """
    path = item.get('file_path', default=None)
    file_hash = item.get('hash', default=None)
    new_entry = None
    if path and file_hash:
        new_entry = FileStatus(path, item.tag, file_hash)
    elif path:
        emsg = f"ERROR: No hash for '{item.tag}', '{path}'"
        raise ValueError(emsg)
    elif file_hash:
        emsg = f"ERROR: No path for '{item.tag}' XML item"
        raise ValueError(emsg)
    else:
        raise ValueError(f"ERROR: Invalid '{item.tag}' XML item")
    # end if
    return new_entry

###############################################################################
def clean_xml_text(item):
###############################################################################
    """Return a 'clean' (stripped) version of <item>.text or an empty
       string if <item>.text is None or not a string-type variable

       doctests

       1. Test that the function works as expected when passed a string.
       >>> test_xml = ET.Element("text")
       >>> test_xml.text = " THIS IS A test "
       >>> clean_xml_text(test_xml)
       'THIS IS A test'

       2. Verify that the function returns an empty string when not passed a string.
       >>> test_xml = ET.Element("text")
       >>> test_xml.text = 2
       >>> clean_xml_text(test_xml)
       ''

    """
    itext = item.text
    iret = ""
    if isinstance(itext, str):
        iret = itext.strip()
    # end if
    return iret

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
            emsg = f"ERROR: '{description}', '{file_path}', does not exist"
            raise ValueError(emsg)
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

        Relevant Stack Overflow post:

        https://stackoverflow.com/questions/22058048/hashing-a-file-in-python/44873382#44873382
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
                        elif item.tag == 'reg_gen_file':
                            self.__reg_gen_files.append(clean_xml_text(item))
                        elif item.tag == 'ic_name_entry':
                            stdname = item.get('standard_name')
                            if stdname not in self.__ic_names:
                                self.__ic_names[stdname] = []
                            # end if
                            itext = clean_xml_text(item)
                            self.__ic_names[stdname].append(itext)
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
                            if isinstance(item.text, str):
                                if item.text:
                                    self.__scheme_nl_metadata.append(item.text.strip())
                                # end if
                            # end if
                        elif item.tag == 'scheme_namelist_groups':
                            group_list = []
                            if isinstance(item.text, str):
                                if item.text:
                                    group_list = [x for x in
                                                  item.text.strip().split(' ') if x]
                                # end if
                            # end if
                            self.__scheme_nl_groups = group_list
                        elif item.tag == 'preproc_defs':
                            self.__preproc_defs = clean_xml_text(item)
                        elif item.tag == 'kind_type':
                            if isinstance(item.text, str):
                                if item.text:
                                    kname, ktype = item.text.strip().split('=')
                                    self.__kind_types[kname.strip()] = ktype.strip()
                                # end if
                            # end if
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
                        dycore, reg_file_list, ic_names):
        """Replace the registry cache data with input data
        """
        self.__dycore = dycore
        self.__gen_reg_file = FileStatus(gen_reg_file, 'generate_registry_file')
        self.__registry_files = {}
        for rfile in registry_source_files:
            new_entry = FileStatus(rfile, 'registry_file')
            self.__registry_files[new_entry.key] = new_entry
        # end for
        # reg_file_list contains the files generated from the registry
        self.__reg_gen_files = reg_file_list
        # ic_names are the initial condition variable names from the registry,
        # and should already be of type dict:
        self.__ic_names = ic_names

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
            scheme_nlmeta = ET.SubElement(ccpp, 'scheme_namelist_meta_file')
            scheme_nlmeta.text = sfile
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
            kind_elem = ET.SubElement(ccpp, 'kind_type')
            kind_elem.text = f"{kind_def}={kind_type}"
        # end for

        #Combine elments into an Element Tree object:
        new_cache_tree = ET.ElementTree(new_cache)

        #Convert Element Tree to a Document Object Model (DOM) XML object:
        dom_xml = minidom.parseString(ET.tostring(new_cache_tree.getroot()))

        #Write XML in a "pretty-print" format:
        with open(self.__build_cache, "w", encoding="UTF-8") as xml_file:
            xml_file.write(dom_xml.toprettyxml(indent="   "))
        #End with

    def registry_mismatch(self, gen_reg_file, registry_source_files,
                          dycore):
        """
        Determine if the registry input data differs from the data
        stored in our cache. Return True if the data differs.
        """
        mismatch = False
        mismatch = (not self.__dycore) or (self.__dycore != dycore)
        if not mismatch:
            mismatch = self.__gen_reg_file.hash_mismatch(gen_reg_file)
        # end if
        # For registry files, we need to make sure we have 1-1 files
        # Note that this method will ignore duplicated files.
        if not mismatch:
            my_reg_keys = set(self.__registry_files.keys())
            test_reg_keys = {FileStatus.gen_key(x)
                             for x in registry_source_files}
            mismatch = my_reg_keys != test_reg_keys
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
            mismatch = my_scheme_keys != test_scheme_keys
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
            mismatch = my_host_keys != test_host_keys
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
            mismatch = my_xml_keys != test_xml_keys
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
        """

        #Initialize variable:
        mismatch = False

        #Check file hash to see if mis-match exists:
        mismatch = self.__gen_init_file.hash_mismatch(gen_init_file)

        #Return mismatch logical:
        return mismatch

    def scheme_nl_metadata(self):
        """Return the stored list of scheme namelist metadata files"""
        return self.__scheme_nl_metadata

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

#############
# End of file
