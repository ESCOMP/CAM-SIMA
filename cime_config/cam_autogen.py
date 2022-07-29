'''
Location of CAM's "generate" routines,
which are used to autogenerate fortran
source code based off of the registry
and physics suites chosen by the user.

To run doctests on this file: python cam_autogen.py
'''

########################################
# Import needed python libraries/modules
########################################

# Python library imports
import sys
import os
import logging
import shutil
import filecmp
import glob

#pylint: disable=wrong-import-position

_CIME_CONFIG_DIR = os.path.dirname(os.path.abspath(__file__))
_CAM_ROOT_DIR = os.path.dirname(_CIME_CONFIG_DIR)
_REG_GEN_DIR = os.path.abspath(os.path.join(_CAM_ROOT_DIR, "src", "data"))
_CCPP_FRAMEWORK_DIR = os.path.join(_CAM_ROOT_DIR, "ccpp_framework", "scripts")
# Add CCPP-framework path to python path:
sys.path.append(_CCPP_FRAMEWORK_DIR)
# Add registry generator path to python path:
sys.path.append(_REG_GEN_DIR)

# Import needed registry and other src/data scripts:
from generate_registry_data import gen_registry
from write_init_files import write_init_files

###############################################################################

class CamAutoGenError(ValueError):
    """Class used to handle CAM config errors
    (e.g., log user errors without backtrace)"""

###############################################################################

# Import needed CCPP-framework scripts:
try:
    from ccpp_capgen import capgen
    from create_readnl_files import gen_namelist_files
    from framework_env import CCPPFrameworkEnv
    #pylint: disable=redefined-outer-name
    # pylint change because of doctest import below
    from metadata_table import find_scheme_names
    #pylint: enable=redefined-outer-name
    from parse_tools import read_xml_file
    ##XXgoldyXX: See note below about when these imports can be removed
    from ccpp_datafile import DatatableReport
    from ccpp_datafile import datatable_report
except ImportError as ierr:
    _EMSG = "ERROR: Cannot find CCPP-framework routines in '{}'\n{}"
    raise CamAutoGenError(_EMSG.format(_CCPP_FRAMEWORK_DIR, ierr))
#pylint: enable=wrong-import-position
# Cleanup python path
sys.path.remove(_CCPP_FRAMEWORK_DIR)
sys.path.remove(_REG_GEN_DIR)

# Acquire python logger:
_LOGGER = logging.getLogger(__name__)

###############################################################################
def _find_file(filename, search_dirs):
###############################################################################
    """
    Find file <filename> in the list of search directories, <search_dirs>.
    Return the first match (full path, match dir) or None, None
    """
    match_file = None
    for sdir in search_dirs:
        test_path = os.path.join(sdir, filename)
        if os.path.exists(test_path):
            match_file = test_path
            break
        # End if
    # End for
    return match_file

###############################################################################
def _update_file(filename, source_path, bld_dir):
###############################################################################
    """
    If <filename> does not exist in <bld_dir>, copy <source_path>
    into <bld_dir>.
    If the file in <source_path> is different than <bld_dir>/<filename>,
    overwrite it with <source_path>.
    Take no action if the file in <source_path> is the same as
    <bld_dir>/<filename>.
    """
    test_path = os.path.join(bld_dir, filename)
    if os.path.exists(test_path):
        if not filecmp.cmp(source_path, test_path, shallow=True):
            os.remove(test_path)
            shutil.copy2(source_path, bld_dir)
        # End if
    else:
        shutil.copy2(source_path, bld_dir)
    # End if

###############################################################################
def _find_scheme_source(source_dirs, metadata_file_name):
###############################################################################
    """
    Given a metadata file name, find the associated Fortran file, and
    an optional associated XML namelist definition file
    by searching through the relavant source code directories.
    Log a warning if no Fortran file exists and return None.
    Return two values, the Fortran file pathname and the XML file
    pathname (if found, otherwise, None).
    """

    # Set fortran extensions:
    fortran_extensions = ['.F90', '.F', '.f', '.f90']

    # Initialize return variable:
    source_file = None
    xml_file = None

    # Loop over fortran extensions:
    for ext in fortran_extensions:

        # Break loop if source file is found:
        if source_file:
            break

        # Generate possible source file name:
        test_file = metadata_file_name + ext

        # Search through all physics source directories,
        # starting with SourceMods:
        for direc in source_dirs:

            # Break loop if source file is found:
            if source_file:
                break

            # Loop over all files in all relevant
            # sub-directories:
            for root, _, files in os.walk(direc):

                # Break loop if source file is found:
                if source_file:
                    break

                for fname in files:
                    # If file name matches what is expected, then
                    # set it as the associated source file name:
                    if fname == test_file:
                        source_file = os.path.join(root, test_file)
                        break
                    # End if
                # End for
            # End for
        # End for
    # End for
    # Look for an associated XML file
    test_file = metadata_file_name + "_namelist.xml"

    # Search through all physics source directories,
    # starting with SourceMods:
    for direc in source_dirs:

        # Break loop if source file is found:
        if xml_file:
            break

        # Loop over all files in all relevant
        # sub-directories:
        for root, _, files in os.walk(direc):

            # Break loop if source file is found:
            if xml_file:
                break

            for fname in files:
                # If file name matches what is expected, then
                # set it as the associated source file name:
                if fname == test_file:
                    xml_file = os.path.join(root, test_file)
                    break
                # End if
            # End for
        # End for

    return source_file, xml_file

###############################################################################
def _find_schemes_in_sdf(suite_part):
###############################################################################
    """
    Parse the suite, <suite_part>, and find all of the scheme names
    called by the suite.
    NB: This function is recursive as schemes may be nested inside other
        suite objects (e.g., group, subcycle)
    """
    scheme_list = [] # Attempt to retain ordering
    for section in suite_part:
        item_type = section.tag.lower()
        if item_type == 'scheme':
            scheme_name = section.text
            if scheme_name and (scheme_name not in scheme_list):
                scheme_list.append(scheme_name)
            # End if
        else:
            for sub_section in section:
                if sub_section.tag.lower() == 'scheme':
                    scheme_name = sub_section.text
                    if scheme_name and (scheme_name not in scheme_list):
                        scheme_list.append(scheme_name)
                    # End if
                else:
                    sub_schemes = _find_schemes_in_sdf(sub_section)
                    for sscheme in sub_schemes:
                        if sscheme not in scheme_list:
                            scheme_list.append(sscheme)
                        # End if
                    # End for
                # End if
            # End for
        # End if
    # End for
    return scheme_list

###############################################################################
def _find_metadata_files(source_dirs, scheme_finder):
###############################################################################
    """
    Find all the metadata files (with associated Fortran source) in
    <source_dirs>. Only include the first file with a given name.
    Return a dictionary with keys of scheme names and values a tuple of the
    metadata file containing that key scheme name, the associated Fortran
    file, and an associated XML namelist definition file (or None if no
    XML file is found).
    <scheme_finder> is a function for finding schemes in a metadata file.

    doctests:

    1.  Check that the function works properly if given the proper inputs:

    >>> _find_metadata_files([TEST_SOURCE_MODS_DIR], find_scheme_names) #doctest: +ELLIPSIS
    {'temp_adjust': ('.../SourceMods/temp_adjust.meta', '.../SourceMods/temp_adjust.F90', None)}

    2.  Check that the function throws the correct error if no Fortran file is found:

    >>> _find_metadata_files([os.path.join(SUITE_TEST_PATH, os.pardir)], \
                             find_scheme_names) #doctest: +ELLIPSIS +NORMALIZE_WHITESPACE
    Traceback (most recent call last):
    ...
    CamAutoGenError: ERROR: No Fortran files were found for the following meta files:
    ...test/unit/sample_files/write_init_files/../phys_types_dup_section.meta
    ...test/unit/sample_files/write_init_files/../phys_types_no_table.meta
    ...test/unit/sample_files/write_init_files/../ref_pres.meta
    ...test/unit/sample_files/write_init_files/../ref_pres_SourceMods.meta
    """

    meta_files = {}
    missing_source_files = []
    bad_xml_sources = []

    for direc in source_dirs:
        for root, _, files in os.walk(direc):
            if '.git' not in root:
                for file in [x for x in files if x[-5:] == '.meta']:
                    if file not in meta_files:
                        path = os.path.join(root, file)
                        # Check for Fortran source
                        base_name = os.path.splitext(file)[0]
                        source_file, xml_file = _find_scheme_source(source_dirs,
                                                                    base_name)
                        if source_file:
                            # Find all the schemes in the file
                            schemes = scheme_finder(path)
                            if (len(schemes) > 1) and xml_file:
                                bad_xml_sources.append(xml_file)
                            # end if
                            for scheme in schemes:
                                meta_files[scheme] = (path, source_file,
                                                      xml_file)
                            # End for
                        else:
                            # Add meta file to list of files
                            # with missing source files:
                            missing_source_files.append(path)
                        # End if
                    # End if
                # End for
            # End if
        # End for
    # End for

    # Raise exception if source files are missing:
    emsg = ""
    if missing_source_files:
        ess = "s" if (len(missing_source_files) > 1) else ""
        emsg += "ERROR: No Fortran files were found for the following "      \
                f"meta file{ess}:\n"
        emsg += "\n".join(sorted(missing_source_files))
    # end if
    if bad_xml_sources:
        if (len(bad_xml_sources) > 1):
            emsg += "ERROR: These XML files were associated with more than " \
                    "one scheme"
        else:
            emsg += "ERROR: This XML file was associated with more than " \
                    "one scheme"
        # end if
        emsg += "\n".join(bad_xml_sources)
    # end if
    if emsg:
        raise CamAutoGenError(emsg)
    # end if

    # Return meta_files dictionary:
    return meta_files

###############################################################################
def _update_genccpp_dir(utility_files, genccpp_dir):
###############################################################################
    """
    Copy any non-generated source code into <genccpp_dir>.
    Non-generated source code is any <utility_files> that are not
      auto-generated.
    Copy is only performed if correct code is not already present in
      <genccpp_dir>.
    XXgoldyXX: This is a temporary fix; this routine should go away when
    all the source code can be compiled (currently, there are several files
    in the source code directory which cannot be compiled by CAM).
    """
    for ufile in utility_files:
        ufdir, uname = os.path.split(ufile)
        if ufdir != genccpp_dir:
            # First, check to see if file exists in <genccpp_dir>
            gfile = os.path.join(genccpp_dir, uname)
            docopy = (not os.path.exists(gfile)) or (not filecmp.cmp(ufile,
                                                                     gfile))
            if docopy:
                _ = shutil.copy2(ufile, gfile)
            # end if
        # end if
    # end for

###############################################################################
def generate_registry(data_search, build_cache, atm_root, bldroot,
                      source_mods_dir, dycore, gen_fort_indent,
                      reg_config=None):
###############################################################################
    """
    Generate the CAM data source and metadata from the registry,
    if required (new case or changes to registry source(s) or script).

    doctests:

    1.  Check that the correct error is raised with a bad search path:
        NOTE:  This must be done first to avoid having the module
        permanently imported by the "successful" test.

    >>> generate_registry(["/bad/path"], TestBuildCache, TEST_ATM_ROOT,       \
                          TEST_BLDROOT, TEST_SOURCE_MODS_DIR, 'se',           \
                          TEST_FORT_INDENT) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    ...
    CamAutoGenError: ERROR: Unable to find CAM registry, registry.xml, in [/bad/path]

    2. Check that generate_registry works properly a good path is given:

    >>> generate_registry(TEST_DATA_SEARCH, TestBuildCache, TEST_ATM_ROOT,    \
                          TEST_BLDROOT, TEST_SOURCE_MODS_DIR, 'se',           \
                          TEST_FORT_INDENT) #doctest: +ELLIPSIS
    ('.../test_bldroot/cam_registry', False, [], {})
    """

    # Find the registry file. Try SourceMods first.
    registry_file = _find_file("registry.xml", data_search)
    if not registry_file:
        emsg = "ERROR: Unable to find CAM registry, registry.xml, in [{}]"
        raise CamAutoGenError(emsg.format(', '.join(data_search)))
    # end if
    registry_files = [registry_file]
    genreg_dir = os.path.join(bldroot, "cam_registry")
    # Create empty registry file objects list:
    reg_files_list = []
    # Figure out if we need to generate new data source and metadata files
    gen_reg_file = os.path.join(_REG_GEN_DIR, "generate_registry_data.py")
    if os.path.exists(genreg_dir):
        do_gen_registry = build_cache.registry_mismatch(gen_reg_file,
                                                        registry_files,
                                                        dycore, reg_config)
    else:
        os.makedirs(genreg_dir)
        do_gen_registry = True
    # End if
    if do_gen_registry:
        for reg_file in registry_files:
            retvals = gen_registry(reg_file, dycore, reg_config, genreg_dir,
                                   gen_fort_indent, source_mods_dir, atm_root,
                                   logger=_LOGGER, schema_paths=data_search,
                                   error_on_no_validate=True)
            retcode, reg_file_list, ic_names = retvals
            # Raise error if gen_registry failed:
            if retcode != 0:
                emsg = "ERROR:Unable to generate CAM data structures from {}, err = {}"
                raise CamAutoGenError(emsg.format(reg_file, retcode))
            # end if

            # Add files to list:
            reg_files_list += reg_file_list
        # End for

        # Save build details in the build cache
        reg_file_paths = [x.file_path for x in reg_file_list if x.file_path]
        build_cache.update_registry(gen_reg_file, registry_files, dycore,
                                    reg_config, reg_file_paths, ic_names)
    else:
        # If we did not run the registry generator, retrieve info from cache
        reg_file_paths = build_cache.reg_file_list()
        ic_names = build_cache.ic_names()
    # End if

    return genreg_dir, do_gen_registry, reg_file_paths, ic_names

###############################################################################
def generate_physics_suites(build_cache, preproc_defs, host_name,
                            phys_suites_str, atm_root, bldroot,
                            reg_dir, reg_files, source_mods_dir, force):
###############################################################################
    """
    Generate the source for the configured physics suites,
    if required (new case or changes to suite source(s) or metadata).
    Return several values:
       - A list of directories with generated code (to be added to Filepath)
       - A flag set to True if the CCPP framework was run
       - The pathname of the CCPP Framework database
       - A list of CCPP namelist groups to add to atm_in

    doctests:

    1.  Check that the correct error is raised when a physics suite SDF cannot be found:

    >>> generate_physics_suites(TestBuildCache, "UNSET", "cam", "missing",    \
                                TEST_ATM_ROOT, TEST_BLDROOT, TEST_REG_DIR,    \
                                TEST_REGFILES, TEST_SOURCE_MODS_DIR,          \
                                False) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    CamAutoGenError: ERROR: Unable to find SDF for suite 'missing'

    2.  Check that the correct error is raised when a scheme's metadata file cannot be found:

    >>> generate_physics_suites(TestBuildCache, "UNSET", "cam", "bad",        \
                                TEST_ATM_ROOT, TEST_BLDROOT, TEST_REG_DIR,    \
                                TEST_REGFILES, TEST_SOURCE_MODS_DIR,          \
                                False) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    CamAutoGenError: ERROR: No metadata file found for physics scheme 'bad_scheme'

    3. Check that generate_physics_suites works properly when good inputs are provided:

    >>> generate_physics_suites(TestBuildCache, "UNSET", "cam", "simple",     \
                                TEST_ATM_ROOT, TEST_BLDROOT, TEST_REG_DIR,    \
                                TEST_REGFILES, TEST_SOURCE_MODS_DIR,          \
                                False) #doctest: +ELLIPSIS
    (['.../test_bldroot/ccpp_physics', '.../test_bldroot/ccpp'], False, '.../test_bldroot/ccpp/ccpp_datatable.xml', dict_values([]), None)
    """

    # Physics source gets copied into blddir
    physics_blddir = os.path.join(bldroot, "ccpp_physics")
    if not os.path.exists(physics_blddir):
        os.makedirs(physics_blddir)
    # End if
    # Collect all source directories
    source_search = [source_mods_dir,
                     os.path.join(atm_root, "src", "physics", "ncar_ccpp")]
    # Find all metadata files, organize by scheme name
    all_scheme_files = _find_metadata_files(source_search, find_scheme_names)

    # Find the SDFs specified for this model build
    sdfs = []
    scheme_files = []
    xml_files = {} # key is scheme, value is xml file path
    for sdf in phys_suites_str.split(';'):
        sdf_path = _find_file(f"suite_{sdf}.xml", source_search)
        if not sdf_path:
            emsg = "ERROR: Unable to find SDF for suite '{}'"
            raise CamAutoGenError(emsg.format(sdf))
        # End if
        sdfs.append(sdf_path)
        # Given an SDF, find all the schemes it calls
        _, suite = read_xml_file(sdf_path)
        sdf_schemes = _find_schemes_in_sdf(suite)
        # For each scheme, find its metadata file
        for scheme in sdf_schemes:
            if scheme in all_scheme_files:
                scheme_file = all_scheme_files[scheme][0]
                if scheme_file not in scheme_files:
                    scheme_files.append(scheme_file)
                    scheme_src = all_scheme_files[scheme][1]
                    _update_file(os.path.basename(scheme_src),
                                 scheme_src, physics_blddir)
                # End if (else, it is already in the list)
                if all_scheme_files[scheme][2]:
                    xml_files[scheme] = all_scheme_files[scheme][2]
                # end if (else this scheme does not have a namelist def file)
            else:
                emsg = "ERROR: No metadata file found for physics scheme '{}'"
                raise CamAutoGenError(emsg.format(scheme))
            # End if
        # End for
    # End for
    # Figure out if we need to generate new physics code
    genccpp_dir = os.path.join(bldroot, "ccpp")
    kind_phys = ['kind_phys = REAL64']
    kind_types = kind_phys

    # Set location of CCPP "capfiles.txt" file:
    cap_output_file = os.path.join(genccpp_dir, "ccpp_datatable.xml")

    # reg_dir needs to be first as the DDTs are defined there.
    host_files = glob.glob(os.path.join(reg_dir, "*.meta"))

    # Add in files generated from the registry:
    host_files.extend(reg_files)

    # Convert preproc defs to string:
    if preproc_defs:
        preproc_cache_str = ', '.join(preproc_defs)
    else:
        preproc_cache_str = 'UNSET'
    # end if

    # Initialize namelist generation logical:
    do_gen_nl = False

    if os.path.exists(genccpp_dir):
        do_gen_ccpp = force or build_cache.ccpp_mismatch(sdfs, scheme_files,
                                                         host_files,
                                                         preproc_cache_str,
                                                         kind_phys)
    else:
        os.makedirs(genccpp_dir)
        do_gen_ccpp = True
        do_gen_nl   = True
    # End if

    create_nl_file = os.path.join(_CIME_CONFIG_DIR, "create_readnl_files.py")
    if not do_gen_nl:
        do_gen_nl = force or build_cache.xml_nl_mismatch(create_nl_file,
                                                         xml_files)
    # end if
    if do_gen_nl:
        args = []
        for scheme, xml_file in xml_files.items():
            args.extend(["--namelist-file-arg",
                         f"{scheme}:{xml_file}"])
        # end for
        args.append("--namelist-read-mod")
        args.append("cam_ccpp_scheme_namelists")
        args.append("--namelist-read-subname")
        args.append("cam_read_ccpp_scheme_namelists")
        # print extra info to bldlog if DEBUG is TRUE
        _LOGGER.debug("Calling gen_namelist_files with schemes: "      \
                      f"{', '.join(xml_files.keys())}")
        namelist_obj = gen_namelist_files(args, genccpp_dir, _LOGGER)
        # Save generated namelist groups
        nl_groups = namelist_obj.groups()
        # include generated metadata files
        scheme_nl_meta_files = namelist_obj.meta_files()
    else:
        # Not running namelist generator, collect metadata files and
        #   namelist group names from the build cache
        nl_groups = build_cache.scheme_nl_groups()
        scheme_nl_meta_files = build_cache.scheme_nl_metadata()
    # end if
    if scheme_nl_meta_files:
        do_gen_ccpp = True
        host_files.extend(scheme_nl_meta_files)
    # end if (no else)

    if do_gen_ccpp:
        gen_docfiles = False
        use_error_obj = False

        # print extra info to bldlog if DEBUG is TRUE
        _LOGGER.debug("Calling capgen: ")
        _LOGGER.debug("   host files: %s", ", ".join(host_files))
        _LOGGER.debug("   scheme files: %s", ', '.join(scheme_files))
        _LOGGER.debug("   suite definition files: %s", ', '.join(sdfs))
        _LOGGER.debug("   preproc defs: %s", preproc_cache_str)
        _LOGGER.debug("   output directory: '%s'", genccpp_dir)
        _LOGGER.debug("   kind definitions:")
        for kind_type in kind_types:
            name, ktype = [x.strip() for x in kind_type.split('=')]
            _LOGGER.debug("   %s: '%s'", name, ktype)
        # end for

        # generate CCPP caps
        run_env = CCPPFrameworkEnv(_LOGGER, host_files=host_files,
                                   scheme_files=scheme_files, suites=sdfs,
                                   preproc_directives=preproc_defs,
                                   generate_docfiles=gen_docfiles,
                                   host_name=host_name, kind_types=kind_types,
                                   use_error_obj=use_error_obj,
                                   force_overwrite=False,
                                   output_root=genccpp_dir,
                                   ccpp_datafile=cap_output_file)
        capgen_db = capgen(run_env, return_db=True)
    else:
        capgen_db = None
    # end if

    if do_gen_ccpp or do_gen_nl:
        # save build details in the build cache
        build_cache.update_ccpp(sdfs, scheme_files, host_files, xml_files,
                                scheme_nl_meta_files, nl_groups, create_nl_file,
                                preproc_cache_str, kind_types)
        ##XXgoldyXX: v Temporary fix: Copy CCPP Framework source code into
        ##XXgoldyXX: v   generated code directory
        request = DatatableReport("utility_files")
        ufiles_str = datatable_report(cap_output_file, request, ";")
        utility_files = ufiles_str.split(';')
        _update_genccpp_dir(utility_files, genccpp_dir)
        ##XXgoldyXX: ^ Temporary fix:
    # End if

    return [physics_blddir, genccpp_dir], do_gen_ccpp, cap_output_file,       \
        xml_files.values(), capgen_db

###############################################################################
def generate_init_routines(build_cache, bldroot, force_ccpp, force_init,
                           source_mods_dir, gen_fort_indent,
                           cap_database, ic_names):
###############################################################################
    """
    Generate the host model initialization source code files
    (phys_vars_init_check.F90 and physics_inputs.F90) using
    both the registry and the CCPP physics suites if required
    (new case or changes to registry or CCPP source(s), meta-data,
    and/or script).

    doctests:

    1. Check that generate_init_routines works properly when good inputs are given:

    >>> generate_init_routines(TestBuildCache, TEST_BLDROOT, False, False,    \
                               TEST_SOURCE_MODS_DIR, TEST_FORT_INDENT, None,  \
                               {}) #doctest: +ELLIPSIS
    '.../test_bldroot/phys_init'
    """

    #Add new directory to build path:
    init_dir = os.path.join(bldroot, "phys_init")
    # Use this for cache check
    gen_init_file = os.path.join(_REG_GEN_DIR, "write_init_files.py")

    # Figure out if we need to generate new initialization routines:
    if os.path.exists(init_dir):
        # Check if registry and / or CCPP suites were modified:
        if force_ccpp or force_init:
            do_gen_init = True
        else:
            #If not, then check cache to see if actual
            #"write_init_files.py" was modified:
            do_gen_init = build_cache.init_write_mismatch(gen_init_file)
    else:
        #If no directory exists, then one will need
        # to create new routines:
        os.mkdir(init_dir)
        do_gen_init = True
    # End if

    if do_gen_init:

        # Run initialization files generator:
        # Yes, we are passing a pointer to the find_file function for use
        #   within write_init_files (so that write_init_files can be the place
        #   where the source include files are stored).
        source_paths = [source_mods_dir, _REG_GEN_DIR]
        retmsg = write_init_files(cap_database, ic_names, init_dir,
                                  _find_file, source_paths,
                                  gen_fort_indent, _LOGGER)

        #Check that script ran properly:
        #-----
        if retmsg:
            emsg = "ERROR: Unable to generate CAM init source code, error message is:\n{}"
            raise CamAutoGenError(emsg.format(retmsg))
        #-----

        # save build details in the build cache
        build_cache.update_init_gen(gen_init_file)
    # End if

    return init_dir

###############################################################################
# IGNORE EVERYTHING BELOW HERE UNLESS RUNNING TESTS ON CAM_AUTOGEN!
###############################################################################

# Call testing routine, if script is run directly
if __name__ == "__main__":

    # Import modules needed for testing:
    import doctest
    import inspect

    # Create fake buildcache object for testing:
    #++++++++++++++++++++++++++++++++++++++++++
    #pylint: disable=missing-function-docstring
    #pylint: disable=unused-argument
    #pylint: disable=no-self-use
    class FakeBuildCache:

        """
        Fake BuildCache class with methods
        needed to test the above "generate"
        functions.
        """

        def __init__(self):
            pass

        def update_registry(self, gen_reg_file, registry_files,
                            dycore, reg_config):
            pass

        def reg_file_list(self):
            return []

        def registry_mismatch(self, gen_reg_file, registry_files,
                              dycore, reg_config):

            # Always return False, in order to avoid running the
            # actual generation routines when performing doctests:
            return False

        def update_ccpp(self, sdfs, scheme_files, xml_files,
                        preproc_defs, kind_phys):
            pass

        def ccpp_mismatch(self, sdfs, scheme_files, host_files,
                          preproc_defs, kind_phys):

            # Always return False, in order to avoid running the
            # actual generation routines when performing doctests:
            return False

        def xml_nl_mismatch(self, create_nl_file, xml_files):
            # Always return False, in order to avoid running the
            # actual generation routines when performing doctests:
            return False

        def update_init_gen(self, input_file):
            pass

        def init_write_mismatch(self, input_file):

            # Always return False, in order to avoid running the
            # actual generation routines when performing doctests:
            return False

        def scheme_nl_metadata(self):
            return []

        def scheme_nl_groups(self):
            return []

        def ic_names(self):
            return {}

    #pylint: enable=missing-function-docstring
    #pylint: enable=unused-argument
    #pylint: enable=no-self-use
    #++++++++++++++++++++++++++++++++++++++++++

    # Create new, fake BuildCache object:
    #pylint: disable=invalid-name
    TestBuildCache = FakeBuildCache()
    #pylint: enable=invalid-name

    # Determine current working directory:
    TEST_AUTO_DIR = os.path.dirname(os.path.abspath(__file__))
    TEST_ATM_ROOT = os.path.abspath(os.path.join(TEST_AUTO_DIR, os.pardir))

    # Create variables for testing:
    TEST_DATA_SEARCH = [os.path.join(TEST_ATM_ROOT, "src", "data")]
    TEST_CCPP_PATH = os.path.join(TEST_ATM_ROOT, "ccpp_framework", "scripts")
    TEST_BLDROOT = os.path.join(TEST_ATM_ROOT, "test_bldroot")
    TEST_SOURCE_MODS_DIR = os.path.join(TEST_ATM_ROOT, "SourceMods")
    TEST_FORT_INDENT = 3

    # Remove old test directories if they exist:
    if os.path.exists(TEST_BLDROOT):
        shutil.rmtree(TEST_BLDROOT)

    if os.path.exists(TEST_SOURCE_MODS_DIR):
        shutil.rmtree(TEST_SOURCE_MODS_DIR)

    # For generate_physics_suites:
    TEST_REG_DIR = os.path.join(TEST_BLDROOT, "cam_registry")

    # For generate_init_routines:
    TEST_REGFILES = []
    TEST_CAP_DATAFILE = os.path.join("test_bldroot", "ccpp", "capfiles.txt")

    # Create testing buildroot directory:
    os.mkdir(TEST_BLDROOT)

    # Create "SourceMods directory:
    os.mkdir(TEST_SOURCE_MODS_DIR)

    # Create source code directories needed in order
    # to avoid running the actual code generators
    # while testing:
    os.mkdir(os.path.join(TEST_BLDROOT, "cam_registry"))
    os.mkdir(os.path.join(TEST_BLDROOT, "ccpp"))
    os.mkdir(os.path.join(TEST_BLDROOT, "phys_init"))

    # Set test CCPP suite paths:
    SUITE_TEST_PATH = os.path.join(TEST_ATM_ROOT, "test", "unit", "sample_files",
                                   "write_init_files")
    TEST_SDF = os.path.join(SUITE_TEST_PATH, "simple_suite.xml")
    BAD_SDF = os.path.join(SUITE_TEST_PATH, "suite_bad.xml")
    TEST_META = os.path.join(SUITE_TEST_PATH, "temp_adjust.meta")
    TEST_F90 = os.path.join(SUITE_TEST_PATH, "temp_adjust.F90")

    # Copy test CCPP suite into SourceMods directory:
    shutil.copy2(TEST_SDF, os.path.join(TEST_SOURCE_MODS_DIR, "suite_simple.xml"))
    shutil.copy2(BAD_SDF, TEST_SOURCE_MODS_DIR)
    shutil.copy2(TEST_META, TEST_SOURCE_MODS_DIR)
    shutil.copy2(TEST_F90, TEST_SOURCE_MODS_DIR)

    # Set logger to fatal, to avoid log messages:
    _LOGGER.setLevel(logging.FATAL)

    # Create doctest parsing object:
    DOCPARSE = doctest.DocTestParser()

    # Create doctest runner object:
    DOC_RUNNER = doctest.DocTestRunner()

    # Parse doc_tests:
    GEN_PHYS_TESTS = DOCPARSE.get_doctest(inspect.getdoc(generate_physics_suites), globals(), generate_physics_suites, None, None)
    GEN_REG_TESTS  = DOCPARSE.get_doctest(inspect.getdoc(generate_registry), globals(), generate_registry, None, None)
    GEN_INIT_TESTS = DOCPARSE.get_doctest(inspect.getdoc(generate_init_routines), globals(), generate_init_routines, None, None)

    #Note:  Due to a bug in the "summarize" command, as soon as a doctest failure occurs
    #       it should not be called again.

    # Run doctests in a specific order (to avoid import issues):

    #generate_physics_suites:
    DOC_RUNNER.run(GEN_PHYS_TESTS)
    NUM_FAILS, _ = DOC_RUNNER.summarize()

    #generate_registry:
    DOC_RUNNER.run(GEN_REG_TESTS)
    if NUM_FAILS == 0:
        NUM_FAILS, _ = DOC_RUNNER.summarize()

    #generate_init_routines:
    DOC_RUNNER.run(GEN_INIT_TESTS)
    if NUM_FAILS == 0:
        NUM_FAILS, _ = DOC_RUNNER.summarize()

    # Run additional doctests:

    #find_metadata_files:
    METADATA_TESTS = DOCPARSE.get_doctest(inspect.getdoc(_find_metadata_files), globals(), _find_metadata_files, None, None)
    DOC_RUNNER.run(METADATA_TESTS)
    if NUM_FAILS == 0:
        NUM_FAILS, _ = DOC_RUNNER.summarize()

    # Remove testing directories:
    shutil.rmtree(TEST_BLDROOT)
    shutil.rmtree(TEST_SOURCE_MODS_DIR)

    # Exit script with number of failures as the error code:
    sys.exit(NUM_FAILS)

#############
# End of file
#############
