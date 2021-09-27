'''
Location of CAM's "generate" routines,
which are used to autogenerate fortran
source code based off of the registry
and physics suites chosen by the user.
'''

########################################
# Import needed python libraries/modules
########################################

import sys
import os
import logging
import shutil
import filecmp
import glob

# Acquire python logger:
_LOGGER = logging.getLogger(__name__)

###############################################################################

class CamAutoGenError(ValueError):
    """Class used to handle CAM config errors
    (e.g., log user errors without backtrace)"""

###############################################################################
def _find_file(filename, search_dirs):
###############################################################################
    """
    Find file <filename> in the list of search directories, <search_dirs>.
    Return the first match (full path, match dir) or None, None
    """
    match_file = None
    match_path = None
    for sdir in search_dirs:
        test_path = os.path.join(sdir, filename)
        if os.path.exists(test_path):
            match_path = sdir
            match_file = test_path
            break
        # End if
    # End for
    return match_file, match_path

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
    Given a metadata file name, find the associated Fortran file
    by searching through the relavant source code directories.
    Log a warning if no Fortran file exists and return None.
    """

    # Set fortran extensions:
    fortran_extensions = ['.F90', '.F', '.f', '.f90']

    # Initialize return variable:
    source_file = None

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

    return source_file

###############################################################################
def _find_schemes_in_sdf(suite_part):
###############################################################################
    """
    Parse the suite, <suite_part>, and find all of the scheme names
    called by the suite.
    NB: This function is recursive as schemes may be nested inside other
        suite objects (e.g., group, subcycle)
    """
    scheme_list = list() # Attempt to retain ordering
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
    metadata file containing that key scheme name and the associated Fortran
    file.
    <scheme_finder> is a function for finding schemes in a metadata file.

    doctests:

    1.  Check that the function works properly if given the proper inputs:

    >>> _find_metadata_files([TEST_SOURCE_MODS_DIR], find_scheme_names) #doctest: +ELLIPSIS
    {'temp_adjust': ('.../SourceMods/temp_adjust.meta', '.../SourceMods/temp_adjust.F90')}

    2.  Check that the function throws the correct error if no fortran file is found:

    >>> _find_metadata_files([os.path.join(SUITE_TEST_PATH, os.pardir)], \
                             find_scheme_names) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamAutoGenError: ERROR: No fortran files were found for the following meta files:
    /glade/work/nusbaume/SE_projects/new_cam_sandbox/CAMDEN/test/unit/sample_files/write_init_files/../ref_pres.meta
    /glade/work/nusbaume/SE_projects/new_cam_sandbox/CAMDEN/test/unit/sample_files/write_init_files/../ref_pres_SourceMods.meta
    """

    meta_files = dict()
    missing_source_files = list()

    for direc in source_dirs:
        for root, _, files in os.walk(direc):
            if '.git' not in root:
                for file in [x for x in files if x[-5:] == '.meta']:
                    if file not in meta_files:
                        path = os.path.join(root, file)
                        # Check for Fortran source
                        base_name = os.path.splitext(file)[0]
                        source_file = _find_scheme_source(source_dirs, base_name)
                        if source_file:
                            # Find all the schemes in the file
                            schemes = scheme_finder(path)
                            for scheme in schemes:
                                meta_files[scheme] = (path, source_file)
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
    if missing_source_files:
        emsg = "ERROR: No fortran files were found for the following meta files:\n"
        emsg += "\n".join(missing_source_files)
        raise CamAutoGenError(emsg)

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

    >>> generate_registry(["/bad/path"], TestBuildCache, TEST_ATM_ROOT, \
                          TEST_BLDROOT, TEST_SOURCE_MODS_DIR, 'se', \
                          TEST_FORT_INDENT) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamAutoGenError: "ERROR: Cannot find generate_registry_data in '['/bad/path']'


    2. Check that generate_registry works properly a good path is given:

    >>> generate_registry(TEST_DATA_SEARCH, TestBuildCache, TEST_ATM_ROOT, \
                          TEST_BLDROOT, TEST_SOURCE_MODS_DIR, 'se', \
                          TEST_FORT_INDENT) #doctest: +ELLIPSIS
    ('.../test_bldroot/cam_registry', False, [])
    """
    #pylint: disable=wrong-import-position
    #pylint: disable=import-outside-toplevel

    # Search for registry generator file and path:
    gen_reg_file, gen_reg_path = _find_file("generate_registry_data.py",
                                            data_search)

    # Add registry generator path to python path:
    sys.path.append(gen_reg_path)

    # Import needed registry scripts:
    try:
        from generate_registry_data import gen_registry
    except ImportError as ierr:
        emsg = "ERROR: Cannot find generate_registry_data in '{}'\n{}"
        raise CamAutoGenError(emsg.format(data_search, ierr))
    #pylint: enable=wrong-import-position
    #pylint: enable=import-outside-toplevel

    # Find the registry file, registry schema, and generation routine
    # Try SourceMods first for each one.
    registry_file, _ = _find_file("registry.xml", data_search)
    registry_files = [registry_file]
    genreg_dir = os.path.join(bldroot, "cam_registry")
    # Create empty registry file objects list:
    reg_files_list = list()
    # Figure out if we need to generate new data source and metadata files
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
            retcode, reg_file_list = gen_registry(reg_file, dycore, reg_config,
                                                  genreg_dir, gen_fort_indent,
                                                  source_mods_dir, atm_root,
                                                  logger=_LOGGER,
                                                  schema_paths=data_search,
                                                  error_on_no_validate=True)
            # Raise error if gen_registry failed:
            if retcode != 0:
                emsg = "ERROR:Unable to generate CAM data structures from {}, err = {}"
                raise CamAutoGenError(emsg.format(reg_file, retcode))

            # Add files to list:
            reg_files_list += reg_file_list
        # End for

        # Save build details in the build cache
        build_cache.update_registry(gen_reg_file, registry_files,
                                    dycore, reg_config)
    # End if

    # Remove registry generator location from python path:
    sys.path.remove(gen_reg_path)

    return genreg_dir, do_gen_registry, reg_files_list

###############################################################################
def generate_physics_suites(ccpp_scripts_path, build_cache, preproc_defs, host_name,
                            phys_suites_str, atm_root, bldroot, reg_dir, reg_files,
                            source_mods_dir, force):
###############################################################################
    """
    Generate the source for the configured physics suites,
    if required (new case or changes to suite source(s) or metadata).

    doctests:

    1.  Check that the correct error is raised with a bad CCPP scripts path:
        NOTE:  This must be done first to avoid having the module
        permanently imported by the "successful" test.

    >>> generate_physics_suites("/bad/path", TestBuildCache, "UNSET", \
                                "cam", "simple", TEST_ATM_ROOT, \
                                TEST_BLDROOT, TEST_REG_DIR, TEST_REGFILES, \
                                TEST_SOURCE_MODS_DIR, False) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamAutoGenError: ERROR: Cannot find CCPP-framework routines in '/bad/path'

    2.  Check that the correct error is raised when a physics suite SDF cannot be found:

    >>> generate_physics_suites(TEST_CCPP_PATH, TestBuildCache, "UNSET", \
                                "cam", "missing", TEST_ATM_ROOT, \
                                TEST_BLDROOT, TEST_REG_DIR, TEST_REGFILES, \
                                TEST_SOURCE_MODS_DIR, False)
    Traceback (most recent call last):
    CamAutoGenError: ERROR: Unable to find SDF for suite 'missing'

    3.  Check that the correct error is raised when an SDF's metadata file cannot be found:

    >>> generate_physics_suites(TEST_CCPP_PATH, TestBuildCache, "UNSET", \
                                "cam", "bad", TEST_ATM_ROOT, \
                                TEST_BLDROOT, TEST_REG_DIR, TEST_REGFILES, \
                                TEST_SOURCE_MODS_DIR, False)
    Traceback (most recent call last):
    CamAutoGenError: ERROR: No metadata file found for physics scheme 'bad_scheme'

    4. Check that generate_physics_suites works properly when good inputs are provided:

    >>> generate_physics_suites(TEST_CCPP_PATH, TestBuildCache, "UNSET", \
                                "cam", "simple", TEST_ATM_ROOT, \
                                TEST_BLDROOT, TEST_REG_DIR, TEST_REGFILES, \
                                TEST_SOURCE_MODS_DIR, False) #doctest: +ELLIPSIS
    (['.../test_bldroot/ccpp_physics', '.../test_bldroot/ccpp'], False, '.../test_bldroot/ccpp/ccpp_datatable.xml')
    """
    #pylint: disable=wrong-import-position
    #pylint: disable=import-outside-toplevel

    # Add CCPP-framework path to python path:
    sys.path.append(ccpp_scripts_path)

    # Import needed CCPP-framework scripts:
    try:
        from ccpp_capgen import capgen
        #pylint: disable=redefined-outer-name
        # pylint change because of doctest import below
        from metadata_table import find_scheme_names
        #pylint: enable=redefined-outer-name
        from parse_tools import read_xml_file
        ##XXgoldyXX: See note below about when these imports can be removed
        from ccpp_datafile import DatatableReport
        from ccpp_datafile import datatable_report
    except ImportError as ierr:
        emsg = "ERROR: Cannot find CCPP-framework routines in '{}'\n{}"
        raise CamAutoGenError(emsg.format(ccpp_scripts_path, ierr))
    #pylint: enable=wrong-import-position
    #pylint: enable=import-outside-toplevel

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
    # Find the SDFs
    sdfs = list()
    scheme_files = list()
    for sdf in phys_suites_str.split(';'):
        sdf_path, _ = _find_file("suite_{}.xml".format(sdf), source_search)
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
            else:
                emsg = "ERROR: No metadata file found for physics scheme '{}'"
                raise CamAutoGenError(emsg.format(scheme))
            # End if
        # End for
    # End for
    # Figure out if we need to generate new physics code
    genccpp_dir = os.path.join(bldroot, "ccpp")
    kind_phys = 'REAL64'

    # Set location of CCPP "capfiles.txt" file:
    cap_output_file = os.path.join(genccpp_dir, "ccpp_datatable.xml")

    # reg_dir needs to be first as the DDTs are defined there.
    host_files = glob.glob(os.path.join(reg_dir, "*.meta"))

    # Loop over all registered files:
    for reg_file in reg_files:
        if reg_file.file_path:
            #If the file path is provided, then add it to
            #the host model files list for use by CCPP's capgen:
            host_files.append(reg_file.file_path)

    # Convert preproc defs to string:
    if preproc_defs:
        preproc_cache_str = ', '.join(preproc_defs)
    else:
        preproc_cache_str = 'UNSET'

    if os.path.exists(genccpp_dir):
        do_gen_ccpp = force or build_cache.ccpp_mismatch(sdfs, scheme_files,
                                                         preproc_cache_str,
                                                         kind_phys)
    else:
        os.makedirs(genccpp_dir)
        do_gen_ccpp = True
    # End if
    if do_gen_ccpp:
        gen_hostcap = True
        gen_docfiles = False

        # print extra info to bldlog if DEBUG is TRUE
        _LOGGER.debug("Calling capgen: ")
        _LOGGER.debug("   host files: %s", ", ".join(host_files))
        _LOGGER.debug("   scheme files: %s", ', '.join(scheme_files))
        _LOGGER.debug("   suite definition files: %s", ', '.join(sdfs))
        _LOGGER.debug("   preproc defs: %s", preproc_cache_str)
        _LOGGER.debug("   output directory: '%s'", genccpp_dir)
        _LOGGER.debug("   kind_phys: '%s'", kind_phys)

        # generate CCPP caps
        force_overwrite = False
        capgen(host_files, scheme_files, sdfs, cap_output_file,
               preproc_cache_str, gen_hostcap, gen_docfiles, genccpp_dir,
               host_name, kind_phys, force_overwrite, _LOGGER)

        # save build details in the build cache
        build_cache.update_ccpp(sdfs, scheme_files, preproc_cache_str, kind_phys)
        ##XXgoldyXX: v Temporary fix: Copy CCPP Framework source code into
        ##XXgoldyXX: v   generated code directory
        request = DatatableReport("utility_files")
        ufiles_str = datatable_report(cap_output_file, request, ";")
        utility_files = ufiles_str.split(';')
        _update_genccpp_dir(utility_files, genccpp_dir)
        ##XXgoldyXX: ^ Temporary fix:
    # End if

    # Remove CCPP scripts location from python path:
    sys.path.remove(ccpp_scripts_path)

    return [physics_blddir, genccpp_dir], do_gen_ccpp, cap_output_file

###############################################################################
def generate_init_routines(ccpp_scripts_path, data_search, build_cache,
                           bldroot, reg_files, force_reg, force_ccpp,
                           gen_fort_indent, cap_datafile):
###############################################################################
    """
    Generate the host model initialization source code files
    (phys_vars_init_check.F90 and physics_inputs.F90) using
    both the registry and the CCPP physics suites if required
    (new case or changes to registry or CCPP source(s), meta-data,
    and/or script).

    doctests:

    1.  Check that the correct error is raised with a bad search path:
        NOTE:  This must be done first to avoid having the module
        permanently imported by the "successful" test.

    >>> generate_init_routines(TEST_CCPP_PATH, ["/bad/path"], \
                               TestBuildCache, TEST_BLDROOT, \
                               TEST_REGFILES, False, \
                               False, TEST_FORT_INDENT, \
                               TEST_CAP_DATAFILE) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamAutoGenError: ERROR: Cannot find write_init_files in '['/bad/path']'

    2. Check that generate_init_routines works properly when good inputs are given:

    >>> generate_init_routines(TEST_CCPP_PATH, TEST_DATA_SEARCH, \
                               TestBuildCache, TEST_BLDROOT, \
                               TEST_REGFILES, False, \
                               False, TEST_FORT_INDENT, \
                               TEST_CAP_DATAFILE) #doctest: +ELLIPSIS
    '.../test_bldroot/phys_init'
    """
    #pylint: disable=wrong-import-position
    #pylint: disable=import-outside-toplevel

    # Check that the CCPP scripts are in the Python path, and if not
    # then add them (as they are required for the "write_init_files"
    # module):
    if ccpp_scripts_path not in sys.path:
        sys.path.append(ccpp_scripts_path)

    # Search for the "write_init_files.py" file and path:
    gen_init_file, gen_init_path = _find_file("write_init_files.py",
                                              data_search)

    # Append init source writer to python path:
    sys.path.append(gen_init_path)

    # Import physics init generator module:
    try:
        import write_init_files as write_init
    except ImportError as ierr:
        emsg = "ERROR: Cannot find write_init_files in '{}'\n{}"
        raise CamAutoGenError(emsg.format(data_search, ierr))
    # End try
    #pylint: enable=wrong-import-position
    #pylint: enable=import-outside-toplevel

    #Add new directory to build path:
    init_dir = os.path.join(bldroot, "phys_init")

    # Figure out if we need to generate new initialization routines:
    if os.path.exists(init_dir):
        #Check if registry or CCPP suites were modified:
        if force_reg or force_ccpp:
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

        #Run initialization files generator:
        retmsg = write_init.write_init_files(reg_files, init_dir, gen_fort_indent,
                                             cap_datafile, _LOGGER)

        #Check that script ran properly:
        #-----
        if retmsg:
            emsg = "ERROR:Unable to generate CAM init source code, error message is:\n{}"
            raise CamAutoGenError(emsg.format(retmsg))
        #-----

        # save build details in the build cache
        build_cache.update_init_gen(gen_init_file)
    # End if

    # Remove init source writer anc CCPP scripts
    # locations from python path:
    sys.path.remove(gen_init_path)
    sys.path.remove(ccpp_scripts_path)

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

        def registry_mismatch(self, gen_reg_file, registry_files,
                              dycore, reg_config):

            # Always return False, in order to avoid running the
            # actual generation routines when performing doctests:
            return False

        def update_ccpp(self, sdfs, scheme_files, preproc_defs, kind_phys):
            pass

        def ccpp_mismatch(self, sdfs, scheme_files, preproc_defs, kind_phys):

            # Always return False, in order to avoid running the
            # actual generation routines when performing doctests:
            return False

        def update_init_gen(self, input_file):
            pass

        def init_write_mismatch(self, input_file):

            # Always return False, in order to avoid running the
            # actual generation routines when performing doctests:
            return False

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
    TEST_REGFILES = list()
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

    # Add CCPP framework external to path:
    sys.path.append(os.path.join(os.pardir, "ccpp_framework", "scripts"))

    # Import needed CCPP-framework module:
    #pylint: disable=unused-import
    from metadata_table import find_scheme_names
    #pylint: enable=unused-import

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
