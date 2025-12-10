"""
Location of CAM's "config" python data structure, which
is used to pass any needed data between the relevant CAM
"cime_config" scripts, and which stores all meta-data and
descriptions associated with the CAM configuration of a
CIME case.

To run doctests on this file: python cam_config.py
"""

#----------------------------------------
# Import generic python libraries/modules
#----------------------------------------

import re
import sys
import argparse
import os.path
from collections import OrderedDict

#-----------------------------------
# Import CAM-specific python modules
#-----------------------------------

# Import internal CAM configuration classes:
from cam_config_classes import ConfigInteger, ConfigString, ConfigList
from cam_config_classes import CamConfigValError, CamConfigTypeError

# Import build cache object:
from cam_build_cache import BuildCacheCAM # Re-build consistency cache

# Import fortran auto-generation routines:
from cam_autogen import generate_registry, generate_physics_suites
from cam_autogen import generate_init_routines

###############################################################################
#HELPER FUNCTIONS
###############################################################################

def get_atm_hgrid(atm_grid_str):

    """
    Processes the provided atmospheric grid string
    to determine what dynamical core and horizontal
    grid regex are being used for this model run.

    Inputs:
    atm_grid_str ->  The "ATM_GRID" string provided by CIME

    Outputs:
    dycore   ->  A string which specifies the dycore being used.
    hgrid_re ->  A regular expression that matches the provided grid string.

    Doctests:

    1.  Check that a FV grid returns the correct results:
    >>> get_atm_hgrid("1.9x2.5")
    ('fv', re.compile('[0-9][0-9.]*x[0-9][0-9.]*'))

    2.  Check that an SE grid returns the correct results:
    >>> get_atm_hgrid("ne5np4.pg2")
    ('se', re.compile('ne[0-9]+np[1-8](.*)(pg[1-9])?'))

    3.  Check that an SE variable resolution grid returns the correct results:
    >>> get_atm_hgrid("ne0np4CONUS.ne30x8")
    ('se', re.compile('ne[0-9]+np[1-8](.*)(pg[1-9])?'))

    4.  Check that an FV3 grid returns the correct results:
    >>> get_atm_hgrid("C96")
    ('fv3', re.compile('C[0-9]+'))

    5.  Check that an MPAS grid returns the correct results:
    >>> get_atm_hgrid("mpasa480")
    ('mpas', re.compile('mpasa[0-9]+'))

    6.  Check that an MPAS grid with a "decimal" returns the correct results:
    >>> get_atm_hgrid("mpasa7p5")
    ('mpas', re.compile('mpasa[0-9]+'))

    7.  Check that an MPAS grid with a variable resolution grid returns the correct results:
    >>> get_atm_hgrid("mpasa15-3")
    ('mpas', re.compile('mpasa[0-9]+'))

    8.  Check that a null dycore returns the correct results:
    >>> get_atm_hgrid("null")
    ('none', None)

    9.  Check that an Eulerian (EUL) grid returns the correct results:
    >>> get_atm_hgrid("T42")
    ('eul', re.compile('T[0-9]+'))

    10.  Check that a horizontal grid with with no matches fails
         with the correct error message:
    >>> get_atm_hgrid("1.9xC96") # doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigValError: ERROR: The specified CAM horizontal grid, '1.9xC96', does not match any known format.
    """

    # Check if specified grid matches any of the pre-defined grid options.
    #   If so, then add both the horizontal grid regex and dynamical core
    #   to the configure object:

    #Finite-Volume (FV) grid:
    grid_regex = re.compile(r"[0-9][0-9.]*x[0-9][0-9.]*")
    if grid_regex.match(atm_grid_str) is not None:
        return "fv", grid_regex
    #End if

    #Spectral Element (SE) grid:
    grid_regex = re.compile(r"ne[0-9]+np[1-8](.*)(pg[1-9])?")
    if grid_regex.match(atm_grid_str) is not None:
        return "se", grid_regex
    #End if

    #Finite-Volume Cubed-Sphere (FV3) grid:
    grid_regex = re.compile(r"C[0-9]+")
    if grid_regex.match(atm_grid_str) is not None:
        return "fv3", grid_regex
    #End if

    #Model for Prediction Across Scales (MPAS) grid:
    grid_regex = re.compile(r"mpasa[0-9]+")
    if grid_regex.match(atm_grid_str) is not None:
        return "mpas", grid_regex
    #End if

    #Eulerian Spectral (EUL) grid:
    grid_regex = re.compile(r"T[0-9]+")
    if grid_regex.match(atm_grid_str) is not None:
        return "eul", grid_regex
    #End if

    #Null dycore (no specified grid):
    if atm_grid_str == "null":
        return "none", None
    #End if

    #If one has made it here, then the horizontal grid is not recognized,
    #so throw an error:
    emsg = "ERROR: The specified CAM horizontal grid, '{}', "
    emsg += "does not match any known format."
    raise CamConfigValError(emsg.format(atm_grid_str))

###############################################################################
# MAIN CAM CONFIGURE OBJECT
###############################################################################

class ConfigCAM:

    """
    Main CAM configuration object.

    Inputs to initalize class are:
    case                   -> CIME case that uses CAM
    logger                 -> Python logger object (ususally the CIME log)

    """

    def __init__(self, case, case_log):

        # pylint: disable=too-many-locals
        """
        Initalize configuration object
        and associated dictionary.
        """

        # Check if using python 3.8 or later. If not, then end build here:
        if sys.version_info[0] < 3 or (sys.version_info[0] == 3 and sys.version_info[1] < 8):
            emsg = "SIMA requires python 3.8 or later, currently using python version"
            emsg += f" {sys.version_info[0]}.{sys.version_info[1]}"
            raise SystemError(emsg)
        #End if

        # Read in needed case variables
        atm_grid = case.get_value("ATM_GRID")               # Atmosphere (CAM) grid
        cam_config_opts = case.get_value("CAM_CONFIG_OPTS") # CAM configuration options
        case_nx = case.get_value("ATM_NX")                  # Number of x-dimension grid-points (longitudes)
        case_ny = case.get_value("ATM_NY")                  # Number of y-dimension grid-points (latitudes)
        comp_ocn = case.get_value("COMP_OCN")               # CESM ocean component
        exeroot = case.get_value("EXEROOT")                 # Model executable path
        nthrds = case.get_value("NTHRDS_ATM")               # Number of model OpenMP threads
        start_date = case.get_value("RUN_STARTDATE")        # Model simulation start date
        debug_case = case.get_value("DEBUG")                # Case debug flag

        # Save case variables needed for code auto-generation:
        self.__atm_root = case.get_value("COMP_ROOT_DIR_ATM")
        self.__caseroot = case.get_value("CASEROOT")
        self.__bldroot = os.path.join(exeroot, "atm", "obj")
        self.__atm_name = case.get_value("COMP_ATM")

        # Save CPP definitions as a list:
        self.__cppdefs = [x for x in case.get_value("CAM_CPPDEFS").split() if x]

        # If only "UNSET" is present in the list, then convert to
        # empty list:
        if len(self.__cppdefs) == 1 and "UNSET" in self.__cppdefs:
            self.__cppdefs = []

        # The following translation is hard-wired for backwards compatibility
        # to support the differences between how the config_grids specifies the
        # atmosphere grid and how it is specified internally

        if atm_grid == 'ne30pg3':
            atm_grid = 'ne30np4.pg3'
        # End if

        # Level information for CAM is part of the atm grid name
        #    and must be stripped out
        match = re.match(r'(.+)z(\d+)', atm_grid)
        if match:
            atm_grid = match.groups()[0]
        # End if

        # Save user options as list
        user_config_opts = ConfigCAM.parse_config_opts(cam_config_opts)

        #-----------------------------------------------

        # Check if "-dyn" is specifed in user_config_opts
        user_dyn_opt = user_config_opts.dyn
        dyn_valid_vals = ["eul", "fv", "se", "fv3", "mpas", "none"]
        if user_dyn_opt == "none":
            # If so, then set the atmospheric grid to "null"
            atm_grid = "null"
            case_nx = "null"
            case_ny = "null"
        elif not user_dyn_opt:
            user_dyn_opt = None
        elif user_dyn_opt not in dyn_valid_vals:
            emsg = f"ERROR: '{user_dyn_opt}' is not a valid dycore,"
            emsg += f"\n       Valid values: {dyn_valid_vals}"
            raise CamConfigValError(emsg)
        # End if (no else, dyn is valid
        #-----------------------------------------------

        # Create empty dictonary
        self.__config_dict = {}

        # Save local (cime_config) directory path:
        cime_conf_path = os.path.dirname(os.path.abspath(__file__))

        # Save path to the "data" src directory:
        data_nml_path = os.path.join(cime_conf_path, os.pardir, "src", "data")

        # Save path to the "cpl/nuopc" src directory:
        cpl_nuopc_nml_path = os.path.join(cime_conf_path, os.pardir, "src", "cpl",
                                          "nuopc")

        # Create empty XML namelist definition files dictionary:
        self.__xml_nml_def_files = OrderedDict()

        #Add the default host model namelists:
        self._add_xml_nml_file(cime_conf_path, "namelist_definition_cam.xml")
        self._add_xml_nml_file(data_nml_path, "namelist_definition_physconst.xml")
        self._add_xml_nml_file(data_nml_path, "namelist_definition_ref_pres.xml")
        self._add_xml_nml_file(cpl_nuopc_nml_path,
                               "namelist_definition_atm_stream_ndep.xml")

        #----------------------------------------------------
        # Set CAM start date (needed for namelist generation)
        #----------------------------------------------------

        # Remove dashes from CIME-provided start date:
        start_date_cam = start_date.replace('-','')

        # Remove leading zeros:
        while start_date_cam[0] == "0":
            start_date_cam = start_date_cam[1:]
        # End while

        self.create_config("ic_ymd", "Start date of model run.",
                           start_date_cam, is_nml_attr=True)

        #----------------------------------------------------
        # Set CAM debug flag (needed for namelist generation)
        #----------------------------------------------------

        #Please note that the boolean debug_case is converted to
        #an integer in order to match other namelist XML attribute
        #logicals.

        self.create_config("debug",
                           "Flag to check if debug mode is enabled.",
                           int(debug_case), is_nml_attr=True)

        #------------------------
        # Set CAM physics columns
        #------------------------

        # Physics column per chunk
        pcols_desc = "Maximum number of columns assigned to a thread."
        self.create_config("pcols", pcols_desc, 16,
                           (1, None), is_nml_attr=True)

        # Physics sub-columns
        psubcols_desc = "Maximum number of sub-columns in a column."
        self.create_config("psubcols", psubcols_desc, 1,
                           (1, None), is_nml_attr=True)

        #-----------------------
        # Set CAM dynamical core
        #-----------------------

        # Cam dynamics package (dynamical core) meta-data
        dyn_desc = "Dynamics package, which is set by the horizontal grid" \
                   " specified."

        # Cam horizontal grid meta-data
        hgrid_desc = "Horizontal grid specifier."

        # dynamics package source directories meta-data
        dyn_dirs_desc = ["Comma-separated list of local directories containing",
                         "dynamics package source code.",
                         "These directories are assumed to be located under",
                         "src/dynamics, with a slash ('/') indicating directory hierarchy."]


        #Determine dynamical core and grid-matching regex to use for validation:
        dycore, grid_regex = get_atm_hgrid(atm_grid)

        #Add dynamical core to config object:
        self.create_config("dyn", dyn_desc, dycore,
                           dyn_valid_vals, is_nml_attr=True)

        #Add horizontal grid to config object:
        if dycore == "se":

            #Determine location of period (".") in atm_grid string:
            dot_idx = atm_grid.find(".")

            # Horizontal grid
            if dot_idx == -1:
                self.create_config("hgrid", hgrid_desc, atm_grid,
                                   grid_regex, is_nml_attr=True)
            else:
                self.create_config("hgrid", hgrid_desc, atm_grid[:dot_idx],
                                   grid_regex, is_nml_attr=True)
            #End if

        else:

            #Add horizontal grid as-is:
            self.create_config("hgrid", hgrid_desc, atm_grid,
                               grid_regex, is_nml_attr=True)
        #End if

        #Add dycore-specific settings:
        #------------
        if dycore == "se":
            # Source code directories
            self.create_config("dyn_src_dirs", dyn_dirs_desc, ["se",os.path.join("se","dycore")],
                               valid_list_type="str")

            # Set paths for the SE dycore namelist definition file:
            se_dyn_nml_path = os.path.join(cime_conf_path, os.pardir, "src", "dynamics", "se")

            #Add NML definition files to dictionary:
            self._add_xml_nml_file(se_dyn_nml_path, "namelist_definition_se_dycore.xml")

            # Add required CPP definitons:
            self.add_cppdef("_MPI")
            self.add_cppdef("SPMD")

            # Add OpenMP CPP definitions, if needed:
            if nthrds > 1:
                self.add_cppdef("_OPENMP")
            #End if
        elif dycore == "eul":
            # Wavenumber variable descriptions
            trm_desc = "Maximum Fourier wavenumber."
            trn_desc = "Highest degree of the Legendre polynomials for m=0."
            trk_desc = "Highest degree of the associated Legendre polynomials."

            # Add variables to configure object
            self.create_config("trm", trm_desc, 1, (1, None))
            self.create_config("trn", trn_desc, 1, (1, None))
            self.create_config("trk", trk_desc, 1, (1, None))
        elif dycore == "mpas":
            # This only includes the driver and interface code between CAM-SIMA and MPAS dynamical core.
            # MPAS dynamical core relies on its upstream build infrastructure for compilation instead of CIME to take advantage of future upstream changes automatically.
            self.create_config("dyn_src_dirs", dyn_dirs_desc, ["mpas"],
                               valid_list_type="str")

            # Add XML namelist definition file for MPAS dynamical core.
            mpas_dyn_nml_path = os.path.normpath(os.path.join(cime_conf_path, os.pardir, "src", "dynamics", "mpas"))
            self._add_xml_nml_file(mpas_dyn_nml_path, "namelist_definition_mpas_dycore.xml")
        elif dycore == "none":
            # Source code directories
            self.create_config("dyn_src_dirs", dyn_dirs_desc, ["none"],
                               valid_list_type="str")
        #End if
        #------------

        # If user-specified dynamics option is present,
        #    check that it matches the grid-derived value
        if user_dyn_opt is not None and user_dyn_opt != dycore:
            emsg = "ERROR:  User-specified dynamics option, '{}', "
            emsg += "does not match dycore expected from case grid: '{}'"
            raise CamConfigValError(emsg.format(user_dyn_opt, dycore))
        # End if

        #---------------------------------------
        # Set CAM grid variables (nlat and nlon)
        #---------------------------------------

        #Set horizontal dimension variables:
        if dycore == "se":

            # Determine location of "np" in atm_grid string:
            np_idx = atm_grid.find("np")

            #Determine location of "pg" in atm_grid string:
            pg_idx = atm_grid.find(".pg")

            # Extract cubed-sphere grid values from atm_grid/hgrid string:
            # Note that the string always starts with "ne".

            csne_val = int(atm_grid[2:np_idx])
            if pg_idx > -1:
                csnp_val = int(atm_grid[np_idx+2:pg_idx])
                npg_val  = int(atm_grid[pg_idx+3:])
            else:
                csnp_val = int(atm_grid[np_idx+2:])
                npg_val  = 0

            # Add number of elements along edge of cubed-sphere grid
            csne_desc = "Number of elements along one edge of a cubed sphere grid."
            self.create_config("csne", csne_desc, csne_val, is_nml_attr=True)

            # Add number of points on each cubed-sphere element edge
            csnp_desc = "Number of points on each edge of each element in a cubed sphere grid."
            self.create_config("csnp", csnp_desc, csnp_val)

            # Add number of CSLAM physics grid points:
            npg_desc = "Number of finite volume grid cells on each edge of" \
                       " each element in a cubed sphere grid."
            self.create_config("npg", npg_desc, npg_val, is_nml_attr=True)

            # Add number of points (NP) CPP definition:
            self.add_cppdef("NP", csnp_val)

        else:
            # Additional dyn value checks are not required,
            # as the "dyn_valid_vals" list in the "create_config" call
            # prevents non-supported dycores from being used, and all
            # dycores are lat/lon-based.

            # Add number of latitudes in grid to configure object
            nlat_desc = ["Number of unique latitude points in rectangular lat/lon grid.",
                         "Set to 1 (one) for unstructured grids."]
            self.create_config("nlat", nlat_desc, case_ny)

            # Add number of longitudes in grid to configure object
            nlon_desc = ["Number of unique longitude points in rectangular lat/lon grid.",
                         "Total number of columns for unstructured grids."]
            self.create_config("nlon", nlon_desc, case_nx)
        #End if

        #---------------------------------------
        # Set initial and/or boundary conditions
        #---------------------------------------

        # Check if user specified Analytic Initial Conditions (ICs):
        if user_config_opts.analytic_ic:
            # Set "analytic_ic" to True (1):
            analy_ic_val = 1 #Use Analytic ICs

            #Add analytic IC namelist definition file to dictionary:
            analy_ic_nml_path = os.path.join(cime_conf_path, os.pardir, "src",
                                             "dynamics", "tests")

            #Add NML definition files to dictionary:
            self._add_xml_nml_file(analy_ic_nml_path, "namelist_definition_analy_ic.xml")

            #Add new CPP definition:
            self.add_cppdef("ANALYTIC_IC")

        else:
            analy_ic_val = 0 #Don't use Analytic ICs
        #End if

        analy_ic_desc = ["Switch to turn on analytic initial conditions for the dynamics state: ",
                         "0 => no ",
                         "1 => yes."]

        self.create_config("analytic_ic", analy_ic_desc,
                           analy_ic_val, [0, 1], is_nml_attr=True)

        #--------------------------
        # Check if running an
        # aquaplanet configuration
        #--------------------------

        if user_config_opts.aquaplanet:
            aquap_flag = 1
        else:
            aquap_flag = 0

        aquap_desc = ["Switch to use aquaplanet configuration: ",
                      "0 => no ",
                      "1 => yes."]

        self.create_config("aquaplanet", aquap_desc,
                           aquap_flag, [0, 1], is_nml_attr=True)

        #--------------------------
        # Set physics_suites string
        #--------------------------

        phys_desc = ["A semicolon-separated list of physics suite definition "
                     "file (SDF) names.",
                     "To specify the Kessler and Held-Suarez suites as ",
                     "run time options, use '--physics-suites kessler;held_suarez_1994'."]

        self.create_config("physics_suites", phys_desc,
                           user_config_opts.physics_suites)

        #------------------------------------------------------------------
        # Set Fortran kinds for real-type variables in dynamics and physics
        #------------------------------------------------------------------

        kind_valid_vals = ["REAL32","REAL64"]

        #dycore kind:
        self.create_config("dyn_kind",
                           "Fortran kind used in dycore for type real.",
                           user_config_opts.dyn_kind, kind_valid_vals)

        #physics kind:
        self.create_config("phys_kind",
                           "Fortran kind used in physics for type real.",
                           user_config_opts.phys_kind, kind_valid_vals)

        #--------------------------------------------------------
        # Print CAM configure settings and values to debug logger
        #--------------------------------------------------------

        self.print_all(case_log)

    #+++++++++++++++++++++++
    # config_cam properties
    #+++++++++++++++++++++++

    # Create properties needed to return configure dictionary
    # and namelist groups list without underscores
    @property
    def config_dict(self):
        """Return the configure dictionary of this object."""
        return self.__config_dict

    @property
    def xml_nml_def_files(self):
        """
        Return a list of all XML namelist definition files
        stored by this object.
        """
        return self.__xml_nml_def_files

    @property
    def cpp_defs(self):
        """Return the CPP definitions list of this object."""
        return self.__cppdefs

    #++++++++++++++++++++++
    # ConfigCAM functions
    #++++++++++++++++++++++

    @classmethod
    def parse_config_opts(cls, config_opts, test_mode=False):
        """Parse <config_opts> and return the results

        doctests:

        1.  Check that parse_config_opts fails correctly when no arguments are given:
        >>> ConfigCAM.parse_config_opts("", test_mode=True)
        Traceback (most recent call last):
        SystemExit: 2

        2.  Check that parse_config_opts fails correctly when no physics suite is given:
        >>> ConfigCAM.parse_config_opts("--dyn se", test_mode=True)
        Traceback (most recent call last):
        SystemExit: 2

        3.  Check that parse_config_opts works as expected when given only a single physics suite entry:
        >>> config_opts = ConfigCAM.parse_config_opts("--physics-suites kessler")
        >>> vargs = vars(config_opts)
        >>> [(x, vargs[x]) for x in sorted(vargs)]
        [('analytic_ic', False), ('aquaplanet', False), ('dyn', ''), ('dyn_kind', 'REAL64'), ('phys_kind', 'REAL64'), ('physics_suites', 'kessler')]

        4.  Check that parse_config_opts works as expected when given a physics suite and a second argument:
        >>> config_opts = ConfigCAM.parse_config_opts("--physics-suites kessler --dyn se")
        >>> vargs = vars(config_opts)
        >>> [(x, vargs[x]) for x in sorted(vargs)]
        [('analytic_ic', False), ('aquaplanet', False), ('dyn', 'se'), ('dyn_kind', 'REAL64'), ('phys_kind', 'REAL64'), ('physics_suites', 'kessler')]

        5.  Check that parse_config_opts works as expected when given both a string and logical argument:
        >>> config_opts = ConfigCAM.parse_config_opts("--physics-suites kessler --dyn se --analytic-ic")
        >>> vargs = vars(config_opts)
        >>> [(x, vargs[x]) for x in sorted(vargs)]
        [('analytic_ic', True), ('aquaplanet', False), ('dyn', 'se'), ('dyn_kind', 'REAL64'), ('phys_kind', 'REAL64'), ('physics_suites', 'kessler')]

        6.  Check that parse_config_opts works as expected when given multiple physics suites:
        >>> config_opts = ConfigCAM.parse_config_opts("--physics-suites kessler;musica")
        >>> vargs = vars(config_opts)
        >>> [(x, vargs[x]) for x in sorted(vargs)]
        [('analytic_ic', False), ('aquaplanet', False), ('dyn', ''), ('dyn_kind', 'REAL64'), ('phys_kind', 'REAL64'), ('physics_suites', 'kessler;musica')]

        7.  Check that parse_config_opts fails correctly when given an un-recognized argument:
        >>> ConfigCAM.parse_config_opts("--phys kessler musica", test_mode=True)
        Traceback (most recent call last):
        SystemExit: 2
        """
        cco_str = "CAM_CONFIG_OPTS"

        #Create parser object:
        parser = argparse.ArgumentParser(description=cco_str,
                                         prog="ConfigCAM", allow_abbrev=False,
                                         epilog="Allowed values of "+cco_str)

        #Add argument options:
        parser.add_argument("--physics-suites",
                            type=str,
                            required=True,
                            metavar='<CCPP_SDFs>',
                            help="""Semicolon-separated list of Physics Suite
                                 Definition Files (SDFs)""")

        parser.add_argument("--dyn",
                            type=str,
                            required=False,
                            metavar='<dycore>',
                            default="",
                            help="""Name of dycore""")

        parser.add_argument("--aquaplanet",
                            action='store_true',
                            required=False,
                            help="""Flag to turn on aquaplanet
                                 settings (0 = False, 1 = True).""")

        parser.add_argument("--analytic-ic",
                            action='store_true',
                            required=False,
                            help="""Flag to turn on Analytic Initial
                                 Conditions (ICs). 0 = False, 1 = True.""")

        parser.add_argument("--dyn-kind",
                            type=str,
                            required=False,
                            metavar='<kind string>',
                            default="REAL64",
                            help="""Fortran kind used in dycore for type real.""")

        parser.add_argument("--phys-kind",
                            type=str,
                            required=False,
                            metavar='<kind string>',
                            default="REAL64",
                            help="""Fortran kind used in physics for type real.""")

        popts = [opt for opt in config_opts.split(" ") if opt]
        if test_mode:
            stderr_save = sys.stderr
            sys.stderr = sys.stdout
        # end if
        pargs = parser.parse_args(popts)
        if test_mode:
            sys.stderr = stderr_save
        # end if
        return pargs

    def create_config(self, name, desc, val, valid_vals=None,
                      valid_list_type=None, is_nml_attr=False):

        """
        Create new CAM "configure" object, and add it
        to the configure dictionary.
        """

        # Check for given value type
        if isinstance(val, int):
            # If integer, then call integer configure object
            conf_obj = ConfigInteger(name, desc, val,
                                     valid_vals, is_nml_attr=is_nml_attr)

        elif isinstance(val, str):
            # If string, then call string configure object
            conf_obj = ConfigString(name, desc, val,
                                    valid_vals, is_nml_attr=is_nml_attr)

        elif isinstance(val, list):
            # If list, then call list configure object
            conf_obj = ConfigList(name, desc, val,
                                  valid_type=valid_list_type,
                                  valid_vals=valid_vals)
        else:
            # If not an integer, string, or a list, then throw an error
            emsg = ("ERROR:  The input value for new CAM config variable, '{}', "
                    "must be an integer, string, or list, not {}")
            raise CamConfigTypeError(emsg.format(name, type(val)))

        # Next, check that object name isn't already in the config list
        if name in self.config_dict:
            # If so, then throw an error
            emsg = ("ERROR:  The CAM config variable, '{}', already exists! "
                    "Any new config variable must be given a different name")
            raise CamConfigValError(emsg.format(name))

        # If not, then add object to dictionary
        self.__config_dict[name] = conf_obj

    #++++++++++++++++++++++++

    def print_config(self, obj_name, case_log):

        """
        Print the value and description of a specified
        CAM configure object to the CIME debug log.
        """

        # Check that the given object name exists in the dictionary
        if obj_name in self.config_dict:
            obj = self.config_dict[obj_name]
        else:
            raise  CamConfigValError(f"ERROR:  Invalid configuration name, '{obj_name}'")

        # Print variable to logger
        case_log.debug(f"{obj.desc}")
        case_log.debug(f"{obj.name} = {obj.value}")

    #++++++++++++++++++++++++

    def print_all(self, case_log):

        """
        Print the names, descriptions, and values of all CAM
        configuration objects.
        """

        # Print separator
        case_log.debug("CAM configuration variables:")
        case_log.debug("-----------------------------")

        # Loop over config dictionary values
        for obj_name in self.config_dict:
            # Print variable to logger
            self.print_config(obj_name, case_log)

        # Also print CPP definitions, if any:
        if self.__cppdefs:
            case_log.debug(f"\nCAM CPP Defs: {' '.join(self.__cppdefs)}")

        # Print additional separator (to help separate this output from
        #     additional CIME output)
        case_log.debug("-----------------------------")

    #++++++++++++++++++++++++

    def set_value(self, obj_name, val):

        """
        Set configure object's value to the value given.
        """

        # First, check that the given object name exists in the dictionary
        if obj_name in self.config_dict:
            obj = self.config_dict[obj_name]
        else:
            raise CamConfigValError(f"ERROR:  Invalid configuration name, '{obj_name}'")

        # Next, check that the given value is either an integer or a string
        if not isinstance(val, (int, str)):
            emsg = ("ERROR:  Value provided for variable, '{}', "
                    "must be either an integer or a string."
                    "  Currently it is type {}")
            raise  CamConfigTypeError(emsg.format(obj_name, type(val)))

        # Finally, set configure object's value to the value given
        obj.set_value(val)

    #++++++++++++++++++++++++

    def add_cppdef(self, cppname, value=None):

        """
        Add a CPP definition value to be used during the
        building of the model.  An error is thrown if
        the CPP macro has already been defined.
        """

        #Create string to check if CPP definition is already present:
        check_str = r"-D"+cppname

        #Check if CPP definition name already exists in CPP string list.
        #This is done because a CPP definition should only be set once,
        #in order to avoid variable overwriting or other un-expected
        #compiler behaviors:
        if any(re.match(check_str+r"($|=)", cppdef.strip()) for cppdef in self.__cppdefs):
            #If match is found, then raise an error:
            emsg = "ERROR: CPP definition '{}' has already been set"
            raise CamConfigValError(emsg.format(cppname.upper()))

        # Check if input value is a logical:
        if value is None:
            # Create CPP flag string with no equals sign:
            cpp_str = check_str
        else:
            # Create CPP definition flag string:
            cpp_str = f"{check_str}={value}"

        # Add string to CPP definition list:
        self.__cppdefs.append(cpp_str)

    #++++++++++++++++++++++++

    def get_value(self, obj_name):

        """
        Return value for specified configure object.
        """

        # First check that the given object name exists in the dictionary
        if obj_name in self.config_dict:
            obj = self.config_dict[obj_name]
        else:
            raise  CamConfigValError(f"ERROR:  Invalid configuration name, '{obj_name}'")

        # If it does, then return the object's value
        return obj.value

    #++++++++++++++++++++++++

    def generate_cam_src(self, gen_fort_indent):

        """
        Run CAM auto-generation functions, which
        check if the required Fortran source code
        and meta-data are present in the model bld
        directory and build cache, and if not,
        generates them based on CAM configure settings
        and the model registry file.
        """

        # Set SourceMods path:
        source_mods_dir = os.path.join(self.__caseroot, "SourceMods", "src.cam")

        # Set possible locations to search for generation routines
        # with the SourceMods directory searched first:
        data_path = os.path.join(self.__atm_root, "src", "data")
        data_search = [source_mods_dir, data_path]

        # Extract atm model config settings:
        dyn = self.get_value("dyn")
        phys_suites = self.get_value("physics_suites")

        #---------------------------------------------------------
        # Load a build cache, if available
        #---------------------------------------------------------
        build_cache = BuildCacheCAM(os.path.join(self.__bldroot,
                                                 "cam_build_cache.xml"))

        #---------------------------------------------------------
        # Create the physics derived data types using the registry
        #---------------------------------------------------------
        retvals = generate_registry(data_search, build_cache, self.__atm_root,
                                    self.__bldroot, source_mods_dir,
                                    dyn, gen_fort_indent)
        reg_dir, force_ccpp, reg_files, ic_names, registry_constituents, vars_init_value = retvals

        #Add registry path to config object:
        reg_dir_desc = "Location of auto-generated registry code."
        self.create_config("reg_dir", reg_dir_desc, reg_dir)

        #---------------------------------------------------------
        # Call SPIN (CCPP Framework) to generate glue code
        #---------------------------------------------------------
        retvals = generate_physics_suites(build_cache, self.__cppdefs,
                                          self.__atm_name, phys_suites,
                                          self.__atm_root, self.__bldroot,
                                          reg_dir, reg_files, source_mods_dir,
                                          force_ccpp)
        phys_dirs, force_init, _, nml_fils, capgen_db, scheme_names = retvals

        # Add namelist definition files to dictionary:
        for nml_fil in nml_fils:
            self.__xml_nml_def_files[os.path.basename(nml_fil)] = nml_fil

        #Convert physics directory list into a string:
        phys_dirs_str = ';'.join(phys_dirs)

        #Add physics directory paths to config object:
        phys_dirs_desc = "Locations of auto-generated CCPP physics codes."
        self.create_config("phys_dirs", phys_dirs_desc, phys_dirs_str)

        #---------------------------------------------------------
        # Create host model variable initialization routines
        #---------------------------------------------------------
        init_dir = generate_init_routines(build_cache, self.__bldroot,
                                          force_ccpp, force_init,
                                          source_mods_dir, gen_fort_indent,
                                          capgen_db, ic_names, registry_constituents,
                                          vars_init_value)

        #Add registry path to config object:
        init_dir_desc = "Location of auto-generated physics initialization code."
        self.create_config("init_dir", init_dir_desc, init_dir)

        #--------------------------------------------------------------
        # write out the cache here as we have completed pre-processing
        #--------------------------------------------------------------
        build_cache.write()

        #Return the set of all scheme names present in the SDFs:
        return scheme_names

    #++++++++++++++++++++++++

    def ccpp_phys_set(self, cam_nml_attr_dict, phys_nl_pg_dict):

        """
        Find the physics suite to run.

        If more than one physics suite is available,
        then make sure the user has specified a physics
        suite from the list of available suites.

        If exactly one physics suite is available,
        then make sure that either the user did not
        specify a suite or that they did specify a
        suite and that it matches the available suite.

        Inputs:

        cam_nml_attr_dict -> Dictionary of ParamGen (XML)
                             attribute values.

        phys_nl_pg_dict -> ParamGen data dictionary for
                           the "physics_nl" namelist group
        """

        #Extract physics suites list:
        phys_suites = [x.strip() for x in self.get_value('physics_suites').split(';')]

        #Determine current value of "physics_suite" namelist variable:
        phys_nl_val = phys_nl_pg_dict['physics_suite']['values'].strip()

        #Check if "physics_suite" has been set by the user:
        if phys_nl_val != 'UNSET':

            #Next, check if only one physics suite is listed:
            if len(phys_suites) == 1:
                #If so, then check that user-provided suite matches
                #suite in physics_suites config list:
                if phys_nl_val == phys_suites[0]:
                    #If so, then set attribute to phys_suites value:
                    phys_nl_pg_dict['physics_suite']['values'] = phys_suites[0]
                    cam_nml_attr_dict["phys_suite"] = phys_suites[0]
                else:
                    #If not, then throw an error:
                    emsg  = "physics_suite specified in user_nl_cam, '{}', does not\n"
                    emsg += "match the suite listed in CAM_CONFIG_OPTS: '{}'"
                    raise CamConfigValError(emsg.format(phys_nl_val,
                                                        phys_suites[0]))
                #End if

            else:
                #If more than one suite is listed, then check if user-provided
                #value is present in the physics_suites config list:
                if phys_nl_val in phys_suites:
                    phys_nl_pg_dict['physics_suite']['values'] = phys_nl_val
                    cam_nml_attr_dict["phys_suite"] = phys_nl_val
                else:
                    #If not, then throw an error:
                    emsg  = "physics_suite specified in user_nl_cam, '{}', doesn't match any suites\n"
                    emsg += "listed in CAM_CONFIG_OPTS: '{}'"
                    raise CamConfigValError(emsg.format(phys_nl_val,
                                                        self.get_value('physics_suites')))
                #End if
            #End if

        elif len(phys_suites) == 1:
            #Just set the attribute and nl value to phys_suites value:
            phys_nl_pg_dict['physics_suite']['values'] = phys_suites[0]
            cam_nml_attr_dict["phys_suite"] = phys_suites[0]
        else:
            #If more then one suite listed, then throw an error,
            #because one needs to be specified by the user:
            emsg  = "No 'physics_suite' variable is present in user_nl_cam.\n"
            emsg += "This is required because more than one suite is listed\n"
            emsg += f"in CAM_CONFIG_OPTS: '{self.get_value('physics_suites')}'"
            raise CamConfigValError(emsg)
        #End if

    #++++++++++++++++++++++++

    def _add_xml_nml_file(self, path, filename):

        """
        Utility function to add XML namelist
        definition file path to file list.

        Inputs:

        path     -> Path to XML namelist file
        filename -> XML namelist definition filename

        """

        #Combine file name with path and add to list:
        self.__xml_nml_def_files[filename] = os.path.join(path, filename)

#############
# End of file
#############
