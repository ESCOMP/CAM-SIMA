"""
Location of CAM's "config" python data structure, which
is used to pass any needed data between the relevant CAM
"cime_config" scripts, and which stores all meta-data and
descriptions associated with the CAM configuration of a
CIME case.
"""

#----------------------------------------
# Import generic python libraries/modules
#----------------------------------------
import re
import argparse

# Determine regular rexpression type  (for later usage in Config_string)
REGEX_TYPE = type(re.compile(r" "))

###############################################################################
# Error-handling classes
###############################################################################

class CamConfigValError(ValueError):
    """Class used to handle CAM config value errors
    (e.g., log user errors without backtrace)"""
    # pylint: disable=useless-super-delegation
    def __init__(self, message):
        super(CamConfigValError, self).__init__(message)
    # pylint: enable=useless-super-delegation

###############################################################################

class CamConfigTypeError(TypeError):
    """Class used to handle CAM config type errors
    (e.g., log user errors without  backtrace)"""
    # pylint: disable=useless-super-delegation
    def __init_(self, message):
        super(CamConfigTypeError, self).__init__(message)
    # pylint: enable=useless-super-delegation

###############################################################################
# CAM configure option classes
###############################################################################

class ConfigGen:

    """
    Generic configuration class used to
    store CAM configuration names and
    descriptions.

    Inputs to initalize class are:
    name -> Name of new CAM configure option
    desc -> Text description of CAM configure option
    is_nml_attr (optional) -> Logical that determines if this option
                              is also a namelist attribute (defaut is False)

    Doctests:

    1. Check that ConfigGen works properly:

    >>> ConfigGen("test", "test object description").name
    'test'

    >>> ConfigGen("test", "test object description").desc
    'test object description'

    >>> ConfigGen("test", "test object description", is_nml_attr=True).is_nml_attr
    True

    2.  Check that non-optional inputs must be strings:

    >>> ConfigGen(5, "test_object_description").name #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigTypeError: ERROR:  Configuration variable name '5' must be a string, not <type 'int'>

    >>> ConfigGen("test", {5,}).desc #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigTypeError: ERROR:  Configuration variable, 'test', must have a string-type description, not <type 'tuple'>

    """

    def __init__(self, name, desc, is_nml_attr=False):

        # Check that "name" is a string
        if not isinstance(name, str):
            raise CamConfigTypeError("ERROR:  Configuration variable name '{}' must be a string, not {}".format(name, type(name)))

        # Check that "desc" is a string
        if not isinstance(desc, str):
            raise CamConfigTypeError("ERROR:  Configuration variable, '{}', must have a string-type description, not {}".format(name, type(desc)))

        # Add name, description, and namelist attribute logical to object
        self.__name = name
        self.__desc = desc
        self.__is_nml_attr = is_nml_attr

    #++++++++++++++++++++++++

    # Create properties needed to return name and description properties
    @property
    def name(self):
        """Return the name of this config object"""
        return self.__name

    @property
    def desc(self):
        """Return the description of this config object"""
        return self.__desc

    @property
    def is_nml_attr(self):
        """Return the namelist attribute logical of this config object"""
        return self.__is_nml_attr

###############################################################################

class ConfigInteger(ConfigGen):

    """
    Configuration class used to store
    integer-based CAM configuration
    options.

    Inputs to initalize class are:
    name                   -> Name of new CAM configure option
    desc                   -> Text description of CAM configure option
    val                    -> Integer value for CAM configure option
    valid_vals (optional)  -> Range or list of valid CAM configure option values (default is None)
    is_nml_attr (optional) -> Logical that determines if option is also a namelist attribute (defaut is False)

    Doctests:

    1. Check that ConfigInteger works properly:

    With no valid values:
    >>> ConfigInteger("test", "test object description", 5).value
    5

    With valid values tuple:
    >>> ConfigInteger("test", "test object description", 5, (1, 10)).valid_vals
    (1, 10)

    With valid values list:
    >>> ConfigInteger("test", "test object description", 5, [4, 5, 6]).valid_vals
    [4, 5, 6]

    With namelist attribute set to "True":
    >>> ConfigInteger("test", "test object description", 5, [4, 5, 6], is_nml_attr=True).is_nml_attr
    True

    2.  Check that valid_vals must be None, a tuple, or a list:

    >>> ConfigInteger("test", "test object description", 5, "valid_vals").valid_vals #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigTypeError: ERROR:  The valid values for variable, 'test', must either be None, a list, or a tuple, not <type 'str'>

    3.  Check that elements in list/tuple must be type None or integer:

    >>> ConfigInteger("test", "Test object description", 5, (1, 5.0)).valid_vals #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigTypeError: ERROR:  Valid value for variable, 'test', must be either None or an integer.  Currently it is <type 'float'>

    4.  Evaluate if the "check_value" method works properly:

    With tuple length < 2:
    >>> ConfigInteger("test", "Test object description", 5, (1,)).valid_vals #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigValError: Error:  Valid values tuple for variable, 'test', must have two elements, not '1' elements

    With tuple length > 2:
    >>> ConfigInteger("test", "Test object description", 5, (1, 2, 10)).valid_vals #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigValError: Error:  Valid values tuple for variable, 'test', must have two elements, not '3' elements

    With tuple full of Nones:
    >>> ConfigInteger("test", "Test object description", 5, (None, None)).valid_vals #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigValError: Error:  Valid values tuple for variable, 'test', must contain at least one integer

    With given value less than valid minimum:
    >>> ConfigInteger("test", "Test object description", 5, (6, None)).value #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigValError: Error:  Value, '5', provided for variable, 'test', is less than minimum valid value, '6'

    With given value more than valid maximum:
    >>> ConfigInteger("test", "Test object description", 5, (None, 4)).value #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigValError: Error:  Value, '5', provided for variable, 'test', is greater than max valid value, '4'

    With given value outside valid range:
    >>> ConfigInteger("test", "Test object description", 5, (1, 4)).value #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigValError: Error:  Value, '5', provided for variable, 'test', is outside valid value range, '(1, 4)'

    With given value not present in valid value list:
    >>> ConfigInteger("test,", "Test object description", 5, [3, 4, 6]).value #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigValError: ERROR:  Value, '5', provided for variable, 'test', does not match any of the valid values: '[3, 4, 6]'
    """

    def __init__(self, name, desc, val, valid_vals=None, is_nml_attr=False):

        # Add generic attributes
        ConfigGen.__init__(self, name, desc, is_nml_attr=is_nml_attr)

        # Check that "valid_vals" is either "None", a list, or a tuple
        if valid_vals is not None:
            if not isinstance(valid_vals, (list, tuple)):
                raise CamConfigTypeError("ERROR:  The valid values for variable, '{}', must either be None, a list, or a tuple, not {}".format(name, type(valid_vals)))

            # If list or tuple, check that all entries are either
            #   "None" or integers
            for valid_val in valid_vals:
                if valid_val is not None and not isinstance(valid_val, int):
                    emsg = ("ERROR:  Valid value for variable, '{}', must be "
                            "either None or an integer.  Currently it is {}")
                    raise CamConfigTypeError(emsg.format(name,
                                                         type(valid_val)))

        # If ok, then add valid_vals to object
        self.__valid_vals = valid_vals

        # Next, check that provided value is "valid" based on the
        #    valid values list or tuple
        self.__check_value(val)

        # If everything is ok, then add provided value to object
        self.__value = val

    #++++++++++++++++++++++++

    # Create properties needed to return given value and valid values
    @property
    def value(self):
        """Return the value of this config object"""
        return self.__value

    @property
    def valid_vals(self):
        """Return the valid values of this config object"""
        return self.__valid_vals

    #++++++++++++++++++++++++

    def __check_value(self, val):

        """
        Checks input/given value to make sure
        it is valid according to the
        object's valid values list or tuple.

        If a tuple, then assume the two
        valid values in the tuple represent a range,
        and check that the given value is
        in-between that range.

        If a list, then assume the given value
        must match at least one of the valid values
        included in that list.
        """

        # Extract valid values (valid_vals) from object
        valid_vals = self.valid_vals

        # Only check the given value if valid_vals is not "None"
        if valid_vals is not None:
            # Check if valid values is a tuple
            if isinstance(valid_vals, tuple):
                # Check that length of valid values tuple is 2
                if len(valid_vals) != 2:
                    emsg = ("Error:  Valid values tuple for variable, "
                            "'{}', must have two elements, not '{}' elements")
                    raise CamConfigValError(emsg.format(self.name,
                                                        len(valid_vals)))
                # End if
                if valid_vals[0] is None:
                    # If first valid value is "None", then just check that
                    #   given value is less than second valid value, and
                    #   that second value is an integer
                    if valid_vals[1] is None:
                        emsg = "Error: Valid values tuple for variable, '{}', "
                        emsg += "must contain at least one integer"
                        raise CamConfigValError(emsg.format(self.name))
                    # End if
                    if val > valid_vals[1]:
                        emsg = "Error:  Value, '{}', provided for variable, "
                        emsg += "'{}', is greater than max valid value, '{}'"
                        raise CamConfigValError(emsg.format(val, self.name,
                                                            valid_vals[1]))
                    # End if
                elif valid_vals[1] is None:
                    # Check if second value is "None".
                    #   If so, then just check that given value is greater
                    #      than first valid value
                    if val < valid_vals[0]:
                        emsg = "Error: Value, '{}', provided for variable, "
                        emsg += "'{}', is less than minimum valid value, '{}'"
                        raise CamConfigValError(emsg.format(val, self.name,
                                                            valid_vals[0]))
                    # End if
                else:
                    # If both valid values are integers, then check that
                    #     given value is between both valid values
                    if (val < valid_vals[0]) or (val > valid_vals[1]):
                        emsg = "Error:  Value, '{}', provided for variable, "
                        emsg += "'{}', is outside valid value range, '{}'"
                        raise CamConfigValError(emsg.format(val, self.name,
                                                            valid_vals))
                    # End if
                # End if
            else:
                # If valid_vals is a list, then just check that the given value
                # matches one of the valid values in the list
                if not val in valid_vals:
                    emsg = "ERROR:  Value, '{}', provided for variable, '{}', "
                    emsg += "does not match any of the valid values: '{}'"
                    raise CamConfigValError(emsg.format(val, self.name,
                                                        valid_vals))
                # End if
            # End if
        # End if
    #++++++++++++++++++++++++

    def set_value(self, val):

        """
        Set configure object's value to the one provided.
        """

        # First, check that the provided value is valid
        self.__check_value(val)

        # If ok, then set object's value to one provided
        self.__value = val

###############################################################################

class ConfigString(ConfigGen):

    """
    Configuration class used to store
    string-based CAM configuration
    options.

    Inputs to initalize class are:
    name                   -> Name of new CAM configure option
    desc                   -> Text description of CAM configure option
    val                    -> Integer value for CAM configure option
    valid_vals (optional)  -> List of valid CAM configure option values (default is None)
    is_nml_attr (optional) -> Logical that determines if option is also a namelist attribute (defaut is False)

    Doctests:

    1. Check that ConfigString works properly:

    With no valid values:
    >>> ConfigString("test", "test object description", "test_val").value
    'test_val'

    With valid values list:
    >>> ConfigString("test", "test object description", "test_val", ["test_val", "test_val_II"]).valid_vals
    ['test_val', 'test_val_II']

    With valid values regular expression:
    >>> ConfigString("test", "test_object description", "test_val", re.compile(r"test_val")).value
    'test_val'

    With namelist attribute set to "True":
    >>> ConfigString("test", "test_object description", "test_val", re.compile(r"test_val"), is_nml_attr=True).is_nml_attr
    True

    2. Check that valid_vals must be either None, a list, or a regular expression:

    >>> ConfigString("test", "test object description", "test_val", "test_valid_vals").valid_vals #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigTypeError: ERROR:  The valid values for variable, 'test', must either be None, a list, or a regex object, not <type 'str'>

    3. Check that if valid_vals is a list, all elements must be strings:

    >>> ConfigString("test", "test object description", "test_val", ["test_val", 5]).valid_vals #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigTypeError: ERROR:  All valid value list options for variable, 'test', must be strings.

    4.  Evaluate if the "check_value" method works properly:

    With given value not present in valid value list:
    >>> ConfigString("test", "test object description", "test_val", ["real_val", "other_val"]).valid_vals #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigValError: ERROR:  Value, 'test_val', provided for variable, 'test', does not match any of the valid values: '['real_val', 'other_val']'

    With given value not matching the valid regular expression:
    >>> ConfigString("test", "test object description", "test_val", re.compile(r"real_val")).value #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigValError: ERROR:  Value, 'test_val', provided for variable, 'test', does not match the valid regular expression

    """

    def __init__(self, name, desc, val, valid_vals=None, is_nml_attr=False):

        # Add generic attributes
        ConfigGen.__init__(self, name, desc, is_nml_attr=is_nml_attr)

        # Check if Valid_vals is not None
        if valid_vals is not None:
            # If not None, check if valid_vals is either a list or a
            #     regular expression (regex) object
            if not isinstance(valid_vals, (list, REGEX_TYPE)):
                emsg = "ERROR:  The valid values for variable, '{}', must "
                emsg += "either be None, a list, or a regex object, not {}"
                raise CamConfigTypeError(emsg.format(name, type(valid_vals)))
            # End if
            if isinstance(valid_vals, list):
                # If list, check that every entry is a string
                if not all(isinstance(n, str) for n in valid_vals):
                    emsg = "ERROR:  All valid value list options for "
                    emsg += "variable, '{}', must be strings."
                    raise CamConfigTypeError(emsg.format(name))
                # End if
            # End if
        # End if
        # If ok, then add valid_vals to object
        self.__valid_vals = valid_vals

        # Next, check that provided value is "valid" based on the
        #     valid values list or regular expression
        self.__check_value(val)

        # If everything is ok, then add provided value to object
        self.__value = val

    #++++++++++++++++++++++++

    # Create properties needed to return given value and valid values
    #     without underscores
    @property
    def value(self):
        """Return the value of this config object"""
        return self.__value

    @property
    def valid_vals(self):
        """Return the valid values of this config object"""
        return self.__valid_vals

    #++++++++++++++++++++++++

    def __check_value(self, val):

        """
        Checks input/given value to make sure
        it is valid according to the
        object's valid values list or
        regular expression.

        If a list, then assume the given value
        must match at least one of the valid values
        included in that list.

        If a compiled regular expression, then
        assume the value must match the regular
        expression.
        """

        # Extract valid values (valid_vals) from object
        valid_vals = self.valid_vals

        # If a list, then check that the given value
        # matches one of the valid values in the list
        if isinstance(valid_vals, list):
            if not val in valid_vals:
                raise CamConfigValError("ERROR:  Value, '{}', provided for variable, '{}', does not match any of the valid values: '{}'".format(val, self.name, valid_vals))

        elif valid_vals is not None:
            # If a regular expression object, then check that
            # value is matched by the expression
            if valid_vals.match(val) is None:
                raise CamConfigValError("ERROR:  Value, '{}', provided for variable, '{}', does not match the valid regular expression".format(val, self.name))

    #++++++++++++++++++++++++

    def set_value(self, val):

        """
        Set configure object's value to the one provided.
        """

        # First, check that the provided value is valid
        self.__check_value(val)

        # If ok, then set object's value to one provided
        self.__value = val

###############################################################################
# MAIN CAM CONFIGURE OBJECT
###############################################################################

class ConfigCAM:

    """
    Main CAM configuration object.

    Inputs to initalize class are:
    case                   -> CIME case that uses CAM
    logger                 -> Python logger object (ususally the CIME log)

    Doctests:

    1.  Check that "create_config" works properly:

    With a given integer value:
    >>> FCONFIG.create_config("test_int", "test object description", 5)
    >>> FCONFIG.get_value("test_int")
    5

    With a given string value:
    >>> FCONFIG.create_config("test_str", "test object description", "test_val")
    >>> FCONFIG.get_value("test_str")
    'test_val'

    2.  Check that the same configure object can't be created twice:

    >>> FCONFIG.create_config("test_int", "test object description", 5) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigValError: ERROR:  The CAM config variable, 'test_int', already exists!  Any new config variable must be given a different name

    3.  Check that a configure object's given value must be either a string or integer:

    >>> FCONFIG.create_config("test_list", "test_object_description", [5]) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigTypeError: ERROR:  The input value for new CAM config variable, 'test_list', must be either an integer or a string, not <type 'list'>

    """

    def __init__(self, case, case_log):

        # pylint: disable=too-many-locals
        """
        Initalize configuration object
        and associated dictionary.
        """

        # Read in needed case variables
        atm_grid = case.get_value("ATM_GRID")               # Atmosphere (CAM) grid
        cam_config_opts = case.get_value("CAM_CONFIG_OPTS") # CAM configuration options
        case_nx = case.get_value("ATM_NX")                  # Number of x-dimension grid-points (longitudes)
        case_ny = case.get_value("ATM_NY")                  # Number of y-dimension grid-points (latitudes)
        comp_ocn = case.get_value("COMP_OCN")               # CESM ocean component

        # The following translation is hard-wired for backwards compatibility
        # to support the differences between how the config_grids specifies the
        # atmosphere grid and how it is specified internally

        if atm_grid == 'ne30pg3':
            atm_grid = 'ne30np4.pg3'
        # End if

        # Level information for CAM is part of the atm grid name
        #    and must be stripped out
        case_nlev = ''
        match = re.match(r'(.+)z(\d+)', atm_grid)
        if match:
            atm_grid = match.groups()[0]
            case_nlev = match.groups()[1]
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
            case_nlev = "null"
            case_nx = "null"
            case_ny = "null"
        elif not user_dyn_opt:
            user_dyn_opt = None
        elif user_dyn_opt not in dyn_valid_vals:
            emsg = "ERROR: '{}' is not a valid dycore,".format(user_dyn_opt)
            emsg += "\n       Valid values: {}".format(dyn_valid_vals)
            raise CamConfigValError(emsg)
        # End if (no else, dyn is valid
        #-----------------------------------------------

        # Create empty dictonary
        self.__config_dict = dict()

        # Create namelist group list, starting with default namelist groups
        self.__nml_groups = ['cam_initfiles_nl', 'cam_logfile_nl',
                             'phys_ctl_nl', 'qneg_nl']

        #----------------------------------------
        # Set CAM grid variables (nlat,nlon,nlev)
        #----------------------------------------

        # Set number of vertical levels
        if case_nlev:
            # Save variable for CPPDEFs
            nlev = case_nlev
        else:
            # Save variable for CPPDEFs
            nlev = 30

        # Add vertical levels to configure object
        nlev_desc = "Number of vertical levels."
        self.create_config("nlev", nlev_desc, nlev, None, is_nml_attr=True)

        # Add number of latitudes in grid to configure object
        nlat_desc = "Number of unique latitude points in rectangular lat/lon" \
                    " grid.\nSet to 1 (one) for unstructured grids."
        self.create_config("nlat", nlat_desc, case_ny)

        # Add number of longitudes in grid to configure object
        nlon_desc = "Number of unique longitude points in rectangular lat/lon" \
                    " grid.\nTotal number of columns for unstructured grids."
        self.create_config("nlon", nlon_desc, case_nx)

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

        # Create regex expressions to search for the different dynamics grids
        eul_grid_re = re.compile(r"T[0-9]+")                      # Eulerian dycore
        fv_grid_re = re.compile(r"[0-9][0-9.]*x[0-9][0-9.]*")     # FV dycore
        se_grid_re = re.compile(r"ne[0-9]+np[1-8](.*)(pg[1-9])?") # SE dycore
        fv3_grid_re = re.compile(r"C[0-9]+")                      # FV3 dycore
        mpas_grid_re = re.compile(r"mpasa[0-9]+")                 # MPAS dycore (not totally sure about this pattern)

        # Check if specified grid matches any of the pre-defined grid options.
        #   If so, then add both the horizontal grid and dynamical core
        #   to the configure object
        if fv_grid_re.match(atm_grid) is not None:
            # Dynamical core
            self.create_config("dyn", dyn_desc, "fv",
                               dyn_valid_vals, is_nml_attr=True)
            # Horizontal grid
            self.create_config("hgrid", hgrid_desc, atm_grid,
                               fv_grid_re, is_nml_attr=True)

        elif se_grid_re.match(atm_grid) is not None:
            # Dynamical core
            self.create_config("dyn", dyn_desc, "se",
                               dyn_valid_vals, is_nml_attr=True)
            # Horizontal grid
            self.create_config("hgrid", hgrid_desc, atm_grid,
                               se_grid_re, is_nml_attr=True)

        elif fv3_grid_re.match(atm_grid) is not None:
            # Dynamical core
            self.create_config("dyn", dyn_desc, "fv3",
                               dyn_valid_vals, is_nml_attr=True)
            # Horizontal grid
            self.create_config("hgrid", hgrid_desc, atm_grid,
                               fv3_grid_re, is_nml_attr=True)

        elif mpas_grid_re.match(atm_grid) is not None:
            # Dynamical core
            self.create_config("dyn", dyn_desc, "mpas",
                               dyn_valid_vals, is_nml_attr=True)
            # Horizontal grid
            self.create_config("hgrid", hgrid_desc, atm_grid,
                               mpas_grid_re, is_nml_attr=True)

        elif eul_grid_re.match(atm_grid) is not None:
            # Dynamical core
            self.create_config("dyn", dyn_desc, "eul",
                               dyn_valid_vals, is_nml_attr=True)
            # Horizontal grid
            self.create_config("hgrid", hgrid_desc, atm_grid,
                               eul_grid_re, is_nml_attr=True)

            # If using the Eulerian dycore, then add wavenumber variables

            # Wavenumber variable descriptions
            trm_desc = "Maximum Fourier wavenumber."
            trn_desc = "Highest degree of the Legendre polynomials for m=0."
            trk_desc = "Highest degree of the associated Legendre polynomials."

            # Add variables to configure object
            self.create_config("trm", trm_desc, 1, (1, None))
            self.create_config("trn", trn_desc, 1, (1, None))
            self.create_config("trk", trk_desc, 1, (1, None))

        elif atm_grid == "null":
            # Dynamical core
            self.create_config("dyn", dyn_desc, "none",
                               dyn_valid_vals, is_nml_attr=True)
            # Atmospheric grid
            self.create_config("hgrid", hgrid_desc, atm_grid,
                               None, is_nml_attr=True)

        else:
            emsg = "ERROR: The specified CAM horizontal grid, '{}', "
            emsg += "does not match any known format."
            raise CamConfigValError(emsg.format(atm_grid))
        #End if

        # Extract dynamics option
        dyn = self.get_value("dyn")

        # If user-specified dynamics option is present,
        #    check that it matches the grid-derived value
        if user_dyn_opt is not None and user_dyn_opt != dyn:
            emsg = "ERROR:  User-specified dynamics option, '{}', "
            emsg += "does not match dycore expected from case grid: '{}'"
            raise CamConfigValError(emsg.format(user_dyn_opt, dyn))
        # End if

        #--------------------
        # Set ocean component
        #--------------------

        ocn_valid_vals = ["docn", "dom", "som", "socn",
                          "aquaplanet", "pop", "mom"]

        ocn_desc = "\n\
        The ocean model being used.  Valid values include prognostic\n\
        ocean models (pop or mom), data ocean models (docn or dom),\n\
        a stub ocean (socn), and an aqua planet ocean (aquaplanet).\n\
        This doesn't impact how the case is built, only how\n\
        attributes are matched when searching for namelist defaults."

        self.create_config("ocn", ocn_desc, comp_ocn,
                           ocn_valid_vals, is_nml_attr=True)

        phys_desc = """\nA comma-separate list of physics suite definition
        file (SDF) names.\nTo specify the Kessler and Held-Suarez suites as \
        run time options, use '--physics-suites kessler,hs94'.
        """
        self.create_config("physics_suites", phys_desc,
                           user_config_opts.physics_suites, is_nml_attr=True)

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
        """Return the configure dictionary of this object"""
        return self.__config_dict

    @property
    def nml_groups(self):
        """Return the namelist groups list of this object"""
        return self.__nml_groups


    #++++++++++++++++++++++
    # ConfigCAM functions
    #++++++++++++++++++++++

    @classmethod
    def parse_config_opts(cls, config_opts):
        """Parse <config_opts> and return the results"""
        cco_str = "CAM_CONFIG_OPTS"
        parser = argparse.ArgumentParser(description=cco_str,
                                         prog="ConfigCAM",
                                         epilog="Allowed values of "+cco_str)

        parser.add_argument("--physics-suites", type=str, required=True,
                            help="""Comma-separated list of Physics Suite
                            Definition Files (SDFs)""")
        parser.add_argument("--dyn", "-dyn", metavar='<dycore>',
                            type=str, required=False, default="",
                            help="Name of dycore")
        popts = [opt for opt in config_opts.split(" ") if opt]
        pargs = parser.parse_args(popts)
        return pargs

    def create_config(self, name, desc, val,
                      valid_vals=None, is_nml_attr=False):

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

        else:
            # If neither an integer or a string, then throw an error
            raise CamConfigTypeError("ERROR:  The input value for new CAM config variable, '{}', must be either an integer or a string, not {}".format(name, type(val)))

        # Next, check that object name isn't already in the config list
        if name in self.config_dict:
            # If so, then throw an error
            raise CamConfigValError("ERROR:  The CAM config variable, '{}', already exists!  Any new config variable must be given a different name".format(name))

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
            raise  CamConfigValError("ERROR:  Invalid configuration name, '{}'".format(obj_name))

        # Print variable to logger
        case_log.debug("#{}".format(obj.desc))
        case_log.debug("{} = {}".format(obj.name, obj.value))

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

        # Print additional separator (to help seperate this output from
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
            raise CamConfigValError("ERROR:  Invalid configuration name, '{}'".format(obj_name))

        # Next, check that the given value is either an integer or a string
        if not isinstance(val, (int, str)):
            raise  CamConfigTypeError("ERROR:  Value provided for variable, '{}', must be either an integer or a string.  Currently it is type {}".format(obj_name, type(val)))

        # Finally, set configure object's value to the value given
        obj.set_value(val)

    #++++++++++++++++++++++++

    def get_value(self, obj_name):

        """
        return value for specified configure object.
        """

        # First check that the given object name exists in the dictionary
        if obj_name in self.config_dict:
            obj = self.config_dict[obj_name]
        else:
            raise  CamConfigValError("ERROR:  Invalid configuration name, '{}'".format(obj_name))

        # If it does, then return the object's value
        return obj.value

###############################################################################
#IGNORE EVERYTHING BELOW HERE UNLESS RUNNING TESTS ON CAM_CONFIG!
###############################################################################

#Call testing routine, if script is run directly
if __name__ == "__main__":

    # Import modules needed for testing
    import doctest
    import logging

    #--------------------------------------
    # Create fake case for Config_CAM tests
    #--------------------------------------

    class FakeCase:

        # pylint: disable=too-few-public-methods
        """
        Fake CIME case class with variables needed to test
        the "Config_CAM" object.
        """

        def __init__(self):


            # Create dictionary (so get_value works properly)
            self.conf_opts = {
                "ATM_GRID" : "f19_f19_mg17",
                "ATM_NX"   : 180,
                "ATM_NY"   : 90,
                "COMP_OCN" : "socn",
                "CAM_CONFIG_OPTS" : "-dyn none --physics-suites adiabatic"
                }

        def get_value(self, key):

            """
            Function used to return value
            from conf_opts dictionary,
            with the key as input.
            """

            val = self.conf_opts[key]

            return val

    #-------------------------------------------
    # Create new "Config_CAM" object for testing
    #-------------------------------------------

    # Create new "fake" CIME case
    FCASE = FakeCase()

    # Create python logger object
    LOGGER = logging.getLogger("cam_config")

    # Create ConfigCAM object using "fake" CIME case and logger
    FCONFIG = ConfigCAM(FCASE, LOGGER)

    # Run doctests on this file's python objects
    doctest.testmod()

##############
# End of file##
############
