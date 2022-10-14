"""
Location of internal python classes used by the
"ConfigCAM" class to generate, store, and pass-on
any CAM configuration variables to other components
of the build system.

Please note that running or testing these routines
requires python 3.7 or later.

To run doctests on this file: python cam_config_classes.py
"""

#----------------------------------------
# Import generic python libraries/modules
#----------------------------------------

import re

###############################################################################
# Error-handling classes
###############################################################################

class CamConfigValError(ValueError):
    """Class used to handle CAM config value errors
    (e.g., log user errors without backtrace)"""

###############################################################################

class CamConfigTypeError(TypeError):
    """Class used to handle CAM config type errors
    (e.g., log user errors without  backtrace)"""

###############################################################################
# Valid value-checking functions
###############################################################################

def _check_integer_val(name, val, valid_vals=None):

    """
    Checks if a provided integer value is "valid"
    as defined by the provided "valid_vals" entry
    for the given config variable (name).

    If value is not valid, then an error message is returned,
    otherwise the function returns None.

    Possible valid_val types are:

    list  -> If a list, then just check that provided value is in the list.

    tuple -> If a tuple, then there must be only two values, which define
             a possible range of values, e.g. (min, max). If only one value
             is provided, then only a minimum (or maximum) value will be
             enforced, depending on if the tuple is (x, None) or (None ,x).

    Doctests:

    Please note that "successful" validation tests are done in the ConfigInteger doctests.

    1.  Check that using a non-integer value throws an error:
    >>> _check_integer_val("test", 5.0, valid_vals=None) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigTypeError: ERROR: Value being checked in 'check_integer_val' must be an integer type, not '<class 'float'>'.

    2.  Check that using a valid_vals option that is not a list or tuple throws an error:
    >>> _check_integer_val("test", 5, valid_vals="test_vals") #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigTypeError: ERROR: Valid values for integers must by provided as either a tuple or a list, not '<class 'str'>'.

    3.  Check that using non-integer values inside the valid_vals list or tuple throws an error:
    >>> _check_integer_val("test", 5, valid_vals=[1,2,5,"test_val"]) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigTypeError: ERROR:  Valid value, 'test_val', for variable 'test', must be an integer.  Currently it is '<class 'str'>'.
    <BLANKLINE>

    4.  Check that using a tuple with only one entry throws an error:
    >>> _check_integer_val("test", 5, valid_vals=(1,)) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigValError: ERROR: Valid values tuple for variable, 'test', must have two elements, not '1' elements.

    5.  Check that using a tuple with more than two entries throws an error:
    >>> _check_integer_val("test", 5, valid_vals=(1,2,5)) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigValError: ERROR: Valid values tuple for variable, 'test', must have two elements, not '3' elements.

    6.  Check that using a tuple with only Nones throws an error:
    >>> _check_integer_val("test", 5, valid_vals=(None,None)) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigValError: ERROR: Valid values tuple for variable, 'test', must contain at least one integer.

    7.  Check that an integer less than the tuple min is "invalid":
    >>> _check_integer_val("test", 5, valid_vals=(6,None))
    "ERROR: Value, '5', provided for variable, 'test', is less than minimum valid value, '6'"

    8.  Check that an integer greater than the tuple max is "invalid":
    >>> _check_integer_val("test", 5, valid_vals=(None,4))
    "ERROR:  Value, '5', provided for variable, 'test', is greater than max valid value, '4'"

    9.  Check that an integer outside min/max tuple range is "invalid":
    >>> _check_integer_val("test", 5, valid_vals=(10,13))
    "ERROR:  Value, '5', provided for variable, 'test', is outside valid value range, '(10, 13)'"

    10. Check that an integer not included in the list is "invalid":
    >>> _check_integer_val("test", 5, valid_vals=[1,2,3,4])
    "ERROR:  Value, '5', provided for variable, 'test', does not match any of the valid values: '[1, 2, 3, 4]'"

    """

    # Make sure that provided value is an integer:
    if not isinstance(val, int):
        emsg = "ERROR: Value being checked in 'check_integer_val' "
        emsg += "must be an integer type, not '{}'."
        raise CamConfigTypeError(emsg.format(type(val)))
    # End if

    # Only check the given value if valid_vals is not "None"
    if valid_vals is not None:

        # Check if valid values is a tuple
        if isinstance(valid_vals, tuple):

            # Check that all tuple elements are either None or integers
            emsg = ""
            for valid_val in valid_vals:
                if valid_val is not None and not isinstance(valid_val, int):
                    emsg += "ERROR:  Valid value, '{}', for variable '{}', must be "
                    emsg += "either None or an integer.  Currently it is '{}'.\n"
                    emsg = emsg.format(valid_val, name, type(valid_val))
                # End if
            # End for
            if emsg:
                raise CamConfigTypeError(emsg)
            # end if

            # Check that length of valid values tuple is 2
            if len(valid_vals) != 2:
                emsg = ("ERROR: Valid values tuple for variable, "
                        "'{}', must have two elements, not '{}' elements.")
                raise CamConfigValError(emsg.format(name,
                                                    len(valid_vals)))
            # End if

            if valid_vals[0] is None:
                # If first valid value is "None", then just check that
                # given value is less than second valid value, and
                # that second value is an integer
                if valid_vals[1] is None:
                    emsg = "ERROR: Valid values tuple for variable, '{}', "
                    emsg += "must contain at least one integer."
                    raise CamConfigValError(emsg.format(name))
                # End if
                if val > valid_vals[1]:
                    emsg = "ERROR:  Value, '{}', provided for variable, "
                    emsg += "'{}', is greater than max valid value, '{}'"
                    return emsg.format(val, name, valid_vals[1])
                # End if
            elif valid_vals[1] is None:
                # Check if second value is "None".
                # If so, then just check that given value is greater
                # than first valid value
                if val < valid_vals[0]:
                    emsg = "ERROR: Value, '{}', provided for variable, "
                    emsg += "'{}', is less than minimum valid value, '{}'"
                    return emsg.format(val, name, valid_vals[0])
                # End if
            else:
                # If both valid values are integers, then check that
                # given value is between both valid values
                if (val < valid_vals[0]) or (val > valid_vals[1]):
                    emsg = "ERROR:  Value, '{}', provided for variable, "
                    emsg += "'{}', is outside valid value range, '{}'"
                    return emsg.format(val, name, valid_vals)
                # End if
            # End if

        elif isinstance(valid_vals, list):

            # Check that all list elements are integers
            emsg = ""
            for valid_val in valid_vals:
                if not isinstance(valid_val, int):
                    emsg += "ERROR:  Valid value, '{}', for variable '{}', "
                    emsg += "must be an integer.  Currently it is '{}'.\n"
                    emsg = emsg.format(valid_val, name, type(valid_val))
                # End if
            # End for
            if emsg:
                raise CamConfigTypeError(emsg)
            # end if


            # If valid_vals is a list, then just check that the given value
            # matches one of the valid values in the list
            if val not in valid_vals:
                emsg = "ERROR:  Value, '{}', provided for variable, '{}', "
                emsg += "does not match any of the valid values: '{}'"
                return emsg.format(val, name, valid_vals)
            # End if

        else:
            # valid_vals is neither a list nor a tuple, so throw an error:
            emsg = "ERROR: Valid values for integers must by provided as "
            emsg += "either a tuple or a list, not '{}'."
            raise CamConfigTypeError(emsg.format(type(valid_vals)))

        # End if
    # End if

    # Return nothing if value is valid
    return None

###############################################################################

def _check_string_val(name, val, valid_vals=None):

    """
    Checks if a provided string value is "valid"
    as defined by the provided "valid_vals" entry.

    Possible valid_val types are:

    list  -> If a list, then just check that provided value is in the list.

    regex -> If a compiled regular expression, then check that the provided
             value is matched by the regular expression.

    Doctests:

    Please note that "successful" validation tests are done in the ConfigString doctests.

    1.  Check that using a non-string value throws an error:
    >>> _check_string_val("test", [5], valid_vals=None) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigTypeError: ERROR: Value being checked in 'check_string_val' must be a string type, not '<class 'list'>'.

    2.  Check that using a valid_vals option that is not None, a list, or a regex throws an error:
    >>> _check_string_val("test", "test_val", valid_vals=5) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigTypeError: ERROR: Valid values for strings must by provided as either a regular expression or a list, not '<class 'int'>'.

    3.  Check that using non-string values inside the valid_vals list throws an error:
    >>> _check_string_val("test", "1", valid_vals=["1","2","5",6]) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigTypeError: ERROR:  Valid value, '6', for variable 'test', must be a string.  Currently it is '<class 'int'>'.

    9.  Check that a string that doesn't match the provided regex is "invalid":
    >>> _check_string_val("test", "test_val", valid_vals=re.compile(r"foo"))
    "ERROR:  Value, 'test_val', provided for variable, 'test', does not match the valid regular expression."

    10. Check that a string not included in the list is "invalid":
    >>> _check_string_val("test", "test_val", valid_vals=["1","2","3","4"])
    "ERROR:  Value, 'test_val', provided for variable, 'test', does not match any of the valid values: '['1', '2', '3', '4']'"
    """

    # Make sure that provided value is a string:
    if not isinstance(val, str):
        emsg = "ERROR: Value being checked in 'check_string_val' "
        emsg += "must be a string type, not '{}'."
        raise CamConfigTypeError(emsg.format(type(val)))
    # End if

    # Only check the given value if valid_vals is not "None"
    if valid_vals is not None:

        # If a list, then check that the given value
        # matches one of the valid values in the list
        if isinstance(valid_vals, list):

            # Check that all list elements are strings:
            for valid_val in valid_vals:
                if not isinstance(valid_val, str):
                    emsg = ("ERROR:  Valid value, '{}', for variable '{}', must be "
                            "a string.  Currently it is '{}'.")
                    raise CamConfigTypeError(emsg.format(valid_val, name, type(valid_val)))
                # End if
            # End for

            if not val in valid_vals:
                emsg = "ERROR:  Value, '{}', provided for variable, '{}', "
                emsg += "does not match any of the valid values: '{}'"
                return emsg.format(val, name, valid_vals)
            # End if
        elif isinstance(valid_vals, re.Pattern):
            # If a regular expression object, then check that
            # value is matched by the expression
            if valid_vals.match(val) is None:
                emsg = "ERROR:  Value, '{}', provided for variable, '{}', "
                emsg += "does not match the valid regular expression."
                return emsg.format(val, name)
                # End if
            # End if
        else:
            # valid_vals is neither a list nor a regex, so throw an error:
            emsg = "ERROR: Valid values for strings must by provided as "
            emsg += "either a regular expression or a list, not '{}'."
            raise CamConfigTypeError(emsg.format(type(valid_vals)))

        # End if
    # End if

    # Return nothing if value is valid
    return None

# Helper function to better generalize config value checking:
_TYPE_CHECK_FUNCTIONS = {"int" : _check_integer_val, "str" : _check_string_val}

# Set of valid types (for faster checking):
_VALID_TYPE_SET = {"int", "str"}

###############################################################################
# Internal generic CAM configure class
###############################################################################

class _ConfigGen:

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

    >>> _ConfigGen("test", "test object description").name
    'test'

    >>> _ConfigGen("test", "test object description").desc
    '# test object description'

    >>> print(_ConfigGen("test", ["test", "object", "description"]).desc)
    # test
    #    object
    #    description

    >>> _ConfigGen("test", "test object description", is_nml_attr=True).is_nml_attr
    True

    2.  Check that non-optional inputs must be strings:

    >>> _ConfigGen(5, "test_object_description").name #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigTypeError: ERROR:  Configuration variable name '5' must be a string, not <class 'int'>

    >>> _ConfigGen("test", (5,)).desc  #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigTypeError: ERROR:  Configuration variable, 'test', must have a string-type description, or a list of string-type descriptions, not tuple ((5,))

    >>> _ConfigGen("test", ["test", ("object", "description")]).desc #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigTypeError: ERROR:  Configuration variable, 'test', must have a string-type description, or a list of string-type descriptions, not [<class 'str'>, <class 'tuple'>]

    """

    def __init__(self, name, desc, is_nml_attr=False):

            # Check that "name" is a string
        if not isinstance(name, str):
            emsg = "ERROR:  Configuration variable name '{}' must be a string, not {}"
            raise CamConfigTypeError(emsg.format(name, type(name)))
        # end if

        # Check that "desc" is a string or a list of strings
        desc_ok = isinstance(desc, str)
        if (not desc_ok) and isinstance(desc, list):
            desc_ok = all(isinstance(x, str) for x in desc)
        # end if
        if not desc_ok:
            emsg = ("ERROR:  Configuration variable, '{}', "
                    "must have a string-type description, or a list of "
                    "string-type descriptions, not {}")
            if isinstance(desc, str):
                derr = type(desc)
            elif isinstance(desc, list):
                derr = [type(x) for x in desc]
            else:
                derr = f"{type(desc).__name__} ({desc})"
            # end if
            raise CamConfigTypeError(emsg.format(name, derr))
        # end if

        # Add name, description, and namelist attribute logical to object
        self.__name = name
        if isinstance(desc, str):
            self.__desc = f"# {desc}"
        elif isinstance(desc, list):
            self.__desc = "# " + "\n#    ".join(desc)
        # end if
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
# CAM configure option classes
###############################################################################

class ConfigInteger(_ConfigGen):

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

    """

    def __init__(self, name, desc, val, valid_vals=None, is_nml_attr=False):

        # Add generic attributes
        super().__init__(name, desc, is_nml_attr=is_nml_attr)

        # Add valid_vals to object
        self.__valid_vals = valid_vals

        # Check that provided value is "valid" based on the
        # valid values list or tuple.  Note that this function
        # also checks valid_vals itself to ensure that it is
        # of the correct type and format.
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

        # Check if integer value is valid
        bad_val_msg = _check_integer_val(self.name, val,
                                         valid_vals=self.valid_vals)

        # Raise an error if a bad value is found:
        if bad_val_msg:
            raise CamConfigValError(bad_val_msg)

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

class ConfigString(_ConfigGen):

    """
    Configuration class used to store
    string-based CAM configuration
    options.

    Inputs to initalize class are:
    name                   -> Name of new CAM configure option
    desc                   -> Text description of CAM configure option
    val                    -> Integer value for CAM configure option
    valid_vals (optional)  -> List or regex of valid CAM configure option values (default is None)
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

    """

    def __init__(self, name, desc, val, valid_vals=None, is_nml_attr=False):

        # Add generic attributes
        super().__init__(name, desc, is_nml_attr=is_nml_attr)

        # If ok, then add valid_vals to object
        self.__valid_vals = valid_vals

        # Next, check that provided value is "valid" based on the
        # valid values list or regular expression. Note that this
        # function also checks valid_vals itself to ensure that it
        # is of the correct type and format.

        self.__check_value(val)

        # If everything is ok, then add provided value to object
        self.__value = val

    #++++++++++++++++++++++++

    # Create properties needed to return given value and valid values
    # without underscores
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

        # Check if string value is valid
        bad_val_msg = _check_string_val(self.name, val,
                                        valid_vals=self.valid_vals)

        # Raise an error if a bad value is found:
        if bad_val_msg:
            raise CamConfigValError(bad_val_msg)

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

class ConfigList(_ConfigGen):

    """
    Configuration class used to store list-based
    CAM configuration options.

    Inputs to initalize class are:
    name                   -> Name of new CAM configure option
    desc                   -> Text description of CAM configure option
    list_vals              -> List values for CAM configure option
    valid_type (optional)  -> Specify valid type for CAM configure option list values.
                              Currently accepts "int" for integer and "str" for string.
    valid_vals (optional)  -> Valid CAM configure option values (default is None),
                              valid_type must be included in order to use valid_vals.

    Doctests:

    1. Check that ConfigList works properly with no valid_type:

    >>> ConfigList("test", "test object description", [1,2,3]).value
    [1, 2, 3]

    2. Check that ConfigList works with a correct valid type provided:
    >>> ConfigList("test", "test object description", ["x", "y", "z"], valid_type="str").value
    ['x', 'y', 'z']


    3. Check that ConfigList With a non-string passed to "valid_type" fails with the correct error:
    >>> ConfigList("test", "test object description", [1, 2, 3], valid_type=5).value #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigTypeError: ERROR: valid_type entry for variable 'test' must be a string,  not type '<class 'int'>'.

    4. Check that ConfigList with a non-recognized "valid_type" option fails with the correct error:
    >>> ConfigList("test", "test object description", [1, 2, 3], valid_type="foo").value #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigValError: ERROR: 'foo' is not a recognized option for 'valid_type'. Please use either 'int' or 'str'.

    5.  Check that ConfigList with list entries that don't match the valid_type entry fails with the correct error:
    >>> ConfigList("test", "test object description", [1, 2, 3], valid_type="str").value #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigValError: ERROR: The following list entries, provided for variable, 'test', are not strings, but instead are:
    '1': type='<class 'int'>'
    '2': type='<class 'int'>'
    '3': type='<class 'int'>'

    6.  Check that ConfigList with "valid_vals" but no "valid_type" fails with the correct error:
    >>> ConfigList("test", "test object description", [1, 2, 3], valid_vals=[1,2,3,4,5]).value #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigValError: ERROR: valid values can only be used if valid_type is 'int' or 'str', not 'None'.

    7.  check that ConfigList with a list that matches the "valid_vals" entry works as expected:
    >>> ConfigList("test", "test object description", [1, 2, 3], valid_type="int", valid_vals=(0,5)).value
    [1, 2, 3]

    8. check that ConfigList with a list that does not mach the "valid_vals" entry fails with the correct error:
    >>> ConfigList("test", "test object description", ["1", "b", "c"], valid_type="str", valid_vals=["1","2","3"]).value #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigValError: The following errors were found for a list-type config variable:
    ERROR:  Value, 'b', provided for variable, 'test', does not match any of the valid values: '['1', '2', '3']'
    <BLANKLINE>
    ERROR:  Value, 'c', provided for variable, 'test', does not match any of the valid values: '['1', '2', '3']'

    9. check that ConfigList with a list that does not mach the "valid_vals" range fails with the correct error:
    >>> ConfigList("test", "test object description", [1, 2, 6], valid_type="int", valid_vals=(0,5)).value #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    cam_config_classes.CamConfigValError: The following errors were found for a list-type config variable:
    ERROR:  Value, '6', provided for variable, 'test', is outside valid value range, '(0, 5)'

    """

    def __init__(self, name, desc, val, valid_type=None, valid_vals=None):

        # Add generic attributes
        super().__init__(name, desc, is_nml_attr=False)

        # Check if valid_type is not None
        if valid_type is not None:
            # If not None, make sure valid_type is a string:
            if not isinstance(valid_type, str):
                emsg = "ERROR: valid_type entry for variable '{}' must be a string, "
                emsg += " not type '{}'."
                raise CamConfigTypeError(emsg.format(name, type(valid_type)))
            # End if
        # End if

        # Check that the valid values option is only being used with a valid type:
        if valid_vals is not None and valid_type not in _VALID_TYPE_SET:
            # Currently valid values can only be used with strings or integers,
            # so throw an error:
            emsg = "ERROR: valid values can only be used if valid_type is 'int' or 'str', not '{}'."
            raise CamConfigValError(emsg.format(valid_type))

        # If ok, then add valid_type and valid_vals to object
        self.__valid_type = valid_type
        self.__valid_vals = valid_vals

        # Next, check that provided list entry types and values are "valid"
        # based on the valid type and valid values provided:
        if self.__valid_type is not None:
            self.__check_type(val)

            #If valid values are provided, then check them as well:
            if self.__valid_vals is not None:
                self.__check_values(val)

        # If everything is ok, then add provided value to object
        self.__value = val

    #++++++++++++++++++++++++

    # Create properties needed to return given value and valid values
    # without underscores
    @property
    def value(self):
        """Return the value of this config object"""
        return self.__value

    @property
    def valid_type(self):
        """Return the valid type of this config object"""
        return self.__valid_type

    @property
    def valid_vals(self):
        """Return the valid values of this config object"""
        return self.__valid_vals

    #++++++++++++++++++++++++

    def __check_type(self, val):

        """
        Check if the entries in the provided
        list (val) are of the correct type as
        specified by the "valid_type" entry.
        """

        # Extract valid type (valid_type) from object
        valid_type = self.valid_type

        # Create empty dictionary to store errors:
        bad_val_type_msgs = []

        good_type = "??"
        if valid_type == "str":
            #All list entries should be strings:
            good_type = "string"
            for list_entry in val:
                if not isinstance(list_entry, str):
                    bad_val_type_msgs.append(f"'{list_entry}': type='{type(list_entry)}'")
                # end if
            # end for
        elif valid_type == "int":
            #All list entries should be integers:
            good_type = "int"
            for list_entry in val:
                if not isinstance(list_entry, int):
                    bad_val_type_msgs.append(f"'{list_entry}': type='{type(list_entry)}'")
                # end if
            # end for
        else:
            #Invalid option given for "valid_type", so raise error:
            emsg = "ERROR: '{}' is not a recognized option for 'valid_type'."
            emsg += " Please use either 'int' or 'str'."
            raise CamConfigValError(emsg.format(valid_type))
        # End if
        #If bad values dictionary is non-empty, then raise error:
        if bad_val_type_msgs:
            if len(bad_val_type_msgs) > 1:
                emsg = "ERROR: The following list entries, provided for variable,"
                emsg += f" '{self.name}', are not {good_type}s, but instead are:\n"
            else:
                emsg = "ERROR: The following list entry, provided for variable,"
                emsg += f" '{self.name}', is not a {good_type}, but instead is: "
            # end if
            emsg += '\n'.join(bad_val_type_msgs)
            raise CamConfigValError(emsg)
        # End if

    #++++++++++++++++++++++++

    def __check_values(self, list_vals):

        """
        Check if the entries in the provided
        list (val) are valid as specified by
        specified by the "valid_vals" entry.
        """

        # Create empty list:
        bad_val_msgs = []

        # Check if valid type is string or integer
        if self.valid_type in _TYPE_CHECK_FUNCTIONS:
            for val in list_vals:
                #Check if integer or string value in list is valid
                bad_val_msg = _TYPE_CHECK_FUNCTIONS[self.valid_type](self.name, val,
                                                                     valid_vals=self.valid_vals)
                # If return value is not None, then add
                # to bad value list
                if bad_val_msg:
                    bad_val_msgs.append(bad_val_msg)
                # End if
            # end for
        else:
            emsg = "Internal Error: Bad valid_type, '{}'"
            raise CamConfigTypeError(emsg.format(self.valid_type))
        # end if

        # If bad values are present, then raise an error
        if bad_val_msgs:
            emsg = "The following errors were found for a list-type config variable:\n"
            emsg += "\n\n".join(bad_val_msgs)
            raise CamConfigValError(emsg)
        # End if

    #++++++++++++++++++++++++

    def set_value(self, list_vals):

        """
        Set configure object's value to the one provided.
        """

        # First, check that the provided value is valid
        if self.__valid_type is not None:
            self.__check_type(list_vals)

        # If ok, then set object's value to one provided
        self.__value = list_vals

#############
# End of file
#############
