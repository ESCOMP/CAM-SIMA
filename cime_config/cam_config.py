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
import sys
import argparse
import os.path

from collections import OrderedDict

#-----------------------------------
# Import CAM-specific python modules
#-----------------------------------

# Import build cache object:
from cam_build_cache import BuildCacheCAM # Re-build consistency cache

# Import fortran auto-generation routines:
from cam_autogen import generate_registry, generate_physics_suites
from cam_autogen import generate_init_routines


# Determine regular rexpression type (for later usage in check_string_val)
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
    CamConfigTypeError: ERROR: Value being checked in 'check_integer_val' must be an integer type, not '<class 'float'>'.

    2.  Check that using a valid_vals option that is not a list or tuple throws an error:
    >>> _check_integer_val("test", 5, valid_vals="test_vals") #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    CamConfigTypeError: ERROR: Valid values for integers must by provided as either a tuple or a list, not '<class 'str'>'.

    3.  Check that using non-integer values inside the valid_vals list or tuple throws an error:
    >>> _check_integer_val("test", 5, valid_vals=[1,2,5,"test_val"]) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    CamConfigTypeError: ERROR:  Valid value, 'test_val', for variable 'test', must be an integer.  Currently it is '<class 'str'>'.
    <BLANKLINE>

    4.  Check that using a tuple with only one entry throws an error:
    >>> _check_integer_val("test", 5, valid_vals=(1,)) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    CamConfigValError: ERROR: Valid values tuple for variable, 'test', must have two elements, not '1' elements.

    5.  Check that using a tuple with more than two entries throws an error:
    >>> _check_integer_val("test", 5, valid_vals=(1,2,5)) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    CamConfigValError: ERROR: Valid values tuple for variable, 'test', must have two elements, not '3' elements.

    6.  Check that using a tuple with only Nones throws an error:
    >>> _check_integer_val("test", 5, valid_vals=(None,None)) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    CamConfigValError: ERROR: Valid values tuple for variable, 'test', must contain at least one integer.

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
            if not val in valid_vals:
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
    CamConfigTypeError: ERROR: Value being checked in 'check_string_val' must be a string type, not '<class 'list'>'.

    2.  Check that using a valid_vals option that is not None, a list, or a regex throws an error:
    >>> _check_string_val("test", "test_val", valid_vals=5) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    CamConfigTypeError: ERROR: Valid values for strings must by provided as either a regular expression or a list, not '<class 'int'>'.

    3.  Check that using non-string values inside the valid_vals list throws an error:
    >>> _check_string_val("test", "1", valid_vals=["1","2","5",6]) #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    CamConfigTypeError: ERROR:  Valid value, '6', for variable 'test', must be a string.  Currently it is '<class 'int'>'.

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
        elif isinstance(valid_vals, REGEX_TYPE):
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

###############################################################################
# CAM configure option classes
###############################################################################

class ConfigGen(object):

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
    '# test object description'

    >>> print(ConfigGen("test", ["test", "object", "description"]).desc)
    # test
    #    object
    #    description

    >>> ConfigGen("test", "test object description", is_nml_attr=True).is_nml_attr
    True

    2.  Check that non-optional inputs must be strings:

    >>> ConfigGen(5, "test_object_description").name #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigTypeError: ERROR:  Configuration variable name '5' must be a string, not <type 'int'>

    >>> ConfigGen("test", (5,)).desc #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigTypeError: ERROR:

    >>> ConfigGen("test", ["test", ("object", "description")]).desc #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigTypeError: ERROR:  Configuration variable, 'test', must have a string-type description, or a list of string-type descriptions, not [<class 'str'>, <class 'tuple'>]

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
                derr = "{} ({})".format(type(desc), desc)
            # end if
            raise CamConfigTypeError(emsg.format(name, derr))
        # end if

        # Add name, description, and namelist attribute logical to object
        self.__name = name
        if isinstance(desc, str):
            self.__desc = "# {}".format(desc)
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

    """

    def __init__(self, name, desc, val, valid_vals=None, is_nml_attr=False):

        # Add generic attributes
        super(ConfigInteger, self).__init__(name, desc, is_nml_attr=is_nml_attr)

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

class ConfigString(ConfigGen):

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
        super(ConfigString, self).__init__(name, desc, is_nml_attr=is_nml_attr)

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

class ConfigList(ConfigGen):

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
    CamConfigTypeError: ERROR: valid_type entry for variable 'test' must be a string,  not type '<class 'int'>'.

    4. Check that ConfigList with a non-recognized "valid_type" option fails with the correct error:
    >>> ConfigList("test", "test object description", [1, 2, 3], valid_type="foo").value #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    CamConfigValError: ERROR: 'foo' is not a recognized option for 'valid_type'. Please use either 'int' or 'str'.

    5.  Check that ConfigList with list entries that don't match the valid_type entry fails with the correct error:
    >>> ConfigList("test", "test object description", [1, 2, 3], valid_type="str").value #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    CamConfigValError: ERROR: The following list entries, provided for variable, 'test', are not strings, but instead are:
    '1': type='<class 'int'>'
    '2': type='<class 'int'>'
    '3': type='<class 'int'>'
    <BLANKLINE>

    6.  Check that ConfigList with "valid_vals" but no "valid_type" fails with the correct error:
    >>> ConfigList("test", "test object description", [1, 2, 3], valid_vals=[1,2,3,4,5]).value #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    CamConfigValError: ERROR: valid values can only be used if valid_type is 'int' or 'str', not 'None'.

    7.  check that ConfigList with a list that matches the "valid_vals" entry works as expected:
    >>> ConfigList("test", "test object description", [1, 2, 3], valid_type="int", valid_vals=(0,5)).value
    [1, 2, 3]

    8. check that ConfigList with a list that does not mach the "valid_vals" entry fails with the correct error:
    >>> ConfigList("test", "test object description", ["1", "b", "c"], valid_type="str", valid_vals=["1","2","3"]).value #doctest: +ELLIPSIS
    Traceback (most recent call last):
    ...
    CamConfigValError: The following errors were found for a list-type config variable:
    ERROR:  Value, 'b', provided for variable, 'test', does not match any of the valid values: '['1', '2', '3']'
    <BLANKLINE>
    ERROR:  Value, 'c', provided for variable, 'test', does not match any of the valid values: '['1', '2', '3']'
    """

    def __init__(self, name, desc, val, valid_type=None, valid_vals=None):

        # Add generic attributes
        super(ConfigList, self).__init__(name, desc, is_nml_attr=False)

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
        if valid_vals is not None and valid_type not in ["int", "str"]:
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
        bad_val_types = OrderedDict()

        good_type = "??"
        if valid_type == "str":
            #All list entries should be strings:
            good_type = "string"
            for list_entry in val:
                if not isinstance(list_entry, str):
                    bad_val_types[str(list_entry)] = str(type(list_entry))
                # end if
            # end for
        elif valid_type == "int":
            #All list entries should be integers:
            good_type = "int"
            for list_entry in val:
                if not isinstance(list_entry, int):
                    bad_val_types[str(list_entry)] = str(type(list_entry))
                # end if
            # end for
        else:
            #Invalid option given for "valid_type", so raise error:
            emsg = "ERROR: '{}' is not a recognized option for 'valid_type'."
            emsg += " Please use either 'int' or 'str'."
            raise CamConfigValError(emsg.format(valid_type))
        # End if
        #If bad values dictionary is non-empty, then raise error:
        if bad_val_types:
            if len(bad_val_types) > 1:
                emsg = "ERROR: The following list entries, provided for variable,"
                emsg += " '{}', are not {}s, but instead are:\n".format(self.name, good_type)
            else:
                emsg = "ERROR: The following list entry, provided for variable,"
                emsg += " '{}', is not a {}, but instead is: ".format(self.name, good_type)
            # end if
            for key_str, type_str in bad_val_types.items():
                emsg += "'{}': type='{}'\n".format(key_str, type_str)
            # end for
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

    With a given list value:
    >>> FCONFIG.create_config("test_list", "test object description", [1, 2])
    >>> FCONFIG.get_value("test_list")
    [1, 2]

    2.  Check that the same configure object can't be created twice:

    >>> FCONFIG.create_config("test_int", "test object description", 5) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigValError: ERROR:  The CAM config variable, 'test_int', already exists!  Any new config variable must be given a different name

    3.  Check that a configure object's given value must be either a string, integer or list:

    >>> FCONFIG.create_config("test_dict", "test_object_description", {"x": "y"}) #doctest: +IGNORE_EXCEPTION_DETAIL
    Traceback (most recent call last):
    CamConfigTypeError: ERROR:  The input value for new CAM config variable, 'test_dict', must be either an integer or a string, not <type 'list'>

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
        exeroot = case.get_value("EXEROOT")                 # Model executable path
        nthrds = case.get_value("NTHRDS_ATM")               # Number of model OpenMP threads
        start_date = case.get_value("RUN_STARTDATE")        # Model simulation starte date

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
            self.__cppdefs = list()

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
            emsg = "ERROR: '{}' is not a valid dycore,".format(user_dyn_opt)
            emsg += "\n       Valid values: {}".format(dyn_valid_vals)
            raise CamConfigValError(emsg)
        # End if (no else, dyn is valid
        #-----------------------------------------------

        # Create empty dictonary
        self.__config_dict = dict()

        # Create namelist group list, starting with default namelist groups
        self.__nml_groups = ['cam_initfiles_nl',
                             'cam_logfile_nl',
                             'physics_nl',
                             'qneg_nl',
                             'vert_coord_nl',
                             'ref_pres_nl']

        #----------------------------------------------------
        # Set CAM start date (needed for namelist generation)
        #----------------------------------------------------

        # Remove dashes from CIME-provided start date:
        start_date_cam = start_date.replace('-','')

        self.create_config("ic_ymd", "Start date of model run.",
                           start_date_cam, is_nml_attr=True)

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

            # Source code directories
            self.create_config("dyn_src_dirs", dyn_dirs_desc, ["se",os.path.join("se","dycore")],
                               valid_list_type="str")

            # Add SE namelist groups to nmlgen list
            self.__nml_groups.append("air_composition_nl")
            self.__nml_groups.append("dyn_se_nl")

            # Add required CPP definitons:
            self.add_cppdef("_MPI")
            self.add_cppdef("SPMD")

            # Add OpenMP CPP definitions, if needed:
            if nthrds > 1:
                self.add_cppdef("_OPENMP")

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

            # Source code directories
            self.create_config("dyn_src_dirs", dyn_dirs_desc, ["none"],
                               valid_list_type="str")

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

        #---------------------------------------
        # Set CAM grid variables (nlat and nlon)
        #---------------------------------------

        #Set horizontal dimension variables:
        if dyn == "se":

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

        #---------------------------------------
        # Set initial and/or boundary conditions
        #---------------------------------------

        # Check if user specified Analytic Initial Conditions (ICs):
        if user_config_opts.analytic_ic:
            # Set "analytic_ic" to True (1):
            analy_ic_val = 1 #Use Analytic ICs

            # Add analytic_ic to namelist group list:
            self.__nml_groups.append("analytic_ic_nl")

            #Add new CPP definition:
            self.add_cppdef("ANALYTIC_IC")

        else:
            analy_ic_val = 0 #Don't use Analytic ICs

        analy_ic_desc = ["Switch to turn on analytic initial conditions for the dynamics state: ",
                         "0 => no ",
                         "1 => yes."]

        self.create_config("analytic_ic", analy_ic_desc,
                           analy_ic_val, [0, 1], is_nml_attr=True)

        #--------------------
        # Set ocean component
        #--------------------

        ocn_valid_vals = ["docn", "dom", "som", "socn",
                          "aquaplanet", "pop", "mom"]

        ocn_desc = ["The ocean model being used.",
                    "Valid values include prognostic ocean models (POP or MOM),",
                    "data ocean models (DOCN or DOM), a stub ocean (SOCN), ",
                    "and an aqua planet ocean (aquaplanet).",
                    "This does not impact how the case is built, only how",
                    "attributes are matched when searching for namelist defaults."]

        self.create_config("ocn", ocn_desc, comp_ocn,
                           ocn_valid_vals, is_nml_attr=True)

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
    def nml_groups(self):
        """Return the namelist groups list of this object."""
        return self.__nml_groups

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
        >>> ConfigCAM.parse_config_opts("", test_mode=True) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        SystemExit: 2
        >>> ConfigCAM.parse_config_opts("--dyn se", test_mode=True) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        SystemExit: 2
        >>> vlist(ConfigCAM.parse_config_opts("--physics-suites kessler"))
        [('analytic_ic', False), ('dyn', ''), ('dyn_kind', 'REAL64'), ('phys_kind', 'REAL64'), ('physics_suites', 'kessler')]
        >>> vlist(ConfigCAM.parse_config_opts("--physics-suites kessler --dyn se"))
        [('analytic_ic', False), ('dyn', 'se'), ('dyn_kind', 'REAL64'), ('phys_kind', 'REAL64'), ('physics_suites', 'kessler')]
        >>> vlist(ConfigCAM.parse_config_opts("--physics-suites kessler --dyn se --analytic_ic"))
        [('analytic_ic', True), ('dyn', 'se'), ('dyn_kind', 'REAL64'), ('phys_kind', 'REAL64'), ('physics_suites', 'kessler')]
        >>> vlist(ConfigCAM.parse_config_opts("--physics-suites kessler;musica"))
        [('analytic_ic', False), ('dyn', ''), ('dyn_kind', 'REAL64'), ('phys_kind', 'REAL64'), ('physics_suites', 'kessler;musica')]
        >>> ConfigCAM.parse_config_opts("--phys kessler musica", test_mode=True) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        SystemExit: 2
        """
        cco_str = "CAM_CONFIG_OPTS"

        #Don't allow abbreviations if using python 3.5 or greater:
        if sys.version_info[0] < 3 or (sys.version_info[0] == 3 and sys.version_info[1] < 5):
            parser = argparse.ArgumentParser(description=cco_str,
                                             prog="ConfigCAM",
                                             epilog="Allowed values of "+cco_str)
        else:
            parser = argparse.ArgumentParser(description=cco_str,
                                             prog="ConfigCAM", allow_abbrev=False,
                                             epilog="Allowed values of "+cco_str)


        parser.add_argument("--physics-suites", "-physics-suites", type=str,
                            required=True, metavar='<CCPP_SDFs>',
                            help="""Semicolon-separated list of Physics Suite
                                 Definition Files (SDFs)""")
        parser.add_argument("--dyn", "-dyn", metavar='<dycore>',
                            type=str, required=False, default="",
                            help="""Name of dycore""")
        parser.add_argument("--analytic_ic", "-analytic_ic",
                            action='store_true', required=False,
                            help="""Flag to turn on Analytic Initial
                                 Conditions (ICs).""")
        parser.add_argument("--dyn_kind", "-dyn_kind",
                            type=str, required=False, default="REAL64",
                            help="""Fortran kind used in dycore for type real.""")
        parser.add_argument("--phys_kind", "-phys_kind",
                            type=str, required=False, default="REAL64",
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
            raise  CamConfigValError("ERROR:  Invalid configuration name, '{}'".format(obj_name))

        # Print variable to logger
        case_log.debug("{}".format(obj.desc))
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

        # Also print CPP definitions, if any:
        if self.__cppdefs:
            case_log.debug("\nCAM CPP Defs: {}".format(" ".join(self.__cppdefs)))

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
            raise CamConfigValError("ERROR:  Invalid configuration name, '{}'".format(obj_name))

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

        Check that add_cppdef works properly:
        >>> FCONFIG.add_cppdef("TEST"); FCONFIG.cpp_defs
        ['-DTEST_CPPDEF', '-DNEW_TEST=5', '-DTEST']

        Check that add_cppdef works properly with provided value:
        >>> FCONFIG.add_cppdef("COOL_VAR", 100); FCONFIG.cpp_defs
        ['-DTEST_CPPDEF', '-DNEW_TEST=5', '-DTEST', '-DCOOL_VAR=100']

        Check that a duplicate cppdef creates an error:
        >>> FCONFIG.add_cppdef("TEST_CPPDEF") # doctest: +ELLIPSIS
        Traceback (most recent call last):
        ...
        CamConfigValError: ERROR: CPP definition 'TEST_CPPDEF' has already been set

        Check that a duplicate cppdef creates an error even if an equals sign
        is present in the stored copy but not the passed variable:
        >>> FCONFIG.add_cppdef("NEW_TEST") # doctest: +ELLIPSIS
        Traceback (most recent call last):
        ...
        CamConfigValError: ERROR: CPP definition 'NEW_TEST' has already been set
        """

        #Create string to check if CPP definition is already present:
        check_str = r"-D"+cppname

        #Check if CPP definition name already exists in CPP string list.
        #This is done because a CPP definition should only be set once,
        #in order to avoid variable overwriting or other un-expected
        #compiler behaviors:
        if any([re.match(check_str+r"($|=)", cppdef.strip()) for cppdef in self.__cppdefs]):
            #If match is found, then raise an error:
            emsg = "ERROR: CPP definition '{}' has already been set"
            raise CamConfigValError(emsg.format(cppname.upper()))

        # Check if input value is a logical:
        if value is None:
            # Create CPP flag string with no equals sign:
            cpp_str = check_str
        else:
            # Create CPP definition flag string:
            cpp_str = "{}={}".format(check_str, value)

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
            raise  CamConfigValError("ERROR:  Invalid configuration name, '{}'".format(obj_name))

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

        # Append CCPP-framework to python path:
        spin_scripts_path = os.path.join(self.__atm_root, "ccpp_framework", "scripts")

        # Check that CCPP-framework scripts directory exists:
        if not os.path.isdir(spin_scripts_path):
            emsg = ("ERROR: ccpp_framework/scripts directory doesn't exist! "
                    "Has 'checkout_externals' been run?")
            raise CamConfigValError(emsg)

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
        reg_dir, force_ccpp, reg_files = generate_registry(data_search,
                                                           build_cache, self.__atm_root,
                                                           self.__bldroot, source_mods_dir,
                                                           dyn, gen_fort_indent)

        #Add registry path to config object:
        reg_dir_desc = "Location of auto-generated registry code."
        self.create_config("reg_dir", reg_dir_desc, reg_dir)

        #---------------------------------------------------------
        # Call SPIN (CCPP Framework) to generate glue code
        #---------------------------------------------------------
        phys_dirs, force_init, cap_datafile = \
                               generate_physics_suites(spin_scripts_path,
                                                       build_cache, self.__cppdefs,
                                                       self.__atm_name, phys_suites,
                                                       self.__atm_root, self.__bldroot,
                                                       reg_dir, reg_files,
                                                       source_mods_dir, force_ccpp)

        #Convert physics directory list into a string:
        phys_dirs_str = ';'.join(phys_dirs)

        #Add physics directory paths to config object:
        phys_dirs_desc = "Locations of auto-generated CCPP physics codes."
        self.create_config("phys_dirs", phys_dirs_desc, phys_dirs_str)

        #---------------------------------------------------------
        # Create host model variable initialization routines
        #---------------------------------------------------------
        init_dir = generate_init_routines(spin_scripts_path, data_search,
                                          build_cache, self.__bldroot,
                                          reg_files, force_ccpp,
                                          force_init, gen_fort_indent,
                                          cap_datafile)

        #Add registry path to config object:
        init_dir_desc = "Location of auto-generated physics initilazation code."
        self.create_config("init_dir", init_dir_desc, init_dir)

        #--------------------------------------------------------------
        # write out the cache here as we have completed pre-processing
        #--------------------------------------------------------------
        build_cache.write()

    #++++++++++++++++++++++++

    def ccpp_phys_set(self, cam_nml_attr_dict, user_nl_file):

        """
        Find the physics suite to run.

        If more than one physics suite is available,
        then make sure the user has specified a physics
        suite from the list of available suites.

        If exactly one physics suite is available,
        then make sure that either the user did not
        specify a suite or that they did specify a
        suite and that it matches the available suite.

        """

        #Extract physics suite list:
        phys_suites = self.get_value('physics_suites').split(';')

        #Check the "user_nl_cam" file to see if user
        #specified a particular suite to use for this
        #simulation:
        with open(user_nl_file, 'r') as nl_file:
            #Read lines in file:
            nl_user_lines = nl_file.readlines()
        #End with

        #Break out "physics_suite" lines:
        phys_suite_lines = []
        for line in nl_user_lines:
            #Must check if line.lstrip is non-empty first,
            #Otherwise blank spaces in user_nl_cam will
            #cause problems:
            if line.lstrip():
                if line.lstrip()[0] != '!' and 'physics_suite' in line:
                    phys_suite_lines.append([x.strip() for x in line.split('=')])
                #End if
            #End if
        #End for

        if not phys_suite_lines:
            #If there is no "physics_suite" line,
            #then check if there is only one physics suite option:
            if len(phys_suites) == 1:
                #If so, then just use the only possible suite option:
                phys_suite_val = phys_suites[0]
            else:
                #If more than one option, then raise an error:
                emsg  = "No 'physics_suite' variable is present in user_nl_cam.\n"
                emsg += "This is required if more than one suite is listed\n"
                emsg += "in CAM_CONFIG_OPTS."
                raise CamConfigValError(emsg)
            #End if
        else:

            #If there is more than one "physics_suite" entry, then throw an error:
            if len(phys_suite_lines) > 1:
                emsg  = "More than one 'physics_suite' variable is present in user_nl_cam.\n"
                emsg += "Only one 'physics_suite' line is allowed."
                raise CamConfigValError(emsg)
            #End if

            #The split string list exists inside another, otherwise empty list, so extract
            #from empty list:
            phys_suite_list = phys_suite_lines[0]

            if len(phys_suite_list) == 1:
                #If there is only one string entry, then it means the equals (=) sign was never found:
                emsg = "No equals (=) sign was found with the 'physics_suite' variable."
                raise CamConfigValError(emsg)
            #End if

            if len(phys_suite_list) > 2:
                #If there is more than two entries, it means there were two or more equals signs:
                emsg = "There must only be one equals (=) sign in the 'physics_suite' namelist line."
                raise CamConfigValError(emsg)
            #End if

            #Remove quotation marks around physics_suite entry, if any:
            phys_suite_val = phys_suite_list[1].strip(''' "' ''')

            #Check that physics suite specified is actually in config list:
            if phys_suite_val not in phys_suites:
                emsg  = "physics_suite specified in user_nl_cam, '{}', doesn't match any suites\n"
                emsg += "listed in CAM_CONFIG_OPTS"
                raise CamConfigValError(emsg.format(phys_suite_val))
            #End if
        #End if (phys_suite_lines check).

        #Add new namelist attribute to dictionary:
        cam_nml_attr_dict["phys_suite"] = phys_suite_val


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
                "COMP_ATM" : "cam",
                "EXEROOT"  : "/some/made-up/path",
                "CASEROOT" : "/another/made-up/path",
                "CAM_CONFIG_OPTS" : "-dyn none --physics-suites adiabatic",
                "COMP_ROOT_DIR_ATM" : "/a/third/made-up/path",
                "CAM_CPPDEFS" : "-DTEST_CPPDEF -DNEW_TEST=5",
                "NTHRDS_ATM" : 1,
                "RUN_STARTDATE" : "101"
                }

        def get_value(self, key):

            """
            Function used to return value
            from conf_opts dictionary,
            with the key as input.
            """

            val = self.conf_opts[key]

            return val


    def vlist(nspace):
        """Convert a namespace into an ordered list view"""
        vargs = vars(nspace)
        return [(x, vargs[x]) for x in sorted(vargs)]

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
    TEST_SUCCESS = doctest.testmod()[0]

    # Exit script with error code matching number of failed tests:
    sys.exit(TEST_SUCCESS)

#############
# End of file
#############
