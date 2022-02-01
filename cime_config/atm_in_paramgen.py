"""
Wrapper-class for the ParamGen
CIME tool, and associated methods
needed to generated the "atm_in"
Fortran namelist file.
"""

#----------------------------------------
# Import generic python libraries/modules
#----------------------------------------

import os
import os.path
import sys
import re
from collections import OrderedDict
#CAM specific config error:
from cam_config_classes import CamConfigValError
from cam_config_classes import CamConfigTypeError

#----------------
# Import ParamGen
#----------------

_CIME_CONF_DIR = os.path.abspath(os.path.dirname(__file__))
_CIME_ROOT = os.path.join(_CIME_CONF_DIR, os.pardir, "cime")

if not os.path.exists(_CIME_ROOT):
    raise SystemExit("ERROR: Cannot find 'cime' directory.  Did you run checkout_externals?")
sys.path.append(os.path.join(_CIME_ROOT, "scripts", "lib", "CIME", "ParamGen"))
from paramgen import ParamGen

##############################
# MAIN "atm_in" ParamGen class
##############################

class AtmInParamGen(ParamGen):
    """
    Encapsulates data and read/write methods for
    the atm_in Fortran namelist file and ParamGen
    object.
    """

    def __init__(self, pg_data_dict):

        """
        Initialize a ParamGen object directly
        using a ParamGen data dictionary, and
        create a new dictionary to match namelist
        variables to their associated groups
        when reading in "user_nl_cam".
        ----------
        pg_data_dict -> python dictionary with ParamGen keys/values

        """

        #Initialize ParamGen directly:
        super().__init__(pg_data_dict)

        #Create namelist var/group dictionary,
        #which used by the "append_user_nl_file"
        #method:
        self._var_group_dict = {}

        #Create empty dictionaries that will contain
        #the namelist definition file and the set
        #of all namelist groups and variables:
        self._nml_def_groups = {}
        self._nml_def_vars   = {}

    ####

    @classmethod
    def from_namelist_xml(cls, nml_xml_file):

        """
        Initialize atm_in ParamGen object with XML file,
        ----------
        nml_xml_file -> path (str) to namelist definition XML file

        """

        #Create ParamGen object using base class:
        _pg_xml = ParamGen.from_xml_nml(nml_xml_file, no_duplicates=True)

        #Initialize new "atm_in" object:
        atm_in_pg = AtmInParamGen(_pg_xml.data)

        #Check if the new ParamGen object has all of the required
        #namelist elements:
        #----------------
        missing_elems = atm_in_pg.check_nml_def_elems()

        if missing_elems:
            emsg = "The XML namelist definition file:\n"
            emsg += f"{nml_xml_file}\n"
            emsg += "has namelist entries that are missing required elements.\n"
            emsg += "Those entries and missing elements are:\n"
            for entry_id, missing_elems in missing_elems.items():
                emsg += f"{entry_id} : {', '.join(missing_elems)}\n"
            raise CamConfigValError(emsg)
        #----------------

        #Initialize file->group/var set dictionary:
        atm_in_pg._nml_def_groups[nml_xml_file] = set()
        atm_in_pg._nml_def_vars[nml_xml_file] = set()

        #Create namelist variable/group dictionary
        #and associated sets:
        #----------------
        for nml_group in atm_in_pg._data:
            for var in atm_in_pg._data[nml_group]:

                #Check if variable already exists in dictionary:
                if var in atm_in_pg._var_group_dict:
                    #No duplicate variables are allowed, even if
                    #in separate namelist groups, so raise an error.
                    #Please note that this error should always be
                    #caught earlier than this, so if it gets to this
                    #point something has gone seriously wrong:
                    emsg = f"Namelist entry id '{var}' exists"
                    emsg += f" in namelist group '{nml_group}'"
                    emsg += f" and '{self.__var_group_dict[var]}'\n"
                    emsg += "Namelist variables can belong to only one group."
                    raise SystemError(emsg)
                else:
                    #If not, then add variable and group to dictionary:
                    atm_in_pg._var_group_dict[var] = nml_group

                    #Add namelist groups and variables to their
                    #respective sets:
                    atm_in_pg._nml_def_groups[nml_xml_file].add(nml_group)
                    atm_in_pg._nml_def_vars[nml_xml_file].add(var)

        #----------------

        #Return object:
        return atm_in_pg

    ####

    def check_nml_def_elems(self):

        """
        Function that checks if certain namelist definition
        file elements/tags that are optional for ParamGen
        but required by CAM/SIMA are present in the provided
        ParamGen atm_in object.
        """

        #Please note that "group" and "values" are automatically
        #required by the ParamGen schema.

        #Required namelist elements:
        req_elems = ["type", "desc", "category"]

        #Set missing  attributes dictionary:
        missing_elems = {}

        #Assume it is a ParamGen object, and loop over namelist groups:
        for nml_group in self._data:
            #Now loop over variables in group:
            for var in self._data[nml_group]:
                #Lastly loop over required namelist elements:
                for req_elem in req_elems:
                    #Check if required element is present:
                    if not req_elem in self._data[nml_group][var]:
                        #Add missing attribute to dictionary:
                        if var in missing_elems:
                            missing_elems[var].append(req_elem)
                        else:
                            missing_elems[var] = [req_elem]
                        #End if
                    #End if
                #End for
            #End for
        #End for

        #Return missing elements dictionary:
        return missing_elems

    ####

    def append_atm_in_pg(self, atm_pg_obj):

        """
        Append a new AtmInParamGen object
        to this one, ensuring that there are
        no duplicate namelist groups or variables.
        ----------
        atm_pg_obj -> An AtmInParamGen object

        """

        #Make sure there is only one XML file associated with
        #input PG object:
        if len(atm_pg_obj._nml_def_groups.keys()) > 1:
            emsg = "ParamGen object being appended to another must"
            emsg += " be associated with only one namelist definition file."
            emsg += "\nInstead it is associated with the following files:\n"
            emsg += "\n".join(atm_pg_obj._nml_def_groups.keys())
            raise CamConfigValError(emsg)
        else:
            #Extract namelist definition file name:
            input_file = next(iter(atm_pg_obj._nml_def_groups))

        #Extract the group and variable sets from input PG object:
        input_groups = atm_pg_obj._nml_def_groups[input_file]
        input_vars   = atm_pg_obj._nml_def_vars[input_file]

        #Check that there are no matching namelist groups:
        #------------------------------------------------
        for nml_file, nml_groups in self._nml_def_groups.items():

            #Determine if any namelist groups are the same
            #between the two objects:
            same_groups = nml_groups.intersection(input_groups)

            #If so, then raise an error (as all namelist groups must be unique):
            if same_groups:
                emsg = f"Both\n'{nml_file}'\nand\n'{input_file}'\nhave"
                emsg += " the following conflicting namelist groups:\n"
                emsg += ", ".join(same_groups)
                raise CamConfigValError(emsg)

        #------------------------------------------------

        #Check that there are no matching namelist variables:
        #------------------------------------------------
        for nml_file, nml_vars in self._nml_def_vars.items():

            #Determine if any namelist groups are the same
            #between the two objects:
            same_vars = nml_vars.intersection(input_vars)

            #If so, then raise an error (as all namelist variable ids must be unique):
            if same_vars:
                emsg = f"Both\n'{nml_file}'\nand\n'{input_file}'\nhave"
                emsg += " the following conflicting namelist variables:\n"
                emsg += ", ".join(same_vars)
                raise CamConfigValError(emsg)

        #------------------------------------------------

        #Add input PG object dictionaries to this object's dicts:
        self._nml_def_groups.update(atm_pg_obj._nml_def_groups)
        self._nml_def_vars.update(atm_pg_obj._nml_def_vars)

        #Append input PG object to this object:
        self.append(atm_pg_obj)

    ####

    def append_user_nl_file(self, user_nl_file):
        """
        Reads in user_nl_cam files and converts
        them to the proper ParamGen syntax.
        ----------
        user_nl_file -> path (str) to user_nl_cam file

        """

        _data = OrderedDict()
        with open(user_nl_file,'r') as user_file:
            within_comment_block = False
            for line in user_file:
                if len(line)>1:
                    line_s = line.split()

                    # check if within comment block.
                    if (not within_comment_block) and line.strip()[0:2] == "/*":
                        within_comment_block = True

                    if within_comment_block and line.strip()[-2:] == "*/":
                        within_comment_block = False
                        continue

                    if not within_comment_block and line_s[0][0] != "!": # not a single comment line either

                        #Join string elements back together:
                        line_j = ' '.join(line_s)

                        # now parse the line:
                        if ("=" in line_j):
                            line_ss   = line_j.split("=")
                            var_str   = (line_ss[0]).strip()  # the first element is the parameter name
                            val_str   = ' '.join(line_ss[1:]) # the rest is tha value string
                            if '!' in val_str:
                                val_str = val_str.split("!")[0] # discard the comment in val str, if one exists

                            #Check if variable already exists in group dictionary:
                            if var_str in self._var_group_dict:
                                #Extract namelist group list for variable:
                                data_group = self._var_group_dict[var_str]

                            else:
                                #Raise error that namelist variable isn't listed in
                                #anywhere in a definition file:
                                emsg = "Variable '{}' not found in any namelist definition files."
                                emsg += " Please double-check '{}'."
                                raise CamConfigValError(emsg.format(var_str, user_nl_file))

                            #Add the namelist group if not already in data dict:
                            if not data_group in _data:
                                _data[data_group] = {}

                            #Check if variable already exists in data dictionary:
                            if var_str in _data[data_group]:
                                emsg = "Namelist variable '{}' set more than once in '{}'"
                                emsg += "\nPlease set each variable only once."
                                raise CamConfigValError(emsg.format(var_str, user_nl_file))

                            #Enter the parameter in the dictionary:
                            _data[data_group][var_str] = {'values':val_str}
                        else:
                            emsg = "Cannot parse the following line in '{}' :\n'{}'"
                            raise CamConfigValError(emsg.format(user_nl_file, line))

            #Check if there is unclosed block:
            if within_comment_block:
                raise CamConfigValError(f"Un-closed comment block!  Please check '{user_nl_file}'")

        #Create new ParamGen object:
        pg_user = ParamGen(_data)

        #Append new user_nl_cam object to main atm_in namelist object:
        self.append(pg_user)

    ####

    def write(self, output_path):

        """
        Write data to Fortran namelist file.
        ----------
        output_path   -> path (str) to Fortran namelist (atm_in) file

        """

        #Compile regular expression to determine if variable value
        #is a number or Fortran logical.

        #All "values" stored in ParamGen are strings.  However, booleans and numbers
        #(either integers or reals) shouldn't have wrapping quotes when written to the
        #fortran namelist.  Thus the value needs to be evaluated to see if it is actually
        #a fortran boolean, integer, or real. This done using the following regular expressions:
        #--------------------------------------------------------------------------------------

        # Make sure ParamGen object has been reduced:
        if not self.reduced:
            emsg = "ParamGen object for atm_in must be reduced before being "
            emsg += "written to file. Please check CAM's buildnml script."
            raise SystemError(emsg)

        #Create sets for string evaluation below:
        num_bool_set = {"integer", "real", "logical"} #types that don't need quotes
        quote_set = {"'", '"'}                        #single and double quotes

        # Write Fortran namelist file:
        with open(os.path.join(output_path), 'w') as atm_in_fil:
            #Loop through namelist groups in alphabetical order:
            for nml_group in sorted(self._data):
                # Write namelist group:
                atm_in_fil.write("&"+nml_group+"\n")

                # Write all variables within that group (sorted alphabetically):
                for var in sorted(self._data[nml_group]):
                    #Extract variable value(s):
                    val = self._data[nml_group][var]["values"].strip()

                    #If no value is set then move to the next variable:
                    if val==None:
                        continue

                    #Extract variable type:
                    if "type" in self._data[nml_group][var]:
                        var_type = self._data[nml_group][var]["type"].strip()
                    else:
                        emsg = f"Namelist entry '{var}' is missing required 'type' element."
                        raise CamConfigValError(emsg)

                    #Check if variable value is a number or boolean:
                    if var_type in num_bool_set:
                        if var_type == 'logical':
                            #If logical, then write the associated truth value:
                            if self._is_nml_logical_true(var, val):
                                atm_in_fil.write(f"    {var} = .true.\n")
                            else:
                                atm_in_fil.write(f"    {var} = .false.\n")
                        else:
                            #If a number, then write value as-is:
                            atm_in_fil.write("    {} = {}\n".format(var, val))
                    elif "char*" in var_type:
                        #Value is a string, so check if is already inside quotes:
                        if val[0] in quote_set and val[-1] == val[0]:
                            #If so, then write string value as-is:
                            atm_in_fil.write("    {} = {}\n".format(var, val))
                        else:
                            #If not, then write string with added quotes:
                            atm_in_fil.write("    {} = '{}'\n".format(var, val))
                    else:
                        #This is an un-recognized type option, so raise an error:
                        emsg = f"Namelist type '{var_type}' for entry '{var}' is un-recognized.\n"
                        emsg += "Acceptable namelist types are: logical, integer, real, or char*N."
                        raise CamConfigValError(emsg)

                # Add space for next namelist group:
                atm_in_fil.write('/\n\n')

    ####

    def reduce_atm_in(self, case, atm_attr_dict):

        """
        Reduce XML namelist attributes
        (i.e. replace attribute/guard dictionary with value)
        ----------
        case          -> CIME case object
        atm_attr_dict -> dictionary containing attribute values

        """

        # Set internal variables for use by "expand_func":
        self.__case = case
        self.__atm_attr_dict = atm_attr_dict

        # Reduce Param Data:
        self.reduce(self.__expand_func)

    ####

    def __expand_func(self, varname):

        """
        Function used to convert $XXX
        variables and XML attributes to
        their associated values.
        """

        #Check if varname matches a CIME case variable:
        val = self.__case.get_value(varname)

        #If not, then attempt to extract variable from
        #attribute dictionary:
        if val is None:
            if varname in self.__atm_attr_dict:
                val = self.__atm_attr_dict[varname]
            else:
                #Assume the XML attribute/guard is an empty string:
                val = ""

        #Return value if found:
        return val

    ####

    def _is_nml_logical_true(self, varname, var_val):

        """
        Checks if a "logical" XML namelist value is true or
        false.
        ----------
        varname -> The name of the variable being checked
        var_val -> The value of the variable being checked

        doctests:

        1. Check that a True value returns true:
        >>> AtmInParamGen({})._is_nml_logical_true("test", True)
        True

        2.  Check that a "true" value returns true:
        >>> AtmInParamGen({})._is_nml_logical_true("test", "true")
        True

        3.  Check that a ".true." value returns true:
        >>> AtmInParamGen({})._is_nml_logical_true("test", ".true.")
        True

        4.  Check that a "1" value returns true:
        >>> AtmInParamGen({})._is_nml_logical_true("test", "1")
        True

        5.  Check that a 1 (integer) value returns true:
        >>> AtmInParamGen({})._is_nml_logical_true("test", 1)
        True

        6.  Check that a False value returns false:
        >>> AtmInParamGen({})._is_nml_logical_true("test", False)
        False

        7.  Check that a "FALSE" value returns false:
        >>> AtmInParamGen({})._is_nml_logical_true("test", "FALSE")
        False

        8.  Check that a ".False." value returns false:
        >>> AtmInParamGen({})._is_nml_logical_true("test", ".False.")
        False

        9.  Check that a "0" value returns false:
        >>> AtmInParamGen({})._is_nml_logical_true("test", "0")
        False

        10.  Check that a 0 (integer) value returns false:
        >>> AtmInParamGen({})._is_nml_logical_true("test", 0)
        False

        11.  Check that a bad string value returns the correct error:
        >>> AtmInParamGen({})._is_nml_logical_true("test", "this_wont_work") # doctest: +ELLIPSIS
        Traceback (most recent call last):
        ...
        cam_config_classes.CamConfigValError:...
        XML namelist logical variable, 'test', must have a value of true, false, 1, or 0, not 'this_wont_work'

        12.  Check that a bad integer value returns the correct error:
        >>> AtmInParamGen({})._is_nml_logical_true("test", 3) # doctest: +ELLIPSIS
        Traceback (most recent call last):
        ...
        cam_config_classes.CamConfigValError:...
        XML namelist logical variable, 'test', must have a value of true, false, 1, or 0, not 3

        13.  Check that a non-boolean, string or integer type returns an error:
        >>> AtmInParamGen({})._is_nml_logical_true("test", 13.03) # doctest: +ELLIPSIS
        Traceback (most recent call last):
        ...
        cam_config_classes.CamConfigTypeError:...
        XML namelist variable 'test' must have a value that is either a boolean, string, or integer, not float.

        """

        if isinstance(var_val, bool):
            return var_val
        elif isinstance(var_val, str):
            if var_val.lower() in {"true", ".true.", "1"}:
                return True
            elif var_val.lower() in {"false", ".false.", "0"}:
                return False
            else:
                emsg = f"\nXML namelist logical variable, '{varname}'"
                emsg += ", must have a value of true, false, 1, or 0, not"
                emsg += f" '{var_val}'"
                raise CamConfigValError(emsg)
        elif isinstance(var_val, int):
            if var_val == 1:
                return True
            elif var_val == 0:
                return False
            else:
                emsg = f"\nXML namelist logical variable, '{varname}'"
                emsg += ", must have a value of true, false, 1, or 0, not"
                emsg += f" {var_val}"
                raise CamConfigValError(emsg)
        else:
            emsg = f"\nXML namelist variable '{varname}' must"
            emsg += " have a value that is either a boolean, string, or integer,"
            emsg += f" not {type(var_val).__name__}."
            raise CamConfigTypeError(emsg)

############
#End of file
