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
import sys
import re
from collections import OrderedDict
#CAM specific config error:
from cam_config_classes import CamConfigValError

#----------------
# Import ParamGen
#----------------

_CIMEROOT = os.environ.get("CIMEROOT")
if _CIMEROOT is None:
    raise SystemExit("ERROR: must set CIMEROOT environment variable")
sys.path.append(os.path.join(_CIMEROOT, "scripts", "lib", "CIME", "ParamGen"))
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

        #Create namelist var/group dictionary:
        self.__var_group_dict = {}

        for nml_group in self._data:
            for var in self._data[nml_group]:

                #Check if variable already exists in dictionary:
                if var in self.__var_group_dict:
                    #If so, then append group to list:
                    self.__var_group_dict[var].append(nml_group)
                else:
                    #If not, then add variable and group to dictionary:
                    self.__var_group_dict[var] = [nml_group]

    ####

    @classmethod
    def from_namelist_xml(cls, nml_xml_file):

        """
        Initialize atm_in ParamGen object with XML file,
        ----------
        nml_xml_file -> path (str) to namelist definition XML file

        """

        #Create ParamGen object using base class:
        _pg_xml = ParamGen.from_xml_nml(nml_xml_file)

        #Initialize new "atm_in" object:
        atm_in_pg = AtmInParamGen(_pg_xml.data)

        #Return object:
        return atm_in_pg

    ####

    @classmethod
    def from_user_nl_file(cls, user_nl_file):
        """
        Reads in a given "user_nl_cam" file or equivalent and initializes
        a AtmInParamGen object. This method is an alternative to the xml,
        yaml, and json methods already available from the base ParamGen class.
        ----------
        user_nl_file -> path (str) to namelist definition XML file

        """

        #Parse user_nl_cam file:
        _data = AtmInParamGen._read_user_input(user_nl_file)

        #Create new ParamGen object:
        atm_in_paramgen_obj = AtmInParamGen(_data)

        return atm_in_paramgen_obj

    ####

    def append_user_nl_file(self, user_nl_file):
        """
        Reads in user_nl_cam files and converts
        them to the proper ParamGen syntax.
        """

        _data = OrderedDict()
        with open(user_nl_file,'r') as user_file:
            within_comment_block = False
            curr_group = "UNSET"
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
                        # check format:
                        if curr_group == "UNSET" and line.strip()[0] == "&":
                            curr_group = line.strip()[1:]
                        elif curr_group != "UNSET" and line.strip()[0] == "/":
                            curr_group = "UNSET"
                        else:

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
                                if var_str in self.__var_group_dict:
                                    #Extract namelist group list for variable:
                                    var_group_list = self.__var_group_dict[var_str]

                                    #Check if no group has been assigned (i.e. it is "Global"):
                                    if curr_group == "UNSET":
                                        #If only one group option exists, then assign that group:
                                        if len(var_group_list) == 1:
                                            data_group = var_group_list[0]
                                        else:
                                            #Raise an error stating that the user must
                                            #specify the associated group:
                                            group_list_str = ', '.join(var_group_list)
                                            emsg = "Namelist variable '{}' is associated"
                                            emsg += " with the following namelist groups:\n"
                                            emsg += "{}\nPlease specify which group using"
                                            emsg += " '&groupname' in '{}'"
                                            raise CamConfigValError(emsg.format(var_str, group_list_str,
                                                                                user_nl_file))

                                    else:
                                        #Check that the specified group matches one of
                                        #the defined groups for that variable:
                                        if curr_group in var_group_list:
                                            #If so, then use specified group:
                                            data_group = curr_group
                                        else:
                                            #If not, then raise an error:
                                            emsg = "There is no variable '{}', associated with namelist group '{}'."
                                            emsg += " Please double-check '{}'."
                                            raise CamConfigValError(emsg.format(var_str, curr_group,
                                                                                user_nl_file))

                                else:
                                    #Raise error that namelist variable isn't listed in
                                    #anywhere in a definition file:
                                    emsg = "Variable '{}' not found in any namelist definition files."
                                    emsg += " Please double-check '{}'."
                                    raise CamConfigValError(emsg.format(var_str, user_nl_file))

                                #Add the namelist group if not already in data dict:
                                if not data_group in _data:
                                    _data[data_group] = dict()

                                #Check if variable already exists in data dictionary:
                                if var_str in _data[data_group]:
                                    emsg = "Namelist variable '{}' listed more than once in '{}'"
                                    emsg += "\nPlease either list the variable only once, or specify separate namelist groups"
                                    emsg += "for each listed instance, if relevant."
                                    raise CamConfigValError(emsg.format(var_str, user_nl_file))

                                #Enter the parameter in the dictionary:
                                _data[data_group][var_str] = {'values':val_str}
                            else:
                                emsg = "Cannot parse the following line in '{}' :\n'{}'"
                                raise CamConfigValError(emsg.format(user_nl_file, line))

            #Check if there is unclosed block:
            if within_comment_block:
                raise CamConfigValError("Un-closed comment block!  Please check '{}'".format(user_nl_file))
            if curr_group!="UNSET":
                raise CamConfigValError("Un-closed namelist group block! Please check `{}`".format(user_nl_file))

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
            for nml_group in self._data:
                # Write namelist group:
                atm_in_fil.write("&"+nml_group+"\n")

                # Write all variables within that group:
                for var in self._data[nml_group]:
                    #Extract variable value(s):
                    val = self._data[nml_group][var]["values"].strip()

                    #If no value is set then move to the next variable:
                    if val==None:
                        continue

                    #Extract variable type:
                    var_type = self._data[nml_group][var]["type"].strip()

                    #Check if variable value is a number or boolean:
                    if var_type in num_bool_set:
                        #If so, then write value as-is:
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
                        emsg = f"Namelist type '{var_type}' for variable '{var}' is un-recognized"
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

############
#End of file
