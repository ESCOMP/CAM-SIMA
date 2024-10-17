"""
Routines to parse history configuration and produce namelist output
suitable for atm_in.
Input can be history configuration files or history entries in user_nl_cam.
"""

# Python library imports
import logging
import os
import re
import sys

# Find and include the ccpp-framework scripts directory
# Assume we are in <CAMROOT>/src/data and CCPP is in <CAMROOT>/ccpp_framework
__CURRDIR = os.path.abspath(os.path.dirname(__file__))
__CAMROOT = os.path.abspath(os.path.join(__CURRDIR, os.pardir))
__CCPPSCRIPTS = os.path.join(__CAMROOT, "ccpp_framework", 'scripts')
if __CCPPSCRIPTS not in sys.path:
    sys.path.append(__CCPPSCRIPTS)
# end if

# CCPP framework imports
# pylint: disable=wrong-import-position
from parse_tools import ParseObject, context_string, ParseInternalError
# pylint: enable=wrong-import-position

## Default filename specifications for different history types
# Filename specifiers for history, initial files and restart history files
#  %c = caseid,
#  %y = year,
#  %m = month,
#  %d = day,
#  %s = seconds since midnight GMT in current day,
#  %u = unit number (e.g., h0, i)
#

# Note, these lists should match the corresponding lists in
#    cam_hist_config_file.F90
_TIME_PERIODS = ['nsteps', 'nstep', 'nseconds', 'nsecond',
                 'nminutes', 'nminute', 'nhours', 'nhour', 'ndays',
                 'nday', 'nmonths', 'nmonth', 'nyears', 'nyear',
                 'steps', 'seconds', 'minutes', 'hours',
                 'days', 'months', 'years']
_OUT_PRECS = ['REAL32', 'REAL64']
_TRUE_VALUES  = {"true", "t", ".true."}
_FALSE_VALUES = {"false", "f", ".false."}
# NetCDF variable name requirements:
#  https://docs.unidata.ucar.edu/netcdf-c/current/programming_notes.html#object_name
_NETCDF_ID_RE = re.compile(r"^[a-z][\w._@+]{0,256}$", re.IGNORECASE)

##############################################################################
###
### Support class and functions for history configuration commands
###
##############################################################################

##############################################################################
class HistoryConfigError(ValueError):
##############################################################################
    """Error type specific to history configuration parsing"""

    def __init__(self, message):
        """Initialize this exception"""
        logging.shutdown()
        super().__init__(message)

##############################################################################
def _blank_config_line(line):
##############################################################################
    """Return True if <line> is a valid history config blank or comment
    line. Also return True if we have reached the end of the file
    (no line)
    >>> _blank_config_line("   ! hist_add_avg_field")
    True
    >>> _blank_config_line("      ")
    True
    >>> _blank_config_line("")
    True
    >>> _blank_config_line("hist_add_avg_fields;h0: U")
    False
    >>> _blank_config_line("this is a ! weird line but is not blank")
    False
    """
    sline = line.strip()
    return not sline or sline.startswith('!')

##############################################################################
def _is_integer(entry):
##############################################################################
    """Return the integer value of the string, <entry>, if it represents a
    valid integer. Otherwise, return None
    Also, return an error string or None if no error is found.
    >>> _is_integer("314159")
    (314159, None)
    >>> _is_integer("3.14159")
    (None, '3.14159 is not an integer')
    """
    errmsg = None
    if isinstance(entry, int):
        ival = entry
    else:
        try:
            ival = int(str(entry).strip())
        except ValueError:
            ival = None
            errmsg = f"{entry} is not an integer"
        # end try
    # end if
    return ival, errmsg

##############################################################################
def _list_of_idents(entry, sep=','):
##############################################################################
    """Return a list of identifiers if <entry> is a valid list of identifiers.
    Otherwise, return None.
    A valid identifier is something that can be a NetCDF variable
    The identifiers must be separated by <sep>.
    Whitespace is not significant (but not allowed as part of an identifier).
    Also, return an error string or None if no error is found.
    >>> _list_of_idents("foo")
    (['foo'], None)
    >>> _list_of_idents("foo bar")
    (None, 'Found invalid identifiers:\\n    foo bar')
    >>> _list_of_idents("foo, bAr")
    (['foo', 'bAr'], None)
    >>> _list_of_idents("foo, BA2r3")
    (['foo', 'BA2r3'], None)
    >>> _list_of_idents("foo, 3bar")
    (None, 'Found invalid identifiers:\\n    3bar')
    >>> _list_of_idents("foo,3bar", sep=';')
    (None, 'Found invalid identifiers:\\n    foo,3bar')
    >>> _list_of_idents("foo#3bar, foo3baifijeowfjeiofjewiofjeiwofjewiofejifwjoefdfewfefdfdkjokmcdioanicdiaoilfejieojwiefjidojfioejsiofdjkljnxpoiadjfioenskcodiafkamd199fd9a0fdjkldajfdfjiodanckdalirhgieoskjcdskdfieowfidjfslk129dkjfaiocsriendnaadfasdfbasdlkfap983rasdfvalsda938qjnasdasd98adfasxd")
    (None, 'Found invalid identifiers:\\n    foo#3bar\\n    foo3baifijeowfjeiofjewiofjeiwofjewiofejifwjoefdfewfefdfdkjokmcdioanicdiaoilfejieojwiefjidojfioejsiofdjkljnxpoiadjfioenskcodiafkamd199fd9a0fdjkldajfdfjiodanckdalirhgieoskjcdskdfieowfidjfslk129dkjfaiocsriendnaadfasdfbasdlkfap983rasdfvalsda938qjnasdasd98adfasxd')
    >>> _list_of_idents("foo,3bar; foo", sep=';')
    (None, 'Found invalid identifiers:\\n    foo,3bar')
    >>> _list_of_idents(" ")
    (None, 'No identifiers found')
    """
    if entry.strip():
        potential_list = [x.strip() for x in entry.split(sep)]
        bad_list = [sample for sample in potential_list if _NETCDF_ID_RE.match(sample) is None]
        if len(bad_list) > 0:
            return (None, "Found invalid identifiers:\n    " + "\n    ".join(bad_list))
        # end if
        return (potential_list, None)
    # end if
    return (None, "No identifiers found")

##############################################################################
def _is_mult_period(entry):
##############################################################################
    """Return a tuple (<mult>, <period>), if <entry> is a valid time period
    entry. Otherwise, return None.
    A time-period entry is of the form:
    [<mult> *] <period>
    where <mult> is an optional integer and <period> is one of the recognized
    time periods (e.g., steps, days, months).
    Also, return an error string or None if no error is found.
    >>> _is_mult_period("nsteps")
    ((1, 'nsteps'), None)
    >>> _is_mult_period("3 * nmonths")
    ((3, 'nmonths'), None)
    >>> _is_mult_period("2*fortnights")
    (None, 'period ("fortnights") must be one of nsteps, nstep, nseconds, nsecond, nminutes, nminute, nhours, nhour, ndays, nday, nmonths, nmonth, nyears, nyear, steps, seconds, minutes, hours, days, months, years')
    >>> _is_mult_period("7*nhours of day")
    (None, 'period ("nhours of day") must be one of nsteps, nstep, nseconds, nsecond, nminutes, nminute, nhours, nhour, ndays, nday, nmonths, nmonth, nyears, nyear, steps, seconds, minutes, hours, days, months, years')
    >>> _is_mult_period(" ")
    (None, 'frequency ([<mult>*]period) is required, found " "')
    >>> _is_mult_period("1*nyear")
    ((1, 'nyear'), None)
    >>> _is_mult_period("-6*nhours")
    (None, 'multiplier ("-6") must be a positive integer')
    >>> _is_mult_period("7h*nhours")
    (None, '"7h" in "7h*nhours" is not a valid integer')
    >>> _is_mult_period("5*nhours*ndays")
    (None, 'frequency must be of the form ([<mult>*]period), found "5*nhours*ndays".  Do you have too many multipliers or periods?')
    """
    if not entry or not entry.strip():
        return (None, f"frequency ([<mult>*]period) is required, found \"{entry}\"")
    # end if
    tokens = [x.strip() for x in entry.split('*')]
    num_tokens = len(tokens)

    multiplier = 1
    period = ""
    if num_tokens == 1:
        period = tokens[0].lower()
    elif num_tokens == 2:
        multiplier = tokens[0]
        period = tokens[1].lower()
    else:
        return (None, f"frequency must be of the form ([<mult>*]period), found \"{entry}\".  Do you have too many multipliers or periods?")
    # end if

    if period not in _TIME_PERIODS:
        time_periods = ", ".join(_TIME_PERIODS)
        return (None, f"period (\"{period}\") must be one of {time_periods}")
    # end if
    (candidate_multiplier, errmsg) = _is_integer(multiplier)
    if errmsg:
        return (None, f"\"{multiplier}\" in \"{entry}\" is not a valid integer")
    # end if
    if candidate_multiplier <= 0:
        return (None, f"multiplier (\"{candidate_multiplier}\") must be a positive integer")
    # end if

    return ((candidate_multiplier, period), None)

##############################################################################
def _is_prec_str(entry):
##############################################################################
    """Return the output-precision represented by <entry> or None if it
    is invalid.
    Also, return an error string or None if no error is found.
    >>> _is_prec_str(' REAL32')
    ('REAL32', None)
    >>> _is_prec_str('REAL64 ')
    ('REAL64', None)
    >>> _is_prec_str('real32')
    ('REAL32', None)
    >>> _is_prec_str('real64')
    ('REAL64', None)
    >>> _is_prec_str('double')
    (None, 'precision must be one of REAL32, REAL64')
    """
    ustr = entry.strip().upper()
    errmsg = None
    if ustr not in _OUT_PRECS:
        ustr = None
        out_precs = ", ".join(_OUT_PRECS)
        errmsg = f"precision must be one of {out_precs}"
    # end if
    return ustr, errmsg

##############################################################################
def _is_string(entry):
##############################################################################
    """Return <entry> if it represents a valid history configuration
    filename or None if it is invalid.
    Note, not currently checking this string (just that it is a string).
    >>> _is_string(3)
    (None, None)
    >>> _is_string(3.1)
    (None, None)
    >>> _is_string('string')
    ('string', None)
    """
    fval = None
    if isinstance(entry, str):
        fval = entry.strip()
    # end if
    return fval, None

##############################################################################
def _is_logical(entry):
##############################################################################
    """Return <entry> if it represents a valid history configuration
    logical or None if it is invalid.
    Also, return an error string or None if no error is found.
    >>> _is_logical('true')
    ('true', None)
    >>> _is_logical('.false.')
    ('.false.', None)
    >>> _is_logical('truefalse')
    (None, 'hist_write_nstep0 must be one of .false., .true., f, false, t, true')
    >>> _is_logical(2)
    (None, 'hist_write_nstep0 must be a string, not a int type.')
    """
    fval, _ = _is_string(entry)
    possible_values = _TRUE_VALUES | _FALSE_VALUES
    errmsg = None
    if fval is not None:
        if fval.lower() not in possible_values:
            fval = None
            out_values = ", ".join(sorted(possible_values))
            errmsg = f"hist_write_nstep0 must be one of {out_values}"
        # end if
    else:
        errmsg = f"hist_write_nstep0 must be a string, not a {type(entry).__name__} type."
    # end if
    return fval, errmsg

##############################################################################
def _parse_hist_config_line(line, no_command_ok=False):
##############################################################################
    """Parse <line> if it is a valid history config command line.
    Parse the history configuration command found in <line>.
    Return three arguments:
    The history config command
    A tuple with the command value and command unit number (or None)
    An error message if one was generated during parsing or None if no
      error was found.
    If <line> is not recognized as a valid history config command line, and
    <no_command_ok> is True, then None is returned as the entry and the
    error message.
    If <line> is not recognized as a valid history config command line, and
    <no_command_ok> is False, then None is returned as the entry and an
    error message is returned.
    >>> _parse_hist_config_line("hist_add_avg_fields: T, U, V, PS")
    ('hist_add_avg_fields', (['T', 'U', 'V', 'PS'], None), None)
    >>> _parse_hist_config_line("hist_add_inst_fields;h2: T, U, V, PS")
    ('hist_add_inst_fields', (['T', 'U', 'V', 'PS'], 'h2'), None)
    >>> _parse_hist_config_line("hist_add_avg_fields;h5: foo, bar")
    ('hist_add_avg_fields', (['foo', 'bar'], 'h5'), None)
    >>> _parse_hist_config_line("use_topo_file = .false.")
    (None, None, "Invalid history config line, 'use_topo_file = .false.'")
    >>> _parse_hist_config_line("use_topo_file = .false.", no_command_ok=True)
    ('use_topo_file', None, None)
    >>> _parse_hist_config_line(" ")
    (None, None, None)
    """
    # Find the possible history configuration command for <line>.
    if _blank_config_line(line):
        return None, None, None
    # end if
    sline = line.strip()
    entry = None
    cmd = HistConfigEntry.find_command(sline)
    if cmd in _HIST_CONFIG_ENTRY_OBJS:
        # We have a history configuration command, parse it
        hconfig = _HIST_CONFIG_ENTRY_OBJS[cmd]
        entry, errmsg = hconfig.get_entry(sline)
    elif no_command_ok:
        errmsg = None
    else:
        cmd = None
        errmsg = f"Invalid history config line, '{sline}'"
    # end if
    return cmd, entry, errmsg

##############################################################################
class HistFieldList():
##############################################################################
    """Class to store information about a history configuration field list.
    """

    def __init__(self, volume, list_type, list_desc):
        """Initialize a named HistFieldList with an empty list.
        <volume> is the history volume for this field list.
        <list_type> is the type of field list (e.g., 'inst', 'avg').
        <list_desc> is a field type description for log messages"""
        self.__volume = volume
        self.__type = list_type
        self.__desc = list_desc
        self.__field_names = []
        self.__max_namelen = 0

    def _add_item(self, item, pobj, logger):
        """Add field name, <item> to this object and return True if this
           field name was added.
        <item> is a single item to be added.
        <pobj> is the ParseObject source of <items>.
        """
        if not _is_string(item)[0]:
            errmsg = f"Bad diagnostic name, '{item}'"
            pobj.add_syntax_err(errmsg)
            return False
        # end if
        if item in self.__field_names:
            # Field is a duplicate
            ctx = context_string(pobj)
            logger.warning(f"Field, '{item}' already in {self.desc} fields for hist volume, {self.volume}{ctx}")
            return False
        # end if
        self.__field_names.append(item)
        self.__max_namelen = max(len(item), self.__max_namelen)
        if logger.getEffectiveLevel() <= logging.DEBUG:
            ctx = context_string(pobj)
            logger.debug(f"Added {self.desc} field, '{item}' to hist volume, {self.volume}{ctx}")
        # end if
        return True

    def add_fields(self, items, pobj, logger):
        """Add <items> to this object and return True if all items were added.
        <items> can be a single item or a list.
        <pobj> is the ParseObject source of <items>
        """
        if isinstance(items, list):
            do_add = True
            for item in items:
                do_add &= self._add_item(item, pobj, logger)
            # end for
        else:
            do_add = self._add_item(items, pobj, logger)
        # end if
        return do_add

    def remove_fields(self, fields, pobj, logger):
        """Remove all field names in <fields> from this HistFieldList object.
        Return a set of the removed fields.
        """
        removed_fields = set()
        for field_name in fields:
            if field_name in self.__field_names:
                self.__field_names.remove(field_name)
                removed_fields.add(field_name)
            # end if
        # end for
        if logger.getEffectiveLevel() <= logging.DEBUG:
            ctx = context_string(pobj)
            for field_name in removed_fields:
                errmsg = f"Removed field, '{field_name}' from {self.desc} fields on hist volume, {self.volume}{ctx}"
                logger.debug(errmsg)
            # end for
        # end if
        return removed_fields

    def num_fields(self):
        """Return the number of fields in this HistFieldList object."""
        return len(self.__field_names)


    def output_nl_fieldlist(self, outfile, field_varname):
        """Output the field name of this HistFieldList object as a namelist
        variable that is an array of strings.
        <field_varname>: the name of the namelist variable
        <outfile>: File to write
        A list is only output if there are members in the list
        """
        max_linelen = 120
        if self.__field_names:
            lhs = f"    {field_varname} = "
            blank_lhs = ' '*(len(field_varname) + 5)
            # Break up output into lines
            num_fields = self.num_fields()
            fld_end = -1
            while fld_end < num_fields - 1:
                fld_beg = fld_end + 1
                # Always output at least one field
                fld_end = fld_beg
                line_len = len(lhs) + self.max_namelen + 2
                while line_len < max_linelen:
                    if fld_end + 1 >= num_fields:
                        break
                    # end if
                    next_len = self.max_namelen + 4
                    if (line_len + next_len) > max_linelen:
                        break
                    # end if
                    line_len += next_len
                    fld_end = fld_end + 1
                # end while
                # Output this line
                comma = "," if fld_end < num_fields - 1 else ""
                quotelist = [f"'{x}{' '*(self.max_namelen - len(x))}'"
                             for x in self.__field_names[fld_beg:fld_end+1]]
                outfile.write(f"{lhs}{', '.join(quotelist)}{comma}\n")
                lhs = blank_lhs
            # end while
        # end if

    @property
    def volume(self):
        """Return the volume for this HistFieldList"""
        return self.__volume

    @property
    def type(self):
        """Return the field type for this HistFieldList"""
        return self.__type

    @property
    def desc(self):
        """Return the field type description for this HistFieldList"""
        return self.__desc

    @property
    def max_namelen(self):
        """Return the length of the longest field in this HistFieldList object.
        """
        return self.__max_namelen

    @property
    def field_names(self):
        """Return the list of field names"""
        return self.__field_names

##############################################################################
###
### History configuration types (for parsing history configuration entries
###
##############################################################################

##############################################################################
class HistConfigEntry():
##############################################################################
    """Object to hold information, checking, and conversion functions for
    a history configuration entry type
    """

    __HIST_CONF_ENTRY_RE = re.compile(r"[a-z][a-z_0]*")
    __HIST_VOL = "(?P<vol>(h[0-9]*)|i)"
    __HIST_LIST_OF_IDENTIFIERS = "(?P<idents>.*)"
    __HIST_ASSIGNMENT_OPERATOR = "(:|=)"
    __HIST_STATEMENT = rf"\s*(;\s*{__HIST_VOL})?\s*{__HIST_ASSIGNMENT_OPERATOR}\s*{__HIST_LIST_OF_IDENTIFIERS}$"
    def __init__(self, entry_string, entry_check_fn, process_fn):
        """Set the entry string regular expression and value check function
        for this history configuration entry type
        <entry_string> is the name of the command
        <entry_check_fn> checks the command data for this config command
        <process_fn> processes an entry in the context of a particular
           HistoryConfig object.
        """
        self.__name = entry_string.strip().lower()
        self.__entry_regexp = re.compile(self.name + self.__HIST_STATEMENT,
                                         re.IGNORECASE)
        self.__entry_check_fn = entry_check_fn
        self.__process_fn = process_fn
        # Check that name matches pattern
        nmatch = self.find_command(self.name)
        if (not nmatch) or (nmatch != self.name):
            emsg = f"'{self.name}' is not a valid HistConfigEntry name"
            raise ValueError(emsg)
        # end if

    def get_entry(self, line):
        """If <line> matches this object's command expression, return a
        tuple with the entry value and history file number (or None if no
        file number is present).
        Otherwise, return None.
        Also, return an error string or None if no error is found.
        >>> HistConfigEntry(r"hist_add_avg_fields", _list_of_idents,          \
                            HistoryVolConfig.add_avg_fields).get_entry("foo")
        (None, "Invalid hist_add_avg_fields history config line, 'foo'")
        >>> HistConfigEntry(r"hist_add_avg_fields", _list_of_idents,          \
                            HistoryVolConfig.add_avg_fields).get_entry("hist_add_avg_fields: foo, bar")
        ((['foo', 'bar'], None), None)
        >>> HistConfigEntry(r"hist_add_min_fields", _list_of_idents,          \
                            HistoryVolConfig.add_min_fields).get_entry("hist_add_min_fields;h5: foo, bar")
        ((['foo', 'bar'], 'h5'), None)
        >>> HistConfigEntry(r"hist_add_min_fields", _list_of_idents,          \
                            HistoryVolConfig.add_min_fields).get_entry("hist_add_min_fields;5: foo, bar")
        (None, "Invalid hist_add_min_fields history config line, 'hist_add_min_fields;5: foo, bar'")
        >>> HistConfigEntry(r"hist_add_avg_fields", _list_of_idents,          \
                            HistoryVolConfig.add_avg_fields).get_entry("hist_add_avg_fields;h1: MOE, LARRY, CURLY")
        ((['MOE', 'LARRY', 'CURLY'], 'h1'), None)
        >>> HistConfigEntry(r"hist_write_nstep0", _list_of_idents,            \
                HistoryVolConfig.set_write_nstep0).get_entry("hist_write_nstep0;h3: true")
        ((['true'], 'h3'), None)
        >>> HistConfigEntry(r"hist_output_frequency", _list_of_idents,        \
                HistoryVolConfig.set_output_frequency).get_entry("hist_output_frequency;h2: 5*ndecades")
        (None, 'Found invalid identifiers:\\n    5*ndecades')
        """
        ematch = self.__entry_regexp.match(line.strip())
        if ematch is None:
            return None, f"Invalid {self.name} history config line, '{line.strip()}'"
        groups = ematch.groupdict()
        vol = groups['vol']
        entry_val, errmsg = self.__entry_check_fn(groups['idents'])
        if entry_val:
            return (entry_val, vol), errmsg
        return None, errmsg

    def process_data(self, hist_config, data, pobj, logger):
        """Process <data> according to the rules for this history configuration
        command in the context of the <hist_config> object.
        Return the value from __process_fn
        """
        return self.__process_fn(hist_config, data, pobj, logger)

    @property
    def name(self):
        """Return the command string for this HistConfigEntry object"""
        return self.__name

    @classmethod
    def find_command(cls, line):
        """Return the HistConfigEntry name string from <line> if the
        beginning of <line> matches the correct pattern.
        Otherwise, return the empty string.
        >>> HistConfigEntry.find_command("  add_avg_fields: foo")
        'add_avg_fields'
        >>> HistConfigEntry.find_command("  add_avg_fields3: foo")
        'add_avg_fields'
        >>> HistConfigEntry.find_command("!  add_avg_fields3: foo")
        ''
        """
        cmatch = cls.__HIST_CONF_ENTRY_RE.match(line.strip())
        if cmatch:
            return cmatch.group(0)
        # end if
        return ''

##############################################################################
class HistoryVolConfig():
##############################################################################
    """Object to hold all history configuration for a history file (volume).
    """

    # Note, variable values below must match those in cam_hist_config_file.F90
    #    (without leading undescores)
    __HIST_FILE = "history"
    __SAT_FILE = "satellite"
    __INITIAL_FILE = "initial_value"
    __HFILE_TYPES = [__HIST_FILE, __SAT_FILE, __INITIAL_FILE]
    # rhfilename_spec is the template for history restart files
    _DEFAULT_RESTART_HIST_SPEC = '%c.cam.r%u.%y-%m-%d-%s.nc'
    # hfilename_spec is the template for each history file
    _DEFAULT_HISTORY_SPEC = '%c.cam.%u%f.%y-%m-%d-%s.nc'

    def __init__(self, volume):
        """Initialize a HistoryConfig object to a default state.
        <volume> is the history file descriptor (e.g., h1, i)
        """
        self.__volume = volume
        self.__inst_fields = HistFieldList(self.volume, 'inst', 'instantaneous')
        self.__avg_fields = HistFieldList(self.volume, 'avg', 'average')
        self.__min_fields = HistFieldList(self.volume, 'min', 'minimum sampled')
        self.__max_fields = HistFieldList(self.volume, 'max', 'maximum sampled')
        self.__var_fields = HistFieldList(self.volume, 'var',
                                          'variance of sampled')
        self.__all_fields = [self.__inst_fields, self.__avg_fields,
                             self.__min_fields, self.__max_fields,
                             self.__var_fields]
        self.__precision = 'REAL32'
        self.__precision_set = False
        if self.__volume == 'h0':
            self.__max_frames = 1
            self.__output_freq = (1, 'month')
        else:
            self.__max_frames = 30
            self.__output_freq = (1, 'day')
        # end if
        self.__max_frames_set = False
        self.__file_type = self.__HIST_FILE
        self.__filename_spec = self._DEFAULT_HISTORY_SPEC
        self.__restart_fname_spec = self._DEFAULT_RESTART_HIST_SPEC
        self.__restart_fname_spec_set = False
        self.__write_nstep0 = ".false."

    def add_inst_fields(self, fields, pobj, logger):
        """Add one or more instantaneous (last sampled value)_fields to this
           HistoryVolConfig object.
        Return True if it was okay to add <fields> to list of last fields.
        """
        add_ok = self.__inst_fields.add_fields(fields, pobj, logger)
        return add_ok

    def add_avg_fields(self, fields, pobj, logger):
        """Add one or more time-averaged fields to this HistoryVolConfig
        object.
        Return True if it was okay to add <fields> to list of avg fields.
        """
        add_ok = self.__avg_fields.add_fields(fields, pobj, logger)
        return add_ok

    def add_min_fields(self, fields, pobj, logger):
        """Add one or more min_fields to this HistoryVolConfig object.
        Return True if it was okay to add <fields> to list of min fields.
        """
        add_ok = self.__min_fields.add_fields(fields, pobj, logger)
        return add_ok

    def add_max_fields(self, fields, pobj, logger):
        """Add one or more max_fields to this HistoryVolConfig object.
        Return True if it was okay to add <fields> to list of max fields.
        """
        add_ok = self.__max_fields.add_fields(fields, pobj, logger)
        return add_ok

    def add_var_fields(self, fields, pobj, logger):
        """Add one or more var_fields to this HistoryVolConfig object.
        Return True if it was okay to add <fields> to list of var fields.
        """
        add_ok = self.__var_fields.add_fields(fields, pobj, logger)
        return add_ok

    def remove_fields(self, fields, pobj, logger):
        """Remove each field in <fields> from all lists it is on.
        Return True if each field was found (and removed)."""
        fields_to_delete = set(fields)
        fields_deleted = set()
        all_removed = True
        for fld_list in self.__all_fields:
            removed = fld_list.remove_fields(fields_to_delete, pobj, logger)
            fields_deleted.update(removed)
        # end for
        if fields_to_delete != fields_deleted:
            all_removed = False
            missing_fields = fields_to_delete.difference(fields_deleted)
            ctx = context_string(pobj)
            missing_fields_string = ", ".join(missing_fields)
            errmsg = f"Cannot remove field(s), '{missing_fields_string}', not found on hist volume, {self.volume}{ctx}"
            logger.warning(errmsg)
        # end if
        return all_removed

    @property
    def volume(self):
        """Return the volume for this HistoryVolConfig object"""
        return self.__volume

    @property
    def precision(self):
        """Return the precision property for this HistoryVolConfig object"""
        return self.__precision

    def set_precision(self, prec, pobj, logger):
        """Modify the precision property of this HistoryVolConfig object.
        Return True if <prec> is a recognized precision"""
        precstr, errmsg = _is_prec_str(prec)
        if not errmsg:
            self.__precision = prec.upper()
            self.__precision_set = True
            if logger.getEffectiveLevel() <= logging.DEBUG:
                ctx = context_string(pobj)
                logger.debug(f"Setting precision to '{prec}'{ctx}")
            # end if
            return True
        # end if
        emsg = f"Attempt to set unrecognized precision, '{prec}'"
        pobj.add_syntax_err(emsg)
        return False

    @property
    def max_frames(self):
        """Return the max_frames property for this HistoryVolConfig object"""
        return self.__max_frames

    @property
    def write_nstep0(self):
        """Return the write_nstep0 property for this HistoryVolConfig object"""
        return self.__write_nstep0

    def set_max_frames(self, nframes, pobj, logger):
        """Modify the max_frames property of this HistoryVolConfig object.
        Return True if <nframes> is a valid setting and max_frames is updated."""
        nframes_ok = True
        nframes_i, errmsg = _is_integer(nframes)
        nframes_ok = not errmsg and (nframes_i > 0)
        if nframes_ok:
            self.__max_frames = nframes_i
            self.__max_frames_set = True
            if logger.getEffectiveLevel() <= logging.DEBUG:
                ctx = context_string(pobj)
                logger.debug(f"Setting max frames to '{nframes}'{ctx}")
            # end if
            return True
        else:
            emsg = f"Attempt to set max frames to '{nframes}', must be a positive integer"
            pobj.add_syntax_err(emsg)
            return False
        # end if

    def set_write_nstep0(self, write_nstep0, pobj, logger):
        """Modify the write_nstep0 property of this HistoryVolConfig object.
        Return True if valid and write_nstep0 updated"""
        if write_nstep0.lower() in _TRUE_VALUES:
            self.__write_nstep0 = ".true."
        elif write_nstep0.lower() in _FALSE_VALUES:
            self.__write_nstep0 = ".false."
        else:
            emsg = f"Attempt to set write_nstep0 to '{write_nstep0}', must be one of {_TRUE_VALUES | _FALSE_VALUES}"
            pobj.add_syntax_err(emsg)
            return False
        # end if
        if logger.getEffectiveLevel() <= logging.DEBUG:
            ctx = context_string(pobj)
            logger.debug(f"Setting write_nstep0 to '{self.__write_nstep0}'{ctx}")
        # end if
        return True

    def outfreq_str(self):
        """Return the output_frequency for this HistoryVolConfig object
        as a string"""
        return f"{self.__output_freq[0]}*{self.__output_freq[1]}"

    @property
    def output_frequency(self):
        """Return the output_frequency property for this
        HistoryVolConfig object"""
        return self.__output_freq

    def __out_frequency_is_valid(this, ofreq):
        """
        Determine if a user-supplied output frequency is valid.
        Checks:
         - frequency is tuple (multiplier, period)
         - multiplier is a positive integer
         - period is in list of valid time periods
        """
        return isinstance(ofreq, tuple)                   and \
               (len(ofreq) == 2)                          and \
               (ofreq[0] > 0)                             and \
               (ofreq[1].strip().lower() in _TIME_PERIODS)

    def set_output_frequency(self, ofreq, pobj, logger):
        """Modify the output_frequency property of this HistoryVolConfig
            object. <ofreq> is a tuple consisting of an integer and a period.
        """
        if self.__out_frequency_is_valid(ofreq):
            self.__output_freq = ofreq
            if logger.getEffectiveLevel() <= logging.DEBUG:
                ctx = context_string(pobj)
                logger.debug(f"Setting output_frequency to '{ofreq}'{ctx}")
            # end if
            return True
        # end if
        emsg = f"Attempt to set unrecognized output_frequency, '{ofreq}'"
        pobj.add_syntax_err(emsg)
        return False

    @property
    def file_type(self):
        """Return the file_type property for this HistoryVolConfig object"""
        return self.__file_type

    def set_file_type(self, ftype, pobj, logger):
        """Modify the file_type property of this HistoryVolConfig object"""
        if ftype.strip().lower() in self.__HFILE_TYPES:
            self.__file_type = ftype
        else:
            tstr = f", must be one of ({', '.join(self.__HFILE_TYPES)})."
            pobj.add_syntax_err(f"Bad history file type, '{ftype}'{tstr}")
            return False
        # end if
        if (ftype == self.__INITIAL_FILE) and (not self.__max_frames_set):
            self.__max_frames = 1
        # end if
        if (ftype == self.__INITIAL_FILE) and (not self.__precision_set):
            self.__precision = 'REAL64'
        # end if
        if (logger is not None) and (logger.getEffectiveLevel() <= logging.DEBUG):
            ctx = context_string(pobj)
            logger.debug(f"Setting file type to '{ftype}'{ctx}")
        # end if
        return True

    @property
    def filename_spec(self):
        """Return the filename_spec property for this HistoryVolConfig object"""
        return self.__filename_spec

    def set_filename_spec(self, fnspec, pobj, logger):
        """Modify the filename_spec property of this HistoryVolConfig object.
        If the restart filename spec has not yet been set, set it to the default
        for <fnspec> if possible (i.e., if <fnspec> contains a '%u').
        """
        self.__filename_spec = fnspec
        if not self.__restart_fname_spec_set:
            if '%u' in self.__filename_spec:
                self.__restart_fname_spec = self.__filename_spec.replace("%u",
                                                                         "r%u")
            # end if
        # end if
        if logger.getEffectiveLevel() <= logging.DEBUG:
            ctx = context_string(pobj)
            logger.debug(f"Setting filename spec to '{fnspec}'{ctx}")
        # end if

    @property
    def restart_fname_spec(self):
        """Return the restart history filename_spec property for this
        HistoryVolConfig object"""
        return self.__restart_fname_spec

    def set_restart_fname_spec(self, pobj, logger, rfnspec=None):
        """Modify the restart filename spec property of this HistoryVolConfig object.
        If the restart filename spec has not yet been set, set it to the default
        for <fnspec> if possible (i.e., if <fnspec> contains a '%u').
        """
        if not rfnspec:
            rfnspec = self.__filename_spec.replace("%u", "r%u")
        # end if
        self.__restart_fname_spec = rfnspec
        self.__restart_fname_spec_set = True
        if logger.getEffectiveLevel() <= logging.DEBUG:
            ctx = context_string(pobj)
            logger.debug(f"Setting restart filename spec to '{rfnspec}'{ctx}")
        # end if
        return True

    def num_fields(self, fld_type):
        """Return the number of fields for field list type, <fld_type>."""
        num_flds = 0
        if fld_type == 'avg':
            num_flds = self.__avg_fields.num_fields()
        elif fld_type == 'inst':
            num_flds = self.__inst_fields.num_fields()
        elif fld_type == 'min':
            num_flds = self.__min_fields.num_fields()
        elif fld_type == 'max':
            num_flds = self.__max_fields.num_fields()
        elif fld_type == 'var':
            num_flds = self.__var_fields.num_fields()
        elif fld_type == 'all':
            num_flds = sum([x.num_fields() for x in self.__all_fields])
        else:
            raise ParseInternalError(f"Unknown fld_type, '{fld_type}'")
        # end if
        return num_flds

    def output_config_namelist(self, outfile, logger):
        """Write the fortran namelist object for this HistoryVolConfig
        object"""
        if self.num_fields('all') == 0:
            logger.warning(f"WARNING: Volume '{self.volume}' has no fields; skipping")
            return
        # end if
        outfile.write("\n&hist_file_config_nl\n")
        outfile.write(f"    hist_volume = '{self.volume}'\n")
        self.__inst_fields.output_nl_fieldlist(outfile, "hist_inst_fields")
        self.__avg_fields.output_nl_fieldlist(outfile, "hist_avg_fields")
        self.__min_fields.output_nl_fieldlist(outfile, "hist_min_fields")
        self.__max_fields.output_nl_fieldlist(outfile, "hist_max_fields")
        self.__var_fields.output_nl_fieldlist(outfile, "hist_var_fields")
        outfile.write(f"    hist_max_frames = {self.__max_frames}\n")
        outfile.write(f"    hist_output_frequency = '{self.outfreq_str()}'\n")
        outfile.write(f"    hist_precision = '{self.__precision}'\n")
        outfile.write(f"    hist_file_type = '{self.__file_type}'\n")
        outfile.write(f"    hist_filename_spec = '{self.__filename_spec}'\n")
        outfile.write(f"    hist_write_nstep0 = {self.__write_nstep0}\n")
        outfile.write("/\n")

##############################################################################
###  Objects for identifying and processing history config commands
##############################################################################

_HIST_CONFIG_ENTRY_TYPES = [HistConfigEntry(r"hist_add_avg_fields",
                                            _list_of_idents,
                                            HistoryVolConfig.add_avg_fields),
                            HistConfigEntry(r"hist_add_inst_fields",
                                            _list_of_idents,
                                            HistoryVolConfig.add_inst_fields),
                            HistConfigEntry(r"hist_add_min_fields",
                                            _list_of_idents,
                                            HistoryVolConfig.add_min_fields),
                            HistConfigEntry(r"hist_add_max_fields",
                                            _list_of_idents,
                                            HistoryVolConfig.add_max_fields),
                            HistConfigEntry(r"hist_add_var_fields",
                                            _list_of_idents,
                                            HistoryVolConfig.add_var_fields),
                            HistConfigEntry(r"hist_file_type", _is_string,
                                            HistoryVolConfig.set_file_type),
                            HistConfigEntry(r"hist_max_frames", _is_integer,
                                            HistoryVolConfig.set_max_frames),
                            HistConfigEntry(r"hist_output_frequency",
                                            _is_mult_period,
                                            HistoryVolConfig.set_output_frequency),
                            HistConfigEntry(r"hist_precision", _is_prec_str,
                                            HistoryVolConfig.set_precision),
                            HistConfigEntry(r"hist_diag_file", _is_string,
                                            None),
                            HistConfigEntry(r"hist_write_nstep0", _is_logical,
                                            HistoryVolConfig.set_write_nstep0),
                            HistConfigEntry(r"hist_filename_template", _is_string,
                                            HistoryVolConfig.set_filename_spec),
                            HistConfigEntry(r"hist_remove_fields",
                                            _list_of_idents,
                                            HistoryVolConfig.remove_fields)]

_HIST_CONFIG_ENTRY_OBJS = {x.name : x for x in _HIST_CONFIG_ENTRY_TYPES}

##############################################################################
class HistoryConfig(dict):
##############################################################################
    """Object to hold the history configuration for all history files (volumes).
    """

    def __init__(self, filename=None, logger=None):
        """Initialize this HistoryConfig object as an empty dictionary.
        If <filename> (and <logger>) are present, initialize this object with
        the contents of <filename>
        """
        if filename:
            if not logger:
                raise ParseInternalError("Logger required to parse file")
            # end if
            self.parse_hist_config_file(filename, logger)
        # end if (no else, just leave empty dictionary)

    def parse_hist_config_file(self, filename, logger, volume=None):
        """Parse the history configuration commands from <filename> and store
        the resulting configuration information.
        There are two modes of parsing.
        If <volume> is None, every history configuration command *must* have a
           volume name or history unit number. <hist_configs> must be a
           dictionary of HistoryConfig objects keyed by volume.
        If <volume> is not None, no history configuration command may have a
           volume name or history unit number. <hist_configs> must be a
           single HistoryConfig object.
        Typically, volume will be set except for user_nl_cam files.
        """
        # Store directory information for relative paths
        file_dir = os.path.dirname(os.path.abspath(filename))
        no_comm_ok = volume is None # Can have mixed lines for user_nl_cam
        with open(filename, "r", encoding="UTF-8") as cfile:
            clines = [line.strip() for line in cfile.readlines()]
        # end with
        # create a parse object and context for this file
        pobj = ParseObject(filename, clines)
        curr_line, _ = pobj.curr_line()
        while pobj.valid_line():
            args = _parse_hist_config_line(curr_line,
                                               no_command_ok=no_comm_ok)
            cmd, entry, errmsg = args
            hist_config = None
            if errmsg:
                pobj.add_syntax_err(errmsg)
            elif entry:
                cmd_val, fnum = entry
                # Find a hist_config
                if volume and fnum and (volume != fnum):
                    # This is an error
                    errmsg = f"Volume information not allowed in {filename},"
                    errmsg += f"\n{curr_line}"
                    pobj.add_syntax_err(errmsg)
                elif volume:
                    if volume not in self:
                        # Someone made a boo boo
                        ctx = context_string(pobj)
                        emsg = f"volume, '{volume}', not in configs{ctx}"
                        raise ParseInternalError(emsg)
                    # end if
                    hist_config = self[volume]
                    fnum = volume
                elif fnum:
                    if fnum in self:
                        hist_config = self[fnum]
                    else:
                        hist_config = HistoryVolConfig(fnum)
                        self[fnum] = hist_config
                    # end if
                else:
                    errmsg = f"Volume information required in {filename},"
                    errmsg += f"\n{curr_line}"
                    pobj.add_syntax_err(errmsg)
                # end if
            else:
                if (not no_comm_ok) and (not _blank_config_line(curr_line)):
                    # Something has gone wrong.
                    ctx = context_string(pobj)
                    emsg = f"Bad line but no error{ctx}"
                    raise ParseInternalError(emsg)
                # end if
            # end if
            if hist_config:
                # Process this line's information
                if cmd == 'hist_diag_file':
                    if os.path.exists(cmd_val):
                        dfile = cmd_val
                    elif not os.path.isabs(cmd_val):
                        # Try to find the file relative to this file's directory
                        dfile = os.path.abspath(os.path.join(file_dir, cmd_val))
                    else:
                        dfile = ""
                    # end if
                    if os.path.exists(dfile):
                        lmsg = f"Processing {dfile} for history volume {fnum}"
                        logger.debug(lmsg)
                        self.parse_hist_config_file(dfile, logger, volume=fnum)
                    else:
                        ctx = context_string(pobj)
                        emsg = f"History config file, '{cmd_val}', not found{ctx}"
                        raise HistoryConfigError(emsg)
                    # end if
                else:
                    hconf_entry = _HIST_CONFIG_ENTRY_OBJS[cmd]
                    hconf_entry.process_data(hist_config, cmd_val,
                                                        pobj, logger)
                # end if
            # end if (no else, any error was already generated)
            # Done with this line, move on
            curr_line, _ = pobj.next_line()
        # end while
        if pobj.error_message:
            # Time to dump out error messages
            raise HistoryConfigError(pobj.error_message)
        # end if

    def max_num_fields(self, fld_type):
        """Return the maximum number of fields for <fld_type> on any history
        volume."""
        nums_flds = [x.num_fields(fld_type) for x in self.values()]
        return max(nums_flds, default=0)

    def output_class_namelist(self, ofile):
        """Write the master class namelist (e.g., num fields)"""
        ofile.write("\n&hist_config_arrays_nl\n")
        num_fields = self.max_num_fields('inst')
        ofile.write(f"    hist_num_inst_fields = {num_fields}\n")
        num_fields = self.max_num_fields('avg')
        ofile.write(f"    hist_num_avg_fields = {num_fields}\n")
        num_fields = self.max_num_fields('min')
        ofile.write(f"    hist_num_min_fields = {num_fields}\n")
        num_fields = self.max_num_fields('max')
        ofile.write(f"    hist_num_max_fields = {num_fields}\n")
        num_fields = self.max_num_fields('var')
        ofile.write(f"    hist_num_var_fields = {num_fields}\n")
        ofile.write("/\n")

##############################################################################
#IGNORE EVERYTHING BELOW HERE UNLESS RUNNING TESTS ON CAM_CONFIG!
##############################################################################

# Call testing routine, if script is run directly
if __name__ == "__main__":

    # Import modules needed for testing
    import doctest

    # Run doctests on this file's python objects
    doctest.testmod()

#############
# end of file
#############
