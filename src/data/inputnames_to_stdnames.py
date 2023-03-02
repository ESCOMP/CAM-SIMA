#!/usr/bin/env python3
"""
Change variable names in NetCDF file to match those in Standard Names wiki file
NOTE: Use of this scripts requires the user to have NCO operators (e.g. ncrename) in their path
"""
import sys
import os
import argparse
import re

## Regular expression for stdname parsing
_VARIABLE_LINE = re.compile(r"\* `[A-Za-z0-9_]+`: ([A-Za-z0-9]+( [A-Za-z0-9]+)*)")
_INPUT_NAME_LINE = re.compile(".*IC file input names:.*")

def write_new_ncdata_file(input_filename, output_filename, inputname_dict):
    """Create and run ncrename command"""
    base_cmd = f'ncrename -h -o {output_filename} -O'
    for input_name in inputname_dict:
        base_cmd += f' -v .{input_name},{inputname_dict[input_name]}'
    #end for input_name in inputname_dict
    base_cmd += f' {input_filename}'
    os.system(base_cmd)

def parse_stdname_file(file_to_parse):
    """Parse provided standard names file"""
    input_names = {}
    with open(file_to_parse, encoding='utf-8') as fh1:
        current_stdname = ''
        for line in fh1:
            lmatch = _VARIABLE_LINE.match(line.strip())
            if lmatch is not None:
                # we've found a new variable stdname - parse it out!
                current_stdname = line.split(":")[0].split('*')[1].strip().replace('`', '')
            #end if lmatch is not None
            lmatch = _INPUT_NAME_LINE.match(line.strip())
            if lmatch is not None:
                # we've found an input names line - parse out the input names into a list
                input_names_list = line.split(':')[1].split(',')
                # add (input name, standard name) pairs to dict
                for input_name in input_names_list:
                    input_names[input_name.strip()] = current_stdname.strip()
                #end for input_name in input_names_list
            #end if lmatch is not None
        #end for line in fh1
    #end with open(file_to_parse)
    return input_names

def main(input_file, output_filename, stdname_file):
    """Parse standard name wiki file and then replace input name variables with stdnames"""
    if not os.path.isfile(input_file):
        print(f"Input file {input_file} does not exist")
        return 1
    #end if not os.path.isfile(input_file)
    if not os.path.isfile(stdname_file):
        print(f"Standard name wiki file {stdname_file} does not exist")
        return 2
    #end if not os.path.isfile(stdname_file)
    output_dir = os.path.split(output_filename)[0]
    if output_dir.strip():
        inputfile_dir = os.path.dirname(input_file)
        output_file = os.path.join(inputfile_dir, output_filename)
    else:
        if os.path.isdir(output_dir):
            output_file = output_filename
        else:
            print(f"Directory {output_dir} does not exist")
            return 1
    #end if len(output_dir.strip())) == 0
    inputname_dict = parse_stdname_file(stdname_file)
    if not inputname_dict:
        print("Could not parse standard name wiki file. Are you sure you're pointing to Metadata-standard-names.md?")
        return 1
    #end if len(inputname_dict) == 0
    # use the parsed dictionary to create new NetCDF file
    write_new_ncdata_file(input_file, output_file, inputname_dict)
    return 0

def parse_command_line(arguments, description):
    """Parse command-line arguments"""
    parser = argparse.ArgumentParser(description=description,
                                     formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument("--input", type=str, required=True,
                        metavar='input file - REQUIRED',
                        help="Full path of NetCDF file that contains non-standard variable names (to be converted)")
    parser.add_argument("--output", type=str, required=True,
                        metavar='output filename - REQUIRED',
                        help="Name of the output NetCDF file that will have standard variable names\n"\
                        "Written to the same directory --input file is in;\nif full path is supplied, file is written there")
    parser.add_argument("--stdnames", type=str, required=True,
                        metavar='stdname file - REQUIRED',
                        help="Full path to the Standard Names wiki file (Metadata-standard-names.md)")
    pargs = parser.parse_args(arguments)
    return pargs

if __name__ == "__main__":
    ARGS = parse_command_line(sys.argv[1:], __doc__)
    sys.exit(main(ARGS.input, ARGS.output, ARGS.stdnames))
