#!/usr/bin/env python3
"""
Change variable names in NetCDF file to match those in a standard names dictionary
NOTE: Use of this script requires the user to have NCO operators (e.g. ncrename) in their path
"""
# Python library imports
import sys
import os
import argparse
import xml.etree.ElementTree as ET

def write_new_ncdata_file(input_filename, output_filename, inputname_dict):
    """Create and run ncrename command"""
    base_cmd = f'ncrename -h -o {output_filename} -O'
    for input_name in inputname_dict:
        base_cmd += f' -v .{input_name},{inputname_dict[input_name]}'
    #end for input_name in inputname_dict
    base_cmd += f' {input_filename}'
    os.system(base_cmd)

def parse_stdname_file(file_to_parse, tphys_exclude):
    """Parse XML standard name dictionary"""
    with open(file_to_parse, encoding='utf-8') as fh1:
        try:
            tree = ET.parse(fh1)
            root = tree.getroot()
        except ET.ParseError as perr:
            print(f"Cannot parse XML file {file_to_parse}")
            return {}
        # end try
    # end with open(file_to_parse)
    inputname_dict = {}
    for entry in root:
        stdname = entry.attrib["stdname"]
        for sub_element in entry:
            if sub_element.tag == "ic_file_input_names":
                for input_name in sub_element:
                    if not input_name.text.startswith(tphys_exclude):
                        inputname_dict[input_name.text.strip()] = stdname
                    # end if startswith
                # end for input_name
            # end if sub_element.tag
        # end if for sub_element in entry
    # end if for entry in root
    return inputname_dict


def main(input_file, output_filename, stdname_file, tphys_exclude):
    """Parse standard name dictionary and then replace input name variables with stdnames"""
    if not os.path.isfile(input_file):
        print(f"Input file {input_file} does not exist")
        return 1
    #end if not os.path.isfile(input_file)
    if not os.access(input_file, os.R_OK):
        print(f"Cannot open file {input_file}")
        return 2
    #end if not os.access(input_file)
    if not os.path.isfile(stdname_file):
        print(f"Standard name dictionary {stdname_file} does not exist")
        return 3
    #end if not os.path.isfile(stdname_file)
    if not os.access(stdname_file, os.R_OK):
        print(f"Cannot open standard name dictionary {stdname_file}")
        return 4
    #end if not os.access(stdname_file)
    output_dir = os.path.split(output_filename)[0]
    if not output_dir.strip():
        inputfile_dir = os.path.dirname(input_file)
        output_file = os.path.join(inputfile_dir, output_filename)
    else:
        if os.path.isdir(output_dir):
            output_file = output_filename
        else:
            print(f"Directory {output_dir} does not exist")
            return 5
        #end if os.path.isdir(output_dir)
    #end if len(output_dir.strip())) == 0
    # Parse the standard name dictionary
    inputname_dict = parse_stdname_file(stdname_file, tphys_exclude)
    if not inputname_dict:
        print(f"Standard name dictionary {stdname_file} empty or not parse-able")
        return 6
    #end if inputname_dict
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
                        metavar='stdname file',
                        help="Full path to the standard names dictionary (e.g. stdnames_to_inputnames_dictionary.xml)")
    parser.add_argument('--tphys-exclude', type=str, required=True,
                        metavar='tphysac or tphysbc group - REQUIRED',
                        help='Group to exclude when converting variable names to stdandard names')
    pargs = parser.parse_args(arguments)
    return pargs

if __name__ == "__main__":
    ARGS = parse_command_line(sys.argv[1:], __doc__)
    sys.exit(main(ARGS.input, ARGS.output, ARGS.stdnames, ARGS.tphys_exclude))
