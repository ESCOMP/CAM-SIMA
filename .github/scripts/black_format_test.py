#!/usr/bin/env python3

"""
Module name: black_format_test.py

Purpose:  To test whether the provided list of python
          files are formatted according to the "black"
          python code formatter.

Written by:  Jesse Nusbaumer <nusbaume@ucar.edu> - November, 2026
"""

# +++++++++++++++++++++
# Import needed modules
# +++++++++++++++++++++

import argparse
import subprocess
import sys

#################
# HELPER FUNCTIONS
#################

# ++++++++++++++++++++++++++++++
# Input Argument parser function
# ++++++++++++++++++++++++++++++


def parse_arguments():
    """
    Parses command-line input arguments using the argparse
    python module and outputs the final argument object.
    """

    # Create parser object:
    parser = argparse.ArgumentParser(
        description="Check if python files are formatted with black."
    )

    # Add input arguments to be parsed:
    parser.add_argument(
        "--python_files",
        metavar="<space-separated list>",
        nargs="+",
        action="store",
        type=str,
        help="list of python files to test",
    )

    # Parse Argument inputs
    args = parser.parse_args()
    return args


#################
# Main test script
#################


def black_check(pyfile_list):
    """
    Checks if the provided python files are
    already formatted according to "black".

    Returns a list of messages (one per failing
    file), which includes the diff black would
    apply.  An empty list means all files passed.
    """

    # Create empty list to store black output:
    black_msgs = []

    # Check if pyfile_list is empty.  If so then exit
    # function, as there are no python files to test:
    if not pyfile_list:
        return black_msgs

    # Loop through the list of files:
    for pyfile in pyfile_list:

        # Run black in "check" mode, which never modifies the file.
        # Return codes: 0 = already formatted, 1 = would be
        # reformatted, 123 = internal error (e.g. unparsable file):
        result = subprocess.run(
            [sys.executable, "-m", "black", "--check", "--diff", pyfile],
            capture_output=True,
            text=True,
            check=False,
        )

        if result.returncode == 1:
            black_msgs.append(
                f"'{pyfile}' is not formatted with black. Required changes:\n"
                + result.stdout
            )
        elif result.returncode != 0:
            black_msgs.append(f"black failed to process '{pyfile}':\n" + result.stderr)
        # End if
    # End for

    # Return black message list:
    return black_msgs


####################
# Command-line script
####################


def _black_check_commandline():
    """
    Runs the "black_check" test using
    command line inputs. This will
    print the test results to stdout
    (usually the screen).
    """

    # Read in command-line arguments:
    args = parse_arguments()

    # Run black formatting check:
    msgs = black_check(args.python_files)

    # Print black info to screen:
    if msgs:
        # If test(s) failed, then print black message(s):
        for msg in msgs:
            print(msg)
        sys.exit(1)
    else:
        print("All files are formatted with black")


#############################################

# Run main script using provided command line arguments:
if __name__ == "__main__":
    _black_check_commandline()
