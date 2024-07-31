#!/usr/bin/env python3

"""
Script name:  pr_mod_file_tests.py

Goal:  To generate a list of files modified in the associated
       Github Pull Request (PR), using the PyGithub interface,
       and then to run tests on those files when appropriate.

Written by:  Jesse Nusbaumer <nusbaume@ucar.edu> - November, 2020
"""

#+++++++++++++++++++++
#Import needed modules
#+++++++++++++++++++++

import sys
import os
import argparse

from stat import S_ISREG
from github import Github

#Local scripts:
from pylint_threshold_test import pylint_check

#################

class PrModTestFail(ValueError):
    """Class used to handle file test failures
    (i.e., raise test failures without backtrace)"""

#################
#HELPER FUNCTIONS
#################

def _file_is_python(filename):

    """
    Checks whether a given file
    is a python script or
    python source code.
    """

    #Initialize return logical:
    is_python = False

    #Extract status of provided file:
    file_stat = os.stat(filename)

    #Check if it is a "regular" file:
    if S_ISREG(file_stat.st_mode):

        #Next, check if file ends in ".py":
        file_ext = os.path.splitext(filename)[1]

        if file_ext.strip() == ".py":
            #Assume file is python:
            is_python = True
        else:
            #If no ".py" extension exists, then
            #try to open the file and look for
            #a shabang that contains the word "python".
            try:
                with open(filename, "r", encoding='utf-8') as mod_file:
                    #Loop over lines in file:
                    for line in mod_file:

                        #Ignore blank lines:
                        if line.strip():

                            #Check that first non-blank
                            #line is a shabang:
                            if line.startswith("#!"):
                                #If so, then check that the word
                                #"python" is also present:
                                if "python" in line:
                                    #If the word exists, then assume
                                    #it is a python file:
                                    is_python = True
                                #End if
                            #End if

                            #Exit loop, as only the first non-blank
                            #line should be examined:
                            break
                        #End if
                    #End for
                #End with
            except UnicodeDecodeError:
                #Binary files, which we do not care about here,
                #can result in a decode error, so if that error
                #is raised just skip the file with a message stating
                #that it is being skipped (just in case):
                wmsg = f"WARNING: The file '{filename}' cannot currently be opened,\n"
                wmsg += "so skipping any attempt at analysing."
                print(wmsg)
            #End except
        #End if
    #End if

    #Return file type result:
    return is_python

#++++++++++++++++++++++++++++++
#Input Argument parser function
#++++++++++++++++++++++++++++++

def parse_arguments():

    """
    Parses command-line input arguments using the argparse
    python module and outputs the final argument object.
    """

    #Create parser object:
    parser = argparse.ArgumentParser(description='Generate list of all files modified by pull request.')

    #Add input arguments to be parsed:
    parser.add_argument('--access_token', metavar='<GITHUB_TOKEN>', action='store', type=str,
                        help="access token used to access GitHub API")

    parser.add_argument('--pr_num', metavar='<PR_NUMBER>', action='store', type=int,
                        help="pull request number")

    parser.add_argument('--rcfile', metavar='<pylintrc file path>', action='store', type=str,
                       help="location of pylintrc file (full path)")

    parser.add_argument('--pylint_level', metavar='<number>', action='store', type=float,
                        required=False, help="pylint score that file(s) must exceed")

    #Parse Argument inputs
    args = parser.parse_args()
    return args

#############
#MAIN PROGRAM
#############

def _main_prog():

    # pylint: disable=too-many-locals
    # pylint: disable=too-many-branches
    # pylint: disable=too-many-statements

    #++++++++++++
    #Begin script
    #++++++++++++

    print("Generating list of modified files...")

    #+++++++++++++++++++++++
    #Read in input arguments
    #+++++++++++++++++++++++

    args = parse_arguments()

    #Add argument values to variables:
    token = args.access_token
    pr_num = args.pr_num
    rcfile = args.rcfile
    pylev = args.pylint_level

    #++++++++++++++++++++++++++++++++
    #Log-in to github API using token
    #++++++++++++++++++++++++++++++++

    ghub = Github(token)

    #++++++++++++++++++++
    #Open ESCOMP/CAM repo
    #++++++++++++++++++++

    #Official CAM repo:
    cam_repo = ghub.get_repo("ESCOMP/CAM-SIMA")

    #++++++++++++++++++++++++++++++++++++++++++
    #Open Pull Request which triggered workflow
    #++++++++++++++++++++++++++++++++++++++++++

    pull_req = cam_repo.get_pull(pr_num)

    #++++++++++++++++++++++++++++++
    #Extract list of modified files
    #++++++++++++++++++++++++++++++

    #Create empty list to store python files:
    pyfiles = []

    #Extract Github file objects:
    file_obj_list = pull_req.get_files()

    for file_obj in file_obj_list:

        #Check if file exists. If not,
        #then it was likely deleted in the
        #PR itself, so don't check its file type:
        if os.path.exists(file_obj.filename):

            #Don't analyze 'git-fleximod' files,
            #as they are an external repo and thus
            #not our responsibility:
            if 'git-fleximod' not in file_obj.filename:

                #Check if it is a python file:
                if _file_is_python(file_obj.filename):
                    #If so, then add to python list:
                    pyfiles.append(file_obj.filename)

    #++++++++++++++++++++++++++++++++++++++++++++
    #Check if any python files are being modified:
    #++++++++++++++++++++++++++++++++++++++++++++
    if pyfiles:

        #Notify users of python files that will
        #be tested:
        print("The following modified python files will be tested:")
        for pyfile in pyfiles:
            print(pyfile)

        #+++++++++++++++++++++++++
        #Run pylint threshold test
        #+++++++++++++++++++++++++

        lint_msgs = pylint_check(pyfiles, rcfile,
                                 threshold=pylev)

        #++++++++++++++++++
        #Check test results
        #++++++++++++++++++

        #If pylint check lists are non-empty, then
        #a test has failed, and an exception should
        #be raised with the relevant pytlint info:
        if lint_msgs:
            #Print pylint results for failed tests to screen:
            print("+++++++++++PYLINT FAILURE MESSAGES+++++++++++++")
            for lmsg in lint_msgs:
                print(lmsg)
            print("+++++++++++++++++++++++++++++++++++++++++++++++")

            #Raise test failure exception:
            fail_msg = "One or more files are below allowed pylint "
            fail_msg += f"score of {pylev}.\nPlease see pylint message(s) "
            fail_msg += "above for possible fixes."
            raise PrModTestFail(fail_msg)

        #All tests have passed, so exit normally:
        print("All pylint tests passed!")
        sys.exit(0)

    #If no python files exist in PR, then exit script:
    else:
        print("No python files present in PR, so there is nothing to test.")
        sys.exit(0)

#############################################

#Run the main script program:
if __name__ == "__main__":
    _main_prog()
