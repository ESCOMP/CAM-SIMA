#!/usr/bin/env python

"""
Script name:  branch_PR_issue_closer.py

Goal:  To check if the newly-merged PR's commit message attempted to close an issue.
       If so, then move the associated project card to the "closed issues" column.

       Also checks if the newly-merged PR is the final PR needed to fix the issue
       for all related branches.  If so, then the issue is formally closed.

       Finally, this script also checks to see if the merged PR attempted
       to close other PRs, and does so if the merge was not to the repo's default branch.

Written by:  Jesse Nusbaumer <nusbaume@ucar.edu> - October, 2019
"""

#+++++++++++++++++++++
#Import needed modules
#+++++++++++++++++++++

import re
import sys
import argparse

from github import Github
from github import Auth

###############
#REGEX PATTERNS
###############

#Issue-closing Keywords are:
#close, closes, closed
#fix, fixes, fixed
#resolve, resolves, resolved

#The keywords are designed to match
#the keywords that exist in Github
#already for default branches, which
#can be found here:
#https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue

#Create relevant regex patterns:
_CLOSE_KEY              = r'close[sd]?'
_FIX_KEY                = r'fix(e[sd])?'
_RESOLVE_KEY            = r'resolve[sd]?'
_KEYWORDS               = rf'{_CLOSE_KEY}|{_FIX_KEY}|{_RESOLVE_KEY}'
_KEYWORDS_CAPTURE_GROUP = rf'(?P<keyword>{_KEYWORDS})'
_ID_NUMBER              = r'\d+'
_ID_CAPTURE_GROUP       = rf'(?P<id>{_ID_NUMBER})'
_LINKED_ISSUE_PATTERN   = rf'{_KEYWORDS_CAPTURE_GROUP}\s*#{_ID_CAPTURE_GROUP}'

#################
#HELPER FUNCTIONS
#################

#++++++++++++++++++++++++++++++
#Input Argument parser function
#++++++++++++++++++++++++++++++

def parse_arguments():

    """
    Parses command-line input arguments using the argparse
    python module and outputs the final argument object.
    """

    #Create parser object:
    parser = argparse.ArgumentParser(description='Close issues and pull requests specified in merged pull request.')

    #Add input arguments to be parsed:
    parser.add_argument('--access_token', metavar='<GITHUB_TOKEN>', action='store', type=str,
                        help="access token used to access GitHub API")

    parser.add_argument('--trigger_sha', metavar='<GITHUB SHA>', action='store', type=str,
                        help="Commit SHA that triggered the workflow")

    #Parse Argument inputs
    args = parser.parse_args()
    return args

#++++++++++++++++++++++++++++++++
#Script message and exit function
#++++++++++++++++++++++++++++++++

def end_script(msg):

    """
    Prints message to screen, and then exits script.
    """
    print(f"\n{msg}\n")
    print("Issue closing check has completed successfully.")
    sys.exit(0)

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

    print("Checking if issue needs to be closed...")

    #+++++++++++++++++++++++
    #Read in input arguments
    #+++++++++++++++++++++++

    args = parse_arguments()

    #Add argument values to variables:
    token = args.access_token
    trigger_sha = args.trigger_sha

    #++++++++++++++++++++++++++++++++
    #Log-in to github API using token
    #++++++++++++++++++++++++++++++++

    auth = Auth.Token(token)
    ghub = Github(auth=auth)

    #+++++++++++++++++++++++++
    #Open ESCOMP/CAM-SIMA repo
    #+++++++++++++++++++++++++

    cam_repo = ghub.get_repo("ESCOMP/CAM-SIMA")

    #++++++++++++++++++++++++++++++
    #Get PRs associated with commit
    #++++++++++++++++++++++++++++++

    github_commit = cam_repo.get_commit(trigger_sha)

    commit_prs = github_commit.get_pulls()

    pr_nums = [pr.number for pr in commit_prs]


    #If list is empty, then no PRs are associated
    #with this commit, so go ahead and close:
    if not pr_nums:
        endmsg = f"No PRs associated with commit:\n{trigger_sha}\n"
        endmsg += " so issue-closing script is stopping here."
        end_script(endmsg)

    #++++++++++++++++++++++++++++
    #Loop over all associated PRs
    #++++++++++++++++++++++++++++

    for pr_num in pr_nums:

        #+++++++++++++++++++++++++++++++++++++
        #Check that PR has in fact been merged
        #+++++++++++++++++++++++++++++++++++++

        #Extract pull request info:
        merged_pull = cam_repo.get_pull(pr_num)

        #If pull request has not been merged, then exit script:
        if not merged_pull.merged:
            endmsg = f"Pull request #{pr_num} associated with commit:\n{trigger_sha}\n"
            endmsg += "was not actually merged, so the script will not close anything."
            end_script(endmsg)

        #++++++++++++++++++++++++++++++++++++++++
        #Check that PR was not for default branch
        #++++++++++++++++++++++++++++++++++++++++

        #Determine default branch on repo:
        default_branch = cam_repo.default_branch

        #Extract merged branch from latest Pull request:
        merged_branch = merged_pull.base.ref

        #If PR was to default branch, then exit script (as github will handle it automatically):
        if merged_branch == default_branch:
            endmsg = f"Pull request #{pr_num} was merged into default repo branch. "
            endmsg += "Thus issue is closed automatically"
            end_script(endmsg)

        #++++++++++++++++++++++++++++++++++++++
        #Create integer list of all open issues:
        #++++++++++++++++++++++++++++++++++++++

        #Extract list of open issues from repo:
        open_repo_issues = cam_repo.get_issues(state='open')

        #Collect all open repo issues:
        open_issues = [issue.number for issue in open_repo_issues]

        #+++++++++++++++++++++++++++++++++++++++++++++
        #Create integer list of all open pull requests
        #+++++++++++++++++++++++++++++++++++++++++++++

        #Extract list of open PRs from repo:
        open_repo_pulls = cam_repo.get_pulls(state='open')

        #Collect all open pull requests:
        open_pulls = [pr.number for pr in open_repo_pulls]

        #+++++++++++++++++++++++++++++++++++++++++++++++++
        #Check if one of the keywords exists in PR message
        #+++++++++++++++++++++++++++++++++++++++++++++++++

        #Compile regex patterns into object:
        keyword_pattern = re.compile(_LINKED_ISSUE_PATTERN)

        #Extract (lower case) Pull Request message:
        pr_msg_lower = merged_pull.body.lower()

        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        #Extract issue and PR numbers associated with found keywords in merged PR message
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        #Create new "close" issues list:
        close_issues = []

        #Create new "closed" PR list:
        close_pulls = []

        #Create iterator of all keyword/id pairs:
        word_matches = keyword_pattern.finditer(pr_msg_lower, re.IGNORECASE)

        #Go through all matches to pull out PR and issue numbers:
        found_ids = set()
        for match in word_matches:
            issue_dict = match.groupdict()
            issue_num  = int(issue_dict['id'].lstrip('0'))
            found_ids.add(issue_num)

        #End script if no keyword/id pairs were found:
        if not found_ids:
            endmsg = f"Pull request #{pr_num} was merged without using any of the keywords. "
            endmsg += "Thus there are no issues to close."
            end_script(endmsg)

        close_pulls = list(found_ids.intersection(open_pulls))
        close_issues = list(found_ids.intersection(open_issues))


    #+++END REFERENCED PR LOOP+++

    #If no issue numbers are present after any of the keywords, then exit script:
    if not close_issues and not close_pulls:
        endmsg = "No open issue or PR numbers were found in the merged PR message.  Thus there is nothing to close."
        end_script(endmsg)

    #Print list of referenced issues to screen:
    if close_issues:
        print("Issues referenced by the merged PR: "+", ".join(\
              str(issue) for issue in close_issues))

    #Print list of referenced PRs to screen:
    if close_pulls:
        print("PRs referenced by the merged PR: "+", ".join(\
              str(pull) for pull in close_pulls))

    #++++++++++++++++++++++++++++++++++++++++++++++
    #Attempt to close all referenced issues and PRs
    #++++++++++++++++++++++++++++++++++++++++++++++

    #Loop over referenced issues:
    for issue_num in close_issues:
        #Extract github issue object:
        cam_issue = cam_repo.get_issue(number=issue_num)
        #Close issue:
        cam_issue.edit(state='closed')
        print(f"Issue #{issue_num} has been closed.")

    #Loop over referenced PRs:
    for pull_num in close_pulls:
        #Extract Pull request object:
        cam_pull = cam_repo.get_pull(number=pull_num)

        #Close Pull Request:
        cam_pull.edit(state='closed')
        print(f"Pull Request #{pull_num} has been closed.")

    #++++++++++
    #End script
    #++++++++++

    print("Issue closing check has completed successfully.")

#############################################

#Run the main script program:
if __name__ == "__main__":
    _main_prog()
