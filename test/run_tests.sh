#! /bin/bash

# Script to run doctest tests and unit tests on CAM python scripts
# Note: This script should be kept up to date with
#       .github/workflows/pr_open_sync_workflow.yml

# We only test python3 now
PYTHON=python3

# Try to find the correct directory
scriptdir="$( cd $( dirname ${0} ); pwd -P )"
if [ ! -d "cime_config" ]; then
  # Script was not called from top level directory, try to reset
  cd $( dirname ${scriptdir} )
fi

# Make sure we are at the top level
if [ ! -d "cime_config" ]; then
  echo "ERROR: Cannot find CAM sandbox to test"
  exit 1
fi

# CAM config doctests:
${PYTHON} cime_config/cam_config.py
# CAM config unit tests:
${PYTHON} test/unit/cam_config_unit_tests.py
# CAM autogen doctests:
${PYTHON} cime_config/cam_autogen.py
# CAM build cache doctests:
${PYTHON} cime_config/cam_build_cache.py
# Registry generator doctests:
${PYTHON} -m doctest src/data/generate_registry_data.py
# Registry generator unit tests:
${PYTHON} test/unit/test_registry.py
# Physics variable init (phys_init) generator unit tests:
${PYTHON} test/unit/write_init_unit_tests.py
