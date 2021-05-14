#! /bin/bash

# Script to run doctest tests on CAM python scripts
# Note: This script should be kept up to date with 
#       .github/workflows/pr_open_sync_workflow.yml 

# CAM config doctests:
python cime_config/cam_config.py
# CAM config unit tests:
python test/unit/cam_config_unit_tests.py
# CAM autogen doctests:
python cime_config/cam_autogen.py
# Registry generator doctests:
python -m doctest src/data/generate_registry_data.py
# Registry generator unit tests:
python test/unit/test_registry.py
# Physics variable init (phys_init) generator unit tests:
python test/unit/write_init_unit_tests.py
