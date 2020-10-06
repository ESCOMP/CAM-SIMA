#! /bin/bash

# Script to run pylint tests on CAM python scripts

# Add CAM paths to PYTHONPATH so pylint can find them
CURRDIR="$( cd $( dirname ${0} ); pwd -P )"
CAMROOT="$( dirname ${CURRDIR} )"
CCPPDIR="${CAMROOT}/ccpp_framework/scripts"
if [ -d "${CAMROOT}/cime" ]; then
  export CIMEROOT="${CAMROOT}/cime"
else
  CESMROOT="$( dirname $( dirname ${CAMROOT} ) )"
  export CIMEROOT="${CESMROOT}/cime"
fi
CIMEDIR="${CIMEROOT}/scripts/lib"
export PYTHONPATH="${CIMEDIR}:${CCPPDIR}:$PYTHONPATH"

pylintcmd="pylint --rcfile=${CURRDIR}/.pylintrc"

${pylintcmd} ${CAMROOT}/cime_config/cam_config.py
${pylintcmd} ${CAMROOT}/cime_config/cam_autogen.py
${pylintcmd} ${CAMROOT}/cime_config/cam_build_cache.py
${pylintcmd} ${CAMROOT}/cime_config/buildlib
${pylintcmd} ${CAMROOT}/cime_config/buildnml
${pylintcmd} ${CAMROOT}/src/data/generate_registry_data.py
${pylintcmd} ${CAMROOT}/src/data/write_init_files.py
