#!/bin/sh -f

show_help() {
cat << EOF1
NAME

	archive_baseline.sh - archive pretag baselines to set locations on
                              izumi and derecho.


SYNOPSIS

	archive_baseline.sh TAGNAME [--no-symlink]
	  [--help]


ENVIROMENT VARIABLES

	CESM_TESTDIR - Directory that contains the CESM finished results you wish to archive.
	CAM_FC      - Compiler used, used on izumi and derecho (GNU,NAG,INTEL,NVHPC), where the compiler
                      name is appended to the archive directory.


BASELINE ARCHIVED LOCATION

	izumi:     /fs/cgd/csm/models/atm/sima/pretag_bl/TAGNAME_gnu
	           /fs/cgd/csm/models/atm/sima/pretag_bl/TAGNAME_nag
        derecho:  /glade/campaign/cesm/community/amwg/sima_baselines/TAGNAME


SYMLINK
        By default, this script will create a symlink between the new baseline directory and
        $baseline_dir/latest_${CAM_FC} so that future tests can be run against these baselines
        until the next baselines are established. If you'd like to not create the symlink (e.g.
        you are archiving old baselines), use the "--no-symlink" argument.


WORK FLOW

	This is an example for derecho.

	Modify your sandbox with the changes you want.
        Run the sima test suite.
        Make your trunk tag
        setenv CAM_FC GNU
        setenv CESM_TESTDIR /scratch/cluster/fischer/aux_sima_gnu_20241113133750
	./archive_baseline.sh sima0_00_001

WARNING

	System changes can cause answer changes. So you may need to create new baselines
        if you are getting unexpected baseline failures.

EOF1
}

symlink=true
hostname=`hostname`

# Parse arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    --no-symlink)
      symlink=false
      shift
      ;;
    -h|--help)
      show_help
      exit 0
      ;;
    -*)
      echo "Unknown option: $1"
      show_help
      exit 1
      ;;
    *)
      # Assume the first non-flag argument is the tag
      if [ -z "$cam_tag" ]; then
        cam_tag="$1"
      else
        echo "Unexpected argument: $1"
        show_help
        exit 1
      fi
      shift
  esac
done

case $hostname in

  iz*)
    echo "server: izumi"
    if [ -z "$CAM_FC" ]; then
      CAM_FC="GNU"
    fi
    cam_tag=${cam_tag}_${CAM_FC,,}
    baselinedir="/fs/cgd/csm/models/atm/sima/pretag_bl/$cam_tag"
  ;;

  de*)
    echo "server: derecho"
    if [ -z "$CAM_FC" ]; then
      CAM_FC="INTEL"
    fi
    cam_tag=${cam_tag}_${CAM_FC,,}
    baselinedir="/glade/campaign/cesm/community/amwg/sima_baselines/$cam_tag"
  ;;

  * ) echo "ERROR: machine $hostname not currently supported"; exit 1 ;;
esac

if [ -d ${baselinedir} ]; then
   echo "ERROR: Baseline $baselinedir already exists."
   exit 1
fi

if [ -n "$CESM_TESTDIR" ]; then

    echo " "
    mkdir -p $baselinedir
    root_baselinedir=`dirname $baselinedir`
    echo "CESM Archiving to $root_baselinedir/$cam_tag"
    if [ -d $CESM_TESTDIR/baselines ]; then
      echo "Using cp to archive baselines."
      cp -r $CESM_TESTDIR/baselines/. $root_baselinedir/$cam_tag
      chmod -R a+r ${baselinedir}
      if [ "${symlink}" = true ]; then
        echo "Establishing symlink from '$root_baselinedir/latest_${CAM_FC,,}' to '$root_baselinedir/$cam_tag'"
        ln -sfn $root_baselinedir/$cam_tag $root_baselinedir/latest_${CAM_FC,,}
      fi
    else
      echo "Using bless_test_results to archive baselines."
      ../../cime/CIME/Tools/bless_test_results -p -t '' -c '' -r $CESM_TESTDIR --baseline-root $root_baselinedir -b $cam_tag -f -s
    fi

    echo " "
fi

case $hostname in

    de* | izumi)
	if [ -z "$CESM_TESTDIR" ]; then
	    echo '***********************************************************************************'
	    echo 'INFO: The aux_cam and test_cam tests were NOT archived'
	    echo "INFO: Must set CESM_TESTDIR (test-root in the create_test) to archive aux_cam tests"
	    echo '***********************************************************************************'
	fi
	;;

esac
