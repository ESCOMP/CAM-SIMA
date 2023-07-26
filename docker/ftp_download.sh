#!/bin/sh

FTP_HOST="cesm-inputdata-lowres1.cgd.ucar.edu"
FTP_USER="anonymous"
FTP_PASS="your_email@example.com"


FILES_TO_DOWNLOAD=(
    "/testdata/run_heldsuarez_cam6_nt2_bigg_try005.cam.h5.0001-01-01-00000.nc"
    "/testdata/cami-mam3_0000-01_ne5np4_L30.140707.nc"
)

# Download the file using FTP
ftp -inv $FTP_HOST << EOF
user $FTP_USER $FTP_PASS
cd $(dirname ${FILES_TO_DOWNLOAD[0]})
get $(basename ${FILES_TO_DOWNLOAD[0]}) /home/cam_sima_user/$(basename ${FILES_TO_DOWNLOAD[0]})
get $(basename ${FILES_TO_DOWNLOAD[1]}) /home/cam_sima_user/$(basename ${FILES_TO_DOWNLOAD[1]})
bye
EOF
