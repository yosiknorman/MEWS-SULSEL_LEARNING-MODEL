#!/bin/bash

# dependensi fortran dan grads


# data Himawari 8/9
misal=$1
# misal="201801010000.tir.01.fld.geoss.bz2"  #Make function as $1
# DIR="data_10m"
# OUT="OUTPUT"

# 2km: main20 1km: main10 0.5km: main05
src="main20.f90"
exefile="tir.x"
gfortran ${src} -o ${exefile}

src="main10.f90"
exefile="vis.x"

gfortran ${src} -o ${exefile}

src="main05.f90"
exefile="ext.x"
gfortran ${src} -o ${exefile}


if [ ! -f tir.x ] || [ ! -f vis.x ] || [ ! -f ext.x ];then
   echo "compilernya coba cek"
   exit
fi




YYYY=${misal:0:4}
MM=${misal:4:2}
DD=${misal:6:2}
HH=${misal:8:2}
MN=${misal:10:2}

CHN=TIR
NUM=1

if [ ${CHN} = "VIS" ] && [ ${NUM} -gt 3 ];then
   break
elif [ ${CHN} = "SIR" ] && [ ${NUM} -gt 2 ]; then
   break
elif [ ${CHN} = "EXT" ] && [ ${NUM} -gt 1 ]; then
   break
fi

if [ ${NUM} -lt 10 ];then
   NUM=0${NUM}
fi

# bzip2 -d  ${DIR}/$misal  # klo mo dihapus bz2 nya -k nya di hapus
bzip2 -d -k $misal  # klo mo dihapus bz2 nya -k nya di hapus
dd if=${YYYY}${MM}${DD}${HH}${MN}.${CHN,,}.${NUM}.fld.geoss of=little.geoss conv=swab
         para=`echo ${YYYY}${MM}${DD}${HH}${MN}.${CHN,,}.${NUM}.fld.geoss | cut -c 14-19`


echo "konversi ke tbb"
if [ ${CHN} = "TIR" -o ${CHN} = "SIR" ];then
   ./tir.x little.geoss ${para}
   resolution="0.02";
elif [ ${CHN} = "VIS" ];then
   ./vis.x little.geoss ${para}
   resolution="0.01";
elif [ ${CHN} = "EXT" ];then
   dd if=little.geoss of=01.geoss bs=576000000 count=1
   ./ext.x 01.geoss ${para} && mv grid05.dat grid05_1.dat
   dd if=little.geoss of=02.geoss bs=576000000 skip=1
   ./ext.x 02.geoss ${para} && mv grid05.dat grid05_2.dat
   cat grid05_1.dat grid05_2.dat > grid05.dat
   resolution="0.005";
fi

mv grid??.dat  "${YYYY}${MM}${DD}${HH}${MN}.${CHN,,}.${NUM}.fld.dat"
echo dset ${YYYY}${MM}${DD}${HH}${MN}.${CHN,,}.${NUM}.fld.dat > ${YYYY}${MM}${DD}${HH}${MN}.${CHN,,}.${NUM}.ctl
echo title HIMAWARI-8 >> ${YYYY}${MM}${DD}${HH}${MN}.${CHN,,}.${NUM}.ctl
echo options yrev little_endian >> ${YYYY}${MM}${DD}${HH}${MN}.${CHN,,}.${NUM}.ctl
echo undef -999.0 >> ${YYYY}${MM}${DD}${HH}${MN}.${CHN,,}.${NUM}.ctl
echo xdef 6000 linear 85.01  0.02 >> ${YYYY}${MM}${DD}${HH}${MN}.${CHN,,}.${NUM}.ctl
echo ydef 6000 linear -59.99 0.02 >> ${YYYY}${MM}${DD}${HH}${MN}.${CHN,,}.${NUM}.ctl
echo zdef 1 linear 1 1 >> ${YYYY}${MM}${DD}${HH}${MN}.${CHN,,}.${NUM}.ctl
echo tdef 1 linear 01JUN05  1hr >> ${YYYY}${MM}${DD}${HH}${MN}.${CHN,,}.${NUM}.ctl
echo vars 1 >> ${YYYY}${MM}${DD}${HH}${MN}.${CHN,,}.${NUM}.ctl
echo "tbb 0 99 brightness temperature [K]" >> ${YYYY}${MM}${DD}${HH}${MN}.${CHN,,}.${NUM}.ctl
echo endvars >> ${YYYY}${MM}${DD}${HH}${MN}.${CHN,,}.${NUM}.ctl

echo "#!/usr/bin/grads" > ke_nc.gs
echo "'open ${YYYY}${MM}${DD}${HH}${MN}.${CHN,,}.${NUM}.ctl'" > ke_nc.gs
echo "'set lat -8 -2'" >> ke_nc.gs  # indonesia
echo "'set lon 117 123'" >> ke_nc.gs
echo "'define suhu=tbb'" >> ke_nc.gs
echo "'set sdfwrite ${YYYY}${MM}${DD}${HH}${MN}.${CHN,,}.${NUM}.nc'" >> ke_nc.gs
echo "'sdfwrite suhu'" >> ke_nc.gs

grads -lbxc ke_nc.gs
rm 201801010000.tir.01.fld.dat
rm 201801010000.tir.01.fld.geoss
rm 201801010000.tir.01.fld.geoss.bz2
