#!/bin/bash

tahun=( "2013" "2014" "2015" "2016" "2017" "2018" )
prf='/home/yosik/Data_riset/AWS/AWS/'
out='/home/yosik/Data_riset/AWS/CSV'

for i in ${tahun[*]};do
  dt=`ls $prf$i | grep "xls"`
  for j in "`ls $prf$i | grep "xls"`";do
     for j in ${dt[*]};do
    	libreoffice --headless --convert-to csv $prf$i"/"$j --outdir $out
       	echo $prf$i"/""$j"
	 done
  done
done
