#!/bin/bash

for i in `ls /media/wh/dysk12/wrfout/`
do echo $i	

	for j in `seq 1 3`
	do
		echo $j

	time Rscript R/temperatura.R $i $j
done
done

