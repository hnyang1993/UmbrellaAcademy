#!/bin/bash
 
NUMJOBS="$1"

for i in `seq $NUMJOBS`;
do

#PROGRAM="count.sh"
#PROGRAM="mycode.exe `cat mydata-$i.csv`"
mkdir $i 
cp *.R ./$i
cd ./$i

sbatch -p general -N 1 --mem=5g -n 1 -t 7- --job-name=“rfcv” R CMD BATCH --no-save Mar_Freq_rf.R Mar_Freq_rf.Rout

cd ../
done
