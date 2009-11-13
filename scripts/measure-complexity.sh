#!/bin/sh

if [ -z "$*" ]
then
    echo "Usage: $0 MAXDECLS [STEP] [OPTIONS for dedukti]"
    echo "Script to plot the runtime of dedukti as number of declarations and rules increases, using gnuplot."
    exit 1
fi

maxdecls=$1
step=${2-100}
shift
shift
DKFLAGS=$*
stem=`mktemp -u dedukti-profile-XXXXXX`
testfile=$stem.dk
datafile=$stem.dat
logfile=$stem.log
touch $testfile

echo Using $testfile as test file.

for i in `seq $step $step $maxdecls`
do
    echo Run for $i declarations ...
    (echo "t0 : Type."
     for j in `seq 1 $i`
     do
	 echo "t$j : Type."
	 echo "[] t$j --> t0."
     done) > $testfile
    run=`(TIMEFORMAT='%R'; time dedukti $DKFLAGS $testfile 1>$logfile 2>&1) 2>&1`
    echo -e "\t${run}s"
    echo "$i $run" >> $datafile
done

echo Press return to continue.
(echo "plot \"$datafile\" smooth csplines"; read) | gnuplot

rm -f $testfile ${testfile}o ${testfile}i
echo Removed $testfile.
echo Removed ${testfile}o.
echo Removed ${testfile}i.
echo Data file in $datafile.
echo Log file in $logfile.
