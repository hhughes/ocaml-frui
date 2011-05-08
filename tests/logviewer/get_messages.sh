#!/bin/sh

N=$1

if [ -z $N ]
then
    N=1000
fi

echo \[

for x in $(seq 1 $N)
do
    M=`wget -qO- http://localhost:8080/next_msg | sed '1d' | sed '$d' | sed s/\"/\'/g`
    if [ -n "$M" ]
    then
	echo "$M"
	echo ,
    fi
    sleep .25
done

echo \]