#!/bin/sh

N=$1
COUNT=0

if [ -z $N ]
then
    N=100
fi

while [ $COUNT -lt $N ]
do
    M=`wget -qO- http://localhost:8080/next_msg | sed '1d' | sed '$d' | sed s/\"/\'/g`
    if [ -n "$M" ]
    then
	echo "[$M]" >> "test2-$COUNT.json"
	COUNT=`expr $COUNT + 1`
    fi
    sleep .25
done