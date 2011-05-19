#!/bin/sh

N=$1

if [ -z $N ]
then
    N=1000
fi

echo \[
X=0
while [ "$X" -lt "$N" ]
do
    M=""
    while [ -z "$M" ]
    do
	M=`wget -qO- http://localhost:8080/next_msg | sed '1d' | sed '$d' | sed s/\"/\'/g`
    done
    echo "$M,"
    C=$(( `echo "$M"|wc -l` / 8 ))
    X=$(( $X + $C ))
    #echo $C
    #echo $X
    sleep .25
done

echo \]
