#!/bin/bash

a=0

for vhd_file in `find $1 -mindepth 1 -name "*.vhd" | sort`; do
	word=`wc -l $vhd_file | cut -f 1 -d " "`
	echo $vhd_file "..." $word
	a=$(($a + $word))
done

echo "Total = " $a

exit 0
