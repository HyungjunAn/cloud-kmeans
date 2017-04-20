#!/bin/bash

echo "Running Slave"
input=$1
for i in $(seq ${input})
do
     let port=81+$i
     #stack exec cloud-plag-exe slave 164.125.34.198 $i &
     #~/.local/bin/cloud-kmeans-exe slave 164.125.34.198 $i &
     stack exec cloud-kmeans-exe slave 164.125.34.198 $i &
     pid[$i]=$!
done
echo "------Running complete-------"

read -n 1 -p "Kill Slave"

for i in $(seq ${input})
do
     kill ${pid[$i]}
done

echo "End"
