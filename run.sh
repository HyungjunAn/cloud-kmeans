#!/bin/bash

if   [ "$1" = "master" ]; then
    echo "Running Master"
    sudo stack exec cloud-kmeans-exe master 164.125.34.198 70 5 --allow-different-user
    # <HOST> <PORT> <CHUNKS>

elif [ "$1" = "slave" ]; then
    echo "Running Slave"
    input=$2
    for i in $(seq ${input})
    do
        let port=81+$i
        sudo stack exec cloud-kmeans-exe slave 164.125.34.198 $i --allow-different-user &
        pid[$i]=$!
    done
    echo "------Running complete-------"

    read -n 1 -p "Kill Slave"

    for i in $(seq ${input})
    do
        sudo kill ${pid[$i]}
    done
fi
echo "End"

