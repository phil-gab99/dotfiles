#!/bin/bash

count=0
TOSKIP=$1

function unzipfiles {
    for f in ./"$1"/*; do
	if [ -d "$f" ]; then
	    unzipfiles "$f"
	elif [ -f "$f" ]; then
            ABS=$(realpath "$f")
            DIR=$(dirname "$ABS")
	    case "$f" in
		*.zip) unzip "$f" -d "$DIR";;
		# *.lmc) gedit "$f";;
		*) # echo "$f"
	    esac
	fi
    done
}

for d in *; do
    if [ -d "$d" ]; then
        echo "---------------------"
        ls "$d"
        echo "---------------------"
        if [ $count -ge $TOSKIP ]; then
	    unzipfiles "$d"
        fi
        ((count++))
    fi
done
