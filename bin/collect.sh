#!/bin/bash

count=0
TOSKIP=$1

function collect {
    for f in ./"$1"/*; do
	if [ -d "$f" ]; then
	    collect "$f"
	elif [ -f "$f" ]; then
	    case "$f" in
		corr-*.pdf) mv "$f" ../*-corr/;;
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
	    collect "$d"
        fi
        ((count++))
    fi
done
