#!/bin/bash

count=0
TOSKIP=$1

function launch {
    for f in ./"$1"/*; do
	if [ -d "$f" ]; then
	    launch "$f"
	elif [ -f "$f" ]; then
	    case "$f" in
		*.pdf) xournal "$f";;
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
	    launch "$d"
        fi
        ((count++))
    fi
done
