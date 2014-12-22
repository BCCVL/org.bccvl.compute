#!/bin/bash

function log_eval()
{
    cmd="$1"
    log="$2"
    
    if [[ ! -z $log ]]; then
        echo $(date) $cmd >> $log
    fi

    if ! eval $cmd; then
        echo failure running command: $cmd
        exit 1
    fi
}
