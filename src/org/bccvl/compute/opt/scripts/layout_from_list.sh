#!/bin/bash

scripts_dir=$(dirname $(readlink -f $0))
source $scripts_dir/common.sh

opt_source_dir=$scripts_dir/..
tools_dir=$opt_source_dir/tools

set -e
input_list=$1
name_stub=${2:-inst}
work_dir=${3:-$(pwd)}

index=1
while read file
do
    out_dir=$work_dir/$name_stub.$index
    mkdir -p $out_dir $out_dir/output
    params_out=$out_dir/params.json
    cmd="python $tools_dir/json-field-set.py 
             --input=$file
             --output=$params_out
             --attr-value=\"env/outputdir=$out_dir/output\"
             --attr-value=\"env/scriptdir=$out_dir\""
    eval $cmd

    index=$((index+1))

done < $input_list


