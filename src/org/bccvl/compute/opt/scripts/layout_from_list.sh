#!/bin/bash

set -e
input_list=$1
name_stub=${2:-inst}
work_dir=${3:-$(pwd)}

opt_source_dir=/home/ec2-user/hwork/code/org.bccvl.compute/src/org/bccvl/compute/opt
tools_dir=$opt_source_dir/tools

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


