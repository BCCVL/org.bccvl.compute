#!/bin/sh

scripts_dir=$(dirname $(readlink -f $0))
source $scripts_dir/common.sh

opt_source_dir=$scripts_dir/..
tools_dir=$opt_source_dir/tools

work_dir=$1
search_vars=$2
output=$3


pushd $work_dir 2>&1 > /dev/null

function work()
{
    search_vars=$1

    cmd="python $tools_dir/extract_params.py  \
         --input=params.json \
         --search-variables=$search_vars \
         --header"

    log_eval "$cmd" .cmd_log
    
    echo -n ","
    
    cmd="python $tools_dir/extract_csv_column.py
         --input=output/biomod2.modelEvaluation.csv
          --column=0;"
    
    log_eval "$cmd" .cmd_log
    echo;
}

work $search_vars > $output

popd 2>&1 > /dev/null
