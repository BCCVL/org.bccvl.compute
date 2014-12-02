#!/bin/sh


work_dir=$1
search_vars=$2
output=$3

opt_source_dir=/home/ec2-user/hwork/code/org.bccvl.compute/src/org/bccvl/compute/opt
tools_dir=$opt_source_dir/tools

if [[ -e $work_dir/.model_results_header_done ]];
    then echo Done $work_dir; 
    exit 0; 
fi; 

pushd $work_dir 2>&1 > /dev/null

function work()
{
    search_vars=$1

    cmd="python $tools_dir/extract_params.py  \
         --input=params.json \
         --search-variables=$search_vars \
         --header"
    echo $(date) $cmd >> .cmd_log    
    eval $cmd || ( >&2 echo "Failed: $cmd" && exit 1)
    
    echo -n ","
    
    cmd="python $tools_dir/extract_csv_column.py
         --input=output/biomod2.modelEvaluation.csv
          --column=0;"
    
    echo $(date) $cmd >> .cmd_log    
    eval $cmd || ( >&2 echo "Failed: $cmd" && exit 1)
    echo;
}

if work $search_vars > $output; then
    touch .model_results_header_done; 
fi

popd 2>&1 > /dev/null
