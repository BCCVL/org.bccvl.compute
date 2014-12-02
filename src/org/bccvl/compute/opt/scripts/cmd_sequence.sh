#!/bin/sh

set -e

work_dir=$PWD
opt_source_dir=/home/ec2-user/hwork/code/org.bccvl.compute/src/org/bccvl/compute/opt
scripts_dir=$opt_source_dir/scripts
tools_dir=$opt_source_dir/tools

input_params=$work_dir/params.json
search_vars=$work_dir/search_vars.txt

grid_dir=start_grid
function run_task()
{
    cmd=$1
    lock_file=$2
    if [[ ! -e $lock_file ]]; then
        echo $cmd
        if eval $cmd; then
            echo $(date) $cmd >> $lock_file
        fi
    fi
    
}

generate_params_set_tool="$tools_dir/generate_params_set.py"
gen_grid_cmd="python $generate_params_set_tool \
	      --file=$input_params \
	      --search-variables=$search_vars \
              --workdir=$grid_dir"

run_task "$gen_grid_cmd" .tsk_gen_grid_cmd

seed_list=seed_list.txt
gen_seed_list_cmd="ls $grid_dir/*json > $seed_list"

run_task "$gen_seed_list_cmd" .tsk_gen_seed_list_cmd

layout_tool=$scripts_dir/layout_from_list.sh
inst_dir=inst
layout_cmd="sh $layout_tool $seed_list $inst_dir"

run_task "$layout_cmd" .tsk_layout_cmd

cmds_file=cmds.txt
if [[ ! -s $cmds_file ]]; then
    build_model_tool=$scripts_dir/build_model.sh
    r_script=$work_dir/test_rf.new.bccvl.R

    for f in $inst_dir*; 
        do echo $build_model_tool \
    	$PWD/$f \
    	$r_script; 
    done > $cmds_file
fi

ncpu=16

par_build_model_cmd="parallel -j $ncpu < $cmds_file"
run_task "$par_build_model_cmd" .tsk_par_build_model_cmd

eval_cmds_file=eval_cmds.txt
if [[ ! -s $eval_cmds_file ]]; then
    gen_results_tool=$scripts_dir/gen_results.sh
    for f in $inst_dir*; 
        do echo $gen_results_tool \
    	$PWD/$f \
    	$search_vars \
        $PWD/$f/results.csv ; 
    done > $eval_cmds_file
fi
 
par_gen_results_cmd="parallel -j $ncpu < $eval_cmds_file"
run_task "$par_gen_results_cmd" .tsk_par_gen_results_cmd

header_csv_cmds_file=header_csv_cmds.txt
if [[ ! -s $header_csv_cmds_file ]]; then
    gen_results_tool=$scripts_dir/gen_results_header.sh
    for f in $inst_dir*; 
        do echo $gen_results_tool \
    	$PWD/$f \
    	$search_vars \
        $PWD/$f/results_header.csv ; 
    done > $header_csv_cmds_file
fi
 
par_gen_headers_cmd="parallel -j $ncpu < $header_csv_cmds_file"
run_task "$par_gen_headers_cmd" .tsk_par_gen_headers_cmd

first_header=$(ls $inst_dir*/results_header.csv | head -1)

gen_results_csv_cmd="cat $first_header $inst_dir*/results.csv > results.csv"

run_task "$gen_results_csv_cmd " .tsk_results_csv_cmd
