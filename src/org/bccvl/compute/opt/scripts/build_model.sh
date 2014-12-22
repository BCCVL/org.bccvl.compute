#!/bin/sh

set -e
scripts_dir=$(dirname $(readlink -f $0))
source $scripts_dir/common.sh

opt_source_dir=$scripts_dir/..
tools_dir=$opt_source_dir/tools


work_dir=$1
r_script=$2

pushd $work_dir 2>&1 > /dev/null
echo Working in $work_dir; 

R CMD BATCH $r_script;
ret=$?

popd 2>&1 > /dev/null
