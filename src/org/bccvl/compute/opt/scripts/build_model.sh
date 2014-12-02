#!/bin/sh

work_dir=$1
r_script=$2


if [[ -e $work_dir/.Rbatchdone ]];
    then echo Done $work_dir
    exit 0; 
fi; 

pushd $work_dir 2>&1 > /dev/null
echo Working in $work_dir; 


R CMD BATCH $r_script;
ret=$?
if [[ $ret -eq 0 ]];
    then touch .Rbatchdone; 
fi; 

popd 2>&1 > /dev/null
