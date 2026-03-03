#!/usr/bin/python
import math
import sys
import os
import re
import time
#======================================================================
# functions
#======================================================================
def write_submission_script(filename, scenario):
    submit_file = open(filename,'w')
    submit_str = "#!/bin/bash\n\n"
    submit_str += "#SBATCH --comment=changesc \n"
    submit_str += "#SBATCH --partition=fat_rome \n"
    submit_str += "#SBATCH --time=00-00:15:00 \n"
    submit_str += "#SBATCH --job-name=sc_%s \n" % (scenario)
    submit_str += "#SBATCH --output=sc-%s.txt \n" % (scenario)
    submit_str += "#SBATCH --error=error_sc-%s.txt \n" % (scenario)
    submit_str += "#SBATCH --ntasks=100 \n"
    submit_str += "#SBATCH --mail-type=FAIL \n"
    submit_str += "#SBATCH --mail-user=mariken.dewit@wur.nl \n \n"

    submit_str += "cd /gpfs/work4/0/prjs1002/change_sc/scripts/ \n"

    submit_str += "module load 2023 \n"
    submit_str += "module load R/4.3.2-gfbf-2023a \n"

    submit_str += "for i in `seq 1 $SLURM_NTASKS`; do \n"
    submit_str += "( \n"
    submit_str += "Rscript main_NGM_wnv.R -s '%s' -r $i \n" % (scenario)
    submit_str += ") & \n"
    submit_str += "done \n"
    submit_str += "wait \n"

    submit_str += "exit 0 "

    submit_file.write(submit_str)
    submit_file.close()

#======================================================================
# Generate submission files
#======================================================================
parameterSet = [
'ref',
'SSP1-full',
'SSP3-full',
'SSP4-full',
'SSP5-full',
'ref-uniform',
'SSP1-uniform',
'SSP3-uniform',
'SSP4-uniform',
'SSP5-uniform',
]

for i in range(5,10):
    submit_file_name = 'change_sc_%d.sh' % (i+1)
    scenario = parameterSet[i] 
    write_submission_script(submit_file_name,scenario)
    print('submitting change scenario i:%d' % (i+1))
    os.system("chmod +x %s" % (submit_file_name))
    os.system("sbatch %s" % (submit_file_name))
    time.sleep(.1)
