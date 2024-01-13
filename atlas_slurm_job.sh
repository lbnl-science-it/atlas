#!/bin/sh


#SBATCH --job-name=slurm-test-lr5
#SBATCH --partition=lr5
###SBATCH -n 4
#SBATCH --qos=lr_normal
####SBATCH --qos=lr_lowprio
#SBATCH --account=scs
#SBATCH --nodes=1
#####SBATCH --mem-per-cpu=2G
# Wall clock limit in hh:mm:ss ::
#SBATCH --time=00:10:00     
###SBATCH --mail-type=all
#SBATCH --mail-type=fail
#SBATCH --mail-user=tin@lbl.gov
#SBATCH -o   slurm_TEST_OUT.txt


# submit job as  
#    sbatch   ./atlas_slurm_job.sh 

# specific param can be overwitten when submitting job, eg:
# sbatch --partition=lr_6  --account=scs  ./atlas_slurm_job.sh
hostname
date
uptime
sleep 6



hostname
uptime
date
pwd
echo ---------------------------------------
cat /etc/os-release
echo ---------------------------------------
df -hl
echo ---------------------------------------
cat /proc/mounts
echo ---------------------------------------
echo "date before sleep"
date
sleep 18
echo "date after sleep"
date


echo "Up next: run singularity atlas container"

SIMG_SIF=/global/home/users/tin/gs/singularity-repo/atlas_sfb_v2.sif

#singularity exec -B /global/scratch/users/tin/inbox:/mnt $SIMG_SIF  /usr/bin/Rscript /main.R

singularity exec -B $SIMG_SIF  uptime

singularity exec -B /global/scratch/users/tin/atlas/atlas_input:/atlas_input  -B /global/scratch/users/tin/atlas/atlas_output:/atlas_output $SIMG_SIF bash -c "date >> /atlas_input/test_tin.txt"
singularity exec -B /global/scratch/users/tin/atlas/atlas_input:/atlas_input  -B /global/scratch/users/tin/atlas/atlas_output:/atlas_output $SIMG_SIF bash -c "date >> /atlas_output/test_tin.txt"
singularity exec -B /global/scratch/users/tin/atlas/atlas_input:/atlas_input  -B /global/scratch/users/tin/atlas/atlas_output:/atlas_output $SIMG_SIF /usr/bin/Rscript /main.R


# singularity shell -B /global/scratch/users/tin/atlas/atlas_input:/atlas_input  -B /global/scratch/users/tin/atlas/atlas_output:/atlas_output $SIMG_SIF


sleep 31
