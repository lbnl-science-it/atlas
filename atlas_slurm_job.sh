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
sleep 3600



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
sleep 180
echo "date after sleep"
date

SIMG_SIF=/global/home/users/tin/gs/singularity-repo/atlas_sfb_v2.sif

singularity exec -B /global/scratch/users/tin/inbox:/mnt atlas_sfb_v2.sif  df /mnt
singularity exec -B /global/scratch/users/tin/inbox:/mnt atlas_sfb_v2.sif  uptime
singularity exec -B /global/scratch/users/tin/inbox:/mnt atlas_sfb_v2.sif  /usr/bin/Rscript /main.R
