#!/bin/sh

## this would need to be converted to python code and merged into Pilate's run.py
## but essentially need to execute the instructions here


source settings.env

## for testing till container build finish
echo for test, run this \
docker run -v /home/ubuntu/AWS/PILATES/pilates/atlas:/mnt  \
           -v $atlas_input_dir:/mnt/atlas_input    \
           -v $atlas_output_dir:/mnt/atlas_output  \
    -it --entrypoint=bash  ghcr.io/lbnl-science-it/atlas:main



## debug use without waiting for container to rebuild 
docker pull ghcr.io/lbnl-science-it/atlas:integration  

echo \
docker run \
           -v `pwd`:/opt/gitrepo/atlas   \
           -v $atlas_input_dir:/atlas_input    \
           -v $atlas_output_dir:/atlas_output  \
    -it --entrypoint=/usr/bin/Rscript  \
     ghcr.io/lbnl-science-it/atlas:integration /opt/gitrepo/atlas/main.R --outyear=2017 --freq=1   --indir=/atlas_input  --outdir=/atlas_output  --basedir=/  --codedir=/opt/gitrepo/atlas


## once the container build is complete (which is currently in "integration" branch, and I have restructured the files  to place the script at / of the container so your "library" files can be easily found):
##  
docker run \
           -v $atlas_input_dir:/atlas_input    \
           -v $atlas_output_dir:/atlas_output  \
     ghcr.io/lbnl-science-it/atlas:integration  --outyear=2017 --freq=1    --codedir=/

