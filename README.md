# atlas

Container with R and necessary packages to run BEAM/Atlas vehicle simulation.


Example running R script via Docker
-----------------------------------

``` bash

cd /global/data/transportation/ATLAS/static/urbansim/

## R script in home dir, bind mounted to container
#XX docker run -v /global/data/transportation/ATLAS/static/urbansim:/global/data/transportation/ATLAS/static/urbansim -it --entrypoint=Rscript  ghcr.io/lbnl-science-it/atlas:main  /global/data/transportation/ATLAS/static/urbansim/model_application/Model_application_hima.R 
docker run -v /global/data/transportation/ATLAS/static/urbansim:/mnt -it --entrypoint=Rscript  ghcr.io/lbnl-science-it/atlas:main  /mnt/model_application/Model_application_hima.R 
docker run -v "$PWD":/mnt -it --entrypoint=Rscript  ghcr.io/lbnl-science-it/atlas:main  /mnt/model_application/Model_application_hima.R

## running a bash shell, can call R from there
docker run                -it --entrypoint=bash     ghcr.io/lbnl-science-it/atlas:main
docker run -v /global/data/transportation/ATLAS/static/urbansim:/mnt -it --entrypoint=bash  ghcr.io/lbnl-science-it/atlas:main  
    root@0b5815f5b441:/# export R_LIBS=/usr/local/lib/R/site-library/
    root@0b5815f5b441:/# Rscript /mnt/model_application/Model_application_hima.R

```

Example running R script via Singularity
----------------------------------------

```

cd /global/data/transportation/ATLAS/static/urbansim/

singularity pull  docker://ghcr.io/lbnl-science-it/atlas:main 
singularity exec  docker://ghcr.io/lbnl-science-it/atlas:main  Rscript ./model_application/Model_application_hima.R  

// other things to try for debug use
singularity shell docker://ghcr.io/lbnl-science-it/atlas:main  # get bash prompt, can call R afterward
singularity run   docker://ghcr.io/lbnl-science-it/atlas:main  # get R    prompt

```
