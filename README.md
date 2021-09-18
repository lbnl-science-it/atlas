# atlas

Container with R and necessary packages to run BEAM/Atlas vehicle simulation.


Example running R script via Docker
-----------------------------------

cd /global/data/transportation/ATLAS/static/urbansim/

// R script in home dir, bind mounted to container
docker run -v "$PWD":/mnt -it --entrypoint=Rscript  ghcr.io/lbnl-science-it/atlas:main  /mnt/model_application/Model_application_hima.R

// running a bash shell, can call R from there
docker run                -it --entrypoint=bash     ghcr.io/lbnl-science-it/atlas:main


Example running R script via Singularity
----------------------------------------

cd /global/data/transportation/ATLAS/static/urbansim/

singularity pull  docker://ghcr.io/lbnl-science-it/atlas:main 
singularity exec  docker://ghcr.io/lbnl-science-it/atlas:main  Rscript ./model_application/Model_application_hima.R  

// other things to try for debug use
singularity shell docker://ghcr.io/lbnl-science-it/atlas:main  # get bash prompt, can call R afterward
singularity run   docker://ghcr.io/lbnl-science-it/atlas:main  # get R    prompt


