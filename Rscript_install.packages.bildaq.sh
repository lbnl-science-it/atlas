# from cat Dockerfile.base | grep Rscript | sed 's/\\//g'
# so as to install them on new ec2 instance
# to be run as root so package avail to everyone
# some manual edits
export DEBIAN_FRONTEND=noninteractive
export TERM=dumb
export TZ=PST8PDT
export NO_COLOR=1
#//sudo     Rscript --quiet --no-readline --slave -e 'install.packages( "pacman", repos = "http://cran.us.r-project.org")'        

## needed by tempo team
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("tidyverse")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("arrow")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("igraph")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("readxl")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("stringr")'    
#xx sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load(c("tidyverse", "sf", "data.table", "maptools", "arrow", "rgdal", "igraph", "readxl", "stringr"))'      # this concat version somehow does not work in R 4.1.2 :-\
## LD_emission
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("optparse")'    


#sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("")'    
sudo     Rscript --quiet --no-readline --slave -e 'library()'   | sort | tee R_library_list.out.bildaq.txt  
