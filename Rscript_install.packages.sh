# from cat Dockerfile.base | grep Rscript | sed 's/\\//g'
# so as to install them on new ec2 instance
# to be run as root so package avail to everyone
# some manual edits
export DEBIAN_FRONTEND=noninteractive
export TERM=dumb
export TZ=PST8PDT
export NO_COLOR=1
sudo     Rscript --quiet --no-readline --slave -e 'install.packages( "pacman", repos = "http://cran.us.r-project.org")'        

sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("ggplot2")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("maps")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("dplyr")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("sf")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("fields")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("Imap")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("raster")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("readxl")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("ncdf4")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("rgdal")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("ggmap")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("lawn")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("sp")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("shapefiles")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("tmap")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("spdplyr")'    
#sudo     Rscript --quiet --no-readline --slave -e 'library()'   | sort | tee R_library_list.out.1.txt  
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("MASS")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("reshape2")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("cowplot")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("corrplot")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("RColorBrewer")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("fmsb")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("ggmap")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("tictoc")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("stargazer")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("psych")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("GPArotation")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("cluster")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("factoextra")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("DandEFA")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("xtrable")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("psychTools")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("aCRM")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("clusterCrit")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("data.table")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("tigris")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("DAAG")'    
#sudo     Rscript --quiet --no-readline --slave -e 'library()'   | sort | tee R_library_list.out.2.txt  
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("RSQLite")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("rgeos")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("gpclib")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("utils")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("plyr")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("maptools")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("datamart")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("dismo")'                 # error installing
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("openair")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("broom")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("gridExtra")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("foreach")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("doParallel")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("sandwich")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("lmtest")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("cvTools")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("timeDate")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("lubridate")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("zoo")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("stringr")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("stringi")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("chron")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("proj4")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("akima")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("RColorBrewer")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("directlabels")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("FactoMineR")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("rstudioapi")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("iterators")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("doSNOW")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("Hmisc")'    
#sudo     Rscript --quiet --no-readline --slave -e 'library()'   | sort | tee R_library_list.out.3.txt  
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load(c("aCRM", "akima", "broom", "cluster", "clusterCrit", "corrplot", "DandEFA", "datamart", "data.table", "directlabels", "dismo", "dplyr", "factoextra", "FactoMineR", "fields", "fmsb", "gdata", "ggmap", "ggplot2", "ggthemes", "gpclib", "gridExtra", "Hmisc", "lubridate", "maps", "maptools", "ncdf", "ncdf4", "openair", "openxlsx", "proj4", "psych", "psychTools", "raster", "RColorBrewer", "readxl", "reshape2", "rgdal", "rgeos", "rJava", "rstudioapi", "scales", "sf", "sp", "stargazer", "stringi", "stringr", "tibble", "tictoc", "tidyr", "tigris", "timeDate", "tmap", "units", "utils", "xlsx", "xtable", "zoo"))'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load(c( "ster", "sp", "rgeos", "geosphere", "doParallel", "iterators", "foreach", "rgdal", "doSNOW", "openxlsx"))'    
#sudo     Rscript --quiet --no-readline --slave -e 'library()'   | sort | tee R_library_list.out.4.txt  
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load("tidycensus")'    
sudo     Rscript --quiet --no-readline --slave -e 'library(pacman); p_load(c("psych", "ggpairs", "tableone"))'    


sudo     Rscript --quiet --no-readline --slave -e '{ library(pacman); p_load( utils, foreign, pastecs, mlogit, graphics, VGAM, aod, plotrix, Zelig, Zelig, vctrs, maxLik, plyr, MASS, ordinal, mltest, haven, stargazer, stringr, tidyverse ) }' 
sudo     Rscript --quiet --no-readline --slave -e '{ library(pacman); p_load( gWidgets2, gWidgets2tcltk, miscTools, lmtest, dplyr, BiocManager ) }' 
sudo     Rscript --quiet --no-readline --slave -e '{ library(pacman); p_load( ggplot2, scales ) }' 
sudo     Rscript --quiet --no-readline --slave -e '{ library(pacman); p_load( snow, foreach, parallel, doParallel, tictoc ) }' 
sudo     Rscript --quiet --no-readline --slave -e '{ library(pacman); p_load( apollo ) }' 
sudo     Rscript --quiet --no-readline --slave -e '{ library(pacman); p_load( optparse, argparse, stats ) }' 
#sudo     #Rscript --quiet --no-readline --slave -e 'p_load( )' 
#sudo     #Rscript --quiet --no-readline --slave -e '{ library(pacman); p_load(  ) }' 
sudo     Rscript --quiet --no-readline --slave -e 'library()'   | sort | tee R_library_list.out.5.txt  