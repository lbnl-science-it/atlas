# from cat Dockerfile.base | grep Rscript | sed 's/\\//g'
# so as to install them on new ec2 instance
# to be run as root so package avail to everyone
# some manual edits
export DEBIAN_FRONTEND=noninteractive
export TERM=dumb
export TZ=PST8PDT
export NO_COLOR=1
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("ggplot2",    repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("maps",    repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("dplyr",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("sf",  repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("fields",  repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("Imap",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("raster",  repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("readxl",    repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("ncdf4",   repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("rgdal", repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("ggmap",   repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("lawn",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("sp",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("shapefiles",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("tmap",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("spdplyr",     repos = "http://cran.us.r-project.org")'    ;
#sudo     Rscript --quiet --no-readline --slave -e 'library()'   | sort | tee R_library_list.out.1.txt  ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("MASS",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("reshape2",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("cowplot",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("corrplot",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("RColorBrewer",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("fmsb",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("ggmap",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("tictoc",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("stargazer",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("psych",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("GPArotation",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("cluster",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("factoextra",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("DandEFA",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("xtrable",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("psychTools",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("aCRM",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("clusterCrit",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("data.table",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("tigris",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("DAAG",     repos = "http://cran.us.r-project.org")'    ;
#sudo     Rscript --quiet --no-readline --slave -e 'library()'   | sort | tee R_library_list.out.2.txt  ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("RSQLite",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("rgeos",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("gpclib",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("utils",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("plyr",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("maptools",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("datamart",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("dismo",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("openair",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("broom",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("gridExtra",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("foreach",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("doParallel",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("sandwich",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("lmtest",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("cvTools",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("timeDate",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("lubridate",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("zoo",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("stringr",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("stringi",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("chron",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("proj4",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("akima",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("RColorBrewer",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("directlabels",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("FactoMineR",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("rstudioapi",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("iterators",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("doSNOW",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("Hmisc",     repos = "http://cran.us.r-project.org")'    ;
#sudo     Rscript --quiet --no-readline --slave -e 'library()'   | sort | tee R_library_list.out.3.txt  ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages(c("aCRM", "akima", "broom", "cluster", "clusterCrit", "corrplot", "DandEFA", "datamart", "data.table", "directlabels", "dismo", "dplyr", "factoextra", "FactoMineR", "fields", "fmsb", "gdata", "ggmap", "ggplot2", "ggthemes", "gpclib", "gridExtra", "Hmisc", "lubridate", "maps", "maptools", "ncdf", "ncdf4", "openair", "openxlsx", "proj4", "psych", "psychTools", "raster", "RColorBrewer", "readxl", "reshape2", "rgdal", "rgeos", "rJava", "rstudioapi", "scales", "sf", "sp", "stargazer", "stringi", "stringr", "tibble", "tictoc", "tidyr", "tigris", "timeDate", "tmap", "units", "utils", "xlsx", "xtable", "zoo"),     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages(c( "ster", "sp", "rgeos", "geosphere", "doParallel", "iterators", "foreach", "rgdal", "doSNOW", "openxlsx"),     repos = "http://cran.us.r-project.org")'    ;
#sudo     Rscript --quiet --no-readline --slave -e 'library()'   | sort | tee R_library_list.out.4.txt  ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages("tidycensus",     repos = "http://cran.us.r-project.org")'    ;
sudo     Rscript --quiet --no-readline --slave -e 'install.packages(c("psych", "ggpairs", "tableone"),     repos = "http://cran.us.r-project.org")'    ;

sudo     Rscript --quiet --no-readline --slave -e 'install.packages( "pacman", repos = "http://cran.us.r-project.org")'        ;

sudo     Rscript --quiet --no-readline --slave -e '{ library(pacman); p_load( utils, foreign, pastecs, mlogit, graphics, VGAM, aod, plotrix, Zelig, Zelig, vctrs, maxLik, plyr, MASS, ordinal, mltest, haven, stargazer, stringr, tidyverse ) }' ;
sudo     Rscript --quiet --no-readline --slave -e '{ library(pacman); p_load( gWidgets2, gWidgets2tcltk, miscTools, lmtest, dplyr, BiocManager ) }' ;
sudo     Rscript --quiet --no-readline --slave -e '{ library(pacman); p_load( ggplot2, scales ) }' ;
sudo     Rscript --quiet --no-readline --slave -e '{ library(pacman); p_load( snow, foreach, parallel, doParallel, tictoc ) }' ;
sudo     Rscript --quiet --no-readline --slave -e '{ library(pacman); p_load( apollo ) }' ;
sudo     Rscript --quiet --no-readline --slave -e '{ library(pacman); p_load( optparse, argparse, stats ) }' ;
#sudo     #Rscript --quiet --no-readline --slave -e 'p_load( )' ;
#sudo     #Rscript --quiet --no-readline --slave -e '{ library(pacman); p_load(  ) }' ;
sudo     Rscript --quiet --no-readline --slave -e 'library()'   | sort | tee R_library_list.out.5.txt  ;
