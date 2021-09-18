# Dockerfile for creating R container 
# and add specific library needed by projects by LBNL/ETA
# local build should work, but it no longer has all the updates done Dockerfile.metabolic
# docker build -t lbnl-science-it/atlas -f Dockerfile .  | tee Dockerfile.monolithic.log 

# rscript has its own set of fairly long install...
# (model after tin6150/r4eta container)
 

#FROM r-base:3.6.3
# r-base:3.6.3 was last of 3.x, ca 2019
FROM r-base:4.0.3
#FROM r-base:4.1.1
# r-base:4.1.1  # ca 2021.0815
MAINTAINER Tin (at) LBL.gov

ARG DEBIAN_FRONTEND=noninteractive
#ARG TERM=vt100
ARG TERM=dumb
ARG TZ=PST8PDT 

#ENV PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/opt/conda/bin

RUN echo  ''  ;\
    touch _TOP_DIR_OF_CONTAINER_  ;\
    echo "begining docker build process at " | tee -a _TOP_DIR_OF_CONTAINER_  ;\
    date | tee -a       _TOP_DIR_OF_CONTAINER_ ;\
    echo "installing packages via apt"       | tee -a _TOP_DIR_OF_CONTAINER_  ;\
    export TERM=dumb  ;\
    apt-get update ;\
    # ubuntu:   # procps provides uptime cmd
    apt-get -y --quiet install git file wget gzip bash less vim procps ;\
    apt-get -y --quiet install units libudunits2-dev curl r-cran-rcurl libcurl4 libcurl4-openssl-dev libssl-dev r-cran-httr  r-cran-xml r-cran-xml2 libxml2 rio  java-common javacc javacc4  openjdk-8-jre-headless ;\
    apt-get -y --quiet install openjdk-14-jre-headless   ;\ 
    # gdal cran install fails, cuz no longer libgdal26, but now libgdal28
    # apt-file search gdal-config
    apt-get -y --quiet install gdal-bin gdal-data libgdal-dev  libgdal28  ;\
    apt-get -y --quiet install r-cran-rgdal  ;\
    apt-get -y --quiet install libgeos-dev   ;\
    # default-jdk is what provide javac !   # -version = 11.0.6
    # ref: https://www.digitalocean.com/community/tutorials/how-to-install-java-with-apt-on-ubuntu-18-04
    # update-alternatives --config java --skip-auto # not needed, but could run interactively to change jdk
    apt-get -y --quiet install default-jdk r-cran-rjava  ;\ 
    R CMD javareconf  ;\
    # debian calls it libnode-dev (ubuntu call it libv8-dev?)
    apt-get -y --quiet install libnode-dev libv8-dev ;\
    cd /     ;\
    echo ""  ;\
    echo '==================================================================' ;\
    echo "install for rstudio GUI (Qt)"      | tee -a _TOP_DIR_OF_CONTAINER_  ;\
    date | tee -a      _TOP_DIR_OF_CONTAINER_                                 ;\
    echo '==================================================================' ;\
    #-- rstudio dont seems to exist in Debian bullseye/sid :/
    #-- apt-get --quiet install rstudio  ;\
    #xx apt-get -y --quiet install r-cran-rstudioapi libqt5gui5 libqt5network5  libqt5webenginewidgets5 qterminal net-tools ;\
    apt-get -y --quiet install apt-file ;\
    apt-file update ;\
    mkdir -p Downloads &&  cd Downloads ;\
    #xx wget --quiet https://download1.rstudio.org/desktop/bionic/amd64/rstudio-1.2.5033-amd64.deb  -O rstudio4deb10.deb ;\
    #xx apt-get -y --quiet install ./rstudio4deb10.deb     ;\
    cd /    ;\
    echo ""  

RUN echo ''  ;\
    echo '==================================================================' ;\
    echo "git cloning the repo for reference/tracking" | tee -a _TOP_DIR_OF_CONTAINER_ ;\
    apt-get -y --quiet install git-all  ;\
    test -d /opt/gitrepo  || mkdir -p /opt/gitrepo        ;\
    cd /opt/gitrepo       ;\
    #xx test -d /opt/gitrepo/atlas  || git clone https://github.com/lbnl-science-it/atlas.git  ;\
    test -d /opt/gitrepo/atlas  || mkdir -p /opt/gitrepo/atlas ;\
    #xx cd /opt/gitrepo/atlas &&  git pull             ;\
    cd /     ;\
    echo ""  

COPY . /opt/gitrepo/atlas
#COPY . /r4eta


RUN echo ''  ;\
    cd   /   ;\
    echo '==================================================================' ;\
    echo '==================================================================' ;\
    echo '==================================================================' ;\
    echo "installing packages cran packages - part 1" | tee -a _TOP_DIR_OF_CONTAINER_  ;\
    date | tee -a      _TOP_DIR_OF_CONTAINER_                        ;\
    echo '==================================================================' ;\
    echo '==================================================================' ;\
    echo '==================================================================' ;\
    echo '' ;\
    export TERM=dumb  ;\
    # initialization1.R
    Rscript --quiet --no-readline --slave -e 'install.packages("ggplot2",    repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("maps",    repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("dplyr",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("sf",  repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("fields",  repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("Imap",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("raster",  repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("readxl",    repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("ncdf4",   repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("rgdal", repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("ggmap",   repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("lawn",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("sp",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("shapefiles",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("tmap",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("spdplyr",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'library()'   | sort | tee R_library_list.out.1.txt  ;\
    ls /usr/local/lib/R/site-library | sort | tee R-site-lib-ls.out.1.txt   ;\
    echo "Done installing packages cran packages - part 1" | tee -a _TOP_DIR_OF_CONTAINER_     ;\
    date | tee -a      _TOP_DIR_OF_CONTAINER_                      ;\
    echo ""

RUN echo ''  ;\
    echo '==================================================================' ;\
    echo '==================================================================' ;\
    echo '==================================================================' ;\
    echo "installing packages cran packages - part 2" | tee -a _TOP_DIR_OF_CONTAINER_  ;\
    date | tee -a      _TOP_DIR_OF_CONTAINER_                        ;\
    echo '==================================================================' ;\
    echo '==================================================================' ;\
    echo '==================================================================' ;\
    echo '' ;\
    export TERM=dumb  ;\
    # initialization2.R
    Rscript --quiet --no-readline --slave -e 'install.packages("MASS",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("reshape2",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("cowplot",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("corrplot",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("RColorBrewer",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("fmsb",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("ggmap",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("tictoc",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("stargazer",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("psych",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("GPArotation",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("cluster",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("factoextra",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("DandEFA",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("xtrable",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("psychTools",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("aCRM",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("clusterCrit",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("data.table",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("tigris",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("DAAG",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'library()'   | sort | tee R_library_list.out.2.txt  ;\
    ls /usr/local/lib/R/site-library | sort | tee R-site-lib-ls.out.2.txt   ;\
    echo "Done installing packages cran packages - part 2" | tee -a _TOP_DIR_OF_CONTAINER_     ;\
    date | tee -a      _TOP_DIR_OF_CONTAINER_                      ;\
    echo ""

RUN echo ''  ;\
    echo '==================================================================' ;\
    echo '==================================================================' ;\
    echo '==================================================================' ;\
    echo "installing packages cran packages - part 3" | tee -a _TOP_DIR_OF_CONTAINER_  ;\
    date | tee -a      _TOP_DIR_OF_CONTAINER_                        ;\
    echo '==================================================================' ;\
    echo '==================================================================' ;\
    echo '==================================================================' ;\
    echo ''  ;\
    export TERM=dumb  ;\
    # initialization3.R
    Rscript --quiet --no-readline --slave -e 'install.packages("RSQLite",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("rgeos",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("gpclib",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("utils",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("plyr",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("maptools",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("datamart",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("dismo",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("openair",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("broom",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("gridExtra",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("foreach",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("doParallel",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("sandwich",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("lmtest",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("cvTools",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("timeDate",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("lubridate",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("zoo",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("stringr",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("stringi",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("chron",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("proj4",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("akima",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("RColorBrewer",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("directlabels",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("FactoMineR",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("rstudioapi",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("iterators",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("doSNOW",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages("Hmisc",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'library()'   | sort | tee R_library_list.out.3.txt  ;\
    ls /usr/local/lib/R/site-library | sort | tee R-site-lib-ls.out.3.txt   ;\
    echo "Done installing packages cran packages - part 3" | tee -a _TOP_DIR_OF_CONTAINER_     ;\
    date | tee -a      _TOP_DIR_OF_CONTAINER_                      ;\
    echo ""

RUN echo ''  ;\
    echo '==================================================================' ;\
    echo '==================================================================' ;\
    echo '==================================================================' ;\
    echo "installing packages cran packages - part 4" | tee -a _TOP_DIR_OF_CONTAINER_  ;\
    date | tee -a      _TOP_DIR_OF_CONTAINER_                        ;\
    echo '==================================================================' ;\
    echo '==================================================================' ;\
    echo '==================================================================' ;\
    echo ''  ;\
    export TERM=dumb  ;\
    # from library() calls
    Rscript --quiet --no-readline --slave -e 'install.packages(c("aCRM", "akima", "broom", "cluster", "clusterCrit", "corrplot", "DandEFA", "datamart", "data.table", "directlabels", "dismo", "dplyr", "factoextra", "FactoMineR", "fields", "fmsb", "gdata", "ggmap", "ggplot2", "ggthemes", "gpclib", "gridExtra", "Hmisc", "lubridate", "maps", "maptools", "ncdf", "ncdf4", "openair", "openxlsx", "proj4", "psych", "psychTools", "raster", "RColorBrewer", "readxl", "reshape2", "rgdal", "rgeos", "rJava", "rstudioapi", "scales", "sf", "sp", "stargazer", "stringi", "stringr", "tibble", "tictoc", "tidyr", "tigris", "timeDate", "tmap", "units", "utils", "xlsx", "xtable", "zoo"),     repos = "http://cran.us.r-project.org")'    ;\
    # next one added 2021.0829 for Ling's parallel foreach SNOW cluster 
    Rscript --quiet --no-readline --slave -e 'install.packages(c( "ster", "sp", "rgeos", "geosphere", "doParallel", "iterators", "foreach", "rgdal", "doSNOW", "openxlsx"),     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'library()'   | sort | tee R_library_list.out.4.txt  ;\
    ls /usr/local/lib/R/site-library | sort | tee R-site-lib-ls.out.4.txt   ;\
    dpkg --list | tee dpkg--list.txt   ;\
    echo "Done installing packages cran packages - part 4" | tee -a _TOP_DIR_OF_CONTAINER_     ;\
    date | tee -a      _TOP_DIR_OF_CONTAINER_   ;\
    echo ""

RUN echo ''  ;\
    echo '==================================================================' ;\
    echo "installing packages cran packages - part 5" | tee -a _TOP_DIR_OF_CONTAINER_  ;\
    date | tee -a      _TOP_DIR_OF_CONTAINER_                        ;\
    echo '==================================================================' ;\
    echo ''  ;\
    export TERM=dumb  ;\
		## additions by Tin
    Rscript --quiet --no-readline --slave -e 'install.packages("tidycensus",     repos = "http://cran.us.r-project.org")'    ;\
    Rscript --quiet --no-readline --slave -e 'install.packages(c("psych", "ggpairs", "tableone"),     repos = "http://cran.us.r-project.org")'    ;\
    # https://www.rdocumentation.org/packages/pacman/versions/0.5.1
    Rscript --quiet --no-readline --slave -e 'install.packages("pacman" )'       # provides wrapper function like p_load() to install package if needed, then load the library // R 3.5+, seems in R 4.0.3 now ;\
    Rscript --quiet --no-readline --slave -e 'p_load(utils, foreign, pastecs, mlogit, graphics, VGAM, aod, plotrix, Zelig, Zelig, vctrs, maxLik, plyr, MASS, ordinal, mltest, haven, stargazer, stringr, tidyverse)' ;\
    Rscript --quiet --no-readline --slave -e 'p_load( gWidgets2, gWidgets2tcltk, miscTools, lmtest, dplyr, BiocManager )' ;\
    Rscript --quiet --no-readline --slave -e 'p_load( ggplot2, scales )' ;\
    Rscript --quiet --no-readline --slave -e 'p_load( snow, foreach, parallel, doParallel, tictoc )' ;\
    #Rscript --quiet --no-readline --slave -e 'p_load( )' ;\

    # more pacman pkg needed ++
    Rscript --quiet --no-readline --slave -e 'library()'   | sort | tee R_library_list.out.5.txt  ;\
    echo "Done installing packages cran packages - part 5" | tee -a _TOP_DIR_OF_CONTAINER_     ;\
    date | tee -a      _TOP_DIR_OF_CONTAINER_   ;\
    echo "Dockerfile" | tee  _CONTAINER_tin6150_r4eta_  ;\
    echo ""


RUN  cd / \
  && touch _TOP_DIR_OF_CONTAINER_  \
  && TZ=PST8PDT date  >> _TOP_DIR_OF_CONTAINER_  \
  && echo  "Dockerfile 2021.0917.1753 foreach doSNOW"     >> _TOP_DIR_OF_CONTAINER_   \
  && echo  "Grand Finale"

#- ENV TZ America/Los_Angeles  
ENV TZ America/Los_Angeles 
# ENV TZ likely changed/overwritten by container's /etc/csh.cshrc
#ENV DOCKERFILE Dockerfile[.cmaq]
# does overwrite parent def of ENV DOCKERFILE
ENV TEST_DOCKER_ENV     this_env_will_be_avail_when_container_is_run_or_exec
ENV TEST_DOCKER_ENV_2   Can_use_ADD_to_make_ENV_avail_in_build_process
ENV TEST_DOCKER_ENV_REF https://vsupalov.com/docker-arg-env-variable-guide/#setting-env-values
# but how to append, eg add to PATH?

#ENTRYPOINT ["cat", "/_TOP_DIR_OF_CONTAINER_"]
#ENTRYPOINT [ "/bin/bash" ]
#ENTRYPOINT [ "/usr/bin/rstudio" ]
ENTRYPOINT [ "Rscript" ]
# if no defined ENTRYPOINT, default to bash inside the container
# docker run  -it -e DISPLAY=$DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix -v $HOME:/tmp/home  --user=$(id -u):$(id -g) --entrypoint rstudio tin6150/r4eta
# careful not to cover /home/username (for this container)
