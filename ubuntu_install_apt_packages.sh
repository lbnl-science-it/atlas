# packages installed by the container by Dockerfile.base

export DEBIAN_FRONTEND=noninteractive
export TERM=dumb
export TZ=PST8PDT
export NO_COLOR=1

    apt-get update ;
    echo "installing packages via apt"  ;     
    apt-get -y --quiet install r-base ;      # Ubuntu 22.04 LTS has R 4.1 as of 2022-09-09
    #apt-get update ;
    apt-get -y --quiet install git file wget gzip bash less vim procps ;
    apt-get -y --quiet install units libudunits2-dev curl r-cran-rcurl libcurl4 libcurl4-openssl-dev libssl-dev r-cran-httr  r-cran-xml r-cran-xml2 libxml2 rio  java-common javacc javacc4  openjdk-8-jre-headless ;
    apt-get -y --quiet install openjdk-14-jre-headless   ;
    # apt-file search gdal-config
    apt-get -y --quiet install gdal-bin gdal-data libgdal-dev   ;
    apt-get -y --quiet install   libgdal28  ;
    apt-get -y --quiet install r-cran-rgdal  ;
    apt-get -y --quiet install libgeos-dev   ;
    # ref: https://www.digitalocean.com/community/tutorials/how-to-install-java-with-apt-on-ubuntu-18-04
    apt-get -y --quiet install default-jdk r-cran-rjava  ;
    apt-get -y --quiet install libnode-dev libv8-dev ;
    #-- apt-get --quiet install rstudio  ;
    #xx apt-get -y --quiet install r-cran-rstudioapi libqt5gui5 libqt5network5  libqt5webenginewidgets5 qterminal net-tools ;
    apt-get -y --quiet install apt-file ;
    ##?? apt-file update ;
    #xx apt-get -y --quiet install ./rstudio4deb10.deb     ;
    apt-get -y --quiet install git git-all  ;
# next one is for BILD-AQ
    apt-get -y --quiet install python3-pip  ;
# gcp atlas instance  have python3, just no python
#    apt-get -y --quiet install python3  ;

