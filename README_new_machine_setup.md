# Setting up new machien to run atlas

Run these scripts to install necessary packages, libraries, etc

They assume using a Debian/Ubuntu host.  Rocky/Fedora base machine that use yum would need different script(s).

The command below also capture output to a log file in case need to check for errors.
The machine may have the package installed and such warning/error can be ignored.
Warnings and/or errors about dependency conflict would need to be resolved, unless the package isn't needed anymore.


sudo ./ubuntu_install_apt_packages.sh    2>&1 | tee ubuntu_install_apt_packages.LOG
sudo ./install_docker_debian11.sh        2>&1 | tee install_docker_debian11.LOG

sudo ./Rscript_install.packages.shQ      2>&1 | tee Rscript_install.packages.LOG
sudo ./Rscript_install.packages.fix2.sh  2>&1 | tee Rscript_install.packages.fix2.LOG

install_python_libs.sh  2>&1 | tee install_python_libs.LOG                 # do NOT use sudo here!

# optional install:

Rscript_install.packages.bildaq.sh  # add tidyverse, stringr, etc that may not be needed by Atlas

