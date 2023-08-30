#!/bin/bash
## part 2, install stuff that c() in p_load() didn't handle anymore.
## \047 is octal code for '   awk and printf can parse/convert it
## cat Rscript_install.packages.sh | grep p_load | grep 'c(' | sed 's/,/\n/g' |
## cat p_load_list_fix.txt  |  awk '{print  " sudo     Rscript --quiet --no-readline --slave -e \047 library\(pacman\)\; p_load \(" $1 "\) \047 " }' > Rscript_install.packages.fix2.sh

# write to /usr/local/lib/R/site-library
# 167 entries at the end of this.
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("aCRM") ' 	# package ‘aCRM’ is not available for this version of R
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("akima") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("broom") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("cluster") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("clusterCrit") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("corrplot") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("DandEFA") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("datamart") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("data.table") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("directlabels") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("dismo") ' 		# depends problem
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("dplyr") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("factoextra") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("FactoMineR") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("fields") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("fmsb") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("gdata") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("ggmap") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("ggplot2") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("ggthemes") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("gpclib") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("gridExtra") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("Hmisc") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("lubridate") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("maps") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("maptools") ' 
 # sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("ncdf") ' 		# 1: package ‘ncdf’ is not available for this version of R  ## https://cran.r-project.org/web/packages/ncdf/index.html
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("ncdf4") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("openair") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("openxlsx") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("proj4") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("psych") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("psychTools") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("raster") ' 			# 3.5.15 was found, but >= 3.5.21 is required by ‘dismo’
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("RColorBrewer") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("readxl") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("reshape2") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("rgdal") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("rgeos") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("rJava") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("rstudioapi") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("scales") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("sf") ' 		## 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("sp") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("stargazer") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("stringi") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("stringr") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("tibble") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("tictoc") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("tidyr") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("tigris") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("timeDate") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("tmap") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("units") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("utils") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("xlsx") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("xtable") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("zoo") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("ster") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("sp") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("rgeos") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("geosphere") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("doParallel") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("iterators") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("foreach") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("rgdal") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("doSNOW") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("openxlsx") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("psych") ' 
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("ggpairs") ' 	# package ‘ggpairs’ is not available for this version of R
 sudo     Rscript --quiet --no-readline --slave -e ' library(pacman); p_load ("tableone") ' 