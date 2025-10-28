# ATLAS Static Household Fleet Mix Module
# preliminaries
# Naomi Panjaitan -- naomifp@lbl.gov

# set location of the data folder in Google Drive (either relative or absolute path)
# data_folder <- "G:/.shortcut-targets-by-id/1DEYFdNx9JRN3XaRK69_BmKY_2P19zxb1/Naomi/CA_model_test/"
# setwd(data_folder)

## Load packages
library(data.table)
library(tidyverse)
library(plotrix)



# Set working directory for model application
current_folder <- paste0(data_folder,"code/model_application/") # sub-models used in the integrated model

# Set the directory of data input from estimation
coefdir <- file.path(paste0(data_folder, "data/coefdir/"))


# Function to create directories if they don't exist
create_dir <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

# Set the output directories
## Directory for plots from integrated model
imgdir <- paste0(data_folder,"output/imgdir/")
create_dir(imgdir)
## Directory for data frames from integrated model
tabdir <- paste0(data_folder,"output/tabdir/")
create_dir(tabdir)


