########################################################
# Heterogeneous Banks Research Project
# FR Y-9C
# Author: Daniel Schwindt
# Date: 4/5/2023
# Purpose: 
########################################################

## 0. Housekeeping
rm(list=ls())
setwd("~/Documents/research_projects/heterogeneous_banks/code")

library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)

## 1. Load data
zip_file <- "../data/BHCF/BHCF_1986_2020.zip"

unzip(zip_file, exdir="../data/BHCF/")
csv_files <- list.files("../data/BHCF/", pattern="*.csv", full.names = TRUE)

# Loop through all .csv files
for (csv_file in csv_files) {
  # Read .csv file and append to dataset
  data <- read_csv(csv_file, show_col_types=F)
  if (exists("bhc_data")) {
    bhc_data <- bind_rows(bhc_data, data)
  } else {
    bhc_data <- data
  }
}

# Remove unzipped folder
#file.remove(list.files(pattern = "*", recursive = TRUE))