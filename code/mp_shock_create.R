########################################################
# Heterogeneous Banks Research Project
# Monetary Policy Shocks
# Author: Daniel Schwindt
# Date: 8/22/2023
# Date Updated: 
# Purpose: Create dataset of monetary policy surprises
########################################################

## 0. Housekeeping
rm(list=ls())
setwd("~/Documents/research_projects/heterogeneous_banks/code")

library(tidyverse)
library(haven)
library(readxl)
library(gridExtra)
library(ggpubr)
library(lubridate)
library(gganimate)
library(stringr)

## 1. Load data
brw <- read_csv("../data/brw-shock-series.csv")

## 2. Manipulate data
brw <- brw %>% filter(!is.na(month))
brw <- brw %>% dplyr::select(month, `BRW_monthly (updated)`, date_fomc...3,
                             `BRW_fomc (updated)`)
names(brw) <- c("month_date", "brw_m","date_fomc", "brw_fomc")
# BRW FOMC series
brw_fomc <- brw %>% dplyr::select(date_fomc, brw_fomc)
# Final BRW Format
brw <- brw %>% dplyr::select(month_date, brw_m)
brw <- brw %>% mutate(year = as.numeric(str_split_i(month_date, "m",1)),
                      month = as.numeric(str_split_i(month_date, "m", 2)),
                      day = ifelse(month %in% c(1,3,5,7,8,10,12), 31,
                                   ifelse(month %in% c(4,6,9,11),30, 28)
                                   ),
                      date = as.Date(paste(year, month, day, sep="-"), "%Y-%m-%d")
                      )
# Final BRW FOMC dataset
brw_fomc <- brw_fomc %>% mutate(date_fomc = as.Date(date_fomc, "%d-%b-%y"),
                                year = year(date_fomc),
                                month = month(date_fomc))
# Merge BRW datasets
brw_full <- left_join(brw, brw_fomc, by=c("year"="year", "month"="month"))
brw_full <- brw_full %>% dplyr::select(-month_date, -year, -month, -day)

## 3. Save dataset
saveRDS(brw_full, file="../data/mp_shocks.rds")


