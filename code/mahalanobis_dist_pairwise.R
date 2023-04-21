########################################################
# Heterogeneous Banks Research Project
# Mahalanobis Distance (Pairwise)
# Author: Daniel Schwindt
# Date: 4/20/2023
# Purpose: 
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
library(visNetwork)
library(igraph)
library(scatterplot3d)

source("mahalanobis_pairwise_func.r")

## 1. Load data
call <- readRDS("../data/callreports_1976_2020.rds")

## 2. Subset data for testing
rc_assets <- c("cash_bal_due_depo","int_assets","other_assets","assets",
               "sec","sec_htm","sec_afs","ff_sec_resell",
               "loans","trad_assets","premis_fixed_asset","invest_uncon_sub", 
               "invest_re")
rc_liabs <- c("deposits","trad_liabs","other_borr_money",
              "sub_notes_deben","other_liabs","liabs")
rcb_secs <- c("us_treas_htm","us_treas_afs","munis_htm","munis_afs",
              "rmbs_pt_htm","rmbs_pt_afs","rmbs_other_htm",
              "rmbs_other_afs","cmbs_pt_htm","cmbs_pt_afs",
              "abs_htm","abs_afs","cmbs_other_htm",
              "cmbs_other_afs","other_debt_htm","other_debt_afs","pledge_sec",
              "sec_exc2b2c_lt3m","sec_exc2b2c_3m_1y","sec_exc2b2c_1y_3y",
              "sec_exc2b2c_3y_5y","sec_exc2b2c_5y_15y","sec_exc2b2c_gt15y",
              "rmbs_pt_sec_lt3m","rmbs_pt_sec_3m_1y","rmbs_pt_sec_1y_3y",
              "rmbs_pt_sec_3y_5y","rmbs_pt_sec_5y_15y","rmbs_pt_sec_gt15y",
              "mbs_other_lt3y","mbs_other_gt3y","sec_lt1y")
rcc_loans <- c("lease_fin_receive","loans_less_unearn_inc","loans_re",
               "loans_re_cld","loans_re_farm","loans_re_resi",
               "loans_re_multifam","loans_re_nonfarm_nonresi","loans_depo_acc",
               "loans_depo_acc_commbnk","loans_depo_acc_otherdepo",
               "loans_depo_acc_for","loans_agprod","loans_ci","loans_ci_dom","loans_ci_for",
               "loans_individ_cardrevolv","loans_individ_autoothercons","loans_individ_card",
               "loans_individ_otherrevolv","loans_muni_oblig","loans_nondepoother","loans_nondepo",
               "loans_nondepoother_other","loans_nondepoother_other_secpurch",
               "loans_nondepoother_other_allother")
rcd_trad <- c("trad_assets_ustreas","trad_assets_derivs",
              "trad_assets_agency_nonmbs","trad_assets_munis",
              "trad_assets_rmbs_agency_pt","trad_assets_rmbs_agency_other",
              "trad_assets_rmbs_other","trad_assets_debt_other_other",
              "trad_assets_other","trad_liabs_short","trad_liabs_derivs")
rce_depo <- c("deposits_individ","deposits_commbnk",
              "deposits_forbnk","deposits_forinst")
select_vars <- c(rc_assets, rc_liabs, rcb_secs, rcc_loans, rcd_trad, rce_depo)
call <- call %>% select(date, rssd_id, all_of(select_vars))

## Re-define variables as pct of assets or liabilities
asset_side <- c(rc_assets[-4], rcb_secs, rcc_loans, rcd_trad)
liabs_side <- c(rc_liabs[-6], rce_depo)

call_pctasst <- call %>% 
  mutate(across(all_of(asset_side), ~ ./assets),
         across(all_of(liabs_side), ~./liabs) )

## Select elements for each characteristic vector
#characteristics <- c("assets", "loans", "trad_assets",
#                     "liabs", "deposits", "trad_liabs")

characteristics <- c("loans","trad_assets","deposits")
dates <- call %>% filter(date >= as.Date("1994-03-31")) %>% 
  select(date) %>% distinct() %>% deframe()

mahal_dist_mean_ts <- c()
mahal_dist_sd_ts <- c()
for (i in 1:length(dates)){
  # Compute covariance matrix
  call_mat <- call_pctasst %>% filter(date == dates[i]) %>% select(all_of(characteristics))
  call_mat <- as.matrix(call_mat)
  cov_mat <- cov(call_mat, use="pairwise.complete.obs")
  # Compute inverse covariance matrix
  inv_cov_mat <- solve(cov_mat)
  # Compute pairwise distances
  mahal_dist <- mahalanobis_pairwise(call_mat, inv_cov_mat)
  # Compute aggregate stats from pairwise
  m <- mean(mahal_dist, na.rm=T)
  s <- sd(mahal_dist, na.rm=T)
  mahal_dist_mean_ts <- c(mahal_dist_mean_ts, m)
  mahal_dist_sd_ts <- c(mahal_dist_sd_ts, s)
  
}

heterog_ts <- data.frame(date=dates, mean=mean, sd=sd)

ggplot(heterog_ts) +
  geom_line(aes(x=date, y=mean))

ggplot(heterog_ts) +
  geom_line(aes(x=date, y=mean))
