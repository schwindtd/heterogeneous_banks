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
library(proxy)
library(doParallel)
library(moments)
library(tis)
library(fredr)

# Source needed functions
source("./helper_funcs.R")

## Set up cluster for parallelization (Method 3)
cl <- parallel::makeCluster(detectCores())
# Activate cluster
doParallel::registerDoParallel(cl)

## 1. Load data
call <- readRDS("../data/callreports_1976_2020.rds")

macro_series_daily7 <- read_excel("../data/Bank_Runs_MacroSeries.xls", sheet="Daily,_7-Day", 
                                  col_types = c("date", rep("numeric",5)))

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

## 3. Re-define variables as pct of assets or liabilities
asset_side <- c(rc_assets[-4], rcb_secs, rcc_loans, rcd_trad)
liabs_side <- c(rc_liabs[-6], rce_depo)

call_pctasst <- call %>% 
  mutate(across(all_of(asset_side), ~ ./assets),
         across(all_of(liabs_side), ~./liabs) )

## Select elements for each characteristic vector
#characteristics <- c("loans","trad_assets","deposits")
characteristics <- c("loans", "loans_ci",
                     "us_treas", "munis","pledge_sec",
                     "deposits")
dates <- call %>% filter(date >= as.Date("1994-03-31")) %>% 
  select(date) %>% distinct() %>% deframe()

#mahal_dist_summ <- matrix(ncol=4, nrow=length(dates))
dist_mean <- c()
dist_sd <- c()
dist_skew <- c()
dist_kurtosis <- c()
# Loop to compute summary statistics on similarity
#foreach (i= 1:length(dates), .packages=c("tidyverse", "proxy", "moments")) %dopar% {
for (i in 1:length(dates)){
  # Prepare data for distance function
  call_mat <- call_pctasst %>% filter(date == dates[i]) %>% select(all_of(characteristics))
  call_mat <- as.matrix(call_mat)
  # Compute pairwise distances
  mahal_dist <- proxy::as.matrix(proxy::dist(call_mat, method="Euclidean"))
  # Compute aggregate stats from pairwise
  dist_mean <- c(dist_mean, mean(mahal_dist, na.rm=T))
  dist_sd <- c(dist_sd, sd(mahal_dist, na.rm=T))
  dist_skew <- c(dist_skew, skewness(colMeans(mahal_dist, na.rm=T), na.rm=T))
  dist_kurtosis <- c(dist_kurtosis, kurtosis(colMeans(mahal_dist, na.rm=T), na.rm=T))
}

parallel::stopCluster(cl) # Stop cluster

# Create dataframe with summary statistics
heterog_ts <- data.frame(date=dates, mean=dist_mean, sd=dist_sd, 
                         skew=dist_skew, kurtosis=dist_kurtosis)

# Plot aggregate bank heterogeneity
recessions <- data.frame(nberDates())
recessions <- recessions %>% 
  mutate(Start = as.Date(as.character(Start), format="%Y%m%d"),
         End = as.Date(as.character(End), format="%Y%m%d")) %>%
  filter(Start >=dates[1])

ggp <- ggplot(heterog_ts, aes(x=date, y=mean)) +
  geom_line() + 
  annotate("rect", xmin=recessions$Start, xmax=recessions$End, ymin=-Inf, ymax=Inf, alpha=0.2) +
  labs(x="Date", y="Distance (Pairwise)", title="Aggregate Bank Heterogeneity") +
  theme_classic(base_size=8)

## Plot Federal Funds rate
ffr <- macro_series_daily7 %>% 
  mutate(qtrdate = lubridate::quarter(DATE, with_year=T), qtr=lubridate::quarter(DATE)) %>%
  group_by(qtrdate, qtr) %>% 
  summarise(ffr_mean = mean(DFF, na.rm=T),
            ffr_med = median(DFF, na.rm=T),
            ffr_max = max(DFF, na.rm=T),
            ffr_min = min(DFF, na.rm=T)) %>%
  mutate(year = floor(qtrdate),
         month = qtr*3,
         day = case_when(
           month < 6  ~ 31,
           month >= 6 & month < 9 ~ 30,
           month >= 9 & month < 12 ~ 30,
           TRUE ~ 31
         ),
         date = lubridate::ymd(paste(year, month, day, sep = "-"))
  )

xlimits = c(min(dates), max(dates))
ffr_plot <- ggplot(data=ffr) +
  geom_line(aes(x=date, y=ffr_mean), color="black") + 
  geom_ribbon(aes(x=date, ymin=ffr_min, ymax=ffr_max), 
              fill="purple", alpha=0.2) +
  scale_x_date(limits=xlimits) + 
  scale_y_continuous(limits=c(0,10)) +
  labs(x="Date", y="Percent", title="Federal Funds Rate") +
  annotate("rect", xmin=recessions$Start, xmax=recessions$End, ymin=-Inf, ymax=Inf, alpha=0.2) +
  theme_classic(base_size=8)

## Combine Heterogeneity and interest rate charts
plot_list <- list(ggp, ffr_plot)
heterog_plots <- ggarrange(plotlist = plot_list, ncol=1, nrow=2)
cairo_ps("../output/bank_heterogeneity_pairwise.eps", width = 6.25, height = 4, pointsize = 12)
print(heterog_plots)
dev.off()
