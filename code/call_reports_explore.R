########################################################
# Heterogeneous Banks Research Project
# Call Reports
# Author: Daniel Schwindt
# Date: 4/1/2023
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

## 1. Load data
call <- read_csv("../data/callreports_1976_2020.csv")

# Check for duplicates
dup_check <- call %>% mutate(dups = 1) %>% group_by(RSSD9001, RSSD9999) %>%
  summarise(dups = sum(dups, na.rm=T))

# Drop duplicates
# Sort by assets and then drop the lowest one
call <- call %>% arrange(RSSD9001, RSSD9999, desc(RCFD2170)) %>%
  distinct(RSSD9001, RSSD9999, .keep_all = TRUE)

# check duplicates
dups_check <- call %>% mutate(dups = 1) %>% group_by(RSSD9001, RSSD9999) %>% 
  summarise(dups = sum(dups, na.rm=T))
dups_check %>% filter(dups > 1) %>% count() # no duplicates

rm(dup_check, dups_check)

## 3. Re-name variables
call_name_changes <- c("RCFD0010"="cash_bal_due_depo","RCFD1287"="us_treas_afs",
"RCFD1545"="loans_nondepoother_other_secpurch","RCFD1766"="loans_ci",
"RCFD1777"="other_debt_afs","RCFD2011"="loans_individ_autoothercons",
"RCFD2123"="loans_less_unearn_inc","RCFD2145"="premis_fixed_asset",
"RCFD2170"="assets","RCFD2650"="deposits_forinst",
"RCFD2950"="ff_sec_repo","RCFD3531"="trad_assets_ustreas",
"RCFD3541"="trad_assets_other","RCFD3546"="trad_liabs_short",
"RCFD3656"="invest_re","RCFD8641"="sec",
"RCFDA550"="sec_exc2b2c_3m_1y","RCFDA553"="sec_exc2b2c_5y_15y",
"RCFDA556"="rmbs_pt_sec_3m_1y","RCFDA559"="rmbs_pt_sec_5y_15y",
"RCFDA562"="mbs_other_gt3y","RCFDB535"="loans_depo_acc_for",
"RCFDC026"="abs_htm","RCFDC029"="deposits_commbnk",
"RCFDG327"="cmbs_pt_afs","RCFDG379"="trad_assets_rmbs_agency_pt",
"RCFDG386"="trad_assets_debt_other_other","RCFDG820"="rmbs_other_htm",
"RCFD0211"="us_treas_htm","RCFD1288"="loans_depo_acc",
"RCFD1420"="loans_re_farm","RCFD1480"="loans_re_nonfarm_nonresi","RCFD1563"="loans_nondepoother",
"RCFD1763"="loans_ci_dom","RCFD1773"="sec_afs",
"RCFD1430"="loans_re_resi","RCFD2107"="loans_muni_oblig",
"RCFD2130"="invest_uncon_sub","RCFD2160"="other_assets",
"RCFD2190"="deposits_forbnk","RCFD2930"="other_liabs",
"RCFD3190"="other_borr_money","RCFD3532"="trad_assets_agency_nonmbs",
"RCFD3543"="trad_assets_derivs","RCFD3547"="trad_liabs_derivs",
"RCFD8496"="munis_htm","RCFDA248"="sec_lt1y",
"RCFDA551"="sec_exc2b2c_1y_3y","RCFDA554"="sec_exc2b2c_gt15y",
"RCFDA557"="rmbs_pt_sec_1y_3y","RCFDA560"="rmbs_pt_sec_gt15y",
"RCFDB531"="loans_depo_acc_commbnk","RCFDB538"="loans_individ_card",
"RCFDC027"="abs_afs","RCFDC225"="ff_sec_resell",
"RCFDG328"="cmbs_other_htm","RCFDG380"="trad_assets_rmbs_agency_other",
"RCFDG816"="rmbs_pt_htm","RCFDG823"="rmbs_other_afs",
"RCFD0416"="pledge_sec","RCFD1460"="loans_re_multifam",
"RCFD1590"="loans_agprod","RCFD1764"="loans_ci_for",
"RCFD1774"="other_debt_htm","RCFD1975"="loans_individ_cardrevolv",
"RCFD2122"="loans","RCFD2143"="int_assets",
"RCFD2165"="lease_fin_receive","RCFD2200"="deposits",
"RCFD2948"="liabs","RCFD3200"="sub_notes_deben",
"RCFD3533"="trad_assets_munis","RCFD3545"="trad_assets",
"RCFD3548"="trad_liabs","RCFD8499"="munis_afs",
"RCFDA549"="sec_exc2b2c_lt3m","RCFDA552"="sec_exc2b2c_3y_5y",
"RCFDA555"="rmbs_pt_sec_lt3m","RCFDA558"="rmbs_pt_sec_3y_5y",
"RCFDA561"="mbs_other_lt3y","RCFDB534"="loans_depo_acc_otherdepo",
"RCFDB539"="loans_individ_otherrevolv","RCFDC028"="deposits_individ",
"RCFDG324"="cmbs_pt_htm","RCFDG331"="cmbs_other_afs",
"RCFDG381"="trad_assets_rmbs_other","RCFDG819"="rmbs_pt_afs",
"RCON2170"="dom_assets", "RCON2202"="tta_us_govt",
"RCON2203"="tta_munis", "RCON2520"="tnta_us_govt",
"RCON2530"="tnta_munis", "RCONJ451"="loans_nondepoother_other_allother",
"RCONJ454"="loans_nondepo", "RCONJ464"="loans_nondepoother_other",
"RCFD1410"="loans_re", "RCFD1415"="loans_re_cld", "RCFD1754"="sec_htm",
"RSSD9001"="rssd_id", "RSSD9010"="name_short", "RSSD9048"="charter_type",
"RSSD9053"="date_exit", "RSSD9130"="city", "RSSD9200"="state",
"RSSD9220"="zip", "RSSD9348"="reg_hh1_rssd_id", "RSSD9950"="date_open",
"RSSD9999"="date")

call <- call %>% 
  rename_with(~ ifelse(.x %in% names(call_name_changes), 
                       call_name_changes[.x], .x), 
              .cols = names(call))

call <- call %>% mutate(date = as.Date(as.character(date),format="%Y%m%d"))
saveRDS(call, file="../data/callreports_1976_2020.rds")

# Create name vectors for each type of variable
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

bal_vars <- c(rc_assets, rc_liabs, rcb_secs, rcc_loans, rcd_trad, rce_depo)

## 4. Summary Statistics (Levels)
summ_funcs <- set_names(list(mean = partial(mean, na.rm = TRUE),
                             sd = partial(sd, na.rm = TRUE),
                             min = partial(min, na.rm = TRUE),
                             max = partial(max, na.rm = TRUE),
                             n = function(x) sum(!is.na(x))),
                        nm = c("mean", "sd", "min", "max", "n"))
# Full sample descriptive statistics
summ_stats <- call %>% summarise(across(all_of(bal_vars),summ_funcs))
# Convert from wide format to long
summ_stats_long <- pivot_longer(data = summ_stats,
                                cols = everything(),
                                names_to = c(".value","Series"),
                                names_pattern = "(.*)_(?!.*_)(.*)")
# Export long format to csv
write.csv(summ_stats_long, file="../output/summ_stats_full.csv", row.names=F)

## 5. Summary Statistics over Time
# How many entities per quarter?
nobs <- call %>% group_by(date) %>%
  summarise(n = n())
ggplot(data=nobs, aes(x=date, y=n)) + geom_line()

# What types of entities in the data (by quarter)?
entity_types_qtrly <- call %>% group_by(date, charter_type) %>%
  summarise(n=n(), assets = sum(assets, na.rm=T)) %>%
  mutate(total_n = sum(n, na.rm=T),
         total_assets = sum(assets, na.rm=T),
         pct_n = 100*n/total_n,
         pct_assets = 100*assets/total_assets) %>%
  arrange(date, desc(pct_assets), desc(pct_n))

# Plots of entities by type (No. of institutions)
nums_by_type_plot <- ggplot(data=filter(entity_types_qtrly, charter_type %in% c(200, 300, 400, 0)), 
       aes(x=date, y=n, group=as.factor(charter_type), color=as.factor(charter_type))) +
  geom_line() +
  scale_color_manual(values=c("black", "red", "blue", "green"),
                     labels=c("Commercial", "Savings", "Edge", "N/A"),
                     breaks=c("200", "300", "400", "0")) + 
  labs(x="Date", y="Number of Institutions", color="Bank Type") +
  theme_classic(base_size=8)

# Plots of entities by type (Percent of Total Assets)
assets_by_type_plot <- ggplot(data=filter(entity_types_qtrly, charter_type %in% c(200, 300, 400, 0)), 
                              aes(x=date, y=pct_assets, group=as.factor(charter_type), color=as.factor(charter_type))) +
  geom_line() +
  scale_color_manual(values=c("black", "red", "blue", "green"),
                     labels=c("Commercial", "Savings", "Edge", "N/A"),
                     breaks=c("200", "300", "400", "0")) + 
  labs(x="Date", y="Percent of Total Assets", color="Bank Type") +
  theme_classic(base_size=8)

# Arrange into grid and save to output
entity_type_plots <- ggarrange(plotlist = list(nums_by_type_plot, assets_by_type_plot), ncol=2, nrow=1,common.legend=T)
ggsave("../output/call_report_entity_types.eps", plot=entity_type_plots,width=6.25, height=4, device="eps")


# Breakdown of entity types across all quarters
entity_types <- entity_types_qtrly %>% ungroup() %>% group_by(charter_type) %>%
  summarise(n=sum(n), assets=sum(assets)) %>% 
  mutate(total_n = sum(n, na.rm=T),
         total_assets = sum(assets, na.rm=T),
         pct_n = 100*n/total_n,
         pct_assets = 100*assets/total_assets) %>%
  arrange(desc(pct_assets), desc(pct_n))

write.csv(entity_types, file="../output/entity_types_full.csv", row.names=F)

# Asset Histograms
asset_dist_2020q1 <- call %>% filter(assets > 0 & date==as.Date("2020-03-31")) %>% ggplot(aes(x=assets)) +
  geom_histogram(aes(y=..density..), binwidth=10000000, color="black", fill="white") +
  labs(x="Assets ($K)", y="Density", title="2020:Q1") +
  theme_classic(base_size=8)
asset_dist_2016q1 <- call %>% filter(assets > 0 & date==as.Date("2016-03-31")) %>% ggplot(aes(x=assets)) +
  geom_histogram(aes(y=..density..), binwidth=10000000, color="black", fill="white") +
  labs(x="Assets ($K)", y="Density", title="2016:Q1") +
  theme_classic(base_size=8)
asset_dist_2012q1 <- call %>% filter(assets > 0 & date==as.Date("2012-03-31")) %>% ggplot(aes(x=assets)) +
  geom_histogram(aes(y=..density..), binwidth=10000000, color="black", fill="white") +
  labs(x="Assets ($K)", y="Density", title="2012:Q1") +
  theme_classic(base_size=8)
asset_dist_2008q1 <- call %>% filter(assets > 0 & date==as.Date("2008-03-31")) %>% ggplot(aes(x=assets)) +
  geom_histogram(aes(y=..density..), binwidth=10000000, color="black", fill="white") +
  labs(x="Assets ($K)", y="Density", title="2008:Q1") +
  theme_classic(base_size=8)
# Arrange into grid and save to output
asset_dist_list <- list(asset_dist_2008q1, asset_dist_2012q1, asset_dist_2016q1, asset_dist_2020q1)
assets_distrib_plots <- ggarrange(plotlist = asset_dist_list, ncol=2, nrow=2,common.legend=T)
ggsave("../output/call_report_asset_dist_time.eps", plot=assets_distrib_plots,width=6.25, height=4, device="eps")

# Time series of assets (mean and SD)
summ_assets <- call %>% filter(assets > 0 & charter_type ==200) %>% 
  group_by(date) %>%
  summarise(sd = sd(assets, na.rm=T),
            mean = mean(assets, na.rm=T),
            pct90 = quantile(assets,probs=0.90, na.rm=T),
            pct10 = quantile(assets, probs=0.10, na.rm=T),
            pct75 = quantile(assets, probs=0.75, na.rm=T),
            pct25 = quantile(assets, probs=0.25, na.rm=T))

# Plot Nominal Assets
assets_plot <- ggplot(data=summ_assets) +
  geom_line(aes(x=date, y=mean), color="black") +
  #geom_line(aes(x=date, y=sd), color="red") +
  geom_ribbon(aes(x=date, ymin=pct10, ymax=pct90), fill="gray", alpha=0.4) + 
  geom_ribbon(aes(x=date, ymin=pct25, ymax=pct75), fill="purple", alpha=0.2) +
  labs(x="Date", y="$K", title="Commercial Bank Assets by Quarter") +
  theme_classic(base_size=8)

# Save plot to output
cairo_ps("../output/call_report_nominalassets.eps", width = 6.25, height = 4, pointsize = 12)
print(assets_plot)
dev.off()
#ggsave("../output/call_report_nominalassets.eps", plot=assets_plot,width=6.25, height=4, device="eps")