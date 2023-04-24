########################################################
# Heterogeneous Banks Research Project
# Mahalanobis Distance (to Mean)
# Author: Daniel Schwindt
# Date: 4/18/2023
# Purpose: 
########################################################

## 0. Housekeeping
rm(list=ls())
setwd("~/Documents/research_projects/heterogeneous_banks/code")

library(tis)
library(vars)
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


## 1. Load data
call <- readRDS("../data/callreports_1976_2020.rds")

macro_series_daily7 <- read_excel("../data/Bank_Runs_MacroSeries.xls", sheet="Daily,_7-Day", 
                                  col_types = c("date", rep("numeric",5)))
macro_series_monthly <- read_excel("../data/Bank_Runs_MacroSeries.xls", sheet="Monthly")
macro_series_qtrly <- read_excel("../data/Bank_Runs_MacroSeries.xls", sheet="Quarterly")

## Compute select total series
call <- call %>% mutate(us_treas = us_treas_htm + us_treas_afs,
                        munis = munis_htm + munis_afs,
                        rmbs = rmbs_pt_htm + rmbs_pt_afs + rmbs_other_htm + rmbs_other_afs,
                        cmbs = cmbs_pt_htm + cmbs_pt_afs + cmbs_other_htm + cmbs_other_afs,
                        abs = abs_htm + abs_afs,
                        other_debt = other_debt_htm + other_debt_afs,
                        loans_personal = loans_individ_card + loans_individ_autoothercons,
                        deposits_foreign = deposits_forbnk + deposits_forinst)

## 2. Subset data for testing
rc_assets <- c("cash_bal_due_depo","int_assets","other_assets","assets",
               "sec","sec_htm","sec_afs","ff_sec_resell",
               "loans","trad_assets","premis_fixed_asset","invest_uncon_sub", 
               "invest_re")
rc_liabs <- c("deposits","trad_liabs","other_borr_money",
              "sub_notes_deben","other_liabs","liabs")
rcb_secs <- c("us_treas", "us_treas_htm","us_treas_afs","munis", "munis_htm","munis_afs",
              "rmbs","rmbs_pt_htm","rmbs_pt_afs","rmbs_other_htm",
              "rmbs_other_afs","cmbs", "cmbs_pt_htm","cmbs_pt_afs",
              "abs","abs_htm","abs_afs","cmbs_other_htm",
              "cmbs_other_afs","other_debt", "other_debt_htm","other_debt_afs","pledge_sec",
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
               "loans_personal","loans_individ_cardrevolv","loans_individ_autoothercons","loans_individ_card",
               "loans_individ_otherrevolv","loans_muni_oblig","loans_nondepoother","loans_nondepo",
               "loans_nondepoother_other","loans_nondepoother_other_secpurch",
               "loans_nondepoother_other_allother")
rcd_trad <- c("trad_assets_ustreas","trad_assets_derivs",
              "trad_assets_agency_nonmbs","trad_assets_munis",
              "trad_assets_rmbs_agency_pt","trad_assets_rmbs_agency_other",
              "trad_assets_rmbs_other","trad_assets_debt_other_other",
              "trad_assets_other","trad_liabs_short","trad_liabs_derivs")
rce_depo <- c("deposits_individ","deposits_commbnk",
              "deposits_foreign","deposits_forbnk","deposits_forinst")
select_vars <- c(rc_assets, rc_liabs, rcb_secs, rcc_loans, rcd_trad, rce_depo)
call <- call %>% select(date, rssd_id, all_of(select_vars))

## Re-define variables as pct of assets or liabilities
asset_side <- c(rc_assets[-4], rcb_secs, rcc_loans, rcd_trad)
liabs_side <- c(rc_liabs[-6], rce_depo)

call_pctasst <- call %>% 
  mutate(across(all_of(asset_side), ~ ./assets),
         across(all_of(liabs_side), ~./liabs) )

## Select elements for each characteristic vector
#characteristics <- c("loans","trad_assets","deposits")
#characteristics <- c(rc_assets, rc_liabs, rcb_secs, rcc_loans, rcd_trad, rce_depo)
characteristics <- c("loans", "loans_ci",
                     "us_treas", "munis","pledge_sec",
                     "deposits")
dates <- call %>% filter(date >= as.Date("1994-03-31")) %>% 
  select(date) %>% distinct() %>% deframe()

mahal_dist_mean_ts <- c()
mahal_dist_sd_ts <- c()
for (i in 1:length(dates)){
  # Compute covariance matrix
  call_mat <- call_pctasst %>% filter(date == dates[i]) %>% select(all_of(characteristics))
  call_mat <- as.matrix(call_mat)
  # Change NAs to 0 for distance calculation
  call_mat[is.na(call_mat)] <- 0
  cov_mat <- cov(call_mat, use="pairwise.complete.obs")
  # Compute inverse covariance matrix
  inv_cov_mat <- solve(cov_mat)
  # Compute Mahalanobis Distance to mean for each bank
  mahal_dist_mean <- sqrt(mahalanobis(call_mat, colMeans(call_mat, na.rm=T), inv_cov_mat))
  # Summarize across banks to create aggregate values
  mahal_dist_mean_ts <- c(mahal_dist_mean_ts, mean(mahal_dist_mean, na.rm=T))
  mahal_dist_sd_ts <- c(mahal_dist_sd_ts, sd(mahal_dist_mean, na.rm=T))
}

heterog_ts <- data.frame(date = dates, mean = mahal_dist_mean_ts, sd = mahal_dist_sd_ts)

## Plot aggregate time series
recessions <- data.frame(nberDates())
recessions <- recessions %>% 
  mutate(Start = as.Date(as.character(Start), format="%Y%m%d"),
         End = as.Date(as.character(End), format="%Y%m%d")) %>%
  filter(Start >=dates[1])

ggp <- ggplot(heterog_ts, aes(x=date, y=mean)) +
  geom_line() + 
  annotate("rect", xmin=recessions$Start, xmax=recessions$End, ymin=-Inf, ymax=Inf, alpha=0.2) +
  labs(x="Date", y="Distance to Mean", title="Aggregate Bank Heterogeneity") +
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
cairo_ps("../output/bank_heterogeneity_mahalanobis.eps", width = 6.25, height = 4, pointsize = 12)
print(heterog_plots)
dev.off()

##------------------------------------------------------##
## VAR Regressions
# Prepare macro quarterly data for merge
macro_series_qtrly <- macro_series_qtrly %>% 
  mutate(qtrdate = lubridate::quarter(DATE, with_year=T), 
         qtr=lubridate::quarter(DATE)) %>%
  mutate(year = floor(qtrdate),
         month = qtr*3,
         day = case_when(
           month < 6  ~ 31,
           month >= 6 & month < 9 ~ 30,
           month >= 9 & month < 12 ~ 30,
           TRUE ~ 31
         ),
         date = lubridate::ymd(paste(year, month, day, sep = "-")),
         gdpgr=400*(GDPC1/lag(GDPC1)-1),
         lgdp=log(GDPC1)
  )

cpi <- macro_series_monthly %>% 
  mutate(qtrdate = lubridate::quarter(DATE, with_year=T), 
         qtr=lubridate::quarter(DATE)) %>%
  group_by(qtrdate, qtr) %>% 
  summarise(cpi = mean(CPIAUCSL, na.rm=T)) %>%
  ungroup() %>%
  mutate(year = floor(qtrdate),
         month = qtr*3,
         day = case_when(
           month < 6  ~ 31,
           month >= 6 & month < 9 ~ 30,
           month >= 9 & month < 12 ~ 30,
           TRUE ~ 31
         ),
         date = lubridate::ymd(paste(year, month, day, sep = "-")),
         infl = 400*(cpi/dplyr::lag(cpi)-1),
         lcpi = log(cpi)
  )

# Combine aggregate data
names(heterog_ts) <- c("date", "bank_heterog_m", "bank_heterog_sd")
agg_data <- left_join(heterog_ts, macro_series_qtrly, by=c("date"="date")) %>%
  dplyr::select(-qtrdate, -qtr, -DATE, -year, -month, -day)
agg_data <- left_join(agg_data, ffr, by=c("date"="date"))  %>%
  dplyr::select(-qtrdate, -qtr, -year, -month, -day)
agg_data <- left_join(agg_data, cpi, by=c("date"="date"))  %>%
  dplyr::select(-qtrdate, -qtr, -year, -month, -day)
# Select variables for VAR model
#var_data <- agg_data %>% dplyr::select(ffr_mean, gdpgr, infl, bank_heterog_m)
var_data <- agg_data %>% dplyr::select(lgdp, lcpi, ffr_mean, bank_heterog_m)

# Estimate VAR Model
#vmodel <- VAR(var_data)
vmodel <- VAR(var_data, p=4)

# Impulse Response Functions
irfs <- irf(vmodel, n.ahead=50)
x_irf <- seq(1:51)-1
# x_names <- c("FFR", "GDP Growth", "Inflation", "Heterogeneity")
# y_mins <- c(-0.2, -1, -0.2, -0.0004)
# y_maxs <- c(0.5, 2, 0.75, 0.0003)
y_names <- c("Log Real GDP", "Log CPI", "FFR", "Heterogeneity")
chart_names <- c("lgdp", "lcpi", "ffr", "heterog")
y_mins <- c(-0.01, -0.01, -0.5, -0.0004)
y_maxs <- c(0.02, 0.02, 0.75, 0.001)

# Orthogonal responses to FFR, INFL, GDP GR
for (i in 1:3){
  cairo_ps(paste("../output/irfs_", chart_names[i],".eps", sep=""), width = 6.25, height = 6.25, pointsize = 12)
  par(mfrow=c(2,2), mar = c(4,4,2,2))
  for (j in 1:4){
    graphics::plot(x=x_irf,y=irfs$irf[[i]][,j], type="l", xlab="Quarters", ylab=y_names[j], ylim=c(y_mins[j], y_maxs[j]))
    lines(x=x_irf, y=irfs$Lower[[i]][,j], lty=2, col="red")
    lines(x=x_irf, y=irfs$Upper[[i]][,j], lty=2, col="red")
  }
  dev.off()
}

# Orthogonal responses to Heterogeneity
y_mins <- c(-0.01,-0.01, -0.5, -0.0003)
y_maxs <- c(0.02, 0.02, 0.5, 0.001)
cairo_ps(paste("../output/irfs_", y_names[4],".eps", sep=""), width = 6.25, height = 6.25, pointsize = 12)
par(mfrow=c(2,2), mar = c(4,4,2,2))
for (j in 1:4){
  graphics::plot(x=x_irf,y=irfs$irf[[4]][,j], type="l", xlab="Quarters", ylab=y_names[j], ylim=c(y_mins[j], y_maxs[j]))
  lines(x=x_irf, y=irfs$Lower[[4]][,j], lty=2, col="red")
  lines(x=x_irf, y=irfs$Upper[[4]][,j], lty=2, col="red")
}
dev.off()

par(mfrow=c(1,1))
cairo_ps("../output/ffr_heterog_scatter.eps", width = 6.25, height = 6.25, pointsize = 12)
ols_model <- lm(bank_heterog_m ~ ffr_mean, data=var_data)
plot(x=var_data$ffr_mean, y=var_data$bank_heterog_m, pch=19, xlab="FFR", ylab="Bank Heterogeneity")
abline(ols_model, col="red")
dev.off()

plot(x=var_data$ffr_mean, y=ols_model$residuals, pch=19)
qqplot(x=var_data$ffr_mean, y=ols_model$residuals, pch=19)

