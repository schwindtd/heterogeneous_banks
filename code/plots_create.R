########################################################
# Heterogeneous Banks Research Project
# Heterogeneity Plots
# Author: Daniel Schwindt
# Date: 4/20/2023
# Date Updated: 8/22/2023
# Purpose: Compute VARs and plots
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
library(vars)
library(estimatr)
library(magrittr)
library(texreg)

# Source needed functions
source("./helper_funcs.R")

dates <- c("1994-03-31", "2020-09-30")
dates <- as.Date(dates, "%Y-%m-%d")

## 1. Load data
heterog_ts <- readRDS("../data/bank_heterogeneity_agg.rds")
heterog_ts_restrict <- readRDS("../data/bank_heterogeneity_restrict_agg.rds")
mp_shock <- readRDS("../data/mp_shocks.rds")
macro_series_daily7 <- read_excel("../data/Bank_Runs_MacroSeries.xls", sheet="Daily,_7-Day", 
                                  col_types = c("date", rep("numeric",5)))
macro_series_monthly <- read_excel("../data/Bank_Runs_MacroSeries.xls", sheet="Monthly")
macro_series_qtrly <- read_excel("../data/Bank_Runs_MacroSeries.xls", sheet="Quarterly")


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

medplot <- ggplot(heterog_ts, aes(x=date, y=median)) +
  geom_line() + 
  annotate("rect", xmin=recessions$Start, xmax=recessions$End, ymin=-Inf, ymax=Inf, alpha=0.2) +
  labs(x="Date", y="Distance (Pairwise)", title="Aggregate Bank Heterogeneity: Median") +
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

## Additional Appendix charts
sdplot <- ggplot(heterog_ts, aes(x=date, y=sd)) +
  geom_line() + 
  annotate("rect", xmin=recessions$Start, xmax=recessions$End, ymin=-Inf, ymax=Inf, alpha=0.2) +
  labs(x="Date", y="S.D.", title="Aggregate Bank Heterogeneity") +
  theme_classic(base_size=8)

skewplot <- ggplot(heterog_ts, aes(x=date, y=skew)) +
  geom_line() + 
  annotate("rect", xmin=recessions$Start, xmax=recessions$End, ymin=-Inf, ymax=Inf, alpha=0.2) +
  labs(x="Date", y="Skew", title="Aggregate Bank Heterogeneity") +
  theme_classic(base_size=8)

kurtplot <- ggplot(heterog_ts, aes(x=date, y=kurtosis)) +
  geom_line() + 
  annotate("rect", xmin=recessions$Start, xmax=recessions$End, ymin=-Inf, ymax=Inf, alpha=0.2) +
  labs(x="Date", y="Kurtosis", title="Aggregate Bank Heterogeneity") +
  theme_classic(base_size=8)

other_moments_plot <- ggarrange(plotlist = list(medplot, sdplot, skewplot, kurtplot), 
                                ncol=2, nrow=2,common.legend=T)

cairo_ps("../output/bank_heterogeneity_pairwise_othermoments.eps", 
         width = 6.25, height = 4, pointsize = 12)
print(other_moments_plot)
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
names(heterog_ts) <- c("date", "bank_heterog_m","bank_heterog_med", "bank_heterog_sd", 
                       "bank_heterog_skew", "bank_heterog_kurt")
agg_data <- left_join(heterog_ts, macro_series_qtrly, by=c("date"="date")) %>%
  dplyr::select(-qtrdate, -qtr, -DATE, -year, -month, -day)
agg_data <- left_join(agg_data, ffr, by=c("date"="date"))  %>%
  dplyr::select(-qtrdate, -qtr, -year, -month, -day)
agg_data <- left_join(agg_data, cpi, by=c("date"="date"))  %>%
  dplyr::select(-qtrdate, -qtr, -year, -month, -day)
agg_data <- agg_data %>% mutate(chg_bh_m = bank_heterog_m - lag(bank_heterog_m, 4),
                                l_bh_m = log(bank_heterog_m))
# Select variables for VAR model
#var_data <- agg_data %>% dplyr::select(ffr_mean, gdpgr, infl, bank_heterog_m)
var_data <- agg_data %>% dplyr::select(lgdp, lcpi, ffr_mean, l_bh_m)

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
y_mins <- c(-0.01, -0.01, -0.5, -.005)
y_maxs <- c(0.02, 0.02, 0.75, 0.003)

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
y_mins <- c(-0.01,-0.01, -0.5, -0.005)
y_maxs <- c(0.02, 0.02, 0.5, 0.02)
cairo_ps(paste("../output/irfs_", y_names[4],".eps", sep=""), width = 6.25, height = 6.25, pointsize = 12)
par(mfrow=c(2,2), mar = c(4,4,2,2))
for (j in 1:4){
  graphics::plot(x=x_irf,y=irfs$irf[[4]][,j], type="l", xlab="Quarters", ylab=y_names[j], ylim=c(y_mins[j], y_maxs[j]))
  lines(x=x_irf, y=irfs$Lower[[4]][,j], lty=2, col="red")
  lines(x=x_irf, y=irfs$Upper[[4]][,j], lty=2, col="red")
}
dev.off()

par(mfrow=c(1,1))
cairo_ps("../output/ffr_heterog_pairwise_scatter.eps", width = 6.25, height = 6.25, pointsize = 12)
ols_model <- lm(l_bh_m ~ ffr_mean, data=var_data)
plot(x=var_data$ffr_mean, y=var_data$l_bh_m, pch=19, xlab="FFR", ylab="Log Bank Heterogeneity")
abline(ols_model, col="red")
dev.off()

plot(x=var_data$ffr_mean, y=ols_model$residuals, pch=19)
qqplot(x=var_data$ffr_mean, y=ols_model$residuals, pch=19)


# Plot aggregate bank heterogeneity
ggp <- ggplot(heterog_ts_restrict, aes(x=date, y=mean)) +
  geom_line() + 
  annotate("rect", xmin=recessions$Start, xmax=recessions$End, ymin=-Inf, ymax=Inf, alpha=0.2) +
  labs(x="Date", y="Distance (Pairwise)", title="Aggregate Bank Heterogeneity") +
  theme_classic(base_size=8)

medplot <- ggplot(heterog_ts_restrict, aes(x=date, y=median)) +
  geom_line() + 
  annotate("rect", xmin=recessions$Start, xmax=recessions$End, ymin=-Inf, ymax=Inf, alpha=0.2) +
  labs(x="Date", y="Distance (Pairwise)", title="Aggregate Bank Heterogeneity: Median") +
  theme_classic(base_size=8)

## Plot Federal Funds rate

## Combine Heterogeneity and interest rate charts
plot_list <- list(ggp, ffr_plot)
heterog_plots <- ggarrange(plotlist = plot_list, ncol=1, nrow=2)
cairo_ps("../output/bank_heterogeneity_pairwise_restrict.eps", width = 6.25, height = 4, pointsize = 12)
print(heterog_plots)
dev.off()

## Additional Appendix charts
sdplot <- ggplot(heterog_ts_restrict, aes(x=date, y=sd)) +
  geom_line() + 
  annotate("rect", xmin=recessions$Start, xmax=recessions$End, ymin=-Inf, ymax=Inf, alpha=0.2) +
  labs(x="Date", y="S.D.", title="Aggregate Bank Heterogeneity") +
  theme_classic(base_size=8)

skewplot <- ggplot(heterog_ts_restrict, aes(x=date, y=skew)) +
  geom_line() + 
  annotate("rect", xmin=recessions$Start, xmax=recessions$End, ymin=-Inf, ymax=Inf, alpha=0.2) +
  labs(x="Date", y="Skew", title="Aggregate Bank Heterogeneity") +
  theme_classic(base_size=8)

kurtplot <- ggplot(heterog_ts_restrict, aes(x=date, y=kurtosis)) +
  geom_line() + 
  annotate("rect", xmin=recessions$Start, xmax=recessions$End, ymin=-Inf, ymax=Inf, alpha=0.2) +
  labs(x="Date", y="Kurtosis", title="Aggregate Bank Heterogeneity") +
  theme_classic(base_size=8)

other_moments_plot <- ggarrange(plotlist = list(medplot, sdplot, skewplot, kurtplot), 
                                ncol=2, nrow=2,common.legend=T)

cairo_ps("../output/bank_heterogeneity_pairwise_restrict_othermoments.eps", 
         width = 6.25, height = 4, pointsize = 12)
print(other_moments_plot)
dev.off()

##------------------------------------------------------##
## VAR Regressions
# Combine aggregate data
names(heterog_ts_restrict) <- c("date", "bank_heterog_m","bank_heterog_med", "bank_heterog_sd", 
                                "bank_heterog_skew", "bank_heterog_kurt")
agg_data <- left_join(heterog_ts_restrict, macro_series_qtrly, by=c("date"="date")) %>%
  dplyr::select(-qtrdate, -qtr, -DATE, -year, -month, -day)
agg_data <- left_join(agg_data, ffr, by=c("date"="date"))  %>%
  dplyr::select(-qtrdate, -qtr, -year, -month, -day)
agg_data <- left_join(agg_data, cpi, by=c("date"="date"))  %>%
  dplyr::select(-qtrdate, -qtr, -year, -month, -day)
agg_data <- agg_data %>% mutate(chg_bh_m = bank_heterog_m - lag(bank_heterog_m, 4),
                                l_bh_m = log(bank_heterog_m))
# Select variables for VAR model
#var_data <- agg_data %>% dplyr::select(ffr_mean, gdpgr, infl, bank_heterog_m)
var_data <- agg_data %>% dplyr::select(lgdp, lcpi, ffr_mean, l_bh_m)

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
y_mins <- c(-0.01, -0.01, -0.5, -.005)
y_maxs <- c(0.02, 0.02, 0.75, 0.003)

# Orthogonal responses to FFR, INFL, GDP GR
for (i in 1:3){
  cairo_ps(paste("../output/irfs_restrict_", chart_names[i],".eps", sep=""), width = 6.25, height = 6.25, pointsize = 12)
  par(mfrow=c(2,2), mar = c(4,4,2,2))
  for (j in 1:4){
    graphics::plot(x=x_irf,y=irfs$irf[[i]][,j], type="l", xlab="Quarters", ylab=y_names[j], ylim=c(y_mins[j], y_maxs[j]))
    lines(x=x_irf, y=irfs$Lower[[i]][,j], lty=2, col="red")
    lines(x=x_irf, y=irfs$Upper[[i]][,j], lty=2, col="red")
  }
  dev.off()
}

# Orthogonal responses to Heterogeneity
y_mins <- c(-0.01,-0.01, -0.5, -0.005)
y_maxs <- c(0.02, 0.02, 0.5, 0.02)
cairo_ps(paste("../output/irfs_restrict_", y_names[4],".eps", sep=""), width = 6.25, height = 6.25, pointsize = 12)
par(mfrow=c(2,2), mar = c(4,4,2,2))
for (j in 1:4){
  graphics::plot(x=x_irf,y=irfs$irf[[4]][,j], type="l", xlab="Quarters", ylab=y_names[j], ylim=c(y_mins[j], y_maxs[j]))
  lines(x=x_irf, y=irfs$Lower[[4]][,j], lty=2, col="red")
  lines(x=x_irf, y=irfs$Upper[[4]][,j], lty=2, col="red")
}
dev.off()

par(mfrow=c(1,1))
cairo_ps("../output/ffr_heterog_pairwise_restrict_scatter.eps", width = 6.25, height = 6.25, pointsize = 12)
ols_model <- lm(l_bh_m ~ ffr_mean, data=var_data)
plot(x=var_data$ffr_mean, y=var_data$l_bh_m, pch=19, xlab="FFR", ylab="Log Bank Heterogeneity")
abline(ols_model, col="red")
dev.off()

plot(x=var_data$ffr_mean, y=ols_model$residuals, pch=19)
qqplot(x=var_data$ffr_mean, y=ols_model$residuals, pch=19)

##----------------------------------------------##
## Local Projections

# Prepare MP shocks data
# Convert to quarterly
mp_shock_qtr <- mp_shock %>% mutate(qtr = quarter(date),
                                      year = year(date)
                                      ) %>%
  group_by(year, qtr) %>% 
  summarise(brw_mean = mean(brw_m, na.rm=T),
            brw_max = mean(brw_m, na.rm=T),
            brw_end = dplyr::last(brw_m),
            brw_sum = sum(brw_m, na.rm=T)) %>%
  mutate(day = ifelse(qtr==1, 31, 
                      ifelse(qtr==2, 30,
                             ifelse(qtr==3, 30,31))),
         month = ifelse(qtr==1, 3,
                        ifelse(qtr==2, 6,
                               ifelse(qtr==3, 9, 12))),
         date = as.Date(paste(year, month, day, sep="-"), "%Y-%m-%d")
         )

# Merge data by date
#proj_data <- left_join(heterog_ts, mp_shock_qtr, by=c("date"="date"))
proj_data <- data.frame(var_data, mp_shock_qtr[1:107,"brw_sum"])
proj_data <- proj_data %>% mutate(chg_l_bh_m = l_bh_m - lag(l_bh_m,4))
# Compute local projections for levels
theta <- c()
se <- c()
for(i in 0:20){
  model <- lm(lead(proj_data$l_bh_m,i) ~ brw_sum + lgdp, data=proj_data)
  theta <- c(theta, model$coefficients[2])
  se <- c(se, sqrt(diag(vcov(model)))[2])
}
cairo_ps("../output/local_projs_loglevel.eps", width = 6.25, height = 6.25, pointsize = 12)
x <- 0:20
plot(x, theta, type='l', ylim=c(-0.1, 0.1))
polygon(c(x, rev(x)), c(theta-se*1.96, rev(theta+se*1.96)), col="gray", lty=0)
lines(x, theta, type='l')
abline(a=0, b=0, lty=3)
dev.off()

# Compute local projections for changes
theta_chg <- c()
se_chg <- c()
for(i in 0:20){
  model <- lm(lead(proj_data$chg_l_bh_m,i) ~ brw_sum + lgdp, data=proj_data)
  theta_chg <- c(theta_chg, model$coefficients[2])
  se_chg <- c(se_chg, sqrt(diag(vcov(model)))[2])
}
cairo_ps("../output/local_projs_logchg.eps", width = 6.25, height = 6.25, pointsize = 12)
x <- 0:20
plot(x, theta_chg, type='l', ylim=c(-0.1,0.1), ylab="", xlab="Quarters")
polygon(c(x, rev(x)), c(theta_chg-se_chg*1.96, rev(theta_chg+se_chg*1.96)), 
        col="gray", lty=0)
lines(x, theta_chg, type='l')
abline(a=0, b=0, lty=3, col="black")
dev.off()

##-----------------------------------------------------------##
## Low-for-long regression
# Construct running tally of low interest rate periods
date_cutoffs <- c("2007-12-31")
agg_data <- agg_data %>% mutate(low = ifelse(ffr_mean < 0.5, 1, 0),
                                low_alt = ifelse(date <=as.Date(date_cutoffs, "%Y-%m-%d") & ffr_mean < 4,1,
                                                 ifelse(date > as.Date(date_cutoffs, "%Y-%m-%d") & ffr_mean < 0.5,1,0)
                                                 ),
                                tot_low = cumsum_reset(low),
                                tot_low_alt = cumsum_reset(low_alt),
                                bank_heterog_m_chg = bank_heterog_m - lag(bank_heterog_m, 1),
                                rgdp_chg = 100*(GDPC1/lag(GDPC1,1)-1),
                                l_bh_m_chg = 100*(l_bh_m - lag(l_bh_m,1)),
                                ffr_chg = ffr_mean - lag(ffr_mean,1),
                                ffr_acc = ffr_chg - lag(ffr_chg,1))
x <- 1:107
trend_model <- lm(l_bh_m ~ x, data=agg_data)
ffr_trend_model <- lm(ffr_mean ~ x, data=agg_data)

## OLS Regressions on low for long variable
lfl_model <- lm_robust(trend_model$residuals ~ lag(trend_model$residuals,1) + tot_low + 
                  rgdp_chg, data=agg_data)
lfl_model_alt <- lm_robust(trend_model$residuals ~ lag(trend_model$residuals,1) + 
                      tot_low_alt + rgdp_chg, data=agg_data)

## Convert regression tables to latex
regs <- list(lfl_model, lfl_model_alt)
texreg(regs, include.ci=FALSE, digits=4)

agg_data <- data.frame(agg_data, cyc_heterog = trend_model$residuals/sd(trend_model$residuals))

lfl_start <- c("1994-03-31", "2001-09-30", "2009-03-31", "2020-06-30")
lfl_end <- c("1994-06-30", "2005-12-31", "2016-12-31", "2020-09-30")
lfl_dates <- data.frame(start=as.Date(lfl_start, "%Y-%m-%d"), 
                        end=as.Date(lfl_end, "%Y-%m-%d"))

# Cyclical Heterogeneity Plot
cairo_ps("../output/cyc_heterog.eps", width = 6.25, height = 6.25, pointsize = 12)
ggplot(data=agg_data) +
  geom_line(aes(date, cyc_heterog)) +
  annotate("rect", xmin=lfl_dates$start, xmax=lfl_dates$end, 
           ymin=-Inf, ymax=Inf, alpha=0.2) +
  labs(x="Date", y="Heterogeneity (Cyclical)", title="") +
  theme_classic(base_size=8)
dev.off()