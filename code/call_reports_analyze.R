########################################################
# Heterogeneous Banks Research Project
# Call Reports
# Author: Daniel Schwindt
# Date: 4/1/2023
# Purpose: Construct mean, dispersion, and concentration
#           for key series
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

## 1. Load data
# Set which variables to grab
identifiers <- c("rssdid","cert","bhcid", "name")
date_vars <- c("date",	"year",	"month",	"day",	"quarter")
loans_vars <- c("reloans", "persloans", "agloans", "ciloans","loans")
intinc_vars <- c("intincassets","intincnet", "nonintinc", "intincbaldue",
                 "intinctreasuriesagencydebt", "intincmbs",
                 "intincothersecurities", "intincfedfundsrepoasset",
                 "intincloans","intincreloans1to4fam", "intincreloansother",
                 "intincagloans", "intincciloans", "intincpersccards",
                 "intincpersother",	"intincforloans",	"intincleases",	"intincassets",	
                 "intincpersloans", "intanddivincsecurities", "intincfedfundsrepoasset")
securities <- c("securities", "securities_ammcost", "securitiesheldtomaturity",
                "securitiesavailableforsale", "fedfundsrepoasset")
assets <- c("assets", "tradingassets")
liabilities <- c("liabilities", "tradingliabilities","fedfundsrepoliab")
deposits <- c("demanddep",	"transdep",	"brokereddep",	"timedepge100k",	
              "timedeple100k",	"timedeple250k",	"deposits",	"foreigndep",	
              "nonintbeardep",	"intbearfordep",	"timesavdep",	"nontransdep",	
              "timedep",	"timedepuninsured",	"totsavdep",	"savdep",	
              "brokereddeple100k",	"brokereddepeq100k")
intexp_vars <- c("intexp",	"nonintexp",	"intandnonintexp",	"intexpalldep",	
                 "intexpdomdep",	"intexpsubordinated",	"exponpremises",	
                 "intexptimedepge100k",	"intexptimedeple100k",	
                 "intexptimedeple250k",	"intexptimedepge250k",	
                 "intexpcdge100k",	"intexptimedep",	"intexptransdep",	
                 "intexpsavdep",	"intexpfordep",	"intexpfedfundsrepoliab",	
                 "intexptradingandborrowed",	"intexpfedfundsrepoliab")

all_vars <- c(identifiers, date_vars, loans_vars, intinc_vars, securities, assets, 
              liabilities, deposits, intexp_vars)

# LOAD dataset from stata
call_reports <- read_dta("../data/callreports_1976_2020_WRDS.dta",
                         col_select=all_vars)

# LOAD macro time series
macro_series_monthly <- read_excel("../data/Bank_Runs_MacroSeries.xls", 
                                   sheet="Monthly")
# Convert macro time series to quarterly
macro_series_qtrly <- macro_series_monthly %>% 
  mutate(cpi = ifelse(is.na(CPIAUCSL), CPIAUCNS, CPIAUCSL),
         year=year(DATE),
         quarter = quarter(DATE)) %>%
  group_by(year, quarter) %>%
  summarise(cpi = mean(cpi, na.rm=T))

## 2. Convert nominal values into real terms
# Mege macro time series by year and quarter
call_reports <- left_join(call_reports, macro_series_qtrly, by=c("year", "quarter"))
# Divide values by cpi & normalize income and expense variables by assets
summ_vars <- c(assets, liabilities, loans_vars, securities, deposits, intinc_vars, intexp_vars)
call_reports_real <- call_reports %>% mutate(across(all_of(summ_vars), ~ . / cpi))


## 3. Summary statistics
summ_funcs <- set_names(list(mean = partial(mean, na.rm = TRUE),
                             sd = partial(sd, na.rm = TRUE),
                             min = partial(min, na.rm = TRUE),
                             max = partial(max, na.rm = TRUE)),
                        nm = c("mean", "sd", "min", "max"))
# Summary Statistics: ALL OBSERVATIONS
summ_stats <- call_reports_real %>% summarise(across(all_of(summ_vars),
                                                     summ_funcs))
# Convert from wide format to long
summ_stats_long <- pivot_longer(data = summ_stats,
                                  cols = everything(),
                                  names_to = c(".value","Series"),
                                  names_pattern = "(.*)_(.*)")

# Summary Statistics: BY QUARTER
summ_stats_qtrly <- call_reports_real %>% group_by(date) %>%
  summarise(across(all_of(summ_vars),
                   summ_funcs))

nobs <- call_reports_real %>% group_by(date) %>%
  summarise(n = n())
summ_stats_qtrly <- left_join(summ_stats_qtrly, nobs, by="date")

# Convert date variable into date format
summ_stats_qtrly$date <- as.Date(as.character(summ_stats_qtrly$date), format="%Y%m%d")

## 4. Plot summary statistics by quarter
n_plot <- ggplot(data=summ_stats_qtrly, aes(x=date, y=n)) +
  geom_line() +
  labs(title="Number of Banks", x="Date (Qtrly)", y= "# of Banks") +
  theme_classic(base_size=8)

avg_totalassets_plot <- ggplot(data=summ_stats_qtrly) +
  geom_line(aes(x=date, y=assets_mean), color="black") +
  labs(title="Total Assets (Mean)", x="Date (Qtrly)", y="$ (Real)") + 
  theme_classic(base_size=8)

sd_totalassets_plot <- ggplot(data=summ_stats_qtrly) +
  geom_line(aes(x=date, y=assets_sd), color="black") +
  labs(title="Total Assets (SD)", x="Date (Qtrly)", y="$ (Real)") + 
  theme_classic(base_size=8)

plots <- list(n_plot, avg_totalassets_plot, sd_totalassets_plot)
overview_plot <- ggarrange(plotlist = plots, ncol=1, nrow=3,common.legend=T)
ggsave("../output/call_report_overview_fig.eps", plot=overview_plot,width=6.25, height=4, device="eps")


## Interest Income & Expense Plots
# MEAN
avg_intincexp_plot <- ggplot(data=summ_stats_qtrly) +
  geom_line(aes(x=date, y=intincassets_mean, color="Income")) +
  geom_line(aes(x=date, y=intexp_mean, color="Expense")) +
  geom_ribbon(aes(x=date, ymin=intexp_mean, ymax=intincassets_mean), fill="gray80", alpha=0.5) +
  labs(title="Interest Income & Expense (Mean)", x="Date (Qtrly)", y="$ (Real)") + 
  scale_color_manual(name="Legend", values=c("Income"="black", "Expense"="red")) +
  annotate("text", x=as.Date("2015-06-30", format="%Y-%m-%d"), y=50, label="Net Interest Income", size=2) + 
  theme_classic(base_size=8)

# STDEV
sd_intincexp_plot <- ggplot(data=summ_stats_qtrly) +
  geom_line(aes(x=date, y=intincassets_sd, color="Income")) +
  geom_line(aes(x=date, y=intexp_sd, color="Expense")) +
  labs(title="Interest Income & Expense (SD)", x="Date (Qtrly)", y="$ (Real)") + 
  scale_color_manual(name="Legend", values=c("Income"="black", "Expense"="red")) + 
  theme_classic(base_size=8)

plots <- list(avg_intincexp_plot, sd_intincexp_plot)
intincexp_plot <- ggarrange(plotlist = plots, ncol=2, nrow=1,common.legend=T)
ggsave("../output/call_report_intincexp_fig.eps", plot=intincexp_plot,width=6.25, height=4, device="eps")


## LOANS
summ_stats_qtrly_long <- pivot_longer(data = summ_stats_qtrly,
                                cols = -date,
                                names_to = c("series","stat"),
                                names_pattern = "(.*)_(.*)")

series_codes <- loans_vars
series_names <- c("Real Estate Loans", "Personal Loans", "Agricultural Loans", 
                  "C&I Loans", "Total Loans")
stat_codes <- c("sd")

plots <- list()

for (i in 1:length(series_codes)) {
  # Create a line plot for each variable
  p <- ggplot(filter(summ_stats_qtrly_long, series == series_codes[i] & stat %in% stat_codes),
              aes(x=date, y=value)) +
    geom_line() +
    #scale_x_datetime(limits= as.POSIXct(date_range, format="%Y-%m-%d")) +
    labs(title=series_names[i], x="Date (Qtrly)", y= "$ (Real)") +
    #scale_color_manual(values = c("NLGAM" = "black", "NSMAM" = "red", "NFRAM"="blue")) +
    theme_classic(base_size=8)
  
  plots[[i]] <- p
}

# Arrange the plots in a grid
loans_plot <- ggarrange(plotlist = plots, ncol=2, nrow=3,common.legend=T)
#all_series <- gridExtra::grid.arrange(grobs=plots, ncol=2)

ggsave("../output/call_report_loans_fig.eps", plot=loans_plot,width=6.25, height=4, device="eps")

##----------------------------------------------------##
## Compute Herfindahl-Hirschman Indices for key variables
##----------------------------------------------------##
# Create totals by quarter for each variable of interest
tot_funcs <- set_names(list(sum = partial(sum, na.rm = TRUE)), nm = c("sum"))
tot_vars = c(assets, loans_vars, securities, deposits)
totals <- call_reports_real %>% group_by(date) %>%
  summarise(across(all_of(tot_vars), tot_funcs))
# Merge totals into full dataset
call_reports_real <- left_join(call_reports_real, totals, by="date")
# Divide bank-level vars by totals
call_reports_real <- call_reports_real %>% 
  mutate(across(all_of(tot_vars),
                ~ . / get(str_replace(cur_column(), "(.*)$", "\\1_sum")),
                .names = "{col}_share"),
         across(ends_with("_share"),
                ~ . ^2,
                .names = "{col}_sq"))

# Compute HHI
hhi <- call_reports_real %>% group_by(date) %>%
  summarise(across(ends_with("_share_sq"),
                   tot_funcs,
                   .names="{str_remove(.col, '_share_sq')}_hhi"))

# Convert HHI data from wide to long and plot for all series
hhi_long <- pivot_longer(data = hhi,
                         cols = -date,
                         names_to = c("series"),
                         names_pattern = "(.*)_")

## HHI OVERVIEW
series_codes <- c("assets", "securities", "deposits")
series_names <- c("Assets HHI", "Securities HHI", "Deposits HHI")

plots <- list()

for (i in 1:length(series_codes)) {
  # Create a line plot for each variable
  p <- ggplot(filter(hhi_long, series == series_codes[i]),
              aes(x=as.Date(as.character(date),format="%Y%m%d"), y=value)) +
    geom_line() +
    #scale_x_datetime(limits= as.POSIXct(date_range, format="%Y-%m-%d")) +
    labs(title=series_names[i], x="Date (Qtrly)", y= "HHI") +
    #scale_color_manual(values = c("NLGAM" = "black", "NSMAM" = "red", "NFRAM"="blue")) +
    theme_classic(base_size=8)
  
  plots[[i]] <- p
}

# Arrange the plots in a grid
hhi_overview_plot <- ggarrange(plotlist = plots, ncol=1, nrow=3,common.legend=T)
#all_series <- gridExtra::grid.arrange(grobs=plots, ncol=2)

ggsave("../output/hhi_overview_fig.eps", plot=hhi_overview_plot,width=6.25, height=4, device="eps")

## Write summary statistics tables
write.csv(summ_stats_long, file="../output/summ_stats_tables.csv", row.names=T)