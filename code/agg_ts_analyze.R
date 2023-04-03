########################################################
# Heterogeneous Banks Research Project
# Aggregate Time Series (Commercial Banks)
# Author: Daniel Schwindt
# Date: 3/25/2023
# Purpose: Input aggregate time series data and perform
#          preliminary data analysis
########################################################

## 0. Housekeeping
rm(list=ls())
setwd("~/Documents/research_projects/heterogeneous_banks/code")

library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)

## 1. Load data
frb_h8_assets <- read.csv("../data/FRB_H8_assets_overview.csv", skip=5,
                          stringsAsFactors=F) # H8 commercial bank assets

frb_h8_liabs <- read.csv("../data/FRB_H8_liabilities_overview.csv", skip=5,
                         stringsAsFactors=F) # H8 commercial bank liabilities

macro_series_monthly <- read_excel("../data/Bank_Runs_MacroSeries.xls", 
                                   sheet="Monthly")

macro_series_qtrly <- read_excel("../data/Bank_Runs_MacroSeries.xls", 
                                 sheet="Quarterly")

## 2. Clean Data
# Merge assets & liabilities
frb_h8 <- left_join(frb_h8_assets, frb_h8_liabs, by=c("Time.Period"="Time.Period"))

# Create new date variable
frb_h8 <- frb_h8 %>% mutate(date = as.Date(paste(Time.Period, "-01", sep=""), 
                                           format="%Y-%m-%d"))

# Merge in monthly macro series
frb_h8_macro <- left_join(frb_h8, macro_series_monthly, by=c("date"="DATE"))

# Create series in real terms
frb_h8_macro <- frb_h8_macro %>% 
  mutate(cpi = ifelse(is.na(CPIAUCSL), CPIAUCNS, CPIAUCSL),
         across(starts_with("B"), ~ . / cpi))

# Convert from wide format to long
frb_h8_macro_long <- pivot_longer(data = frb_h8_macro,
                                  cols = starts_with("B"),
                                  names_to = c("series", "group"),
                                  names_pattern = "^B(\\d+)(.*)$")

##--------------------------------------------------------##
## ASSETS
##--------------------------------------------------------##
## 3. Plot All series
series_codes <- c("1003", "1020", "1023", "1026", "1029",
                  "1048", "3305", "1151")
series_names <- c("Treas. & Agency Securities", "Loans & Leases: Bank Credit",
                  "C&I Loans", "RE Loans", "Consumer Loans", "Cash", 
                  "Other Loans & Leases", "Total Assets")
group_codes <- c("NFRAM", "NLGAM", "NSMAM")

plots <- list()
date_range <- c("1973-01-01", "2023-03-01")
for (i in 1:length(series_codes)) {
  # Create a line plot for each variable
  p <- ggplot(filter(frb_h8_macro_long, series == series_codes[i] & group %in% group_codes),
              aes(x=date, y=value, color=group)) +
    geom_line() +
    scale_x_datetime(limits= as.POSIXct(date_range, format="%Y-%m-%d")) +
    labs(title=series_names[i], x="Date (Qtrly)", y= "Real $") +
    scale_color_manual(values = c("NLGAM" = "black", "NSMAM" = "red", "NFRAM"="blue")) +
    theme_classic(base_size=8)
  
  plots[[i]] <- p
}

# Arrange the plots in a grid
all_series <- ggarrange(plotlist = plots, ncol=2, nrow=4,common.legend=T)
#all_series <- gridExtra::grid.arrange(grobs=plots, ncol=2)

ggsave("../output/all_assets_fig.eps", plot=all_series, device="eps")

## 4. Plot Just Loans
codes = c("1020", "1023", "1026", "1029")
codes_names = c("Loans & Leases: Bank Credit",
                "C&I Loans", "RE Loans", "Consumer Loans")
loan_plots <- list()
for (i in 1:length(codes)) {
  # Create a line plot for each variable
  p <- ggplot(filter(frb_h8_macro_long, series == codes[i] & group %in% group_codes),
              aes(x=date, y=value, color=group)) +
    geom_line() +
    scale_x_datetime(limits= as.POSIXct(date_range, format="%Y-%m-%d")) +
    labs(title=codes_names[i], x="Date (Qtrly)", y= "Real $") +
    scale_color_manual(values = c("NLGAM" = "black", "NSMAM" = "red", "NFRAM"="blue")) +
    theme_classic(base_size=8)
  
  loan_plots[[i]] <- p
}

# Arrange the plots in a grid
loans <- ggarrange(plotlist = loan_plots, ncol=2, nrow=2,common.legend=T)
#all_series <- gridExtra::grid.arrange(grobs=plots, ncol=2)

ggsave("../output/loan_fig.eps", plot=loans, device="eps")

## 5. Plot Other Assets
codes = c("1003", "1048", "3305", "1151")
codes_names = c("Treas. & Agency Securities","Cash", 
                "Other Loans & Leases", "Total Assets")
plots <- list()
for (i in 1:length(codes)) {
  # Create a line plot for each variable
  p <- ggplot(filter(frb_h8_macro_long, series == codes[i] & group %in% group_codes),
              aes(x=date, y=value, color=group)) +
    geom_line() +
    scale_x_datetime(limits= as.POSIXct(date_range, format="%Y-%m-%d")) +
    labs(title=codes_names[i], x="Date (Qtrly)", y= "Real $") +
    scale_color_manual(values = c("NLGAM" = "black", "NSMAM" = "red", "NFRAM"="blue")) +
    theme_classic(base_size=8)
  
  plots[[i]] <- p
}

# Arrange the plots in a grid
other_assets <- ggarrange(plotlist = plots, ncol=2, nrow=2,common.legend=T)
#all_series <- gridExtra::grid.arrange(grobs=plots, ncol=2)

ggsave("../output/other_assets_fig.eps", plot=other_assets, device="eps")


##--------------------------------------------------------##
## LIABILITIES
##--------------------------------------------------------##
## 3. Plot All series
series_codes <- c("1058", "1072", "1110", "3094", "3095", "1152")
series_names <- c("Deposits", "Lg Time Deposits", "Other Deposits", "Borrowings", "Other Liabilities", "Total Liabilities")
group_codes <- c("NFRAM", "NLGAM", "NSMAM")

plots <- list()
date_range <- c("1973-01-01", "2023-03-01")
for (i in 1:length(series_codes)) {
  # Create a line plot for each variable
  p <- ggplot(filter(frb_h8_macro_long, series == series_codes[i] & group %in% group_codes),
              aes(x=date, y=value, color=group)) +
    geom_line() +
    scale_x_datetime(limits= as.POSIXct(date_range, format="%Y-%m-%d")) +
    labs(title=series_names[i], x="Date (Qtrly)", y= "Real $") +
    scale_color_manual(values = c("NLGAM" = "black", "NSMAM" = "red", "NFRAM"="blue")) +
    theme_classic(base_size=8)
  
  plots[[i]] <- p
}

# Arrange the plots in a grid
all_series <- ggarrange(plotlist = plots, ncol=2, nrow=3,common.legend=T)
#all_series <- gridExtra::grid.arrange(grobs=plots, ncol=2)

ggsave("../output/all_liabs_fig.eps", plot=all_series, width=6.25, height=4, device="eps")


## 4. Deposits & Total Liabilities
series_codes <- c("1058", "1072", "1110", "1152")
series_names <- c("Deposits", "Lg Time Deposits", "Other Deposits", "Total Liabilities")
group_codes <- c("NFRAM", "NLGAM", "NSMAM")

plots <- list()
date_range <- c("1973-01-01", "2023-03-01")
for (i in 1:length(series_codes)) {
  # Create a line plot for each variable
  p <- ggplot(filter(frb_h8_macro_long, series == series_codes[i] & group %in% group_codes),
              aes(x=date, y=value, color=group)) +
    geom_line() +
    scale_x_datetime(limits= as.POSIXct(date_range, format="%Y-%m-%d")) +
    labs(title=series_names[i], x="Date (Qtrly)", y= "Real $") +
    scale_color_manual(values = c("NLGAM" = "black", "NSMAM" = "red", "NFRAM"="blue")) +
    theme_classic(base_size=8)
  
  plots[[i]] <- p
}

# Arrange the plots in a grid
deposits_series <- ggarrange(plotlist = plots, ncol=2, nrow=2,common.legend=T)
#all_series <- gridExtra::grid.arrange(grobs=plots, ncol=2)

ggsave("../output/deposits_fig.eps", plot=deposits_series, width=6.25, height=4, device="eps")