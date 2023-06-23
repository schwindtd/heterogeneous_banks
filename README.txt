README

Heterogeneous Banks Research Project

./: root directory
./code/: contains all code files (.R)
./data/: contains all data files
./output/: contains all output files

PROGRAMS
1- call_reports_explore.R: 
loads raw data, preliminary data cleaning, creates charts and graphs for summary statistics and time series
2- call_reports_analyze.R: 
loads data from Drechsler et al (2017) to create charts and graphs for summary statistics and time series [NOTE: think have found data errors in their real estate and household loans series due to changes in reporting in 2011:Q1]
3- distance_pairwise_compute.R:
computes adjacency matrices by quarter, creates aggregate heterogeneity series, performs VAR analysis, additional robustness checks and plotting the network graphs [this takes a really long time]
4- helper_funcs.R:
additional functions to help with computing and extracting information

DATA
1- callreports_1976_2020.csv:
Contains raw Call Report data from 1976Q1 to 2020Q3
2- Bank_Runs_MacroSeries.xls:
Contains macroeconomic data used for VAR analysis in (3): from FRED
3- callreports_1976_2020_WRDS.dta:
Stata data file from Drechsler et al. (2017): used in (2) above