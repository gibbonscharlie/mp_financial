################################################################################
##############################     PRIVILEGED     ##############################
##############################    CONFIDENTIAL    ##############################
###################### PREPARED AT THE REQUEST OF COUNSEL ######################
################################################################################
###
### Title:  Master Script for MP Financial Analysis
###
### Author: Charlie Gibbons
###
################################################################################
############################   DRAFT :: UNAUDITED   ############################
################################################################################

library(assertive)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)
library(stringr)
library(zoo)

### Path to latest YTD ledger
LEDGER_LATEST <- "../inputs/ledger/Museum Parc HOA & Master General Ledger YTD 3-31-17.xls"

### Clean ledger
source("functions_ledger.R")
source("clean_ledger.R")

### Clean budget
source("clean_budget.R")

### Summarize
source("summarize.R")

### Figures
source("figures.R")
