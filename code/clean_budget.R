################################################################################
##############################     PRIVILEGED     ##############################
##############################    CONFIDENTIAL    ##############################
###################### PREPARED AT THE REQUEST OF COUNSEL ######################
################################################################################
###
### Title:  Clean MP budget
###
### Author: Charlie Gibbons
###
################################################################################
############################   DRAFT :: UNAUDITED   ############################
################################################################################

categories <- read.csv("../inputs/budget/categories.csv",
                       stringsAsFactors = FALSE) %>%
    mutate(account = as.character(account))

budget <- read_excel("../inputs/budget/HOA 2017 Budget - Jenark codes on left.xls",
                     sheet = "2017 Budget")
budget <- budget[, 1:8]
names(budget) <- c("account", "account_name", "spend_2015", "x1",
                   "x2", "x3", "budget_2016", "budget_2017")
budget <- budget %>%
    select(account, account_name, spend_2015, budget_2016, budget_2017) %>%
    filter(!is.na(account)) %>%
    mutate_at(vars(matches("budget|spend")), funs(as.numeric))

budget <- left_join(budget, categories) %>%
    select(book, category, account, account_name,
           budget_2017, budget_2016) %>%
    mutate(budget_2017 = ifelse(book == "Revenues", -budget_2017, budget_2017),
           budget_2016 = ifelse(book == "Revenues", -budget_2016, budget_2016))
