################################################################################
##############################     PRIVILEGED     ##############################
##############################    CONFIDENTIAL    ##############################
###################### PREPARED AT THE REQUEST OF COUNSEL ######################
################################################################################
###
### Title:  Summarize Financial Data
###
### Author: Charlie Gibbons
###
################################################################################
############################   DRAFT :: UNAUDITED   ############################
################################################################################

ledger_latest <- bind_rows(ledger_latest[[1]], ledger_latest[[2]])
ledger_2016   <- bind_rows(ledger_2016[[1]],   ledger_2016[[2]])

account_latest <- ledger_latest %>%
    mutate(month = month(date, label = TRUE)) %>%
    group_by(account, sub, month) %>%
    summarize(debit_2017  = sum(debit),
              credit_2017 = sum(credit))

account_2016 <- ledger_2016 %>%
    mutate(month = month(date, label = TRUE)) %>%
    group_by(account, sub, month) %>%
    summarize(debit_2016  = sum(debit),
              credit_2016 = sum(credit))

combos <- expand.grid(month = unique(account_latest$month),
                      account = unique(budget$account),
                      stringsAsFactors = FALSE)

accounts <- full_join(account_latest, account_2016) %>%
    full_join(combos) %>%
    full_join(budget) %>%
    filter(month <= max(account_latest$month)) %>%
    mutate(budget_2017 = budget_2017/12,
           budget_2016 = budget_2016/12,
           spend_2017 = debit_2017 - credit_2017,
           spend_2016 = debit_2016 - credit_2016)

accounts_ytd <- accounts %>%
    group_by(book, category, account_name, account, sub) %>%
    summarize_at(vars(matches("201")), funs(ytd = sum), na.rm = TRUE) %>%
    mutate(spend_ratio = spend_2017_ytd/budget_2017_ytd,
           spend_ratio = ifelse(spend_ratio < 0, 0, spend_ratio))

categories <- accounts %>%
    group_by(book, category, month) %>%
    summarize_at(vars(matches("201")), sum, na.rm = TRUE)

categories_ytd <- accounts_ytd %>%
    group_by(book, category) %>%
    summarize_at(vars(matches("201")), sum, na.rm = TRUE) %>%
    mutate(spend_ratio = spend_2017_ytd/budget_2017_ytd,
           spend_ratio = ifelse(spend_ratio < 0, 0, spend_ratio))

save(accounts, accounts_ytd, categories, categories_ytd,
     file = "../intermediate/cleaned.RData")
