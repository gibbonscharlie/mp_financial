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


##################################################
###  Group by account
##################################################


##############################
###  Total by account
##############################

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


##############################
###  Combine previous and current year
##############################

### Account-month combinations
combos <- expand.grid(month = unique(account_2016$month),
                      account = unique(budget$account),
                      sub = "001",
                      stringsAsFactors = FALSE)

accounts <- full_join(account_latest, account_2016) %>%
    full_join(combos) %>%
    full_join(budget) %>%
    mutate(budget_2017 = budget_2017/12,
           budget_2016 = budget_2016/12,
           spend_2017 = debit_2017 - credit_2017,
           spend_2016 = debit_2016 - credit_2016)

### Spending YTD by account
accounts_ytd <- accounts %>%
    filter(month <= max(account_latest$month)) %>%
    group_by(book, category, account_name, account, sub) %>%
    summarize_at(vars(matches("201")), funs(ytd = sum), na.rm = TRUE) %>%
    mutate(spend_ratio = spend_2017_ytd/budget_2017_ytd,
           spend_ratio = ifelse(spend_ratio < 0, 0, spend_ratio))


##############################
###  Group by category
##############################

categories <- accounts %>%
    group_by(book, category, month) %>%
    summarize_at(vars(matches("201")), sum, na.rm = TRUE)

categories_ytd <- accounts_ytd %>%
    group_by(book, category) %>%
    summarize_at(vars(matches("201")), sum, na.rm = TRUE) %>%
    mutate(spend_ratio = spend_2017_ytd/budget_2017_ytd,
           spend_ratio = ifelse(spend_ratio < 0, 0, spend_ratio))


##############################
###  Save
##############################

save(accounts, accounts_ytd, categories, categories_ytd,
     file = "../intermediate/cleaned.RData")


##################################################
###  Group by payee
##################################################

##############################
###  Fixed known duplicates
##############################

RecodeNames <- function(x){
    recode(x,
           "Adj. Accrued Securit" = "Adj Accrued Security",
           "Adj Security Accrual" = "Security Accrual",
           "Adjust Water accrual" = "Adj Water Accrual",
           "Loral Landscaping, I" = "Loral Landscaping, Inc.",
           "Reclass late fees" = "Reclass Late Fees",
           "Relianrt LLC" = "Reliant LLC",
           "Accrual" = "Accruals",
           "James Ernst Accounti" = "James Ernst Accounting",
           "Record CD Interest" = "Record interest",
           "Record Water Autopay" = "Record Water Accrual")
}


##############################
###  Total by payee
##############################

payee_latest <- ledger_latest %>%
    mutate(month = month(date, label = TRUE)) %>%
    mutate(description = RecodeNames(description)) %>%
    group_by(description, month) %>%
    summarize(debit_2017  = sum(debit),
              credit_2017 = sum(credit))

payee_2016 <- ledger_2016 %>%
    mutate(description = RecodeNames(description)) %>%
    mutate(month = month(date, label = TRUE)) %>%
    group_by(description, month) %>%
    summarize(debit_2016  = sum(debit),
              credit_2016 = sum(credit))


##############################
###  Look for duplicate payees
##############################

PAYEES <- unique(c(payee_latest$description, payee_2016$description))

distances <- adist(PAYEES)
distances <- data.frame(a = rep(PAYEES, each = length(PAYEES)),
                        b = PAYEES,
                        dist = as.numeric(distances),
                        stringsAsFactors = FALSE)
distances$len_a <- str_count(distances$a, "[^[:blank:]]")
distances$len_b <- str_count(distances$b, "[^[:blank:]]")
distances$ratio <- distances$dist/pmin(distances$len_a, distances$len_b)

dist <- distances %>% filter(ratio < 1/3 & dist > 0 & !grepl("VOID", a))


##############################
###  Combine previous and current year
##############################

### Payee-month combinations
combos <- expand.grid(month = unique(account_2016$month),
                      description = PAYEES,
                      stringsAsFactors = FALSE)

payees <- full_join(payee_latest, payee_2016) %>%
    full_join(combos) %>%
    left_join(budget) %>%
    mutate(spend_2017 = debit_2017 - credit_2017,
           spend_2016 = debit_2016 - credit_2016)


##############################
###  Compare
##############################

payees_ytd <- payees %>%
    filter(book == "Operating expenses") %>%
    group_by(description) %>%
    summarize_at(vars(matches("201")), sum, na.rm = TRUE) %>%
    arrange(desc(spend_2017)) %>%
    ungroup() %>%
    filter(spend_2017 > 0) %>%
    filter(cumsum(spend_2017)/sum(spend_2017) < 0.8)
