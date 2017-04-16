################################################################################
##############################     PRIVILEGED     ##############################
##############################    CONFIDENTIAL    ##############################
###################### PREPARED AT THE REQUEST OF COUNSEL ######################
################################################################################
###
### Title:  Functions to clean management ledger
###
### Author: Charlie Gibbons
###
################################################################################
############################   DRAFT :: UNAUDITED   ############################
################################################################################

CleanLedger <- function(ledger){
### Clean ledger file from management
### Args    path to ledger
### Return  list of operating and reserve fund ledgers
    #assert_all_are_existing_files(ledger)

    ## Sheets for funds
    SHEET_OP  <- "1057,Museum Parc HOA OP"
    SHEET_RES <- "1060,Museum Parc HOA RESERVE"
    SHEETS_LEDGER <- excel_sheets(ledger)

    ## Check that the ledger has the correct sheets
    assert_is_subset(SHEET_OP,  SHEETS_LEDGER)
    assert_is_subset(SHEET_RES, SHEETS_LEDGER)

    ledger_op  <- read_excel(ledger, SHEET_OP)
    ledger_res <- read_excel(ledger, SHEET_RES)

    ledger_op  <- CleanFund(ledger_op)
    ledger_res <- CleanFund(ledger_res)

    return(list(operating = ledger_op, reserve = ledger_res))
}

CleanFund <- function(fund){
### Tab from ledger file from management
### Args    data frame of tab
### Return  cleaned ledger
    names(fund) <- c("account", "sub", "X1", "type", "X2reference",
                       "date", "description", "debit", "credit", "balance")

    ## Extract starting balances
    starting_balance <- fund %>% filter(!is.na(account))
    assert_all_are_true(starting_balance$description == "Beginning Balance")
    starting_balance <- starting_balance %>% select(account, sub, balance)

    ## Fill in missing accounts/subaccounts
    fund$sub[is.na(fund$sub) & !is.na(fund$account)] <- "001"
    fund$account <- na.locf(fund$account)
    fund$sub     <- na.locf(fund$sub)

    ## Filter out needless values
    DESCRIPTIONS_REMOVE <- c("Beginning Balance", "Beginning Balances",
                             "Account Total",
                             "Ending Balance")
    REMOVE_REGEX <- "beginning balance|account total|ending balance|entity totals|account balance"
    fund <- fund %>% filter(!grepl(REMOVE_REGEX, description, ignore.case = TRUE) &
                            !is.na(description)) %>%
        select(-balance) %>%
        mutate(date = as.Date(date, format = "%m/%d/%Y"),
               debit  = ifelse(is.na(debit) , 0, debit),
               credit = ifelse(is.na(credit), 0, credit))
    return(fund)
}
