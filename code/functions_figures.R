PlotAccountsYTD <- function(CATEGORY){
  ggplot(filter(accounts_ytd, category == CATEGORY),
         aes(x = reorder(account_name, spend_2017_ytd), y = spend_2017_ytd)) +
    geom_bar(aes(fill = spend_ratio-1), stat = "identity") +
    geom_point(aes(y = spend_2016_ytd), shape = 3) +
    xlab("Operating expense account") + ylab("") +
    scale_y_continuous(labels = scales::dollar) +
    scale_fill_gradient2("Rel. to budget", low = "#0571b0", high = "#ca0020",
                         labels = scales::percent) +
    theme_bw() + coord_flip() #+ theme(legend.position = "top")
}



PlotCategoryByMonth <- function(CATEGORY){
  ggplot(filter(categories, category == CATEGORY),
         aes(x = month, y = spend_2017)) +
    geom_bar(stat = "identity", fill = BAR_COLOR) +
    geom_point(aes(y = spend_2016), shape = 3) +
    geom_hline(aes(yintercept = budget_2017), size = 1, color = LINE_COLOR) +
    xlab("Month") + ylab("") +
    scale_y_continuous(labels = scales::dollar) +
    theme_bw()
}

FacetAccountByMonth <- function(CATEGORY){
  ggplot(filter(accounts, category == CATEGORY),
         aes(x = month, y = spend_2017)) +
    geom_bar(stat = "identity", fill = BAR_COLOR) +
    geom_point(aes(y = spend_2016), shape = 3) +
    geom_hline(aes(yintercept = budget_2017), size = 1, color = LINE_COLOR) +
    facet_wrap(~account_name) +
    xlab("Month") + ylab("") +
    scale_y_continuous(labels = scales::dollar) +
    theme_bw()
}


PlotAccountByMonth <- function(ACCOUNT){
  ggplot(filter(accounts, account_name == ACCOUNT),
         aes(x = month, y = spend_2017)) +
    geom_bar(stat = "identity", fill = BAR_COLOR) +
    geom_point(aes(y = spend_2016), shape = 3) +
    geom_hline(aes(yintercept = budget_2017), size = 1, color = LINE_COLOR) +
    xlab("Month") + ylab("") +
    scale_y_continuous(labels = scales::dollar) +
   theme_bw()
}
