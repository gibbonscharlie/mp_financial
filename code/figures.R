fig_cat_ytd <- ggplot(filter(categories_ytd, book == "Operating expenses"),
               aes(x = reorder(category, spend_2017_ytd), y = spend_2017_ytd)) +
    geom_bar(aes(fill = spend_ratio-1), stat = "identity") +
    geom_point(aes(y = spend_2016_ytd), shape = 3) +
    xlab("Operating expense category") + ylab("") +
    scale_y_continuous(labels = scales::dollar) +
    scale_fill_distiller("Rel. to budget", type = "div",
                         palette = "RdBu", labels = scales::percent) +
    theme_bw() + coord_flip() #+ theme(legend.position = "top")

CATEGORY <- "Repair & Maintenance"
fig_acc_ytd <- ggplot(filter(accounts_ytd, category == CATEGORY),
               aes(x = reorder(account_name, spend_2017_ytd), y = spend_2017_ytd)) +
    geom_bar(aes(fill = spend_ratio-1), stat = "identity") +
    geom_point(aes(y = spend_2016_ytd), shape = 3) +
    xlab("Operating expense account") + ylab("") +
    scale_y_continuous(labels = scales::dollar) +
    scale_fill_distiller("Rel. to budget", type = "div",
                         palette = "RdBu", labels = scales::percent) +
    theme_bw() + coord_flip() #+ theme(legend.position = "top")

fig_cat_mon <- ggplot(filter(categories, category == CATEGORY),
                      aes(x = month, y = spend_2017)) +
    geom_bar(stat = "identity", fill = "dodgerblue") +
    geom_point(aes(y = spend_2016), shape = 3) +
    geom_hline(aes(yintercept = budget_2017), size = 1, color = "red") +
    xlab("Month") + ylab("") +
    scale_y_continuous(labels = scales::dollar) +
    theme_bw()

fig_acc_mon <- ggplot(filter(accounts, account_name == ACCOUNT),
                      aes(x = month, y = spend_2017)) +
    geom_bar(stat = "identity", fill = "dodgerblue") +
    geom_point(aes(y = spend_2016), shape = 3) +
    geom_hline(aes(yintercept = budget_2017), size = 1, color = "red") +
    xlab("Month") + ylab("") +
    scale_y_continuous(labels = scales::dollar) +
    theme_bw()

