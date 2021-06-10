## Clean data ##

library(tidyverse)

## Transactions ##
transactions <- read_csv("data-raw/transactions.csv", col_types = cols(
  date = col_date(format = "%Y%m%d"))) %>% 
  select(credit:debit, date:denarii) %>% 
  rename(l = librae, s = solidi, d = denarii)

write_csv(transactions, "data/transactions.csv")

## Accounts ##
accounts <- read_csv("data-raw/accounts.csv") %>% 
  select(id, account:location) %>% 
  rowid_to_column("id_int")

write_csv(accounts, "data/accounts.csv")

## Transactions and accounts by group ##

# Accounts
# Convert Heir type to Inheritance so Jacques' accounts are under inheritance
# This also makes Hester and Cornelia account under inheritance, but this should be fine
accounts <- read_csv("data-raw/accounts.csv") %>% 
  select(account_id = id, account:location) %>% 
  mutate(type = str_replace_all(type, "Heir", "Inheritance"))

# Get tibble of distinct groups and create ids
groups <- distinct(accounts, group) %>% 
  rowid_to_column("id")

accounts_group <- left_join(groups, accounts, by = "group") %>% 
  distinct(group, .keep_all = TRUE)

# Transactions
# Get the group id for each account
accounts_group_id <- left_join(accounts, groups, by = "group") %>% 
  select(id, account_id, group)

transactions_group <- transactions %>% 
  left_join(accounts_group_id, by = c("credit" = "account_id")) %>% 
  left_join(accounts_group_id, by = c("debit" = "account_id")) %>% 
  select(-credit, -debit) %>% 
  rename(credit = id.x, debit = id.y) %>% 
  select(credit, debit, date, l:d) %>% 
  filter(credit != debit) # Remove transactions between the same group

write_csv(accounts_group, "data/accounts-group.csv")
write_csv(transactions_group, "data/transactions-group.csv")

## lsd list column ##
library(debkeepr)

transactions_lsd <- deb_as_lsd_mutate(transactions, replace = TRUE)
transactions_lsd_group <- deb_as_lsd_mutate(transactions_group, replace = TRUE)

write_rds(transactions_lsd, "data/transactions-lsd.rds")
write_rds(transactions_lsd_group, "data/transactions-lsd-group.rds")

