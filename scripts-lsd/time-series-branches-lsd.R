### Time series with grouped branches ###

library(tidyverse)
library(xts)
library(timetk)
library(lubridate)
library(debkeepr)
library(ggrepel)
library(hrbrthemes)
source("scripts-lsd/time-series-functions-lsd.R")

# Load data
transactions_group <- read_rds("data/transactions-lsd-group.rds")
accounts_group <- read_csv("data/accounts-group.csv") %>% 
  select(id, group, type)

branch_accounts <- filter(accounts_group, type == "Branch") %>% 
  select(id) %>% flatten() %>% as_vector()

branches_running <- deb_running(transactions_group,
                                accounts_group,
                                label = group,
                                ids = branch_accounts)

# To xts
branches_xts <- to_fill_xts(branches_running, group = group) %>% 
  replace_na(0)

# To tbl
branches_tbl <- from_fill_xts(branches_xts) %>% 
  mutate(denarii = if_else(current >= 0, current, -current),
         l = current / 240,
         lsd = deb_d_lsd(denarii),
         value = if_else(current >= 0,
                         paste0("£", scales::comma(purrr::map_dbl(lsd, 1)), " ",
                         purrr::map_dbl(lsd, 2), "s. ",
                         purrr::map_dbl(lsd, 3), "d."),
                         paste0("-£", scales::comma(purrr::map_dbl(lsd, 1)), " ",
                                purrr::map_dbl(lsd, 2), "s. ",
                                purrr::map_dbl(lsd, 3), "d.")),
         id = case_when(id == "London.branch" ~ "London branch",
                        id == "Venice.branch" ~ "Venice branch",
                        id == "Verona.branch" ~ "Verona branch"))

# Plot
ggplot(branches_tbl) + 
  geom_line(aes(x = date, y = l, group = id, color = id), size = 1) + 
  geom_hline(yintercept = 0, size = 1) + 
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) + 
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") + 
  labs(y = NULL, x = NULL, color = "Branches") + 
  theme_ipsum(base_size = 14) + 
  ggtitle("Running values of the branches in the trade of Jan de Oude",
          subtitle = "Estate of Jan della Faille de Oude, 1582–1594") + 
  transition_reveal(id, date)

ggsave("plots-aans/branches-running.png", width = 10, height = 8)

# Plot with only before 16 March 1585 and after 1 December 1594
date_break1 <- ymd("1585-03-16")
date_break2 <- ymd("1594-09-01")


## Did not redo labels
# Make labels
branches1 <- branches_tbl %>% 
  filter(date < date_break1) %>% 
  mutate(label = case_when(date == ymd("1582-11-08") ~ value,
                           date == ymd("1585-03-15") ~ value,
                           l == min(l) & date == ymd("1583-12-11") ~ 
                             paste(value, "11 Dec 1583", sep = "\n"),
                           l == max(l) & date == ymd("1583-07-29") ~
                             paste(value, "29 July 1583", sep = "\n")))

branches2 <- branches_tbl %>% 
  filter(date >= date_break2) %>% 
  mutate(label = case_when(date == ymd("1594-09-01") ~ value,
                           l == 0 & date == ymd("1594-10-04") ~ paste("£0 0s. 0d."),
                           date == ymd("1594-12-16") & id != "London branch" ~ value))

# Separate graphs with date breaks
ggplot(branches1) + 
  geom_line(aes(x = date, y = l, group = id, color = id), size = 1) + 
  geom_text(aes(x = date, y = l, label = label),
            nudge_y = -700) + 
  geom_hline(yintercept = 0, size = 1, alpha = 0.8) + 
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) + 
  scale_x_date(date_breaks = "4 month", date_labels = "%m-%Y",
               expand = c(0.1, 0.1)) + 
  labs(y = NULL, x = NULL, color = "Branches") + 
  theme_ipsum(base_size = 14) + 
  ggtitle("Running values of the branches in the trade of Jan de Oude",
          subtitle = "November 1582 to March 1585")

ggsave("plots-aans/branches-running-1.png", width = 10, height = 8)

ggplot(branches2) + 
  geom_line(aes(x = date, y = l, group = id, color = id), size = 1) + 
  geom_text(aes(x = date, y = l, label = label),
            nudge_y = 500) + 
  geom_hline(yintercept = 0, size = 1) + 
  scale_y_continuous(labels = scales::dollar_format(prefix = "£")) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y",
               expand = c(0.1, 0.1)) + 
  labs(y = NULL, x = NULL, color = "Branches") + 
  theme_ipsum(base_size = 14) + 
  ggtitle("Running values of the branches in the trade of Jan de Oude",
          subtitle = "September 1594 to 16 December 1594")

ggsave("plots-aans/branches-running-2.png", width = 10, height = 8)