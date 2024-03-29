## Overview graph of sterfhuis with groups of accounts ##

library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(debkeepr)

transactions_group <- read_rds("data/transactions-lsd-group.rds")
accounts_group <- read_csv("data/accounts-group.csv") %>% 
  select(id, group, type)

# Create network from groups column in accounts #
nodes <- deb_debit(transactions_group) %>% 
  left_join(accounts_group, by = c("account_id" = "id")) %>% 
  rename(id = account_id) %>% 
  filter(id != 1) %>% 
  mutate(pounds = deb_lsd_l(lsd)) %>% 
  arrange(pounds)

nodes2 <- deb_debit(transactions_group) %>% 
  full_join(accounts_group, by = c("account_id" = "id")) %>% 
  rename(id = account_id) %>% 
  filter(id != 1) %>% 
  mutate(pounds = deb_lsd_l(lsd))

# Take out transactions with balance
transactions_sum <- transactions_group %>% 
  filter(credit != 1) %>% 
  filter(debit != 1) %>% 
  group_by(credit, debit) %>% 
  deb_summarise(lsd) %>% 
  mutate(l = deb_lsd_l(lsd)) %>% 
  filter(credit %in% nodes$id & debit %in% nodes$id)

sterfhuis_groups <- graph_from_data_frame(d = transactions_sum, vertices = nodes, directed = TRUE)
sterfhuis_groups_tbl <- as_tbl_graph(sterfhuis_groups)

# Remove isolated accounts
sterfhuis_groups_tbl <- sterfhuis_groups_tbl %>% 
  activate(nodes) %>% 
  filter(!node_is_isolated())

set_graph_style()

set.seed(12)
ggraph(sterfhuis_groups_tbl, layout = "fr") + 
  geom_edge_fan(aes(edge_alpha = l)) + 
  scale_edge_alpha(labels = scales::dollar_format(prefix = "£")) + 
  geom_node_point(aes(size = pounds, color = type), alpha = 0.9) + 
  scale_size_continuous(range = c(0.8, 10), labels = scales::dollar_format(prefix = "£")) + 
  labs(size = "Accumulated Value",
       edge_alpha = "Transactions",
       color = "Account types") + 
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 4), order = 1)) + 
  theme(legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle("Estate of Jan della Faille de Oude, 1582–1594",
          subtitle = "Groups of accounts")

ggraph(sterfhuis_groups_tbl, layout = "fr") + 
  geom_edge_fan(aes(edge_alpha = l)) + 
  scale_edge_alpha(labels = scales::dollar_format(prefix = "£")) +
  geom_node_point(aes(size = pounds), alpha = 0.9)

ggsave("plots-aans/sterfhuis-network-groups.png", width = 10, height = 8)