### Graph of accounts dealing with Heirs ###
# Uses account and transactions groups

library(tidyverse)
library(igraph)
library(ggraph)
library(debkeepr)

# Load data
transactions_group <- read_csv("data/transactions-group.csv")
accounts_group <- read_csv("data/accounts-group.csv") %>% 
  select(id, group, type) %>% 
  mutate(group = str_replace(group, "Balance on 8 November",
                             paste("Opening", "balance", sep = "\n")))
  
## Create subset of transactions that deal directly with heirs
heirs_accounts <- filter(accounts_group, type == "Inheritance") %>% 
  select(id) %>% flatten() %>% as_vector()
transactions_inheritance <- transactions_group %>%
  filter(debit %in% heirs_accounts | credit %in% heirs_accounts)

## Aggregate accounts dealing with heirs

## Sum of transactions
transactions_sum <- transactions_inheritance %>% 
  group_by(credit, debit) %>% 
  deb_sum_df(l, s, d) %>% 
  deb_lsd_l_mutate(column_name = pounds)

# Recreate accounts that are in transactions_inheritance
inheritance_credit <- transactions_inheritance %>% 
  distinct(credit) %>%
  rename(id = credit)

inheritance_debit <- transactions_inheritance %>% 
  distinct(debit) %>%
  rename(id = debit)

accounts_inheritance <- full_join(inheritance_credit, inheritance_debit, by = "id")

## Total debit of accounts within inheritance transactions
inheritance_debit_l <- deb_debit(transactions_inheritance) %>% 
  deb_lsd_l_mutate(column_name = debit.l) %>% 
  select(account_id, debit.l)

inheritance_credit_l <- deb_credit(transactions_inheritance) %>% 
  deb_lsd_l_mutate(column_name = credit.l) %>% 
  select(account_id, credit.l)

nodes <- accounts_inheritance %>% 
  left_join(accounts_group, by = "id") %>% 
  left_join(inheritance_credit_l, by = c("id" = "account_id")) %>% 
  left_join(inheritance_debit_l, by = c("id" = "account_id")) %>% 
  replace_na(list(credit.l = 0, debit.l = 0)) %>% 
  # labels
  mutate(label = if_else(debit.l > 1000 & type != "Inheritance" | credit.l > 1000 & type != "Inheritance",
                         paste(group), NA_character_),
         label_debit = if_else(debit.l > 9000 & type != "Inheritance",
                               paste(group), NA_character_),
         label_credit = if_else(credit.l > 5000 & type != "Inheritance",
                                paste(group), NA_character_),
         color = if_else(type == "Inheritance", paste(group), NA_character_))

# Create igraph object
# Creates vertices from inheritances_transactions data
inheritance <- graph_from_data_frame(d = transactions_sum,
                                     vertices = nodes, directed = TRUE)
set_graph_style()

set.seed(240)
ggraph(inheritance, layout = "kk") + 
  geom_edge_fan(aes(edge_alpha = l),
                arrow = arrow(length = unit(3, 'mm')), 
                end_cap = circle(2, 'mm')) + 
  scale_edge_alpha(labels = scales::dollar_format("£")) + 
  geom_node_point(aes(size = debit.l, color = color), alpha = 0.9) + 
  geom_node_text(aes(label = label), repel = TRUE) + 
  scale_size_continuous(range = c(0.8, 10), labels = scales::dollar_format("£")) + 
  labs(size = "Total debit",
       edge_alpha = "Transactions",
       color = "Heirs") + 
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 4), order = 1)) + 
  theme(legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle("Subgraph of the inheritance of the heirs of Jan de Oude",
          subtitle = "Estate of Jan della Faille de Oude, 1582–1594")

ggsave("plots-aans/inheritance-network.png", width = 10, height = 8)

# Arc graph

# Change arrangement of nodes to put heirs at the front
nodes2 <- arrange(nodes, color)

inheritance_arc <- graph_from_data_frame(d = transactions_sum,
                                         vertices = nodes2, directed = TRUE)

## Node size and labels as debit ##
ggraph(inheritance_arc, layout = "linear") + 
  geom_edge_arc(aes(edge_alpha = l)) + 
  scale_edge_alpha(labels = scales::dollar_format(prefix = "£")) + 
  geom_node_point(aes(size = debit.l, color = color), alpha = 0.9) + 
  geom_node_text(aes(label = label_debit),
                 nudge_y = 1.75, nudge_x = 1.75, angle = 45) + 
  scale_size_continuous(range = c(0.8, 10), labels = scales::dollar_format(prefix = "£")) + 
  labs(size = "Total debit",
       edge_alpha = "Transactions",
       color = "Heirs",
       title = "Estate of Jan della Faille de Oude, 1582–1594") + 
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 4), order = 1)) + 
  theme(legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle("Subgraph of the inheritance of the heirs of Jan de Oude",
          subtitle = paste("Labels show accounts that held debts to the heirs of over £9,000",
                           "Lines above the nodes move left (creditor) to right (debtor)",
                           "Lines below the nodes move right (creditor) to left (debtor)",
                           sep = "\n"))

ggsave("plots-aans/inheritance-arc-network-debit.png", width = 12, height = 8)

## Node size and labels as credit ##
ggraph(inheritance_arc, layout = "linear") + 
  geom_edge_arc(aes(edge_alpha = l)) + 
  scale_edge_alpha(labels = scales::dollar_format(prefix = "£")) + 
  geom_node_point(aes(size = credit.l, color = color), alpha = 0.9) + 
  geom_node_text(aes(label = label_credit),
                 nudge_y = 2, nudge_x = 2, angle = 45) + 
  scale_size_continuous(range = c(0.8, 10), labels = scales::dollar_format(prefix = "£")) + 
  labs(size = "Total credit",
       edge_alpha = "Transactions",
       color = "Heirs",
       title = "Estate of Jan della Faille de Oude, 1582–1594") + 
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 4), order = 1)) + 
  theme(legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12)) +
  ggtitle("Subgraph of the inheritance of the heirs of Jan de Oude",
          subtitle = paste("Labels show accounts from which the heirs received over £5,000",
                           "Lines above the nodes move left (creditor) to right (debtor)",
                           "Lines below the nodes move right (creditor) to left (debtor)",
                           sep = "\n"))

ggsave("plots-aans/inheritance-arc-network-credit.png", width = 12, height = 8)
