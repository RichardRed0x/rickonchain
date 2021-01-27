library(RPostgreSQL)
library(dplyr)
library(dbplyr)
library(reshape2)
library(ggplot2)
library(zoo)
library(lubridate)
library(scales)
library(ggraph)
library(igraph)


transactions = tbl(pcon, "transactions")
addresses = tbl(pcon, "addresses")

source("clustering-functions.R")

exchange_addresses = tbl(pcon, "exchange_addresses2") %>%
  collect()

origin = "PoW"

node_keep_threshold = 1000
node_tx_keep_threhsold = 10
edge_keep_threshold = 10

pow.tx = transactions %>%
  filter(type_extra == "coinbase" & block_time > "2020-10-01 00:00:01" ) %>%
  collect()


miner.in = addresses %>%
  semi_join(pow.tx, by = c("tx_hash" ),copy=TRUE) %>%
  filter(is_funding == TRUE  & valid_mainchain == TRUE & value > 500000000) %>%
  collect()


nodes.0.all = miner.in %>%
  group_by(address) %>%
  summarise(incoming_dcr = sum(value)/100000000 ) %>%
  collect()
nodes.0.all$type = "Coinbase Receiver"

nodes.0.churn = nodes.0.all %>%
  filter(incoming_dcr < churn_threshold) %>%
  summarize(incoming_dcr = sum(incoming_dcr)) %>%
  collect()
#could fetch PoW tx for these

nodes.0.churn$label = "churn"

nodes.0.churn$type = "Coinbase Receiver"


nodes.0.adr.keep = nodes.0.all %>%
  filter(incoming_dcr >= churn_threshold) %>%
  collect()

miner.in.k = miner.in %>%
  semi_join(nodes.0.adr.keep, by = "address")

edges.1.adr.tx = miner.in.k %>%
  group_by(from=address, to=matching_tx_hash) %>%
  summarize(dcr = sum(value)/100000000, type = max(tx_type)) %>%
  collect()

nodes.1.tx = miner.in.k %>%
  group_by(label = matching_tx_hash) %>%
  summarize(incoming_dcr  = sum(value)/100000000, tx_type = max(tx_type)) %>%
  collect()

nodes.1.tx.keep = nodes.1.tx %>%
  filter(incoming_dcr > node_tx_keep_threshold) %>%
  collect()

sum(nodes.1.tx.keep$incoming_dcr)
sum(nodes.1.tx$incoming_dcr)


hop2.ins = addresses %>%
  semi_join(nodes.1.tx.keep, by = c( "tx_hash"= "label"), copy=TRUE) %>%
  filter(is_funding == TRUE) %>%
  collect()


nodes.2.adr.keep = hop2.ins %>%
  group_by(address) %>%
  summarise(incoming_dcr = sum(value)/100000000) %>%
  filter(incoming_dcr > node_keep_threshold) %>%
  collect()

nodes.2.tx.keep = hop2.ins %>%
  semi_join(nodes.2.adr.keep, by = "address") %>%
  group_by(label = matching_tx_hash) %>%
  summarize(incoming_dcr  = sum(value)/100000000, tx_type = max(tx_type)) %>%
  filter(incoming_dcr > node_tx_keep_threhsold) %>%
  collect()

edges.2.tx.adr = hop2.ins %>%
  semi_join(nodes.2.adr.keep, by = "address") %>%
  group_by(from = tx_hash, to=address) %>%
  summarize(dcr = sum(value)/100000000, type = max(tx_type)) %>%
  collect()

edges.2.adr.tx = hop2.ins %>%
  semi_join(nodes.2.tx.keep, by = c("matching_tx_hash" = "label"), copy=TRUE) %>%
  group_by(from = address, to=matching_tx_hash) %>%
  summarize(dcr = sum(value)/100000000, type = max(tx_type)) %>%
  collect()

hop3.ins = addresses %>%
  semi_join(nodes.2.tx.keep, by = c( "tx_hash"= "label"), copy=TRUE) %>%
  filter(is_funding == TRUE) %>%
  collect()

nodes.3.adr.keep = hop3.ins %>%
  group_by(address) %>%
  summarise(incoming_dcr = sum(value)/100000000) %>%
  filter(incoming_dcr > node_keep_threshold) %>%
  collect()

edges.3.tx.adr = hop3.ins %>%
  semi_join(nodes.3.adr.keep, by = "address") %>%
  group_by(from = tx_hash, to=address) %>%
  summarize(dcr = sum(value)/100000000, type = max(tx_type)) %>%
  collect()

nodes.3.tx.keep = hop3.ins %>%
  semi_join(nodes.3.adr.keep, by = "address") %>%
  group_by(label = matching_tx_hash) %>%
  summarize(incoming_dcr  = sum(value)/100000000, tx_type = max(tx_type)) %>%
  filter(incoming_dcr > node_tx_keep_threhsold) %>%
  collect()

edges.3.adr.tx = hop3.ins %>%
  semi_join(nodes.3.tx.keep, by = c("matching_tx_hash" = "label"), copy=TRUE) %>%
  group_by(from = address, to=matching_tx_hash) %>%
  summarize(dcr = sum(value)/100000000, type = max(tx_type)) %>%
  collect()


hop4.ins = addresses %>%
  semi_join(nodes.3.tx.keep, by = c( "tx_hash"= "label"), copy=TRUE) %>%
  filter(is_funding == TRUE) %>%
  collect()

nodes.4.adr.keep = hop4.ins %>%
  group_by(address) %>%
  summarise(incoming_dcr = sum(value)/100000000) %>%
  filter(incoming_dcr > node_keep_threshold) %>%
  collect()

edges.4.tx.adr = hop4.ins %>%
  semi_join(nodes.4.adr.keep, by = "address") %>%
  group_by(from = tx_hash, to=address) %>%
  summarize(dcr = sum(value)/100000000, type = max(tx_type)) %>%
  collect()


nodes.4.tx.keep = hop4.ins %>%
  semi_join(nodes.4.adr.keep, by = "address") %>%
  group_by(label = matching_tx_hash) %>%
  summarize(incoming_dcr  = sum(value)/100000000, tx_type = max(tx_type)) %>%
  filter(incoming_dcr > node_tx_keep_threhsold) %>%
  collect()

edges.4.adr.tx = hop4.ins %>%
  semi_join(nodes.4.tx.keep, by = c("matching_tx_hash" = "label"), copy=TRUE) %>%
  group_by(from = address, to=matching_tx_hash) %>%
  summarize(dcr = sum(value)/100000000, type = max(tx_type)) %>%
  collect()


hop5.ins = addresses %>%
  semi_join(nodes.4.tx.keep, by = c( "tx_hash"= "label"), copy=TRUE) %>%
  filter(is_funding == TRUE) %>%
  collect()

nodes.5.adr.keep = hop5.ins %>%
  group_by(address) %>%
  summarise(incoming_dcr = sum(value)/100000000) %>%
  filter(incoming_dcr > node_keep_threshold) %>%
  collect()

edges.5.tx.adr = hop5.ins %>%
  semi_join(nodes.5.adr.keep, by = "address") %>%
  group_by(from = tx_hash, to=address) %>%
  summarize(dcr = sum(value)/100000000, type = max(tx_type)) %>%
  collect()

nodes.adr = bind_rows(nodes.0.adr.keep, nodes.2.adr.keep, nodes.3.adr.keep, nodes.4.adr.keep, nodes.5.adr.keep) %>%
  group_by(address) %>%
  summarize(incoming_dcr = sum(incoming_dcr), type = max(type))


nodes.adr$type[is.na(nodes.adr$type) ] = "Address"

nodes.adr.exchange = left_join(nodes.adr, exchange_addresses, by = "address", copy=TRUE) %>% collect()
nodes.adr.exchange$exchange[is.na(nodes.adr.exchange$exchange)] = nodes.adr.exchange$type.x[is.na(nodes.adr.exchange$exchange)]


nodes.adr = nodes.adr.exchange %>%
  select(label = address, incoming_dcr, type = exchange)


nodes.tx = bind_rows(nodes.1.tx.keep, nodes.2.tx.keep, nodes.3.tx.keep, nodes.4.tx.keep)
nodes.tx$label[nodes.tx$label == ""] = "Unspent"

nodes.tx = nodes.tx %>%
  group_by(label) %>%
  summarize(incoming_dcr = sum(incoming_dcr), tx_type = max(tx_type))

nodes.tx$type = "Transaction"
nodes.tx$type[nodes.tx$tx_type == 1] = "Ticket"
nodes.tx$type[nodes.tx$tx_type == 2] = "Vote"
nodes.tx$type[nodes.tx$tx_type == 3] = "Revoke"

nodes.tx = nodes.tx %>%
  select(names(nodes.tx), -tx_type)

nodes.all = bind_rows(nodes.adr, nodes.tx)

edges = bind_rows(edges.1.adr.tx, edges.2.tx.adr, edges.2.adr.tx, edges.3.tx.adr, edges.3.adr.tx, edges.4.tx.adr, edges.5.tx.adr)


edges$to[edges$to == ""] = "Unspent"

edges.g = edges %>%
  group_by(from, to) %>%
  summarize(dcr = sum(dcr), tx_type = max(type))

edges.g$tx_type[edges.g$tx_type == 0] = "Regular Tx"
edges.g$tx_type[edges.g$tx_type == 1] = "Ticket Tx"
edges.g$tx_type[edges.g$tx_type == 2] = "Vote Tx"
edges.g$tx_type[edges.g$tx_type == 3] = "Revoke Tx"


edges.g.s = edges.g %>%
  semi_join(nodes.all, by = c("from" = "label")) %>%
  semi_join(nodes.all, by = c("to" = "label")) %>%
  collect()
                              
net =  graph_from_data_frame(edges.g.s, vertices=nodes.all, directed = TRUE)

l.net = layout_as_tree(net, V(net)$type=="Coinbase Receiver")

g.net = ggraph(net,layout=l.net)+
  geom_edge_diagonal(aes(color = factor(tx_type),width = dcr),
                     arrow = arrow(length = unit(2, 'mm')), 
                     end_cap = circle(1.5, 'mm'))+
  scale_edge_width(range = c(0.2, 1.5))+
  geom_node_point( aes(size=incoming_dcr, shape = factor(type), color = factor(type)))+
  theme_void()+
  labs(title = "Decred PoW Reward flows - 1st October 2020 to 9th January 2021")+
  scale_edge_color_manual(values = c("#69D3F5","#2ED6A1"))+
  scale_color_manual(values = c("#2971ff", "#ff0000", "#41bf53", "#596D81", "#2971ff", "#fd714b"))+
  labs(color = "Address/Tx Type", shape = "Address/Tx Type", size = "Incoming DCR", edge_color = "Transaction Type", edge_width = "Tx Size (DCR)")

ggsave("PoW-Rewards-Flow-Oct2020-Jan2021-4hops.png", width = 16, height = 9, path = "network")



#checking when there is a net mismatch
nodes.all = as.data.frame(nodes.all)
edges.g = as.data.frame(edges.g)

edges.g.a  = edges.g %>%
  anti_join(nodes.all, by = c("from"="label"),copy=TRUE)

edges.g.a  = edges.g.a %>%
  anti_join(nodes.all, by = c("to"="label"),copy=TRUE) %>%
  collect()


nodes.all.a = nodes.all %>%
  anti_join(edges.g, by = c("label"="from"), copy=TRUE)
nodes.all.a = nodes.all.a %>%
  anti_join(edges.g, by = c("label"="to"), copy=TRUE)
