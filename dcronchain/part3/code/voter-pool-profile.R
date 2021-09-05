library(RPostgreSQL)
library(dplyr)
library(dbplyr)
library(reshape2)
library(ggplot2)
library(zoo)
library(scales)
library(ggpubr)
library(jsonlite)



pdrv = dbDriver("PostgreSQL")
pcon = dbConnect(pdrv,host="",dbname="dcrdata",user="dcrdata",pass="")


addresses = tbl(rcon, "addresses")

proposals = tbl(pcon, "proposals")
eligible_proposals = tbl(pcon, "eligible_proposals")
exchange_addresses = tbl(pcon, "exchange_addresses2")
taints_by_address_hop = tbl(pcon, "taints_by_address_hop2")
coinbasetx = tbl(rcon, "transactions") %>% filter(type_extra == 'coinbase') %>% collect()

source("clustering-functions.R")
source("pi-voting-functions.R")
targetblock = 485000
targetblock = 525000

#blockheight is used to narrow down the tickets initially
blockheight = targetblock-85000



tickets = tbl(rcon, "tickets") %>%
  filter(is_mainchain == TRUE) %>%
  collect()

proposal_votes = tbl(pcon, "proposal_votes")

#left join tickets to ticket_blocks

ticket_blocks = tbl(rcon, "ticket_blocks") %>% collect()

tickets.l = tickets %>%
  left_join(ticket_blocks, by = "tx_hash", copy=TRUE) %>%
  collect()


#remove tickets that don't have "lottery_height" because these are early tickets have not been run through the script to fetch their lottery_height
tickets.l = tickets.l[!is.na(tickets.l$lottery_height),]


tickets.l$end_height = tickets.l$lottery_height

tickets.l$end_height[tickets.l$lottery_height == 0] = tickets.l$expiration_height[tickets.l$lottery_height == 0] 

tickets.rel = tickets.l %>%
  filter(maturity_height < targetblock & end_height >= targetblock)


#for each ticket, left join to cluster addresses to get cluster
cluster_addresses = tbl(pcon, "cluster_addresses_voters3")


tickets.cluster = tickets.rel %>%
  left_join(cluster_addresses, by = c("stakesubmission_address"="address"),copy=TRUE) %>%
  collect()

tickets.cluster.g.test = tickets.cluster %>%
  group_by(cluster) %>%
  summarise(rows =n())
sum(tickets.cluster.g.test$rows)

#need to fix a few tickets that are matching >1 cluster

tickets.cluster.dupes = tickets.cluster %>%
  group_by(tx_hash) %>%
  summarize(rows = n()) %>%
  filter(rows > 1)

#where rows > 1, the address came up in more than one cluster, expected as different start points can discover some new addresses for a cluster, so the clusters should be merged then the problem will disappear

ticket.cluster.problemaddresses = tickets.cluster %>%
  semi_join(tickets.cluster.dupes, by = "tx_hash") %>%
  group_by(stakesubmission_address, cluster) %>%
    summarize(rows = n())

#can fix by removing rows where the cluster is not equal to the address

ticket.cluster.problemaddresses.getrid = ticket.cluster.problemaddresses %>%
  filter(stakesubmission_address == cluster) %>%
  collect()

ticket.cluster.nodupes = tickets.cluster %>%
  anti_join(ticket.cluster.problemaddresses.getrid, by = c("cluster"),copy=TRUE) %>%
  collect()

ticket.cluster.nodupes.g = ticket.cluster.nodupes %>%
  group_by(cluster) %>%
  summarise(tickets = n()) %>%
  collect()

unclustered.tickets = ticket.cluster.nodupes  %>%
  filter(is.na(cluster) )


#ticket.cluster.nodupes is now working, but there might be something dodgy about the below method
test = tickets.cluster %>%
     semi_join(tickets.cluster.dupes, by = "tx_hash")


ticket.cluster.nodupes= tickets.cluster %>%
  anti_join(test, by = c("tx_hash"),copy=TRUE) %>%
  collect()


ticket_mix = tbl(oldpcon, "ticket_mix")

#what's the mix status of inputs?
rel.ticket.mix = ticket.cluster.nodupes %>% 
  left_join(ticket_mix, by = c("tx_hash"="ticket"),copy=TRUE) %>%
  collect()


table(rel.ticket.mix$mix_ins)

rel.ticket.mix$split = FALSE
rel.ticket.mix$split[rel.ticket.mix$num_inputs == 2] = TRUE

rel.ticket.mix$mixed = FALSE
rel.ticket.mix$mixed[rel.ticket.mix$mix_ins > 0] = TRUE


#rel.ticket.mixed.cluster.g is what the count of distinct entities is based on
rel.ticket.mixed.cluster.g = rel.ticket.mix %>%
  group_by(cluster ) %>%
  summarise(rows = n())

single.use.addresses = rel.ticket.mixed.cluster.g  %>%
  filter(rows == 1)


rel.mixed = rel.ticket.mix %>%
  filter(mixed == TRUE)

#how many mixed input tickets
nrow(rel.mixed)

politeia.votes = get.politeia.votes(rel.mixed)
proposals = tbl(pcon, "proposals")
eligible_proposals = tbl(pcon, "eligible_proposals")



plot.politeia.votes.titles(politeia.votes, titletext = paste("Politeia voting of mixed tickets live in block ", targetblock, sep=""), threshold = 0.05, filename = paste("mixed-ticket-voting-", targetblock, sep=""))


#how many pi votes/eligible?
nrow(politeia.votes)


#have to go into the function to pull out the eligibility data

proposal.action.g = proposal.action %>%
  group_by(title, choice) %>%
  summarise(votes = sum(votes))

sum(proposal.action.g$votes[proposal.action.g$choice=="Abstain"])/sum(proposal.action.g$votes)


#how many differebt clusters represented?

nrow(ticket.cluster.nodupes.g)
nrow(ticket.cluster.nodupes.g[ticket.cluster.nodupes.g$tickets==1,])

#bar chart of cluster tickets held, excluding the 1 ticket holders
ticket.clusters.no.singles = ticket.cluster.nodupes.g[ticket.cluster.nodupes.g$tickets>1,]

p.tickethist = ggplot(ticket.clusters.no.singles)+
  aes(x=tickets)+
  geom_histogram(binwidth = 1, fill="blue")+
  labs(title=paste("Tickets held by clusters (excluding single ticket addresses), block ", targetblock), x = "Tickets held by cluster", y = "Number of Clusters")
  


p.ticketsheld.hist = ggplot(ticket.clusters.no.singles)+
  aes(x = tickets)+
  #scale_y_continuous()+
  #scale_y_log10(label=comma)+
  scale_y_continuous(labels=comma_format(accuracy=1), trans="log1p", breaks=c(1,10,100,1000,10000))+
  scale_x_continuous(breaks = c(1, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000))+
  geom_histogram(binwidth=1)+
  labs(x = "Tickets held by cluster", y = "Number of Clusters", title = paste("Tickets held by clusters (excluding single ticket addresses). Block: ", targetblock))
#stat_bin(geom="text", size=3.5, colour = "#41bf53" , aes(label=..count.., y=0.8*(..count..)), binwidth = 5)
  
ggsave(paste("tickets-per-cluster-block-", targetblock, ".png", sep=""), width = 8, height = 4.5)


cluster_ticket_owners_1 = ticket.cluster.nodupes %>%
  group_by(cluster) %>%
  summarize(tickets_held = n()) %>%
  collect()

sum(cluster_ticket_owners_1$tickets_held)


cluster_ticket_owners_100 = cluster_ticket_owners_1 %>%
  filter(tickets_held >= 100)

for(adr in cluster_ticket_owners_100$cluster){
  print(adr)
  address.list = cluster_addresses %>%
    filter(cluster == adr) %>%
    collect()
  
  tickets.df = get.tickets.block(address.list, targetblock=485000)
  
  politeia.votes = get.politeia.votes(tickets.df)
  
  if(nrow(politeia.votes)>0){
    plot.politeia.votes.titles(politeia.votes,titletext=paste(substr(adr, 0,5),"-standard-cluster-tickets", sep=""), filename = paste(substr(adr,0,5), "-cluster-voting", sep=""))  
    
  }
  
  
}


tickets.df = get.tickets(address.list)

politeia.votes = get.politeia.votes(tickets.df)

if(nrow(politeia.votes)>0){
  plot.politeia.votes.titles(politeia.votes,titletext=paste(substr(adr, 0,5),"-standard-cluster-tickets", sep=""), filename = paste(substr(adr,0,5), "-cluster-voting", sep=""))  
  
}



cluster_stats_exp_voters = tbl(pcon, "cluster_stats_voters3") %>% distinct() %>% collect()

block485 = rel.ticket.mix
block485.stats = block485 %>%
  left_join(cluster_stats_exp_voters, by = "cluster", copy=TRUE) %>%
  distinct(tx_hash, .keep_all = TRUE) %>%
  collect()


block485.g = block485.stats %>%
  group_by(cluster) %>%
  summarize(tickets_held = n()) %>%
  collect()

block485.clusters = block485.g %>%
  filter(tickets_held > 1)

block485.clusters$block = 485000

#run the early part again to get the block525 tickets
block525 = rel.ticket.mix
block525.stats = block525 %>%
  left_join(cluster_stats_exp_voters, by = "cluster", copy=TRUE) %>%
  distinct(tx_hash, .keep_all = TRUE) %>%
  collect()

block525.g = block525.stats %>%
  group_by(cluster) %>%
  summarize(tickets_held = n()) %>%
  collect()

block525.clusters = block525.g %>%
  filter(tickets_held > 1)

block525.clusters$block = 525000

cluster.persistence = block485.clusters %>%
  left_join(block525.clusters, by = "cluster")


cluster.persistence$tickets_held.y[is.na(cluster.persistence$tickets_held.y)] = 0


p.persistence = ggplot(cluster.persistence)+
  aes(x = tickets_held.x, y = tickets_held.y2)+
  scale_y_log10(label=comma, breaks = c(0, 10, 100, 1000))+
  scale_x_log10(label=comma)+
  geom_smooth()+
  geom_point()+
  labs(x="Tickets held in block 525,000", y = "Tickets held in block 485,000")+
  geom_abline(intercept = 0, slope = 1, colour = "red")

ggsave("cluster-tickets-2blocks.png", width = 8, height = 4.5)






block485.l = block485.stats %>%
  left_join()


block485.g = block485.stats %>%
  group_by(cluster) %>%
  summarize(tickets_held = n()) %>%
  collect()

block485.clusters = block485.g %>%
  filter(tickets_held > 1)


cluster_stat_targets = ticket.clusters.no.singles %>%
  anti_join(cluster_stats_exp_voters, by = "cluster") %>%
  distinct()



#run the clustering stats nographs code on these targets


cluster_ticket_owners_1.stats = cluster_ticket_owners_1 %>%
  left_join(cluster_stats_exp_voters, by = "cluster", copy=TRUE) %>%
  collect()


sum(cluster_ticket_owners_1.stats$tickets_held)



p.ticketsheld.hist = ggplot(cluster_ticket_owners_1.stats)+
  aes(x = tickets_held)+
  #scale_y_continuous()+
  #scale_y_log10(label=comma)+
  scale_y_continuous(labels=comma_format(accuracy=1), trans="log1p", breaks=c(1,10,100,1000,10000))+
  scale_x_continuous(breaks = c(1, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000))+
  geom_histogram(binwidth=5)+
  labs(x = "Tickets held by cluster", y = "Number of Clusters", title = paste("Block: ", targetblock))
  #stat_bin(geom="text", size=3.5, colour = "#41bf53" , aes(label=..count.., y=0.8*(..count..)), binwidth = 5)
  
ggsave(paste("tickets-per-cluster-block-", targetblock, ".png", sep=""), width = 8, height = 4.5)

#clusters with >=100 tickets

cluster_ticket_owners_1.stats.f100 = cluster_ticket_owners_1.stats %>%
  filter(tickets_held>=100)

nrow(cluster_ticket_owners_1.stats.f100)

sum(cluster_ticket_owners_1.stats.f100$tickets_held)


clusterstats = cluster_ticket_owners_1.stats %>%
  distinct(cluster, .keep_all=TRUE)

#try a tickets_held - tickets scatterplot
p.ticketscatter = ggplot(clusterstats)+
  aes(x = tickets, y = tickets_held)+
  geom_point()+
  scale_x_continuous(trans="log10", label = comma)+
  scale_y_continuous(trans="log10")+
  labs(x="Tickets held historically", y = "Tickets held in block 525,000")+
  geom_smooth()

ggsave("current-historical-tickets.png", width = 8, height = 4.5)

cor(clusterstats$tickets_held, clusterstats$tickets, use="complete.obs")


clusterstats$participation = clusterstats$proposal_votes/clusterstats$eligible_proposals
clusterstats$participation[clusterstats$participation > 1] = 1
clusterstats$participation = clusterstats$participation*100

p.agescatter = ggplot(clusterstats)+
  aes(x=first_tx, y = eligible_proposals, colour = participation)+
  geom_point()+
  scale_colour_gradient(low="#2970ff", high="#41bf53")+
  scale_y_continuous(trans="log10",label = comma)+
  labs(x = "First Transaction Date for Cluster", y = "Number of Politeia proposals cluster could have voted on", colour = "Voted %")

ggsave("cluster-age-participation.png", width = 8, height = 4.5)


clusterstats$source_pow = clusterstats$pow_hop1+clusterstats$pow_hop2+clusterstats$pow_hop0
clusterstats$source_airdrop = clusterstats$airdrop_hop1+clusterstats$airdrop_hop2
clusterstats$source_treasury = clusterstats$treasury_hop1+clusterstats$treasury_hop2
clusterstats$source_founders = clusterstats$founders_hop1+clusterstats$founders_hop2


clusterstats.m = clusterstats %>%
  mutate(mainsource = 
           case_when(source_pow > source_airdrop & source_pow > source_treasury & source_pow > source_founders ~ "PoW",
                     source_airdrop > source_pow & source_airdrop > source_treasury & source_pow > source_founders ~ "Airdrop",
                     source_treasury > source_pow & source_treasury > source_airdrop & source_treasury > source_founders ~ "Treasury",
                     source_founders > source_pow & source_founders > source_treasury & source_founders > source_airdrop ~ "Founders")
         )
clusterstats.m$mainsource[is.na(clusterstats.m$mainsource)] = "Unknown"

p.agescatter = ggplot(clusterstats.m)+
  aes(x=first_tx, y = eligible_proposals, colour = participation)+
  geom_point()+
  scale_colour_gradient(low="#2970ff", high="#41bf53")+
  scale_y_continuous(trans="log10",label = comma)+
  labs(x = "First Transaction Date for Cluster", y = "Number of Politeia proposals cluster could have voted on", colour = "Voted %")+
  facet_grid(rows="mainsource")

clusterstats.s = clusterstats.m[!is.na(clusterstats.m$airdrop_hop1),]

#tickets vs votes, colour by proportion of pi eligible proposals voted
clusterstats.pow = clusterstats.s[clusterstats.s$source_pow > 0,]
clusterstats.pow$powprop = clusterstats.pow$source_pow/clusterstats.pow$total_in
clusterstats.pow$powprop[clusterstats.pow$powprop > 1] = 1   

p.powprop = ggplot(clusterstats.pow)+
  aes(x = votes, y = eligible_proposals, colour = powprop, size = total_in )+
  geom_point()

p.agescatter.pow = ggplot(clusterstats.pow)+
  aes(x=first_tx, y = eligible_proposals, colour = participation)+
  geom_point()+
  scale_colour_gradient(low="#2970ff", high="#41bf53")+
  scale_y_continuous(trans="log10",label = comma)+
  labs(x = "First Transaction Date for Cluster", y = "Number of Politeia proposals cluster could have voted on", colour = "Voted %", title = "Clusters with PoW mining rewards")

ggsave("pow-cluster-age-participation.png", width = 8, height = 4.5)

clusterstats.airdrop = clusterstats.s[clusterstats.s$source_airdrop > 0,]
clusterstats.airdrop$airdropprop = clusterstats.airdrop$source_airdrop/clusterstats.airdrop$total_in
clusterstats.airdrop$airdropprop[clusterstats.airdrop$airdropprop > 1] = 1   

p.airdropprop = ggplot(clusterstats.airdrop)+
  aes(x = votes, y = eligible_proposals, colour = airdropprop, size = total_in )+
  geom_point()

p.agescatter.airdrop = ggplot(clusterstats.airdrop)+
  aes(x=first_tx, y = eligible_proposals, colour = participation)+
  geom_point()+
  scale_colour_gradient(low="#2970ff", high="#41bf53")+
  scale_y_continuous(trans="log10",label = comma)+
  labs(x = "First Transaction Date for Cluster", y = "Number of Politeia proposals cluster could have voted on", colour = "Voted %", title = "Clusters with airdrop mining rewards")

ggsave("airdrop-cluster-age-participation.png", width = 8, height = 4.5)

clusterstats.treasury = clusterstats.s[clusterstats.s$source_treasury > 0,]
clusterstats.treasury$treasuryprop = clusterstats.treasury$source_treasury/clusterstats.treasury$total_in
clusterstats.treasury$treasuryprop[clusterstats.treasury$treasuryprop > 1] = 1   

p.treasuryprop = ggplot(clusterstats.treasury)+
  aes(x = votes, y = eligible_proposals, colour = treasuryprop, size = total_in )+
  geom_point()

p.agescatter.treasury = ggplot(clusterstats.treasury)+
  aes(x=first_tx, y = eligible_proposals, colour = participation)+
  geom_point()+
  scale_colour_gradient(low="#2970ff", high="#41bf53")+
  scale_y_continuous(trans="log10",label = comma)+
  labs(x = "First Transaction Date for Cluster", y = "Number of Politeia proposals cluster could have voted on", colour = "Voted %", title = "Clusters with treasury mining rewards")

ggsave("treasury-cluster-age-participation.png", width = 8, height = 4.5)


#ad hoc investigation


cluster = "DsS5p..."

relclusters = c("DsS5p...", "Dscq...", "DsSWTH...", "Dcrh8p...")

for(adr in relclusters){
  print(adr)
  address.list = cluster_addresses %>%
    filter(cluster == adr) %>%
    collect()
  
  tickets.df = get.tickets(address.list)
  
  politeia.votes = get.politeia.votes(tickets.df)
  
  if(nrow(politeia.votes)>0){
    plot.politeia.votes.titles(politeia.votes,titletext=paste(substr(adr, 0,5),"-standard-cluster-tickets", sep=""), filename = paste(substr(adr,0,5), "-cluster-voting", sep=""))  
    
  }
  
  
  analysis_cluster_voters(adr)
}



