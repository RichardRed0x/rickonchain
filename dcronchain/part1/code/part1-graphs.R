#this code generates all the graphs used in the part 1 report

library(RPostgreSQL)
library(dplyr)
library(dbplyr)
library(reshape2)
library(ggplot2)
library(zoo)
library(lubridate)
library(scales)
library(ineq)
library("ggpubr")



transactions = tbl(pcon, "transactions") 
addresses = tbl(pcon, "addresses")



#age of unspent DCR
unspentage = addresses %>%
  filter(matching_tx_hash == "" & is_funding == TRUE) %>% collect()

#relabel the types and identify premine types
unspentage$type = as.character(unspentage$tx_type)
unspentage$dcr = unspentage$value/100000000
unspentage$type[unspentage$block_time == "2016-02-08 18:02:15" & unspentage$value == 28263795424] = "Premine-airdrop"
unspentage$type[unspentage$block_time == "2016-02-08 18:02:15" & unspentage$dcr == 5000] = "Premine-founders"
unspentage$type[unspentage$type == 0] = "Regular tx"
unspentage$type[unspentage$type == 1] = "Ticket Bought"
unspentage$type[unspentage$type == 2] = "Ticket Voted"
unspentage$type[unspentage$type == 3] = "Ticket Revoked"

#value by monthly block_time bins 

unspentage$block_month = floor_date(unspentage$block_time, "month")
unspentage$dcr = unspentage$value/100000000

unspentage.month = unspentage %>%
  group_by(block_month) %>%
  summarise(dcr_sitting = sum(dcr), rows = n())

#plot DCR which stopped moving per month
p.plotunspent.month = ggplot(unspentage.month)+
  aes(x = block_month, y = dcr_sitting)+
  geom_bar(stat = "identity")+
  scale_y_continuous(label=comma)+
  labs(x = "Month in which DCR last moved", y = "DCR of this age")

ggsave("unspent-DCR-month.png", width = 8, height = 4.5)

sum(unspentage.month$dcr_sitting)

unspentage.type = unspentage %>%
  group_by(type) %>%
  summarise(dcr_sitting = sum(dcr), rows = n())


unspentage.month.type = unspentage %>%
  group_by(block_month, type) %>%
  summarise(dcr_sitting = sum(dcr), rows = n())

#plot unspent per month colour by type
p.plotunspent.month.type = ggplot(unspentage.month.type)+
  aes(x = block_month, y = dcr_sitting, fill = type)+
  geom_bar(stat = "identity")+
  scale_y_continuous(label=comma)+
  labs(x = "Month in which DCR last moved", y = "DCR of this age")
ggsave("unspent-DCR-type-month.png", width = 8, height = 4.5)


#distribution between addresses
unspent = addresses %>%
  filter(matching_tx_hash == "") %>%
  group_by(address) %>%
  summarize(rows = n(), sumvalue = sum(value)) %>%
  arrange(desc(sumvalue)) %>%
  mutate(cumsum = cumsum(sumvalue))%>%
  collect()

unspent$cumsumprop = unspent$cumsum/sum(unspent$sumvalue)

unspent$dcrvalue = unspent$sumvalue/100000000


#group unspent by address

#check month tx
transactions = tbl(pcon, "transactions") 

unspent = addresses %>%
  filter(matching_tx_hash == "") %>%
  group_by(address) %>%
  summarize(rows = n(), sumvalue = sum(value)) %>%
  arrange(desc(sumvalue)) %>%
  mutate(cumsum = cumsum(sumvalue))%>%
  collect()

unspent$cumsumprop = unspent$cumsum/sum(unspent$sumvalue)

unspent$dcrvalue = unspent$sumvalue/100000000

unspent.notreasury = unspent %>%
  filter(address != "Dcur2mcGjmENx4DhNqDctW5wJCVyT3Qeqkx")

#get the Gini coefficient 
ineq(unspent.notreasury$sumvalue, type = "Gini")

#remove small balance addresses
unspent.notreasury.notiny = unspent.notreasury %>%
  filter(sumvalue > 100000000)

#Gini is lower now
ineq(unspent.notreasury.notiny$sumvalue, type = "Gini")

unspent.notreasury$dcrvalue = unspent.notreasury$sumvalue/100000000

breaks = c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000)
labels = c("0.0001", "0.001", "0.01", "0.1", "1", "10", "100", "1000", "10K", "100K", "1 Mn")

unspent.notreasury$bin = cut(unspent.notreasury$dcrvalue, breaks = breaks, labels = labels) 
unspent.notreasury = unspent.notreasury%>% collect()


unspent.notreasury.bin.g = unspent.notreasury %>%
  group_by(bin) %>%
  summarize(dcrvalue = sum(sumvalue)/100000000, rows = n()) %>%
  collect()
  
p.unspentbins = ggplot(unspent.notreasury.bin.g)+
  aes(x = bin, y = dcrvalue)+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = comma)+
  labs(x = "Address Bin - Up to X DCR", y = "Value of DCR in addresses")+
  theme(legend.position = "none")

ggsave("unspent-DCR-bins.png", width = 8, height = 4.5)


p.unspentadr = ggplot(unspent.notreasury.bin.g)+
  aes(x = bin, y = rows)+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = comma)+
  labs(x = "Address Bin - Up to X DCR", y = "Number of addresses in bin")+
  theme(legend.position = "none")

ggsave("unspent-DCR-bins-addresses.png", width = 8, height = 4.5)


unspent.notreasury.bin.g$dcr = unspent.notreasury.bin.g$sumvalue/100000000

p.unspentbins = ggplot(unspent.notreasury.bin.g) +
  aes(x = bin, y = dcr)+
  geom_bar(stat = "identity")

p.unspentbins.rows = ggplot(unspent.notreasury.bin.g) +
  aes(x = bin, y = addresses)+
  geom_bar(stat = "identity")


#construct a dataset which is not cumulative but binned, DCR in addresses with <= X DCR


y = 10^(0:14)
x = 10^(-1:13)
upto = seq(1:length(y))


df = data.frame(y, upto, x)

#this loop is a hacky way to get the bins set up 
unspent.notreasury$sumdcr = unspent.notreasury$sumvalue/100000000
for(y in df$y){
  unspent.notreasury.cum = unspent.notreasury %>%
    filter(sumvalue <=y) %>%
    summarize(cumrows = n(), cumsum = sum(sumvalue))
  
  df$upto[df$y == y] = unspent.notreasury.cum$cumsum
  df$cumrows[df$y == y] = unspent.notreasury.cum$cumrows
  
}

df$uptodcr = df$upto/100000000
df$ydcr = df$y/100000000

df = df[df$cumrows > 0,]

p.balances.cumulative = ggplot(df)+
  aes(x = ydcr, y = uptodcr)+
  geom_line()+
  scale_x_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000), labels = c("0.0001", 0.001, 0.01, 0.1, 1, 10, 100, 1000, "10K", "100K", "1 Mn"))+
  scale_y_log10(labels = comma, breaks = c( 10, 100, 1000, 10000, 100000, 1000000, 10000000))+
  geom_point()+
  labs(x = "DCR in address, X", y = "Unspent DCR in addresses with <= X DCR")

ggsave("unspent-DCR-distribution-cumulative.png", width = 8, height = 4.5)


#tx_type

sql = "SELECT date_trunc('day', transactions.block_time) blockday, tx_type, count(tx_type) tx FROM transactions GROUP BY blockday, tx_type;"


#type of transactions per day
tx = dbGetQuery(pcon, sql)

tx$time = paste(tx$blockday, " 00:00:00", sep="")
tx$Day = as.POSIXct(tx$time)
tx$tx_type = as.factor(tx$tx_type)


p.txtypes = ggplot(tx)+
  aes(x = Day, y = rollmean(tx, 7, na.pad = TRUE), colour = tx_type)+
  geom_line()


tx$Type = ""
tx$Type[tx$tx_type == 0] = "Regular"
tx$Type[tx$tx_type == 1] = "Ticket"
tx$Type[tx$tx_type == 2] = "Vote"
tx$Type[tx$tx_type == 3] = "Revoke"

tx$Type = factor(tx$Type, levels=c('Regular','Ticket','Vote','Revoke'))


p.txtypes = ggplot(tx)+
  aes(x = Day, y = tx)+
  facet_grid(Type ~ .)+
  geom_line(size = 0.3)+
  labs(y = "Number of Transactions")
ggsave("transactions-per-day-by-type.png")

p.txtypes.smooth = ggplot(tx)+
  aes(x = Day, y = rollmean(tx, 7, na.pad = TRUE))+
  facet_grid(tx_type ~ .)+
  geom_line()+
  labs(y = "Number of Transactions")


ggsave("transactions-per-day-by-type-rollmean-7obs.png")


transactions = tbl(pcon, "transactions")


transactions.g = transactions %>%
  group_by(tx_type) %>%
  summarize(sumspent = sum(spent),  rows = n(), feespaid = sum(fees), size = sum(size))%>%
  collect()

transactions.g$prop = transactions.g$rows/sum(transactions.g$rows)

#different types of type 0

#vote-setup tx - start from all type 1 tx and take the input tx as vote-prep

type1 = transactions %>%
  filter(tx_type == 1)

setup = addresses %>%
  semi_join(type1, by = c("matching_tx_hash" = "tx_hash")) %>%
  filter(is_funding == TRUE) %>%
  collect()

#group by tx_hash
setup.g = setup %>%
  group_by(tx_hash) %>%
  summarize(sumvalue = sum(value))

nrow(setup.g)

type0 = transactions %>%
  filter(tx_type == 0)

type0.nostaking = type0 %>%
  anti_join(setup.g, by = "tx_hash", copy=TRUE) %>% collect()

#active addresses per day/month

type0.nostaking$Day = as.Date(type0.nostaking$block_time)


type0.nostaking.g = type0.nostaking %>%
  group_by(Day) %>%
    summarize(rows = n(), dcrspent = sum(spent)/100000000)

p.type0.nostaking.txcount = ggplot(type0.nostaking.g) +
  aes(x = Day, y =rows)+
  geom_line()+
  labs(X = "Date", y = "Number of daily transactions not related to staking")



ggsave("non-staking-related-transactions.png", width = 8, height = 4.5)






#PoW miners

all.outcomes = tbl(pcon, "address_taint_outcomes") %>%
  filter(origin == "PoW miners" & type != "small-change") %>% 
  collect()

all.outcomes = all.outcomes[order(all.outcomes$time),]

all.outcomes.g = all.outcomes %>%
  group_by(type, hop) %>%
  summarize(dcrspent = sum(taint_value)/100000000)

all.outcomes.c = all.outcomes %>%
  group_by(type) %>%
  mutate(csum = cumsum(taint_value))

all.outcomes.c$csumdcr = all.outcomes.c$csum/100000000

p.powspending = ggplot(all.outcomes.c) +
  aes(x = time, y = csumdcr, colour = type)+
  geom_line()+
  labs(x = "Outcome time", y = "Cumulative DCR spent", title = "How DCR paid to PoW miners was used (cumulative)")+
  scale_y_continuous(labels = comma)+
  geom_vline(xintercept = as.POSIXct("2018-01-01", size = .5))+
  geom_text(aes(x= as.POSIXct("2018-01-01"), label="ASICs available", y=1500000), colour="blue", angle=90, vjust = 1.2, size = 2.5)

ggsave("DCR-PoW-outcomes.png", width = 8, height = 4.5)


#Treasury

all.outcomes = tbl(pcon, "address_taint_outcomes") %>%
  filter(origin == "Treasury" & type != "small-change") %>% 
  collect()

all.outcomes = all.outcomes[order(all.outcomes$time),]

all.outcomes.g = all.outcomes %>%
  group_by(type) %>%
  summarize(dcrspent = sum(taint_value)/100000000)

all.outcomes.g$prop = all.outcomes.g$dcrspent/sum(all.outcomes.g$dcrspent)

all.outcomes.c = all.outcomes %>%
  group_by(type) %>%
  mutate(csum = cumsum(taint_value))

all.outcomes.c$csumdcr = all.outcomes.c$csum/100000000

p.powspending = ggplot(all.outcomes.c) +
  aes(x = time, y = csumdcr, colour = type)+
  geom_line()+
  labs(x = "Outcome time", y = "Cumulative DCR spent", title = "How DCR paid to contractors was used (cumulative)")+
  scale_y_continuous(labels = comma)

ggsave("DCR-Treasury-outcomes.png", width = 8, height = 4.5)




#PoW miner outcomes


all.outcomes = tbl(pcon, "address_taint_outcomes") %>% collect()

all.outcomes = all.outcomes[order(all.outcomes$time),]


all.outcomes.g = all.outcomes %>%
  group_by(type) %>%
  summarize(dcrspent = sum(taint_value)/100000000)

all.outcomes.g$prop = all.outcomes.g$dcrspent/sum(all.outcomes.g$dcrspent)


all.outcomes.c = all.outcomes %>%
  group_by(type) %>%
  mutate(csum = cumsum(taint_value))

all.outcomes.c$csumdcr = all.outcomes.c$csum/100000000

p.minerspending = ggplot(all.outcomes.c) +
  aes(x = time, y = csumdcr, colour = type)+
  geom_line()+
  labs(x = "Outcome time", y = "Cumulative DCR", title = "How DCR paid to PoW miners was used (cumulative)")+
  scale_y_continuous(labels = comma)+
  geom_vline(xintercept = as.POSIXct("2018-01-01", size = .5))+
  geom_text(aes(x= as.POSIXct("2018-01-01"), label="ASICs available", y=1500000), colour="blue", angle=90, vjust = 1.2, size = 2.5)

ggsave("DCR-miner-outcomes.png", width = 8, height = 4.5)

all.outcomes$dcr = all.outcomes$taint_value/100000000


p.minerspending.nocum = ggplot(all.outcomes) +
  aes(x = time, y = dcr, colour = type)+
  geom_line()+
  labs(x = "Outcome time", y = "DCR", title = "How DCR paid to PoW miners was used")+
  scale_y_continuous(labels = comma)+
  geom_vline(xintercept = as.POSIXct("2018-05-01"))


ggsave("DCR-miner-outcomes-notcumulative.png", width = 8, height = 4.5)



#update the tx that set up ticket buys
for(tx_hash in setup.g$tx_hash){
  sql = paste("UPDATE transactions SET type_extra = 'ticket-setup' WHERE tx_hash = '", tx_hash, "';", sep="")
  
  pcon %>% dbExecute(sql)
  
}


#mixing transactions


#differentiate VSP and solo tickets?
#see is_split in the tickets table


transactions = tbl(pcon, "transactions")

transactions$type_extra[transactions$type_extra == "premine"] = "regular"
##do a boxplot version with sd and mean

transactions.extra.g = transactions %>%
  group_by(type_extra) %>%
  summarize(tx = n(), size_mb = sum(size)/1048576, fees_dcr = sum(fees)/100000000, sent = sum(sent)/100000000) %>%
  collect()


transactions.extra.g$type = transactions.extra.g$type_extra
transactions.extra.g$type[transactions.extra.g$type_extra == "split-ticket"] = "ticket-vsp"
transactions.extra.g$type[transactions.extra.g$type_extra == "ticket-buy"] = "ticket-solo"


transactions.extra.g$costpertx = transactions.extra.g$fees_dcr/transactions.extra.g$tx

p.extratype.size = ggplot(transactions.extra.g)+
  aes(x = type, y = size_mb)+
  geom_bar(stat = "identity")+
  labs(y = "Cumulative size in Mb", x = "Transaction Type (Expanded)")+
  scale_y_continuous(label=comma)+
  labs(y = "size (mb)")

ggsave("transaction-type-size.png", width = 8, height = 4.5)


p.extratype.fees = ggplot(transactions.extra.g)+
  aes(x = type, y = fees_dcr)+
  geom_bar(stat = "identity")+
  scale_y_continuous(label=comma)+
  labs(y = "Cumulative fees paid (DCR)", x = "Transaction Type (Expanded)")+
  labs(y = "fees (DCR)")

ggsave("transaction-type-fees.png", width = 8, height = 4.5)

p.extratype.number = ggplot(transactions.extra.g)+
  aes(x = type, y = tx)+
  geom_bar(stat = "identity")+
  scale_y_continuous(label=comma)+
  labs(y = "Number of Transactions", x = "Transaction Type (Expanded)")+
  labs(y = "# tx")

ggsave("transaction-type-number.png", width = 8, height = 4.5)

arranged = ggarrange(p.extratype.number + rremove("x.text" )+ rremove("x.title") +  rremove("x.ticks"), 
                     p.extratype.size + rremove("x.text")+ rremove("x.title") +  rremove("x.ticks"), 
                     p.extratype.fees, 
                     ncol = 1, align = "v")

ggsave("tx-type-summary-combined.png", width = 8, height = 4.5)
