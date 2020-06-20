#the code in this file selects transactions and then follows the DCR sent in these transactions, by tracking the "taint" through 5 subsequent transactions or "hops".

library(RPostgreSQL)
library(dplyr)
library(dbplyr)
library(reshape2)
library(ggplot2)
library(zoo)
library(lubridate)
library(scales)


transactions = tbl(pcon, "transactions")
addresses = tbl(pcon, "addresses")
known_addresses = tbl(pcon, "known_addresses")

exchange_addresses = tbl(pcon, "known_addresses") %>%
  filter(type == "binance" | type == "bittrex" | type == "poloniex" | type == "kucoin" | type == "EXMO") %>%
  collect()


#first define the set of transactions to be taken as the inputs for tracking

origin = "Treasury"


treasury.tx = addresses %>%
  filter(address == "Dcur2mcGjmENx4DhNqDctW5wJCVyT3Qeqkx" & is_funding == FALSE)

#get the in side of these tx and the matching tx hash, filter all the change outputs
contractor.in = addresses %>%
  semi_join(treasury.tx, by = c("tx_hash" )) %>%
  filter(is_funding == TRUE & address != "Dcur2mcGjmENx4DhNqDctW5wJCVyT3Qeqkx" & valid_mainchain == TRUE)

#group by tx_hash
contractor.in.txg = contractor.in %>%
  group_by(tx_hash) %>%
  summarize(rows = n(), sumvalue = sum(value), taint = min(value), hop1_time = min(block_time))
#can already identify unspent

#isolate unspent tx
contractor.in.unspenttx = contractor.in %>%
  filter(matching_tx_hash == "") %>%
  collect()

#group unspent tx
contractor.in.unspenttx.g = contractor.in.unspenttx %>%
  group_by(tx_hash) %>%
  summarize(sum_value = sum(value), block_time = min(block_time)) 


#this captures everything coming out of the transactions
hop1.out = contractor.in %>%
  left_join(addresses, by = c( "tx_hash" = "matching_tx_hash", "address") ) %>%
  filter(is_funding.y == FALSE  & valid_mainchain.x == TRUE & valid_mainchain.y == TRUE)  

#relabel so they make sense
hop1.out = hop1.out %>%
  select(pool_address = address, coinbase_reward = tx_hash.x, reward_time = block_time.x, reward_value = value.x, hop1_tx = matching_tx_hash, hop1_tx_type = tx_type.y, hop1_value = value.y, hop1_mixed = mixed_broad.y, hop1_time = block_time.y) %>%
  collect()


#identify exchange related transactions now while the addresses are visible
hop1.out.exchange = hop1.out %>%
  semi_join(exchange_addresses, by = c("pool_address" = "address"),copy=TRUE)

hop1.out$exchanged = 0

hop1.out$exchanged[hop1.out$pool_address %in% exchange_addresses$address] = 1

#group by hop1_tx
hop1.out.txg = hop1.out %>%
  group_by( hop1_tx) %>%
  summarize(reward_time= min(reward_time), reward_value = min(reward_value), hop1_tx_type = max(hop1_tx_type), hop1_mixed = max(hop1_mixed), hop1_exchanged = max(exchanged), hop1_total = sum(hop1_value), hop1_time = min(hop1_time)) %>% 
  collect()

#multiply hop1_total by the taint_prop, which for coinbase_reward is always 1 so skipping it here

hop1.out.txg$taint = hop1.out.txg$hop1_total
hop1.out.txg$taint_prop = 1

#this is a hack for hop 1
hop1.taint = hop1.out.txg %>%
  select(hop1_tx, taint)

#sum the taint at the level of the hop1_tx

hop1.out.g = hop1.out.txg %>%
  group_by(hop1_tx) %>%
  select(hop1_tx, reward_time = min(reward_time), hop1_time = min(hop1_time), 
           hop1_tx_type = max(hop1_tx_type), hop1_mixed = max(hop1_mixed), hop1_exchanged = max(hop1_exchanged), 
           hop1_taint = sum(taint))


#get all the inputs these outputs created
hop1.in = addresses %>%
  semi_join(hop1.out.txg, by = c("tx_hash" = "hop1_tx"),copy=TRUE) %>%
  filter(is_funding == TRUE) %>%
  select(miner_address = address, hop1_tx = tx_hash, hop2_tx = matching_tx_hash, hop1_time = block_time, hop1_tx_type = tx_type, hop1_mixed = mixed_broad, hop1_value = value) %>%
  collect()

#identify exchanges
hop1.in$exchanged = 0

hop1.in$exchanged[hop1.in$miner_address %in% exchange_addresses$address] = 1

#group by hop1_tx
hop1.in.g = hop1.in %>%
  group_by(hop1_tx) %>%
  summarize(hop1_total = sum(hop1_value))

#join to get taint
hop1.in.g.l = hop1.in.g %>%
  left_join(hop1.taint)

hop1.in.g.l$hop1_taint_prop = hop1.in.g.l$taint/hop1.in.g.l$hop1_total

#the taint_prop can be over 1 if fees were paid (taint "disappears"), if I was being thorough/ambitious it could be tracked to the miners
hop1.in.g.l$hop1_taint_prop[hop1.in.g.l$hop1_taint_prop> 1] = 1

hop1.in.l = hop1.in %>%
  left_join(hop1.in.g.l) %>%
  select(names(hop1.in), hop1_taint_prop)

hop1.in.l$hop1_taint = hop1.in.l$hop1_value*hop1.in.l$hop1_taint_prop


hop1.unspent = hop1.in.l %>%
  filter(hop2_tx == "")


#identify all outcomes at this hop
hop1.conclusions = hop1.in.l %>%
  filter(hop2_tx == "" | hop1_tx_type > 0 | hop1_mixed > 0 | exchanged == 1)

#this form is good for taint but for the next level the taint needs to be grouped by hop2_tx and there should only be 1 of each hop2_tx

hop1.follow = hop1.in.l %>%
  anti_join(hop1.conclusions, by = "hop2_tx") 

hop1.follow.g = hop1.follow %>%
  group_by(hop1_tx, hop2_tx) %>%
  summarize(hop1_taint = sum(hop1_taint), hop1_time = min(hop1_time))


#start of hop 2

#get inputs
hop2.in = hop1.follow.g %>%
  left_join(addresses, by = c("hop2_tx" = "tx_hash"),copy=TRUE) %>%
  filter(is_funding == TRUE)

#relabel
hop2.in = hop2.in %>%
  select(hop1_tx, hop2_tx, hop3_tx = matching_tx_hash, hop2_address = address, hop2_value = value, hop2_tx_type = tx_type, hop2_time = block_time, hop2_mixed = mixed_broad)

#identify exchange-related tx
hop2.in$exchanged = 0
hop2.in$exchanged[hop2.in$hop2_address %in% exchange_addresses$address] = 1

#group by hop1 and hop2 to merge any repeated rows where multiple outputs moved between the same addresses
hop2.in.txg = hop2.in %>%
  group_by(hop1_tx, hop2_tx) %>%
  summarize(hop2_total = sum(hop2_value))


#join to get the taint
hop2.in.txg.l = hop2.in.txg %>%
  left_join(hop1.follow, by = c("hop1_tx", "hop2_tx")) %>%
  select(names(hop2.in.txg), hop1_taint)


hop2.in.txg.l$hop2_taintprop = hop2.in.txg.l$hop1_taint/hop2.in.txg.l$hop2_total
hop2.in.txg.l$hop2_taintprop[hop2.in.txg.l$hop2_taintprop > 1] = 1

hop2.in.txg.l$hop2_taint = hop2.in.txg.l$hop2_taintprop

#join for taintprop
hop2.in.l = hop2.in %>%
  left_join(hop2.in.txg.l, by = c("hop1_tx", "hop2_tx")) %>%
  select(names(hop2.in), hop2_taintprop)

hop2.in.l$hop2_taint = hop2.in.l$hop2_value*hop2.in.l$hop2_taintprop


hop2.conclusions = hop2.in.l %>%
  filter(hop3_tx == "" | hop2_tx_type > 0 | hop2_mixed > 0 | exchanged == 1 )


hop2.follow = hop2.in.l %>%
  anti_join(hop2.conclusions, by = "hop3_tx")

hop2.follow.g = hop2.follow %>%
  group_by(hop2_tx, hop3_tx) %>%
  summarize(hop2_taint = sum(hop2_taint), hop2_time = min(hop2_time))

#trim it down a bit
hop2.followtest = hop2.follow %>%
  filter(hop2_taint > 10000000)

sum(hop2.followtest$hop2_taint)/100000000

#next hop - code is copy/pasted with different numbers
#this could be turned into a function but it would become less legible and sometimes it is necessary to do more or less filtering out of small taint transactions to preserve memory, when dealing with large tracking sets

hop3.in = hop2.follow.g %>%
  left_join(addresses, by = c("hop3_tx" = "tx_hash"),copy=TRUE) %>%
  filter(is_funding == TRUE)

hop3.in = hop3.in %>%
  select(hop2_tx, hop3_tx, hop4_tx = matching_tx_hash, hop3_address = address, hop3_value = value, hop3_tx_type = tx_type, hop3_time = block_time, hop3_mixed = mixed_broad)

hop3.in$exchanged = 0
hop3.in$exchanged[hop3.in$hop3_address %in% exchange_addresses$address] = 1

#group by hop and hop3 to merge any repeated rows

hop3.in.txg = hop3.in %>%
  group_by(hop2_tx, hop3_tx) %>%
  summarize(hop3_total = sum(hop3_value))

#group the hop2_taint by hop3_tx


hop3.in.txg.l = hop3.in.txg %>%
  left_join(hop2.follow.g, by = c("hop2_tx", "hop3_tx")) %>%
  select(names(hop3.in.txg), hop2_taint)

sum(hop3.in.txg.l$hop2_taint)/100000000

hop3.in.txg.l$hop3_taintprop = hop3.in.txg.l$hop2_taint/hop3.in.txg.l$hop3_total
hop3.in.txg.l$hop3_taintprop[hop3.in.txg.l$hop3_taintprop > 1] = 1

hop3.in.txg.l$hop3_taint = hop3.in.txg.l$hop3_taintprop


sum(hop2.follow$hop2_taint)/100000000

sum(hop3.in.txg.l$hop2_taint)/100000000

#some objects won't be needed again and can be removed to free up space
rm(hop2.in.l)
rm(hop1.in.l)


hop3.in.l = hop3.in %>%
  left_join(hop3.in.txg.l, by = c("hop2_tx", "hop3_tx")) %>%
  select(names(hop3.in), hop3_taintprop)

hop3.in.l$hop3_taint = hop3.in.l$hop3_value*hop3.in.l$hop3_taintprop

sum(hop3.in.l$hop3_taint)/100000000

hop3.conclusions = hop3.in.l %>%
  filter(hop4_tx == "" | hop3_tx_type > 0 | hop3_mixed > 0 | exchanged == 1)

sum(hop3.conclusions$hop3_taint)/100000000

hop3.all.g = hop3.in.l %>%
  anti_join(hop3.conclusions, by = "hop4_tx") %>%
  group_by(hop3_tx, hop4_tx) %>%
  summarize(hop3_taint = sum(hop3_taint), hop3_time = min(hop3_time))

#reduce the size by cutting out some tiny rows
hop3.follow.g  = hop3.all.g %>%
  filter(hop3_taint > 10000000)

#-------------------------------------------------------------------------------- hop 4

hop4.in = hop3.follow.g %>%
  left_join(addresses, by = c("hop4_tx" = "tx_hash"),copy=TRUE) %>%
  filter(is_funding == TRUE)

hop4.in = hop4.in %>%
  select(hop3_tx, hop4_tx, hop5_tx = matching_tx_hash, hop4_address = address, hop4_value = value, hop4_tx_type = tx_type, hop4_time = block_time, hop4_mixed = mixed_broad)

hop4.in$exchanged = 0
hop4.in$exchanged[hop4.in$hop4_address %in% exchange_addresses$address] = 1

#group by hop and hop4 to merge any repeated rows

hop4.in.txg = hop4.in %>%
  group_by(hop3_tx, hop4_tx) %>%
  summarize(hop4_total = sum(hop4_value))

#group the hop3_taint by hop4_tx


hop4.in.txg.l = hop4.in.txg %>%
  left_join(hop3.follow.g, by = c("hop3_tx", "hop4_tx")) %>%
  select(names(hop4.in.txg), hop3_taint)



hop4.in.txg.l$hop4_taintprop = hop4.in.txg.l$hop3_taint/hop4.in.txg.l$hop4_total
hop4.in.txg.l$hop4_taintprop[hop4.in.txg.l$hop4_taintprop > 1] = 1



hop4.in.txg.l$hop4_taint = hop4.in.txg.l$hop4_taintprop


sum(hop3.follow.g$hop3_taint)/100000000

sum(hop4.in.txg.l$hop3_taint)/100000000

rm(hop3.in.l)

rm(hop1.in.l)


hop4.in.l = hop4.in %>%
  left_join(hop4.in.txg.l, by = c("hop3_tx", "hop4_tx")) %>%
  select(names(hop4.in), hop4_taintprop)

hop4.in.l$hop4_taint = hop4.in.l$hop4_value*hop4.in.l$hop4_taintprop

sum(hop4.in.l$hop4_taint)/100000000

hop4.conclusions = hop4.in.l %>%
  filter(hop5_tx == "" | hop4_tx_type > 0 | hop4_mixed > 0 | exchanged == 1)

sum(hop4.conclusions$hop4_taint)/100000000

hop4.all.g = hop4.in.l %>%
  anti_join(hop4.conclusions, by = "hop5_tx") %>%
  group_by(hop4_tx, hop5_tx) %>%
  summarize(hop4_taint = sum(hop4_taint), hop4_time = min(hop4_time))

#reduce the size by cutting out some tiny rows
hop4.follow.g  = hop4.all.g %>%
  filter(hop4_taint > 10000000)


#--------------------------------------------------------------------------------------------------------


hop5.in = hop4.follow.g %>%
  left_join(addresses, by = c("hop5_tx" = "tx_hash"),copy=TRUE) %>%
  filter(is_funding == TRUE)

hop5.in = hop5.in %>%
  select(hop4_tx, hop5_tx, hop6_tx = matching_tx_hash, hop5_address = address, hop5_value = value, hop5_tx_type = tx_type, hop5_time = block_time, hop5_mixed = mixed_broad)

hop5.in$exchanged = 0
hop5.in$exchanged[hop5.in$hop5_address %in% exchange_addresses$address] = 1

#group by hop and hop5 to merge any repeated rows

hop5.in.txg = hop5.in %>%
  group_by(hop4_tx, hop5_tx) %>%
  summarize(hop5_total = sum(hop5_value))

#group the hop4_taint by hop5_tx


hop5.in.txg.l = hop5.in.txg %>%
  left_join(hop4.follow.g, by = c("hop4_tx", "hop5_tx")) %>%
  select(names(hop5.in.txg), hop4_taint)



hop5.in.txg.l$hop5_taintprop = hop5.in.txg.l$hop4_taint/hop5.in.txg.l$hop5_total
hop5.in.txg.l$hop5_taintprop[hop5.in.txg.l$hop5_taintprop > 1] = 1



hop5.in.txg.l$hop5_taint = hop5.in.txg.l$hop5_taintprop


sum(hop4.follow.g$hop4_taint)/100000000

sum(hop5.in.txg.l$hop4_taint)/100000000


hop5.in.l = hop5.in %>%
  left_join(hop5.in.txg.l, by = c("hop4_tx", "hop5_tx")) %>%
  select(names(hop5.in), hop5_taintprop)

hop5.in.l$hop5_taint = hop5.in.l$hop5_value*hop5.in.l$hop5_taintprop

sum(hop5.in.l$hop5_taint)/100000000

hop5.conclusions = hop5.in.l %>%
  filter(hop6_tx == "" | hop5_tx_type > 0 | hop5_mixed > 0 | exchanged == 1)

sum(hop5.conclusions$hop5_taint)/100000000

hop5.follow.g = hop5.in.l %>%
  anti_join(hop5.conclusions, by = "hop6_tx") %>%
  group_by(hop5_tx, hop6_tx) %>%
  summarize(hop5_taint = sum(hop5_taint), hop5_time = min(hop5_time))

# tracking is over now, time to put the pieces together into a data-set of outcomes

outcome.unspent = contractor.in.unspenttx  %>%
  filter(matching_tx_hash == "") %>%
  select(time = "block_time", "tx" = "tx_hash",  taint_value = "value") %>%
  collect()

outcome.unspent$type = "unspent"
outcome.unspent$hop = 0
outcome.unspent$origin = "contractor miners"

outcome.exc0 = hop1.out.exchange %>%
  select(time = "hop1_time", "tx" = "hop1_tx",  taint_value = "hop1_value") %>%
  collect()

outcome.exc0$type = "exchanged"
outcome.exc0$hop = 0
outcome.exc0$origin = "contractor miners"

outcome.exc1 = hop1.conclusions %>%
  filter(exchanged == 1) %>%
  group_by( hop1_tx) %>%
  summarize(time = min(hop1_time), tx = min(hop1_tx),  taint_value = sum(hop1_taint)) %>%
  collect()

outcome.exc1$type = "exchanged"
  outcome.exc1$hop = 1
  outcome.exc1$origin = "contractor miners"

  
outcome.mix1 = hop1.conclusions %>%
  filter(hop1_mixed > 0 & hop1_tx_type == 0) %>%
  group_by( hop1_tx) %>%
  summarize(time = min(hop1_time), tx = min(hop1_tx),  taint_value = sum(hop1_taint)) %>%
  collect()

outcome.mix1$type = "mixed"
outcome.mix1$hop = 1

sum(outcome.mix1$taint_value)/100000000

outcome.mixvote1 = hop1.conclusions %>%
  filter(hop1_mixed > 0 & hop1_tx_type > 0) %>%
  group_by( hop1_tx) %>%
  summarize(time = min(hop1_time), tx = min(hop1_tx),  taint_value = sum(hop1_taint)) %>%
  collect()

outcome.mixvote1$type = "mixed-ticket"
outcome.mixvote1$hop = 1

sum(outcome.mixvote1$taint_value)/100000000

outcome.vote1 = hop1.conclusions %>%
  filter(hop1_mixed == 0 & hop1_tx_type > 0) %>%
  group_by( hop1_tx) %>%
  summarize(time = min(hop1_time), tx = min(hop1_tx),  taint_value = sum(hop1_taint)) %>%
  collect()

outcome.vote1$type = "ticket"
outcome.vote1$hop = 1
outcome.vote1$origin = "contractor miners"


outcome.exc2 = hop2.conclusions %>%
  filter(exchanged == 1) %>%
  group_by( hop2_tx) %>%
  summarize(time = min(hop2_time), tx = min(hop2_tx),  taint_value = sum(hop2_taint)) %>%
  collect()

outcome.exc2$type = "exchanged"
outcome.exc2$hop = 2
outcome.exc2$origin = "contractor miners"


outcome.mix2 = hop2.conclusions %>%
  filter(hop2_mixed > 0 & hop2_tx_type == 0) %>%
  group_by( hop2_tx) %>%
  summarize(time = min(hop2_time), tx = min(hop2_tx),  taint_value = sum(hop2_taint)) %>%
  collect()

outcome.mix2$type = "mixed"
outcome.mix2$hop = 2

sum(outcome.mix2$taint_value)/100000000

outcome.mixvote2 = hop2.conclusions %>%
  filter(hop2_mixed > 0 & hop2_tx_type > 0) %>%
  group_by( hop2_tx) %>%
  summarize(time = min(hop2_time), tx = min(hop2_tx),  taint_value = sum(hop2_taint)) %>%
  collect()

outcome.mixvote2$type = "mixed-ticket"
outcome.mixvote2$hop = 2

sum(outcome.mixvote2$taint_value)/100000000

outcome.vote2 = hop2.conclusions %>%
  filter(hop2_mixed == 0 & hop2_tx_type > 0) %>%
  group_by( hop2_tx) %>%
  summarize(time = min(hop2_time), tx = min(hop2_tx),  taint_value = sum(hop2_taint)) %>%
  collect()

outcome.vote2$type = "ticket"
outcome.vote2$hop = 2
outcome.vote2$origin = "contractor miners"

outcome.exc3 = hop3.conclusions %>%
  filter(exchanged == 1) %>%
  group_by( hop3_tx) %>%
  summarize(time = min(hop3_time), tx = min(hop3_tx),  taint_value = sum(hop3_taint)) %>%
  collect()

outcome.exc3$type = "exchanged"
outcome.exc3$hop = 3
outcome.exc3$origin = "contractor miners"


outcome.mix3 = hop3.conclusions %>%
  filter(hop3_mixed > 0 & hop3_tx_type == 0) %>%
  group_by( hop3_tx) %>%
  summarize(time = min(hop3_time), tx = min(hop3_tx),  taint_value = sum(hop3_taint)) %>%
  collect()

outcome.mix3$type = "mixed"
outcome.mix3$hop = 3
outcome.mix3$origin = "contractor miners"


outcome.mixvote3 = hop3.conclusions %>%
  filter(hop3_mixed > 0 & hop3_tx_type > 0) %>%
  group_by( hop3_tx) %>%
  summarize(time = min(hop3_time), tx = min(hop3_tx),  taint_value = sum(hop3_taint)) %>%
  collect()

outcome.mixvote3$type = "mixed-ticket"
outcome.mixvote3$hop = 3


outcome.vote3 = hop3.conclusions %>%
  filter(hop3_mixed == 0 & hop3_tx_type > 0) %>%
  group_by( hop3_tx) %>%
  summarize(time = min(hop3_time), tx = min(hop3_tx),  taint_value = sum(hop3_taint)) %>%
  collect()

outcome.vote3$type = "ticket"
outcome.vote3$hop = 3
outcome.vote3$origin = "contractor miners"

outcome.unspent3 = hop3.conclusions %>%
  filter(hop4_tx == "" & hop3_tx_type == 0 & hop3_mixed == 0 & exchanged == 0) %>%
  group_by( hop3_tx) %>%
  summarize(time = min(hop3_time), tx = min(hop3_tx),  taint_value = sum(hop3_taint)) %>%
  collect()

outcome.unspent3$type = "unspent"
outcome.unspent3$hop = 3
outcome.unspent3$origin = "contractor miners"

outcome.exc4 = hop4.conclusions %>%
  filter(exchanged == 1) %>%
  group_by( hop4_tx) %>%
  summarize(time = min(hop4_time), tx = min(hop4_tx),  taint_value = sum(hop4_taint)) %>%
  collect()

outcome.exc4$type = "exchanged"
outcome.exc4$hop = 4
outcome.exc4$origin = "contractor miners"
#dbWriteTable(pcon, "address_taint_outcomes", outcome.exc4, append = TRUE, row.names = FALSE)
sum(outcome.exc4$taint_value)/100000000

outcome.mix4 = hop4.conclusions %>%
  filter(hop4_mixed > 0 & hop4_tx_type == 0) %>%
  group_by( hop4_tx) %>%
  summarize(time = min(hop4_time), tx = min(hop4_tx),  taint_value = sum(hop4_taint)) %>%
  collect()

outcome.mix4$type = "mixed"
outcome.mix4$hop = 4
outcome.mix4$origin = "contractor miners"


outcome.mixvote4 = hop4.conclusions %>%
  filter(hop4_mixed > 0 & hop4_tx_type > 0) %>%
  group_by( hop4_tx) %>%
  summarize(time = min(hop4_time), tx = min(hop4_tx),  taint_value = sum(hop4_taint)) %>%
  collect()

outcome.mixvote4$type = "mixed-ticket"
outcome.mixvote4$hop = 4


outcome.vote4 = hop4.conclusions %>%
  filter(hop4_mixed == 0 & hop4_tx_type > 0) %>%
  group_by( hop4_tx) %>%
  summarize(time = min(hop4_time), tx = min(hop4_tx),  taint_value = sum(hop4_taint)) %>%
  collect()

outcome.vote4$type = "ticket"
outcome.vote4$hop = 4
outcome.vote4$origin = "contractor miners"


outcome.unspent4 = hop4.conclusions %>%
  filter(hop5_tx == "" & hop4_tx_type == 0 & hop4_mixed == 0 & exchanged == 0) %>%
  group_by( hop4_tx) %>%
  summarize(time = min(hop4_time), tx = min(hop4_tx),  taint_value = sum(hop4_taint)) %>%
  collect()

outcome.unspent4$type = "unspent"
outcome.unspent4$hop = 4
outcome.unspent4$origin = "contractor miners"

outcome.exc5 = hop5.conclusions %>%
  filter(exchanged == 1) %>%
  group_by( hop5_tx) %>%
  summarize(time = min(hop5_time), tx = min(hop5_tx),  taint_value = sum(hop5_taint)) %>%
  collect()

outcome.exc5$type = "exchanged"
outcome.exc5$hop = 5
outcome.exc5$origin = "contractor miners"

outcome.mix5 = hop5.conclusions %>%
  filter(hop5_mixed > 0 & hop5_tx_type == 0) %>%
  group_by( hop5_tx) %>%
  summarize(time = min(hop5_time), tx = min(hop5_tx),  taint_value = sum(hop5_taint)) %>%
  collect()

outcome.mix5$type = "mixed"
outcome.mix5$hop = 5
outcome.mix5$origin = "contractor miners"


outcome.mixvote5 = hop5.conclusions %>%
  filter(hop5_mixed > 0 & hop5_tx_type > 0) %>%
  group_by( hop5_tx) %>%
  summarize(time = min(hop5_time), tx = min(hop5_tx),  taint_value = sum(hop5_taint)) %>%
  collect()

outcome.mixvote5$type = "mixed-ticket"
outcome.mixvote5$hop = 5


outcome.vote5 = hop5.conclusions %>%
  filter(hop5_mixed == 0 & hop5_tx_type > 0) %>%
  group_by( hop5_tx) %>%
  summarize(time = min(hop5_time), tx = min(hop5_tx),  taint_value = sum(hop5_taint)) %>%
  collect()

outcome.vote5$type = "ticket"
outcome.vote5$hop = 5
outcome.vote5$origin = "contractor miners"

outcome.unspent5 = hop5.conclusions %>%
  filter(hop6_tx == "" & hop5_tx_type == 0 & hop5_mixed == 0 & exchanged == 0) %>%
  group_by( hop5_tx) %>%
  summarize(time = min(hop5_time), tx = min(hop5_tx),  taint_value = sum(hop5_taint)) %>%
  collect()

outcome.unspent5$type = "unspent"
outcome.unspent5$hop = 5
outcome.unspent5$origin = "contractor miners"


outcome.unspent2 = hop2.conclusions %>%
  filter(hop3_tx == "" & hop2_tx_type == 0 & hop2_mixed == 0 & exchanged == 0) %>%
  group_by( hop2_tx) %>%
  summarize(time = min(hop2_time), tx = min(hop2_tx),  taint_value = sum(hop2_taint)) %>%
  collect()

outcome.unspent2$type = "unspent"
outcome.unspent2$hop = 2
outcome.unspent2$origin = "contractor miners"


outcome.unspent1 = hop1.conclusions %>%
  filter(hop2_tx == "" & hop1_tx_type == 0 & hop1_mixed == 0 & exchanged == 0) %>%
  group_by( hop1_tx) %>%
  summarize(time = min(hop1_time), tx = min(hop1_tx),  taint_value = sum(hop1_taint)) %>%
  collect()

outcome.unspent1$type = "unspent"
outcome.unspent1$hop = 1
outcome.unspent1$origin = "contractor miners"


#need to catch the "change" exclusions too and call these as unknown outcomes


outcome.change3 = hop3.all.g %>%
  anti_join(hop3.follow.g, by = "hop3_tx") %>%
  group_by( hop3_tx) %>%
  summarize(time = min(hop3_time), tx = min(hop3_tx),  taint_value = sum(hop3_taint)) %>%
  collect()
  
outcome.change3$type = "small-change"
outcome.change3$hop = 3

outcome.change3$origin = "contractor miners"


outcome.change4 = hop4.all.g %>%
  anti_join(hop4.follow.g, by = "hop4_tx") %>%
  group_by( hop4_tx) %>%
  summarize(time = min(hop4_time), tx = min(hop4_tx),  taint_value = sum(hop4_taint)) %>%
  collect()

outcome.change4$type = "small-change"
outcome.change4$hop = 4

outcome.change4$origin = "contractor miners"

sum(outcome.change4$taint_value)/100000000


outcome.unknown = hop5.follow.g %>%
  group_by(hop6_tx)%>%
  summarize(taint_value = sum(hop5_taint), time = min(hop5_time)) %>%
  collect()


outcome.unknown$type = "unknown"
outcome.unknown$hop = 6

outcome.unknown$origin = "contractor miners"


all.outcomes = bind_rows(outcome.unspent, outcome.exc1)
all.outcomes = bind_rows(all.outcomes, outcome.exc0)
all.outcomes = bind_rows(all.outcomes, outcome.mix1)
all.outcomes = bind_rows(all.outcomes, outcome.mixvote1)
all.outcomes = bind_rows(all.outcomes, outcome.vote1)
all.outcomes = bind_rows(all.outcomes, outcome.mix2)
all.outcomes = bind_rows(all.outcomes, outcome.mixvote2)
all.outcomes = bind_rows(all.outcomes, outcome.vote2)
all.outcomes = bind_rows(all.outcomes, outcome.exc2)
all.outcomes = bind_rows(all.outcomes,outcome.mix3)
all.outcomes = bind_rows(all.outcomes,outcome.mixvote3)
all.outcomes = bind_rows(all.outcomes,outcome.vote3)
all.outcomes = bind_rows(all.outcomes,outcome.exc3)
all.outcomes = bind_rows(all.outcomes,outcome.unspent1)
all.outcomes = bind_rows(all.outcomes,outcome.unspent2)
all.outcomes = bind_rows(all.outcomes,outcome.unspent3)
all.outcomes = bind_rows(all.outcomes,outcome.change3)
all.outcomes = bind_rows(all.outcomes,outcome.change4)
all.outcomes = bind_rows(all.outcomes,outcome.mix4)
all.outcomes = bind_rows(all.outcomes,outcome.mixvote4)
all.outcomes = bind_rows(all.outcomes,outcome.vote4)
all.outcomes = bind_rows(all.outcomes,outcome.exc4)
all.outcomes = bind_rows(all.outcomes,outcome.unspent4)
all.outcomes = bind_rows(all.outcomes,outcome.mix5)
all.outcomes = bind_rows(all.outcomes,outcome.mixvote5)
all.outcomes = bind_rows(all.outcomes,outcome.vote5)
all.outcomes = bind_rows(all.outcomes,outcome.exc5)
all.outcomes = bind_rows(all.outcomes,outcome.unspent5)
all.outcomes = bind_rows(all.outcomes,outcome.unknown)


#group by type and hop to explore
all.outcomes.g = all.outcomes %>%
  group_by(  type, hop) %>%
  summarize(taint_spent = sum(taint_value)/100000000)

#prune for the columns that will be stored
all.outcomes = all.outcomes %>%
  select(time, tx, taint_value, type, hop, origin)

#store for later use
dbWriteTable(pcon, "address_taint_outcomes", all.outcomes, append = TRUE, row.names = FALSE)

