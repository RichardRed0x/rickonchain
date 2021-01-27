

################################# FUNCTIONS
process.type.0 = function(type.0){
  matching.tickets = addresses %>%
    semi_join(type.0, by = c("tx_hash" = "matching_tx_hash"),copy=TRUE) %>%
    filter(is_funding == FALSE & tx_type == 1 & value > 100000000 & mixed_broad == 0 & valid_mainchain == TRUE) %>%
    collect()
  return(matching.tickets)
}


process.vote.ins = function(vote.ins){
  voting.addresses = addresses %>%
    semi_join(vote.ins, by = "tx_hash",copy=TRUE) %>%
    filter(is_funding == FALSE  & mixed_broad == 0 & valid_mainchain == TRUE) %>%
    collect()
  return(voting.addresses)
}



process.ticket.outs.getvotes =  function(ticket.outs){
  ticket.votes = addresses %>%
    semi_join(ticket.outs, by = "tx_hash", copy=TRUE) %>%
    filter(is_funding == TRUE   & mixed_broad == 0 & valid_mainchain == TRUE) %>%
    collect()
  return(ticket.votes)  
}   

process.ticket.outs.getinputs =  function(ticket.outs){
  ticket.inputs = addresses %>%
    semi_join(ticket.outs, by = c("tx_hash" = "matching_tx_hash"), copy=TRUE) %>%
    filter(is_funding == FALSE  & mixed_broad == 0 & valid_mainchain == TRUE) %>%
    collect()
  return(ticket.inputs)  
}   

process.ticket.inputs.getchange =  function(these.inputs){
  ticket.inputs.change = addresses %>%
    semi_join(these.inputs, by = c("tx_hash"), copy=TRUE) %>%
    filter(is_funding == TRUE  & mixed_broad == 0 & valid_mainchain == TRUE) %>%
    collect()
  return(ticket.inputs.change)  
}   



process.ticket.ins.getinputs = function(ticket.ins){
  ticket.outs = addresses %>%
    semi_join(ticket.ins, by = "tx_hash", copy=TRUE) %>%
    filter(is_funding == FALSE  & mixed_broad == 0 & valid_mainchain == TRUE) %>%
    collect()
  return(ticket.outs)  
}   

process.ticket.ins.getoutputs = function(ticket.ins){
  ticket.outputs = addresses %>%
    semi_join(ticket.ins, by = c( "tx_hash" = "matching_tx_hash"), copy=TRUE) %>%
    filter(is_funding == TRUE & value > 100000000  & mixed_broad == 0 & valid_mainchain == TRUE) %>%
    collect()
  return(ticket.outputs)  
}      



multi.inputs.address = function(adr){
  
  #get tx for address directly 
  targets.transactions = addresses %>%
    filter(address == adr) %>%
    filter(is_funding == FALSE & mixed_broad == 0) 
  
  #get the addresses, iterate this
  target.transactions.alladdresses = addresses %>%
    semi_join(targets.transactions, by = c("tx_hash")) %>%
    filter(is_funding == FALSE & mixed_broad == 0) %>%
    collect()
  if(nrow(target.transactions.alladdresses)>0){
  
  target.transactions.alladdresses.alltransactions = addresses %>%
    semi_join(target.transactions.alladdresses, by = c("address"),copy=TRUE) %>%
    filter(is_funding == FALSE & mixed_broad == 0) %>%
    collect()
  
  #this should have max addresses, but no harm to iterate more
  target.transactions.alladdresses.alltransactions.alladdresses = addresses %>%
    semi_join(target.transactions.alladdresses.alltransactions, by = "tx_hash",copy=TRUE) %>%
    filter(is_funding == FALSE & mixed_broad == 0) %>%
    collect()
  
  target.transactions.alladdresses.alltransactions.alladdresses.alltransactions = addresses %>%
    semi_join(target.transactions.alladdresses.alltransactions.alladdresses , by = "address",copy=TRUE) %>%
    filter(is_funding == FALSE & mixed_broad == 0) %>%
    collect()
  
  target.transactions.alladdresses.alltransactions.alladdresses.alltransactions.alladdresses = addresses %>%
    semi_join(target.transactions.alladdresses.alltransactions.alladdresses.alltransactions , by = "address",copy=TRUE) %>%
    filter(is_funding == FALSE & mixed_broad == 0) %>%
    collect()
  
  target.transactions.alladdresses.g = target.transactions.alladdresses %>%
    group_by(address) %>%
    summarize(rows = n())
  
  target.transactions.alladdresses.alltransactions.alladdresses.g = target.transactions.alladdresses.alltransactions.alladdresses %>%
    group_by(address) %>%
    summarize(rows = n())
  
  target.transactions.alladdresses.alltransactions.alladdresses.alltransactions.alladdresses.g = target.transactions.alladdresses.alltransactions.alladdresses.alltransactions.alladdresses %>%
    group_by(address) %>%
    summarize(rows = n())
  
  mi.cluster = addresses %>%
    semi_join(target.transactions.alladdresses.alltransactions.alladdresses.alltransactions.alladdresses.g, by = "address",copy=TRUE) %>%
    filter(is_funding == FALSE & mixed_broad == 0) %>%
    collect()
  
  return(mi.cluster)
  }else{return(NULL)}
}




multi.inputs.target = function(address.list){
  
  targets.transactions = addresses %>%
    semi_join(address.list, by = c("address"), copy=TRUE) %>%
    filter(is_funding == FALSE & mixed_broad == 0) %>%
    collect()

  #get the addresses, iterate this
  target.transactions.alladdresses = addresses %>%
    semi_join(targets.transactions, by = c("tx_hash"), copy=TRUE) %>%
    filter(is_funding == FALSE & mixed_broad == 0) %>%
    collect()
  
  target.transactions.alladdresses.alltransactions = addresses %>%
    semi_join(target.transactions.alladdresses, by = c("address"),copy=TRUE) %>%
    filter(is_funding == FALSE & mixed_broad == 0) %>%
    collect()
  
  old.adr = semi_join(target.transactions.alladdresses.alltransactions, clustered_addresses.d)
  if(nrow(old.adr)> 0){
    return(target.transactions.alladdresses.alltransactions)
  }else{
  
  #this should have max addresses, but no harm to iterate more
  target.transactions.alladdresses.alltransactions.alladdresses = addresses %>%
    semi_join(target.transactions.alladdresses.alltransactions, by = "tx_hash",copy=TRUE) %>%
    filter(is_funding == FALSE & mixed_broad == 0) %>%
    collect()
  
  old.adr = semi_join(target.transactions.alladdresses.alltransactions.alladdresses, clustered_addresses.d)
  if(nrow(old.adr)> 0){
    return(target.transactions.alladdresses.alltransactions.alladdresses)
  }else{
  
  target.transactions.alladdresses.alltransactions.alladdresses.alltransactions = addresses %>%
    semi_join(target.transactions.alladdresses.alltransactions.alladdresses , by = "address",copy=TRUE) %>%
    filter(is_funding == FALSE & mixed_broad == 0) %>%
    collect()
  
  old.adr = semi_join(target.transactions.alladdresses.alltransactions.alladdresses.alltransactions, clustered_addresses.d)
  if(nrow(old.adr)> 0){
    return(target.transactions.alladdresses.alltransactions.alladdresses.alltransactions)
  }else{
    
  
  target.transactions.alladdresses.alltransactions.alladdresses.alltransactions.alladdresses = addresses %>%
    semi_join(target.transactions.alladdresses.alltransactions.alladdresses.alltransactions , by = "address",copy=TRUE) %>%
    filter(is_funding == FALSE & mixed_broad == 0) %>%
    collect()
  
  target.transactions.alladdresses.g = target.transactions.alladdresses %>%
    group_by(address) %>%
    summarise(rows = n())
  
  target.transactions.alladdresses.alltransactions.alladdresses.g = target.transactions.alladdresses.alltransactions.alladdresses %>%
    group_by(address) %>%
    summarize(rows = n())
  
  target.transactions.alladdresses.alltransactions.alladdresses.alltransactions.alladdresses.g = target.transactions.alladdresses.alltransactions.alladdresses.alltransactions.alladdresses %>%
    group_by(address) %>%
    summarize(rows = n())
  
  mi.cluster = addresses %>%
    semi_join(target.transactions.alladdresses.alltransactions.alladdresses.alltransactions.alladdresses.g, by = "address",copy=TRUE) %>%
    filter(is_funding == FALSE & mixed_broad == 0) %>%
    collect()
  
  return(mi.cluster)
  
  }
  }
  }
}



multi.inputs.target.basic = function(adr.tab){
  
  targets.transactions = addresses %>%
    semi_join(adr.tab, by = c("address"), copy=TRUE) %>%
    filter(is_funding == FALSE & mixed_broad == 0) %>%
    collect()
  
  if(nrow(targets.transactions) == 0){
    print(paste("all mixed"))
    return(NULL)
  }
  
  
  #get the addresses, iterate this
  target.transactions.alladdresses = addresses %>%
    semi_join(targets.transactions, by = c("tx_hash"), copy=TRUE) %>%
    filter(is_funding == FALSE & mixed_broad == 0) %>%
    collect()
  
  target.transactions.alladdresses.alltransactions = addresses %>%
    semi_join(target.transactions.alladdresses, by = c("address"),copy=TRUE) %>%
    filter(is_funding == FALSE & mixed_broad == 0) %>%
    collect()
  
    
    #this should have max addresses, but no harm to iterate more
    target.transactions.alladdresses.alltransactions.alladdresses = addresses %>%
      semi_join(target.transactions.alladdresses.alltransactions, by = "tx_hash",copy=TRUE) %>%
      filter(is_funding == FALSE & mixed_broad == 0) %>%
      collect()

      
      target.transactions.alladdresses.alltransactions.alladdresses.alltransactions = addresses %>%
        semi_join(target.transactions.alladdresses.alltransactions.alladdresses , by = "address",copy=TRUE) %>%
        filter(is_funding == FALSE & mixed_broad == 0) %>%
        collect()
        
        target.transactions.alladdresses.alltransactions.alladdresses.alltransactions.alladdresses = addresses %>%
          semi_join(target.transactions.alladdresses.alltransactions.alladdresses.alltransactions , by = "address",copy=TRUE) %>%
          filter(is_funding == FALSE & mixed_broad == 0) %>%
          collect()
        
        target.transactions.alladdresses.g = target.transactions.alladdresses %>%
          group_by(address) %>%
          summarise(rows = n())
        
        target.transactions.alladdresses.alltransactions.alladdresses.g = target.transactions.alladdresses.alltransactions.alladdresses %>%
          group_by(address) %>%
          summarize(rows = n())
        
        target.transactions.alladdresses.alltransactions.alladdresses.alltransactions.alladdresses.g = target.transactions.alladdresses.alltransactions.alladdresses.alltransactions.alladdresses %>%
          group_by(address) %>%
          summarize(rows = n())
        
        mi.cluster = addresses %>%
          semi_join(target.transactions.alladdresses.alltransactions.alladdresses.alltransactions.alladdresses.g, by = "address",copy=TRUE) %>%
          filter(is_funding == FALSE & mixed_broad == 0) %>%
          collect()
        
        return(mi.cluster)
        
      }





got.any.type1s = function(df){
  df.0 = df %>%
    filter(tx_type == 0)
  if(nrow(df.0)> 0){
  hop1.in = addresses %>%
    semi_join(df.0, by = "tx_hash",copy=TRUE) %>%
    filter(is_funding == TRUE & mixed_broad == 0) %>%
    collect()
  
  hop1.out.tickets = addresses %>%
    semi_join(hop1.in, by = c("tx_hash" = "matching_tx_hash"),copy=TRUE) %>%
    filter(is_funding == TRUE & tx_type == 1 & mixed_broad == 0) %>%
    collect()
  if(nrow(hop1.out.tickets)>0){
  return(hop1.out.tickets)
    }else{
    print("no hop 1 tickets here")
    return(NULL)
  }
}
}
get.addresses.type1 = function(step2.1){
  step2.1.1 = addresses %>%
    semi_join(step2.1, by = c("tx_hash"),copy=TRUE) %>%
    filter(is_funding == TRUE) %>%
    collect()
  
  return(step2.1.1)
}

get.addresses.fromtype1 = function(step2.3){
  goingto = addresses %>%
    semi_join(step2.3, by = c("tx_hash"="matching_tx_hash"),copy=TRUE) %>%
    filter(is_funding == TRUE) %>%
    collect()
  
  return(goingto)
}


get.notfunding = function(step){
  notfunding = addresses %>%
    semi_join(step, by = "tx_hash",copy=TRUE) %>%
    filter(is_funding == FALSE & value > 10000000) %>%
  collect()
  return(notfunding)
}
  

get.addresses.from.type1 = function(stepdata){
  reversed = addresses %>%
    semi_join(stepdata, by = c("matching_tx_hash" = "tx_hash"),copy=TRUE) %>%
    filter(is_funding == FALSE) %>%
    collect()
  if(nrow(reversed)>0){
  
  reversed.switch = addresses %>%
    semi_join(reversed, by = c( "tx_hash"),copy=TRUE) %>%
    filter(is_funding == TRUE & value > 10000000) %>%
    collect()
  
  return(reversed.switch)}else{return(NULL)}
}


get.addresses.type2 = function(step4.1){
  step4.1.1 = addresses %>%
    semi_join(step4.1, by = c("tx_hash"),copy=TRUE) %>%
    filter(is_funding == TRUE & value > 10000000) %>%
    collect()
  
  return(step4.1.1)
}


remove.vsp.and.getvoting = function(step4.0){
  novsp = step4.0 %>% filter(value > 10000000)
  step4.0.1 = addresses %>%
    semi_join(novsp, by = c("matching_tx_hash" = "tx_hash"), copy=TRUE) %>%
    filter(is_funding == TRUE) %>%
    collect()
  
  novsp = step4.0 %>% filter(value > 10000000)
  
  
}



get.ins = function(address.list){
  cluster.ins =  semi_join(addresses, address.list, by = "address",copy=TRUE) %>%
    filter(is_funding == TRUE) %>%
    collect()
  
  cluster.outs =  semi_join(addresses, address.list, by = "address",copy=TRUE) %>%
    filter(is_funding == FALSE) %>%
    collect() 
  
  cluster.outs.back1 = addresses %>%
    semi_join(cluster.outs, by = c("matching_tx_hash"),copy=TRUE) %>%
    filter(is_funding == TRUE) %>%
    collect() 
  
  cluster.outs.back1.anti = cluster.outs.back1 %>%
    anti_join(address.list, by = "address", copy = TRUE)
  
  sum(cluster.outs.back1.anti$value)/100000000
  
}


graph.clusterbalance = function(address.list, clustername){
  
  step5 = addresses %>%
    semi_join(address.list, by= "address",copy=TRUE) %>%
    collect()
  
  step5.ins = step5 %>%
    filter(is_funding == TRUE) %>%
    collect()
  
  step5.outs = step5 %>%
    filter(is_funding == FALSE) %>%
    collect()
  
  step5.outs$value = step5.outs$value*-1 
  mixouts = step5.outs %>%
    filter(mixed_broad>0)
  
  
  step5.all = bind_rows(step5.ins, step5.outs)
  step5.all$dcrvalue = step5.all$value/100000000
  
  
  #to smooth this out, the ticket tx should only subtract the amount which is spent in fees
  step5.cumsum = step5.all %>%
    arrange(block_time) %>%
    mutate(current_balance = cumsum(value), dcr_balance = cumsum(dcrvalue)) %>%
    collect()
  
  step5.cumsum$block_time = as.POSIXct(step5.cumsum$block_time)
  
  step5.cumsum$dcr_balance = step5.cumsum$current_balance/100000000
  
  top.addresses = step5.all %>%
    group_by(address) %>%
    summarize(rows = n()) %>%
    arrange(desc(rows))
  
  topaddress = top.addresses$address[1]
  
  cluster = as.character(topaddress)
  
  p.exchangebalances = ggplot(step5.cumsum) +
    aes(x = block_time, y = dcr_balance)+
    labs(title = clustername)+
    geom_line() 

  ggsave(paste( clustername, "-balance.png", sep=""), width = 8, height = 4.5)
  
}
  

#cluster_address

cluster_address = function(adr, tracksource){
  print(adr)
  address = adr
  address.list = data.frame(address) 
  
  do.again = 1
  countdown = 15
  
  
  while(do.again > 0 & countdown > 0 ){
    address.list.saver = address.list
    
    
    multi1 = multi.inputs.target.basic(address.list)
    if(is.null(multi1)){
      target_addresses = target_addresses %>%
        anti_join(address.list)
      step6 = address.list 
      step6$cluster = adr
      step6$type = tracksource
      
      
      step6 = step6 %>%
        select(address, cluster) %>%
        distinct()
      do.again = 0
      #remove all rows that already have that cluster-address combo, to avoid duplicates building up
      dbWriteTable(pcon, "cluster_addresses_function", step6, append = TRUE, row.names = FALSE)
      
      
      print("saved to db, skipping to next adr")
      next()
    }
    multi1.new.adr = multi1 %>%
      anti_join(address.list,by = "address", copy=TRUE) %>%
      group_by(address) %>%
      summarize(rows = n()) %>%
      collect()
    if(nrow(multi1.new.adr)> 0){
      address.list = multi1.new.adr %>%
        select(address) %>%
        bind_rows(address.list)  
    }
    adr.tab = addresses %>%
      semi_join(address.list, by = "address", copy=TRUE) %>%
      filter(valid_mainchain == TRUE) %>%
      collect()
    
    ticket.ins = adr.tab %>%
      filter(is_funding == TRUE & tx_type == 1)
    if(nrow(ticket.ins)> 0){
      these.inputs = process.ticket.ins.getinputs(ticket.ins)
      these.inputs.new.adr = these.inputs %>%
        anti_join(address.list,by = "address", copy=TRUE) %>%
        group_by(address) %>%
        summarize(rows = n()) %>%
        collect()
      if(nrow( these.inputs.new.adr)> 0){
        address.list = these.inputs.new.adr %>%
          select(address) %>%
          bind_rows(address.list)   
      }
      these.outputs = process.ticket.ins.getoutputs(ticket.ins)
      if(nrow(these.outputs)> 0){
        these.outputs.new.adr = these.outputs %>%
          anti_join(address.list,by = "address", copy=TRUE) %>%
          group_by(address) %>%
          summarize(rows = n()) %>%
          collect()
        if(nrow(these.outputs.new.adr)> 0){
          address.list = these.outputs.new.adr %>%
            select(address) %>%
            bind_rows(address.list)      
        }
      }
    }
    
    ticket.outs = adr.tab %>%
      filter(is_funding == FALSE & tx_type == 1 & value > 100000000)
    if(nrow(ticket.outs)> 0){
      these.votes = process.ticket.outs.getvotes(ticket.outs)
      
      these.votes.new.adr = these.votes %>%
        anti_join(address.list,by = "address", copy=TRUE) %>%
        group_by(address) %>%
        summarize(rows = n()) %>%
        collect()
      if(nrow(these.votes.new.adr )> 0){
        address.list = these.votes.new.adr %>%
          select(address) %>%
          bind_rows(address.list)   
      }
      
     
    }
    vote.ins = adr.tab %>%
      filter(is_funding == TRUE & tx_type == 2 & value > 100000000)
    if(nrow(vote.ins)> 0){
      voting.addresses = process.vote.ins(vote.ins)
      voting.addresses.new.adr = voting.addresses %>%
        anti_join(address.list,by = "address", copy=TRUE) %>%
        group_by(address) %>%
        summarize(rows = n()) %>%
        collect()
      if(nrow(voting.addresses.new.adr)> 0){
        address.list = voting.addresses.new.adr %>%
          select(address) %>%
          bind_rows(address.list)  
      }
    }
    
    
    type.0 = adr.tab %>%
      filter(tx_type == 0 & is_funding == TRUE)
    if(nrow(type.0) > 0){
      type.0.to.tickets = process.type.0(type.0)
      if(exists("type1.to.tickets")){
        type.0.to.tickets.new.adr = type.0.to.tickets %>%
          anti_join(address.list,by = "address", copy=TRUE) %>%
          group_by(address) %>%
          summarize(rows = n()) %>%
          collect()
        if(nrow(type.0.to.tickets.new.adr)> 0){
          address.list = type.0.to.tickets.new.adr %>%
            select(address) %>%
            bind_rows(address.list)     
        }
      }
    }
    
    
    all.new.adr = multi1.new.adr
    multi1.new.adr = data.frame()
    if(exists("these.inputs.new.adr")){    all.new.adr = bind_rows(all.new.adr, these.inputs.new.adr)
    these.inputs.new.adr = data.frame()}
    if(exists("voting.addresses.new.adr")){all.new.adr = bind_rows(all.new.adr, voting.addresses.new.adr)
    voting.addresses.new.adr = data.frame()}
    if(exists("type.0.to.tickets.new.adr")){all.new.adr = bind_rows(all.new.adr, type.0.to.tickets.new.adr)
    type.0.to.tickets.new.adr = data.frame()}
    if(exists("these.outputs.new.adr")){all.new.adr = bind_rows(all.new.adr, these.outputs.new.adr)
    these.outputs.new.adr = data.frame()
    }
    if(exists("these.votes.new.adr")){all.new.adr = bind_rows(all.new.adr, these.votes.new.adr)
    these.votes.new.adr = data.frame()}
    if(exists("these.inputs.change.new.adr")){all.new.adr = bind_rows(all.new.adr, these.inputs.change.new.adr)
    these.inputs.change.new.adr = data.frame()}
    
    do.again = nrow(all.new.adr)
    countdown = countdown - 1
    print(paste("did a loop", adr, "new addresses:", do.again))
    
    treasury = all.new.adr %>%
      filter(address == "Dcur2mcGjmENx4DhNqDctW5wJCVyT3Qeqkx") %>%
      collect()
    
    if(nrow(treasury)>0){
      countdown = 0
      print("treasury hit")
    }
    
  }
  
  if(!is.null(multi1)){
    
    step6 = address.list 
    
    topaddress = adr.tab %>%
      group_by(address) %>%
      summarize(rows = n()) %>%
      arrange(desc(rows))
    
    
    step6$cluster = topaddress$address[1]
    step6$type = tracksource
    
    
    #remove all rows that already have that cluster-address combo, to avoid duplicates building up
    
    cluster_addresses_function = tbl(pcon, "cluster_addresses_function")
    

    dbWriteTable(pcon, "cluster_addresses_function", step6, append = TRUE, row.names = FALSE)
    
  }

  rm(adr.tab)
  rm(address.list)
  rm(multi1)
}

#cluster_addresses = tbl(pcon, "cluster_addresses_function")

analysis_cluster = function(cluster){
    adr = cluster
    c = cluster
    c.label = substr(c, 0, 6)
    c.adr = cluster_addresses %>%
      filter(cluster == c) %>%
      collect()
    adr = c
    c.label = substr(c, 0, 4)
    c.adr = cluster_addresses %>%
      filter(cluster == c) %>%
      collect()
    
    all.adr = semi_join(cluster_addresses,c.adr, by = "address", copy = TRUE) %>%
      collect()  
    
    all.adr.g = all.adr %>%
      group_by(cluster) %>%
      summarize(rows = n())
    
    all.cluster.adr = semi_join(cluster_addresses,all.adr.g, by = "cluster", copy = TRUE) %>%
      collect()  
    
    address.list = all.cluster.adr %>%
      group_by(address) %>%
      summarize(rows = n())
    
    
    
    step5 = addresses %>%
      semi_join(address.list, by= "address",copy=TRUE) %>%
      collect()
    
    step5.ins = step5 %>%
      filter(is_funding == TRUE) %>%
      collect()
    
    step5.outs = step5 %>%
      filter(is_funding == FALSE) %>%
      collect()
    
    step5.outs$value = step5.outs$value*-1 
    mixouts = step5.outs %>%
      filter(mixed_broad>0)
    
    
    step5.all = bind_rows(step5.ins, step5.outs)
    step5.all$dcrvalue = step5.all$value/100000000
    
    
    #to smooth this out, the ticket tx should only subtract the amount which is spent in fees
    step5.cumsum = step5.all %>%
      arrange(block_time) %>%
      mutate(current_balance = cumsum(value), dcr_balance = cumsum(dcrvalue)) %>%
      collect()
    
    step5.cumsum$block_time = as.POSIXct(step5.cumsum$block_time)
    
    step5.cumsum$dcr_balance = step5.cumsum$current_balance/100000000
    
    top.addresses = step5.all %>%
      group_by(address) %>%
      summarize(rows = n()) %>%
      arrange(desc(rows))
    
    topaddress = top.addresses$address[1]
    
    cluster = as.character(topaddress)
    
    p.exchangebalances = ggplot(step5.cumsum) +
      aes(x = block_time, y = dcr_balance)+
      labs(title = c.label)+
      geom_line() +
      scale_y_continuous(label=comma)+
      labs(x = "Block Time", y = "DCR Balance")
    
    ggsave(paste("balance-", c.label, ".png", sep=""), width = 8, height = 4.5, path = "balancecharts")
    
    
    
    addressrows = nrow(step5.all)
    clusterdf = data.frame(cluster, addressrows)
    
    #a few of these clusters have more votes than tickets, so going to trace back from the votes and ensure all tickets are covered
    
    step5.votes = step5.all %>%
      filter(tx_type == 2 & is_funding == TRUE) %>%
      group_by(tx_hash) %>%
      summarize(rows = n(), mintime = min(block_time), maxtime = max(block_time))
    
    clustervotes = step5.votes %>%
      summarize(rows = n())
    clusterdf$votes = clustervotes$rows  
    
    votes = tbl(pcon, "votes")
    
    step5.votes.l = step5.votes %>%
      left_join(votes, by = c("tx_hash"), copy=TRUE)
    
    #use this to calculate how much the cluster has earned from staking rewards
    clusterdf$staking_income = sum(step5.votes.l$vote_reward)
    
    
    agenda_votes = tbl(pcon, "agenda_votes")
    
    #this has more rows than step.votes.l because sometimes there is more than 1 vote concurrently
    cluster_agenda_votes =  step5.votes.l %>%
      left_join(agenda_votes, by = c("id"="votes_row_id"),copy=TRUE) %>%
      collect()
    if(nrow(cluster_agenda_votes)>0){
      cluster_agenda_votes$month = substr(cluster_agenda_votes$mintime, 0, 7)
      cluster_agenda_votes$month = as.POSIXct(paste(cluster_agenda_votes$month , "-01 00:00:01", sep=""))
      
      cluster_agenda_votes_eligible = cluster_agenda_votes[!is.na(cluster_agenda_votes$agenda_vote_choice),]
      
      agenda_votes_monthly = cluster_agenda_votes_eligible %>%
        group_by(month, agenda_vote_choice) %>%
        summarize(votes = n()) 
      
      agenda_votes_monthly$vote_choice = "Yes"
      agenda_votes_monthly$vote_choice[agenda_votes_monthly$agenda_vote_choice == 1] = "Abstain"
      agenda_votes_monthly$vote_choice[agenda_votes_monthly$agenda_vote_choice == 2] = "No"
      
      p.monthly.agenda = ggplot(agenda_votes_monthly)+
        aes(x = month, y = votes, fill = vote_choice)+
        geom_bar(stat= "identity")+
        scale_y_continuous(label=comma)+
        labs(x = "Month", y = "Value")
      
      
      
      n.agenda_eligible = nrow(cluster_agenda_votes[!is.na(cluster_agenda_votes$agenda_vote_choice),])
      n.agenda_yes = nrow(cluster_agenda_votes %>% filter(agenda_vote_choice == 1))
      n.agenda_no = nrow(cluster_agenda_votes %>% filter(agenda_vote_choice == 2))

      tickets = step5.all %>%
        filter(tx_type == 1 & is_funding == TRUE) 
      if(nrow(tickets)>0){
        
        
        tickets.notfunding = step5.all %>%
          filter(tx_type == 1 & is_funding == FALSE) 
        
        tickets.l = tickets %>%
          left_join(ticketstats.tbl, copy=TRUE, by=c("tx_hash"="ticket"))
        
        n.eligible_proposals = sum(tickets.l$eligible, na.rm = TRUE)
        n.proposal_votes = sum(tickets.l$votes, na.rm = TRUE)  
        n.yes_votes = sum(tickets.l$yes , na.rm = TRUE)
        n.no_votes = sum(tickets.l$no , na.rm = TRUE)
        n.contrary_votes = sum(tickets.l$contrary , na.rm = TRUE)
        n.contrary_score = sum(tickets.l$contrary_score , na.rm = TRUE)
        
        ticketdf = data.frame(cluster, n.eligible_proposals, n.proposal_votes, n.yes_votes, n.no_votes, n.contrary_votes, n.contrary_score, n.agenda_eligible, n.agenda_no, n.agenda_yes)
        put.in.vote.db = ticketdf %>%
          rename(eligible_proposals = n.eligible_proposals,proposal_votes=  n.proposal_votes, yes_votes= n.yes_votes, no_votes = n.no_votes,contrary_votes= n.contrary_votes,contrary_score = n.contrary_score , agenda_eligible= n.agenda_eligible, agenda_no= n.agenda_no, agenda_yes= n.agenda_yes)
      
        
        cluster_agenda_votes$month = substr(cluster_agenda_votes$mintime, 0, 7)
        cluster_agenda_votes$month = as.POSIXct(paste(cluster_agenda_votes$month , "-01 00:00:01", sep=""))
        
        cluster_agenda_votes_eligible = cluster_agenda_votes[!is.na(cluster_agenda_votes$agenda_vote_choice),]
        
        agenda_votes_monthly = cluster_agenda_votes_eligible %>%
          group_by(month, agenda_vote_choice) %>%
          summarize(votes = n()) 
        
        agenda_votes_monthly$vote_choice = "Yes"
        agenda_votes_monthly$vote_choice[agenda_votes_monthly$agenda_vote_choice == 1] = "Abstain"
        agenda_votes_monthly$vote_choice[agenda_votes_monthly$agenda_vote_choice == 2] = "No"
        
        p.monthly.agenda = ggplot(agenda_votes_monthly)+
          aes(x = month, y = votes, fill = vote_choice)+
          geom_bar(stat= "identity")+
          scale_y_continuous(label=comma)+
          labs(x = "Month", y = "Value", title = "On chain consensus rules voting")
        
        
        
        tickets.l$month = substr(tickets.l$block_time, 0, 7) 
        tickets.l$month = as.POSIXct(paste( tickets.l$month,  "-01 00:00:01", sep=""))
        
        tickets.l.s = tickets.l[!is.na(tickets.l$eligible),]
        
        tickets.l.g = tickets.l.s %>%
          group_by(month) %>%
          summarize(eligible_proposals = sum(eligible), votes_cast = sum(votes), contrary_votes = sum(contrary), contrary_score_sum = sum(contrary_score))
        
        m.tickets.lg = melt(tickets.l.g, id.vars = "month", measure.vars = c("votes_cast", "eligible_proposals", "contrary_votes"))
        
        p.proposalvotes = ggplot(m.tickets.lg)+
          aes(x = month, y = value, fill = variable)+
          geom_bar(stat = "identity", position = "dodge")+
          scale_y_continuous(label=comma)+
          labs(x = "Month", y = "Value", title = "Politeia Voting")
        
        arranged.plots = ggarrange(p.exchangebalances, p.proposalvotes, p.monthly.agenda, ncol = 1)
        ggsave(paste(c.label, "-plot.png", sep=""), arranged.plots,width = 8, height = 8, path = "augcharts")
        
      }
    }
    dcr_staked = step5.all %>%
      filter(tx_type == 1 & is_funding == TRUE) %>%
      summarize(staked = sum(value)/10000000)
    clusterdf$dcr_staked = dcr_staked$staked
    
    
    numtickets = step5.all %>%
      filter(tx_type == 1 & is_funding == TRUE) %>%
      summarize(rows = n())
    clusterdf$tickets = numtickets$rows
    
    
    clusterdf$first_tx = min(step5.all$block_time)
    
    clusterdf$last_tx = max(step5.all$block_time)
    clusterdf$maxbalance = max(step5.cumsum$dcr_balance)
    
    clusterdf$cluster_address_count = nrow(address.list)
    tx = step5.all %>%
      group_by(tx_hash) %>%
      summarize(rows = n())
    clusterdf$transactions = nrow(tx)

    
    step6 = address.list %>%
      select(address)
    step6$cluster = topaddress
    step6$type = "merged"
    
    
    step6.prep = step5.ins %>%
      group_by(tx_hash) %>%
      summarize(rows = n())
    
    step6.ext.all = addresses %>%
      semi_join(step6.prep, by = "tx_hash",copy=TRUE) %>%
      filter(is_funding == FALSE) %>%
      collect()
    
    if(nrow(step6.ext.all) > 0){
      step6.ext.all =  step6.ext.all %>%
        anti_join(address.list, by = "address", copy=TRUE) %>%
        collect()
      
      step6.ext.from.mixing = step6.ext.all %>%
        filter(mixed_broad > 0)
      
      step6.nomixing = step6.ext.all %>%
        filter(mixed_broad == 0)
      
      
      #remove rows that come from an address in the cluster
      if(nrow(step6.nomixing)> 0){
        
        #match against exchange and then group by tx to get the amount the exchange put into those tx
        #this is an estimate of the total DCR into the cluster
        sum(step6.nomixing$value)/100000000
        
        clusterdf$total_in = sum(step6.ext.all$value)/100000000
        
        
        #where did that incoming DCR come from?
        #from exchanges?
        
        #for all of this, step6.2 already knows how much came from these addresses to the cluster, which sets an upper limit on taint
        #use that
        
        step7.exchange = step6.nomixing %>% 
          inner_join(exchange_addresses, by = "address",copy=TRUE)%>%
          collect()
        
        step7.exchange.g = step7.exchange %>%
          group_by(tx_hash) %>%
          summarize(sumvalue = sum(value), exchange = max(type))
        
        step7.exchange.inputs = step5.ins %>%
          left_join(step7.exchange.g, by = "tx_hash", copy=TRUE)
        
        step7.exchange.inputs.g = step7.exchange.inputs %>%
          group_by(exchange) %>%
          summarize(dcr = sum(value)/100000000)
        
        step7.exchange.inputs.g = step7.exchange.inputs.g[!is.na(step7.exchange.inputs.g$exchange),]
        
        clusterdf$exchange_bittrex = 0
        clusterdf$exchange_binance = 0
        clusterdf$exchange_poloniex = 0
        clusterdf$exchange_other = 0
        
        if(length(step7.exchange.inputs.g$dcr[step7.exchange.inputs.g$exchange == "bittrex"])> 0){
          clusterdf$exchange_bittrex = step7.exchange.inputs.g$dcr[step7.exchange.inputs.g$exchange == "bittrex"]
        }
        if(length(step7.exchange.inputs.g$dcr[step7.exchange.inputs.g$exchange == "binance"])> 0){
          clusterdf$exchange_binance = step7.exchange.inputs.g$dcr[step7.exchange.inputs.g$exchange == "binance"]
        }
        if(length(step7.exchange.inputs.g$dcr[step7.exchange.inputs.g$exchange == "poloniex"])> 0){
          clusterdf$exchange_poloniex = step7.exchange.inputs.g$dcr[step7.exchange.inputs.g$exchange == "poloniex"]
        }
        
        if(length(step7.exchange.inputs.g$dcr[step7.exchange.inputs.g$exchange == "other"])> 0){
          clusterdf$exchange_other = step7.exchange.inputs.g$dcr[step7.exchange.inputs.g$exchange == "other"]
        }
        
        
        
        clusterdf$total_in = sum(step6.ext.all$value)/100000000
        
        #then check how much dcr that address sent to this cluster, and apply the taint_prop
        treasury_hop0 = step6.ext.all %>%
          filter(address == "Dcur2mcGjmENx4DhNqDctW5wJCVyT3Qeqkx") %>%
          summarize(treasury_hop0 = sum(value)) %>%
          collect()
        
        #simplified version to test, will just use address hits for taint, and step6.2 for exchanges
        
        clusterdf$treasury_hop0 = treasury_hop0$treasury_hop0/100000000
        
        step7.tainthits = taints_by_address_hop %>%
          semi_join(address.list, by = "address", copy = TRUE) %>%
          collect()
        
        clusterdf$pow_hop0 = sum(step7.tainthits$taint_value[step7.tainthits$origin == "PoW" & step7.tainthits$hop == 0])/100000000
        clusterdf$pow_hop1 = sum(step7.tainthits$taint_value[step7.tainthits$origin == "PoW" & step7.tainthits$hop == 1])/100000000
        clusterdf$pow_hop2 = sum(step7.tainthits$taint_value[step7.tainthits$origin == "PoW" & step7.tainthits$hop == 2])/100000000
        
        clusterdf$treasury_hop0 = sum(step7.tainthits$taint_value[step7.tainthits$origin == "Treasury" & step7.tainthits$hop == 0])/100000000
        clusterdf$treasury_hop1 = sum(step7.tainthits$taint_value[step7.tainthits$origin == "Treasury" & step7.tainthits$hop == 1])/100000000
        clusterdf$treasury_hop2 = sum(step7.tainthits$taint_value[step7.tainthits$origin == "Treasury" & step7.tainthits$hop == 2])/100000000
        
        clusterdf$airdrop_hop0 = sum(step7.tainthits$taint_value[step7.tainthits$origin == "Airdrop" & step7.tainthits$hop == 0])/100000000
        clusterdf$airdrop_hop1 = sum(step7.tainthits$taint_value[step7.tainthits$origin == "Airdrop" & step7.tainthits$hop == 1])/100000000
        clusterdf$airdrop_hop2 = sum(step7.tainthits$taint_value[step7.tainthits$origin == "Airdrop" & step7.tainthits$hop == 2])/100000000
        
        clusterdf$founders_hop0 = sum(step7.tainthits$taint_value[step7.tainthits$origin == "Founders" & step7.tainthits$hop == 0])/100000000
        clusterdf$founders_hop1 = sum(step7.tainthits$taint_value[step7.tainthits$origin == "Founders" & step7.tainthits$hop == 1])/100000000
        clusterdf$founders_hop2 = sum(step7.tainthits$taint_value[step7.tainthits$origin == "Founders" & step7.tainthits$hop == 2])/100000000
        
        clusterdf$origin = adr
        clusterdf$mixed_inputs = 0
        
        if(nrow(step6.ext.from.mixing)>0){
          clusterdf$mixed_inputs = sum(step6.ext.from.mixing$value)/100000000
        }
        
        clusterdf$mixed_outputs = 0
        
        if(nrow(mixouts)>0){
          clusterdf$mixed_outputs = sum(mixouts$value)/100000000
        }
        if(nrow(cluster_agenda_votes) == 0){
          put.in.vote.db = data.frame(cluster = c, eligible_proposals = 0, proposal_votes = 0, yes_votes = 0, no_votes = 0,contrary_votes = 0, contrary_score = 0, agenda_eligible = 0, agenda_no = 0, agenda_yes = 0)
        }
        df.allstats = left_join(clusterdf, put.in.vote.db, by = "cluster", copy = TRUE)
        
        currentbalance = step5.cumsum$dcr_balance[nrow(step5.cumsum)]
        
        df.allstats$current_balance = currentbalance
        
        day90 = as.Date(Sys.time()) -90 
        time90 = paste(day90, " 00:00:01", sep="")
        out.90 = step5.cumsum %>%
          filter(block_time > time90 & is_funding == FALSE) %>%
          collect()
        in.90 = step5.cumsum %>%
          filter(block_time > time90 & is_funding == TRUE) %>%
          collect()
        
        df.allstats$outflow_90d = sum(out.90$dcrvalue)
        df.allstats$inflow_90d = sum(in.90$dcrvalue)
        
        day60 = as.Date(Sys.time()) -60 
        time60 = paste(day60, " 00:00:01", sep="")
        out.60 = step5.cumsum %>%
          filter(block_time > time60 & is_funding == FALSE) %>%
          collect()
        in.60 = step5.cumsum %>%
          filter(block_time > time60 & is_funding == TRUE) %>%
          collect()
        
        df.allstats$outflow_60d = sum(out.60$dcrvalue)
        df.allstats$inflow_60d = sum(in.60$dcrvalue)
        
        day30 = as.Date(Sys.time()) -30 
        time30 = paste(day30, " 00:00:01", sep="")
        out.30 = step5.cumsum %>%
          filter(block_time > time30 & is_funding == FALSE) %>%
          collect()
        in.30 = step5.cumsum %>%
          filter(block_time > time30 & is_funding == TRUE) %>%
          collect()
        
        df.allstats$outflow_30d = sum(out.30$dcrvalue)
        df.allstats$inflow_30d = sum(in.30$dcrvalue)
        
        
        step5.ins.onward = addresses %>%
          semi_join(step5.ins, by = c("tx_hash" = "matching_tx_hash"),copy=TRUE) %>%
          filter(is_funding == TRUE)%>%
          collect()
        
        
        
        step5.onward = step5.ins.onward %>%
          group_by(address) %>%
          summarize(sumvalue = sum(value))
        
        #exchange deposits should be based on "outs"
        
        #need to find the value of the 
        exchange.deposits.ins = step5.ins.onward %>%
          semi_join(exchange_addresses, by = "address", copy=TRUE)      
        exchange.deposits.ins.l = exchange.deposits.ins %>%
          left_join(exchange_addresses, by = "address", copy = TRUE) %>%
          collect()
        
        
        exchange.deposits.ins.l.g = exchange.deposits.ins.l %>%
          group_by(address, exchange) %>%
          summarize(rows = n(), sumdcr = sum(value)/10000000)
        
        exchange.deposits = step5.onward %>%
          semi_join(exchange_addresses, by = "address", copy=TRUE)
        
        
        #left join this against the table which can tell me which exchanges got the deposits?
        exchange.deposits.l = exchange.deposits %>%
          left_join(exchange_addresses, by = "address", copy = TRUE) %>%
          collect()
        
        
        df.allstats$to_binance = 0
        df.allstats$to_bittrex = 0
        df.allstats$to_poloniex = 0
        
        if(length(exchange.deposits.l$sumvalue[exchange.deposits.l$exchange == "binance"])>0){
          df.allstats$to_binance = sum(exchange.deposits.l$sumvalue[exchange.deposits.l$exchange == "binance"])/100000000
          
        }
        if(length(exchange.deposits.l$sumvalue[exchange.deposits.l$exchange == "bittrex"])>0){
          df.allstats$to_bittrex = sum(exchange.deposits.l$sumvalue[exchange.deposits.l$exchange == "bittrex"])/100000000
          
        }
        if(length(exchange.deposits.l$sumvalue[exchange.deposits.l$exchange == "polo"])>0){
          df.allstats$to_poloniex = sum(exchange.deposits.l$sumvalue[exchange.deposits.l$exchange == "polo"])/100000000
          
        }
        
        exchange.deposits.l.g = exchange.deposits.l %>%
          group_by(exchange) %>%
          summarise(sumdcr = sum(sumvalue)/100000000, num_addresses = n()) %>%
          collect()
        
        df.allstats$exchange_outflow_90d = 0  
        if(nrow(exchange.deposits)> 0){
          #df$allstats$exchange_outflow_90d = sum(exchange.deposits.l$sumvalue[exchange.deposits.l$])
          exchange.deposit.tx = addresses %>%
            semi_join(exchange.deposits, by = "address", copy = TRUE) %>%
            filter(is_funding == FALSE & block_time >= time90) %>%
            collect()
          
          
          if(length(exchange.deposit.tx$value) > 0){
            df.allstats$exchange_outflow_90d = sum(exchange.deposit.tx$value)/100000000 
          }
          
        }
        #add distinct_outputs, to see how widely this cluster is spreading the DCR. does it look like a pool?
        
        
        if(nrow(step5.ins.onward)>0){
          step5.onward = step5.ins.onward %>%
            group_by(address) %>%
            summarize(sumvalue = sum(value))
          
          
          #number of different addresses sent to
          step5.onward.only = step5.onward %>%
            anti_join(address.list, by = "address", copy=TRUE) %>%
            collect()
          step5.onward.only$dcr = step5.onward.only$sumvalue/100000000
          
          step5.onward.only.ins = step5.ins.onward %>%
            anti_join(address.list, by = "address", copy=TRUE) %>%
            collect()
          
          p.output.addresses =  ggplot(step5.onward.only)+
            aes(x=dcr)+
            geom_histogram(bins = 50)+
            labs(title = paste(c.label, " - Distribution of outputs",sep=""), x = "DCR Received by Address", y = "Number of Addresses")+
            scale_x_continuous(label=comma)+
            stat_bin(geom="text", size=3.5, colour = "#41bf53" , aes(label=..count.., y=0.8*(..count..)), bins = 50)+
            scale_y_log10()
          
          ggsave(paste("outputs-distribution-", c.label, ".png", sep=""), width = 8, height = 4.5)
          
          
          step5.outs.before = addresses %>%
            semi_join(step5.outs, by = c("tx_hash" = "matching_tx_hash"),copy=TRUE) %>%
            filter(is_funding == FALSE)%>%
            collect()
          
          step5.outs.before.external = step5.outs.before %>%
            anti_join(address.list, by = "address", copy=TRUE) %>%
            collect()
          
          
          #identify coinbase reward tx
          coinbase.ins = step5.ins %>%
            semi_join(coinbasetx, by = "tx_hash", copy=TRUE)
          
          coinbase.ins$type_extra = "coinbase"
          
          step5.outs.before.external$type_extra = "from-outside"
          
          df.allstats$blocks_mined = nrow(coinbase.ins)
          
          df.allstats$output_addresses = nrow(step5.onward.only)
          
          df.allstats$output_rows = nrow(step5.onward.only.ins)
          
          all.ins = bind_rows(coinbase.ins, step5.outs.before.external)  
          all.ins$direction = "in"
          
          #income/outgoing per day double line chart - rigid consistency = likely pool
          
          #step5.all use is_funding to split lines for incoming/outgoing, group by day
          #need to filter to just the ins and outs for the cluster, get rid of wash tx
          
          all.outs = step5.onward.only.ins
          
          
          
          all.outs$direction = "out"
          
          all.boundary = bind_rows(all.ins, all.outs)
          
          all.boundary$day = substr(all.boundary$block_time, 0, 10)
          all.boundary$day = paste(all.boundary$day, " 00:00:01", sep="")
          all.boundary$day = as.POSIXct(all.boundary$day)
          
          
          graph.input = all.boundary %>%
            group_by(day, direction) %>%
            summarize(dcr = sum(value)/100000000)
          
          
          p.in.out = ggplot(graph.input)+
            aes(x = day, y = rollmean(dcr, 3, na.pad = TRUE), colour = direction)+
            geom_line(size = 0.5)+
            geom_point(size = 0.5, position = "dodge")+
            labs(title = paste(c.label, " - Daily in/out flow",sep=""), x = "Day", y = "DCR")
          
          ggsave(paste("flows-", c.label, ".png", sep=""), width = 8, height = 4.5)
          
        }
        if(nrow(step5.ins.onward)==0){
          df.allstats$output_addresses = 0
          df.allstats$output_rows = 0
          
        }

        dbWriteTable(pcon, "cluster_stats_exp_function", df.allstats, append = TRUE, row.names = FALSE)

        
        print(paste("topaddress:",topaddress))
        
        return(df.allstats)
       
      }
      
    }
  }
  
  
cluster_addresses = tbl(pcon, "cluster_addresses_function")

#make network
  
make.network.cluster = function(cluster){
  adr = cluster
  c = cluster
  c.label = substr(c, 0, 6)
  c.adr = cluster_addresses %>%
    filter(cluster == c) %>%
    collect()
  
  all.adr = semi_join(cluster_addresses,c.adr, by = "address", copy = TRUE) %>%
    collect()  
  
  all.adr.g = all.adr %>%
    group_by(cluster) %>%
    summarize(rows = n())
  
  all.cluster.adr = semi_join(cluster_addresses,all.adr.g, by = "cluster", copy = TRUE) %>%
    collect()  
  
  address.list = all.cluster.adr %>%
    group_by(address) %>%
    summarize(rows = n())
  
  
  
  step5 = addresses %>%
    semi_join(address.list, by= "address",copy=TRUE) %>%
    collect()
  
  step5.ins = step5 %>%
    filter(is_funding == TRUE) %>%
    collect()
  
  step5.outs = step5 %>%
    filter(is_funding == FALSE) %>%
    collect()
  
  step5.ins.to = addresses %>%
    semi_join(step5.ins, by = c("tx_hash"="matching_tx_hash"),copy=TRUE) %>%
    filter(is_funding == TRUE) %>%
    collect()
  
  step5.ins.onward = step5.ins.to %>%
    anti_join(address.list, by = "address",copy=TRUE) %>%
    collect()

  step5.ins.onward$nodecolor = "onward"
  step5.ins$nodecolor = "onward"
  
  step5.ins.all = bind_rows(step5.ins, step5.ins.onward)
  
  
  step5.outs.from = addresses %>%
    semi_join(step5.outs, by = c( "tx_hash"="matching_tx_hash"),copy=TRUE)%>%
    filter(is_funding == FALSE) %>%
    collect()
  
  step5.outs.inward = step5.outs.from %>%
    anti_join(address.list, by = "address",copy=TRUE)
  
  
  step5.outs.inward.coinbase = step5.outs.inward %>%
    semi_join(coinbasetx, by = "tx_hash", copy= TRUE)
  step5.outs.inward.coinbase$nodecolor = "coinbase"
  
  step5.outs.inward = step5.outs.inward %>%
    anti_join(coinbasetx, by = "tx_hash", copy= TRUE)
  step5.outs.inward$nodecolor = "inward"
  
  step5.outs$nodecolor = "internal"
  
  edges.in.to.adr = step5.ins.all %>%
    select(from = tx_hash, to = address, tx_type, size = value, nodecolor)
  
  step5.outs.all = bind_rows(step5.outs, step5.outs.inward, step5.outs.inward.coinbase)   
  edges.out.from.adr = step5.outs.all %>%
    select(from = address, to = tx_hash, tx_type, size = value, nodecolor)
  
  edges.all = bind_rows(edges.in.to.adr, edges.out.from.adr)

  net.edge = graph_from_data_frame(edges.all, directed = TRUE)
  #return(net.edge)
  
  #build the nodes
  
  nodes.adr = edges.all$from[substr(edges.all$from,0,1) == "D"]
  
  nodes.adr.1 = edges.all %>%
    filter(substr(edges.all$from,0,1) == "D") %>%
    group_by(label = from)%>%
    summarize(size = sum(size), nodecolor = max(nodecolor))%>%
    collect()
  
  nodes.adr.2 = edges.all %>%
    filter(substr(edges.all$to,0,1) == "D") %>%
    group_by(label = to)%>%
    summarize(size = sum(size), nodecolor = max(nodecolor))%>%
    anti_join(nodes.adr.1, by = "label",copy=TRUE) %>%
    collect()
  nodes.adr = bind_rows(nodes.adr.1, nodes.adr.2)
  nodes.adr$type = "address"
  
  nodes.tx.1 = edges.all %>%
    filter(substr(edges.all$from,0,1) != "D") %>%
    group_by(label = from)%>%
    summarize(size = sum(size), nodecolor = max(tx_type))%>%
    collect()
  
  nodes.tx.2 = edges.all %>%
    filter(substr(edges.all$to,0,1) != "D") %>%
    group_by(label = to)%>%
    summarize(size = sum(size), nodecolor = max(tx_type))%>%
    anti_join(nodes.tx.1, by = "label",copy=TRUE) %>%
    collect()
  nodes.tx = bind_rows(nodes.tx.1, nodes.tx.2)
  nodes.tx$type = "transaction"
  
  nodes.tx$nodecolor[nodes.tx$nodecolor == 0] = "regular"
  nodes.tx$nodecolor[nodes.tx$nodecolor == 1] = "ticket"
  nodes.tx$nodecolor[nodes.tx$nodecolor == 2] = "vote"
  nodes.tx$nodecolor[nodes.tx$nodecolor == 3] = "revoke"
  
  nodes = bind_rows(nodes.tx, nodes.adr)
  
  edges.all$cluster = cluster
  nodes$cluster = cluster
  
  edges = edges.all %>%
    rename(coming_from = from, going_to = to)
  
  net =  graph_from_data_frame(edges.all, vertices=nodes, directed = TRUE)
  
  dbWriteTable(pcon, "network_nodes_cluster", nodes, append = TRUE, row.names = FALSE)
  dbWriteTable(pcon, "network_edges_cluster", edges, append = TRUE, row.names = FALSE)
}

make.network.cluster.sample = function(cluster){
  adr = cluster
  c = cluster
  c.label = substr(c, 0, 6)
  c.adr = cluster_addresses %>%
    filter(cluster == c) %>%
    collect()
  
  all.adr = semi_join(cluster_addresses,c.adr, by = "address", copy = TRUE) %>%
    collect()  
  
  all.adr.g = all.adr %>%
    group_by(cluster) %>%
    summarize(rows = n())
  
  all.cluster.adr = semi_join(cluster_addresses,all.adr.g, by = "cluster", copy = TRUE) %>%
    collect()  
  
  address.list = all.cluster.adr %>%
    group_by(address) %>%
    summarize(rows = n())
  
  step5 = addresses %>%
    semi_join(address.list, by= "address",copy=TRUE) %>%
    filter(block_time > "2020-12-01 00:00:01") %>%
    collect()
  
  step5.ins = step5 %>%
    filter(is_funding == TRUE) %>%
    collect()
  
  step5.outs = step5 %>%
    filter(is_funding == FALSE) %>%
    collect()
  
  step5.ins.to = addresses %>%
    semi_join(step5.ins, by = c("tx_hash"="matching_tx_hash"),copy=TRUE) %>%
    filter(is_funding == TRUE) %>%
    collect()
  
  step5.ins.onward = step5.ins.to %>%
    anti_join(address.list, by = "address",copy=TRUE) %>%
    collect()
  
  step5.ins.onward$nodecolor = "onward"
  step5.ins$nodecolor = "onward"
  
  step5.ins.all = bind_rows(step5.ins, step5.ins.onward)

  step5.outs.from = addresses %>%
    semi_join(step5.outs, by = c( "tx_hash"="matching_tx_hash"),copy=TRUE)%>%
    filter(is_funding == FALSE) %>%
    collect()
  
  step5.outs.inward = step5.outs.from %>%
    anti_join(address.list, by = "address",copy=TRUE)
  
  
  step5.outs.inward.coinbase = step5.outs.inward %>%
    semi_join(coinbasetx, by = "tx_hash", copy= TRUE)
  step5.outs.inward.coinbase$nodecolor = "coinbase"
  
  step5.outs.inward = step5.outs.inward %>%
    anti_join(coinbasetx, by = "tx_hash", copy= TRUE)
  step5.outs.inward$nodecolor = "inward"
  
  step5.outs$nodecolor = "internal"
  
  edges.in.to.adr = step5.ins.all %>%
    select(from = tx_hash, to = address, tx_type, size = value, nodecolor)
  
  step5.outs.all = bind_rows(step5.outs, step5.outs.inward, step5.outs.inward.coinbase)   
  edges.out.from.adr = step5.outs.all %>%
    select(from = address, to = tx_hash, tx_type, size = value, nodecolor)
  
  edges.all = bind_rows(edges.in.to.adr, edges.out.from.adr)
  
  net.edge = graph_from_data_frame(edges.all, directed = TRUE)
  #return(net.edge)
  
  #build the nodes
  
  nodes.adr = edges.all$from[substr(edges.all$from,0,1) == "D"]
  
  nodes.adr.1 = edges.all %>%
    filter(substr(edges.all$from,0,1) == "D") %>%
    group_by(label = from)%>%
    summarize(size = sum(size), nodecolor = max(nodecolor))%>%
    collect()
  
  nodes.adr.2 = edges.all %>%
    filter(substr(edges.all$to,0,1) == "D") %>%
    group_by(label = to)%>%
    summarize(size = sum(size), nodecolor = max(nodecolor))%>%
    anti_join(nodes.adr.1, by = "label",copy=TRUE) %>%
    collect()
  nodes.adr = bind_rows(nodes.adr.1, nodes.adr.2)
  nodes.adr$type = "address"
  
  nodes.tx.1 = edges.all %>%
    filter(substr(edges.all$from,0,1) != "D") %>%
    group_by(label = from)%>%
    summarize(size = sum(size), nodecolor = max(tx_type))%>%
    collect()
  
  nodes.tx.2 = edges.all %>%
    filter(substr(edges.all$to,0,1) != "D") %>%
    group_by(label = to)%>%
    summarize(size = sum(size), nodecolor = max(tx_type))%>%
    anti_join(nodes.tx.1, by = "label",copy=TRUE) %>%
    collect()
  nodes.tx = bind_rows(nodes.tx.1, nodes.tx.2)
  nodes.tx$type = "transaction"
  
  nodes.tx$nodecolor[nodes.tx$nodecolor == 0] = "regular"
  nodes.tx$nodecolor[nodes.tx$nodecolor == 1] = "ticket"
  nodes.tx$nodecolor[nodes.tx$nodecolor == 2] = "vote"
  nodes.tx$nodecolor[nodes.tx$nodecolor == 3] = "revoke"
  
  nodes = bind_rows(nodes.tx, nodes.adr)
  
  edges.all$cluster = cluster
  nodes$cluster = cluster
  
  edges = edges.all %>%
    rename(coming_from = from, going_to = to)
  
  net =  graph_from_data_frame(edges.all, vertices=nodes, directed = TRUE)
  
  dbWriteTable(pcon, "network_nodes_cluster", nodes, append = TRUE, row.names = FALSE)
  dbWriteTable(pcon, "network_edges_cluster", edges, append = TRUE, row.names = FALSE)
  
  
}


plot.network.layout = function(cluster, uselayout){
  edges = tbl(pcon, "network_edges") %>%
    filter(cluster == cluster) %>%
    distinct() %>%
    collect()

  edges.consolidate = edges %>%
    group_by(coming_from, going_to) %>%
    summarize(size = sum(size), tx_type = max(tx_type), nodecolor = max(nodecolor))
  
  nodes =   tbl(pcon, "network_nodes") %>%
    filter(cluster == cluster) %>%
    distinct() %>%
  collect()  
    
    nodes$type_text = nodes$type
  nodes$type = 0
  nodes$type[nodes$type_text == "transaction"] = 1
  
  nodes = nodes %>%
    group_by(label) %>%
    summarize(size = max(size), type = max(type), nodecolor = max(nodecolor)) %>%
    collect()

  net =  graph_from_data_frame(edges.consolidate, vertices=nodes, directed = TRUE)
  
  g.net = ggraph(net,layout="fr")+
    geom_edge_link(aes(color = factor(tx_type)))+
    geom_node_point( aes(size=size, shape = factor(type), color = nodecolor))+
    theme_minimal()
  
  ggsave("Dcgi-network-fr-layout.png", width = 20, height = 20)
  ggsave(paste("layout-", uselayout, "-cluster-", cluster, ".png", sep=""), width = 30, height = 30)


  }
