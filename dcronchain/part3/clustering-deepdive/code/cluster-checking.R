
source("clustering-functions.R")

address =  "Insert address here"
address.list = data.frame(address)

tickets.df = get.tickets(address.list)

politeia.votes = get.politeia.votes(tickets.df)

plot.politeia.votes(politeia.votes, titletext=paste(substr(address, 0,5), " voting only"))

#standard cluster set of tickets

tickets.df = get.tickets(address.list)

politeia.votes = get.politeia.votes(tickets.df)

plot.politeia.votes(politeia.votes,titletext=paste(substr(address, 0,5)," standard cluster tickets", sep=""))  


#for plotting the pi votes of all clustered groups
cluster_addresses.g = standard.clusters %>%
  group_by(cluster) %>%
  summarise(rows = n()) %>%
  arrange(desc(rows)) %>%
  collect()

for(adr in cluster_addresses.g$cluster){
  print(adr)
  address.list = standard.clusters %>%
    filter(cluster == adr) %>%
    collect()
  
  tickets.df = get.tickets(address.list)
  
  politeia.votes = get.politeia.votes(tickets.df)
  
  if(nrow(politeia.votes)>0){
  plot.politeia.votes.titles(politeia.votes,titletext=paste(substr(adr, 0,5),"-standard-cluster-tickets", sep=""))  
  
  }
}

#full cluster set of tickets
address.list = enhanced.clusters %>%
  filter(cluster == address) %>%
  collect()


tickets.df = get.tickets(address.list)

politeia.votes = get.politeia.votes(tickets.df)

plot.politeia.votes.titles(politeia.votes,titletext=paste(substr(address, 0,5)," enhanced cluster tickets", sep=""))  





#fetch the address.list from a cluster table


#function which takes an address list and returns all its tickets
get.tickets = function(address.list){
  adr.tab = addresses %>%
    semi_join(address.list, by = "address", copy=TRUE) %>%
    filter(valid_mainchain == TRUE) %>%
    collect()
  
  tickets.df = adr.tab %>%
    filter(is_funding == TRUE & tx_type == 1)

  return(tickets.df)
}




#function which takes a ticket list and returns all its politeia votes
get.politeia.votes = function(tickets.df){
  tickets.rel = tickets.df
  
  proposal_votes.rel = proposal_votes %>%
    semi_join(tickets.rel, by = c("ticket"="tx_hash"),copy=TRUE) %>%
    collect()
  
  return(proposal_votes.rel)
}

#politeia.votes = get.politeia.votes(tickets.df)




#function which takes a politeia votes df and plots the yes/no breakdown per proposal, above a certain threshold (specified as a proportion of all the set's politeia voting, can be 0)
plot.politeia.votes = function(politeia.votes, threshold = 0.01, titletext = "Proposal voting for ticket set"){
  #put proposal id to proposal row id
  
  proposal_votes.rel.wide = politeia.votes %>%
    left_join(proposals, by = c("proposals_row_id"="id"),copy=TRUE)
  
  proposal_votes.rel.wide$prop = substr(proposal_votes.rel.wide$token,0,7)
  
  
  #eligibility
  
  eligible_proposals.rel = eligible_proposals %>%
    semi_join(proposal_votes.rel.wide, by = c("ticket"),copy=TRUE) %>%
    collect()
  
  if(nrow(eligible_proposals.rel)>0){
  
  eligible_proposals.rel$prop = substr(eligible_proposals.rel$proposal,0,7)
  eligible_proposals.rel$value = "Abstain"
  
  #d.eli = dcast(eligible_proposals.rel, ticket ~ prop, value.var = "value")
  
  
  
  eligible_proposals.l = eligible_proposals.rel %>%
    left_join(proposal_votes.rel.wide, by = c("ticket", "proposal"="token"),copy=TRUE) %>%
    collect()
  
  eligible_proposals.l$choice[is.na(eligible_proposals.l$choice)]="Abstain"
  
  eligible_proposals.l$prop = substr(eligible_proposals.l$proposal,0,7)
  
  
  prop.eligible.g = eligible_proposals.l %>%
    group_by(prop) %>%
    summarize(eligible = n()) %>%
    collect()
  
  #voting
  
  prop.voting = proposal_votes.rel.wide %>%
    group_by(prop, choice) %>%
    summarize(votes = n()) %>%
    collect()
  
  
  
  prop.voting.l = prop.voting %>%
    left_join(prop.eligible.g, by = "prop", copy=TRUE) %>%
    collect()
  
  prop.eligible.l = prop.eligible.g %>%
    left_join(prop.voting, by = "prop", copy=TRUE) %>%
    collect()
  
  
  
  prop.eligible.l$choice[is.na(prop.eligible.l$choice)] = "Abstain"
  
  proposal.action = prop.eligible.l
  

  
  proposal.action$votes[is.na(proposal.action$votes)]=proposal.action$eligible[is.na(proposal.action$votes)]
  
  myColors = c("41bf53","2252a3","fd714b")
  
  #arrange proposals by time on x axis
  #remove early proposals with few votes
  #try it as a panel showing the top X voters
  
  proposal.action$choice = factor(proposal.action$choice, levels = c("Yes", "No", "Abstain"))
  
  #unvoted needs to take into account situations where the cluster voted for and against a proposal, by joining against a table that knows the total (grouped by proposal)
  proposal.g = proposal.action %>%
    group_by(prop) %>%
    summarize(sumvotes = sum(votes)) %>%
    collect()
  
  proposal.action = proposal.action %>%
    left_join(proposal.g, by = "prop") 
  
  proposal.action$unvoted = proposal.action$eligible - proposal.action$sumvotes
  
  
  proposal.gotspare = proposal.action %>% 
    filter(unvoted > 0)
  proposal.gotspare$choice = "Abstain"
  proposal.gotspare$votes = proposal.gotspare$unvoted
  proposal.action = bind_rows(proposal.action, proposal.gotspare)
  
  cbpallette = c("Yes"="#41bf53", "No"="#fd714b", "Abstain"="#C4CBD2")
  
  p.propvoting = ggplot(proposal.action) +
    aes(x = prop, y = votes, fill = choice)+
    geom_bar(stat = "identity")+
    labs(x = "Proposal", title = paste(titletext))+
    scale_y_continuous(label=comma)+
    scale_fill_manual(values = cbpallette)+ 
    theme(axis.text.x = element_text(angle = 25))
  #set colours
  #show eligible/abstain?
  
  
  ggsave(paste( titletext, ".png", sep=""), height=4.5, width=9)
  
  
}
}

plot.politeia.votes.titles = function(politeia.votes, threshold = 0.01, titletext = "Proposal voting for ticket set"){
  #put proposal id to proposal row id
  
  proposal_votes.rel.wide = politeia.votes %>%
    left_join(proposals, by = c("proposals_row_id"="id"),copy=TRUE)
  
  proposal_votes.rel.wide$prop = substr(proposal_votes.rel.wide$token,0,7)
  
  
  #eligibility
  
  eligible_proposals.rel = eligible_proposals %>%
    semi_join(proposal_votes.rel.wide, by = c("ticket"),copy=TRUE) %>%
    collect()
  
  if(nrow(eligible_proposals.rel)>0){
    
    eligible_proposals.rel$prop = substr(eligible_proposals.rel$proposal,0,7)
    eligible_proposals.rel$value = "Abstain"
    
    #d.eli = dcast(eligible_proposals.rel, ticket ~ prop, value.var = "value")
    
    
    
    eligible_proposals.l = eligible_proposals.rel %>%
      left_join(proposal_votes.rel.wide, by = c("ticket", "proposal"="token"),copy=TRUE) %>%
      collect()
    
    eligible_proposals.l$choice[is.na(eligible_proposals.l$choice)]="Abstain"
    
    eligible_proposals.l$prop = substr(eligible_proposals.l$proposal,0,7)
    
    
    prop.eligible.g = eligible_proposals.l %>%
      group_by(prop) %>%
      summarize(eligible = n()) %>%
      collect()
    
    #voting
    
    prop.voting = proposal_votes.rel.wide %>%
      group_by(prop, choice) %>%
      summarize(votes = n()) %>%
      collect()
    
    
    
    prop.voting.l = prop.voting %>%
      left_join(prop.eligible.g, by = "prop", copy=TRUE) %>%
      collect()
    
    prop.eligible.l = prop.eligible.g %>%
      left_join(prop.voting, by = "prop", copy=TRUE) %>%
      collect()
    
    
    
    prop.eligible.l$choice[is.na(prop.eligible.l$choice)] = "Abstain"
    
    proposal.action = prop.eligible.l
  
    proposal.action$title = ""
    proposal.action$title[proposal.action$prop=="020b8b0"] = "Research 2021"
    proposal.action$title[proposal.action$prop=="1d74b88"] = "Journal 2021"
    proposal.action$title[proposal.action$prop=="2bf72e6"] = "withDecred.org"
    proposal.action$title[proposal.action$prop=="4532397"] = "RFP-D.R.E.A.M"
    proposal.action$title[proposal.action$prop=="9e1d644"] = "Money/State Book"
    proposal.action$title[proposal.action$prop=="c093b8a"] = "Translation"
    proposal.action$title[proposal.action$prop=="f279ed5"] = "alexsolo"
    proposal.action$title[proposal.action$prop=="02d9fc2"] = "RFP-Money Evolved-min"
    proposal.action$title[proposal.action$prop=="350f64b"] = "Decred ES"
    proposal.action$title[proposal.action$prop=="391108e"] = "DiD Live"
    proposal.action$title[proposal.action$prop=="3943bff"] = "Address scanner"
    proposal.action$title[proposal.action$prop=="5ce1636"] = "Hackathons Latam"
    proposal.action$title[proposal.action$prop=="bc499c9"] = "Mobile 2021"
    proposal.action$title[proposal.action$prop=="d0c32d5"] = "Decred Arabia"
    proposal.action$title[proposal.action$prop=="d462ac3"] = "DCRDEX Phase 2"
    proposal.action$title[proposal.action$prop=="d6ff458"] = "RFP-Revolution Infra"
    proposal.action$title[proposal.action$prop=="e5c8051"] = "GoDCR"
    proposal.action$title[proposal.action$prop=="f0a00d5"] = "RFP-Money Evolved-fair"
    proposal.action$title[proposal.action$prop=="1dc1571"] = "Design 1"
    proposal.action$title[proposal.action$prop=="1e55a41"] = "Video 2"
    proposal.action$title[proposal.action$prop=="2dcbc3e"] = "Travala"
    proposal.action$title[proposal.action$prop=="32cba00"] = "Moderation 1"
    proposal.action$title[proposal.action$prop=="91becea"] = "RFP: Messaging"
    proposal.action$title[proposal.action$prop=="c81926b"] = "Monde 2"
    proposal.action$title[proposal.action$prop=="df11d7a"] = "Russia 2"
    proposal.action$title[proposal.action$prop=="012b4e3"] = "Ditto 3"
    proposal.action$title[proposal.action$prop=="0230918"] = "Decred OnChain"
    proposal.action$title[proposal.action$prop=="063e382"] = "Facebook Manager"
    proposal.action$title[proposal.action$prop=="073694e"] = "Bug Bounty 2"
    proposal.action$title[proposal.action$prop=="0a1ff84"] = "DCP0005"
    proposal.action$title[proposal.action$prop=="1b4b72f"] = "Offline Ecosystem"
    proposal.action$title[proposal.action$prop=="20e967d"] = "TinyDecred"
    proposal.action$title[proposal.action$prop=="2170df6"] = "Bug Bounty 3"
    proposal.action$title[proposal.action$prop=="27f8717"] = "Ditto 1"
    proposal.action$title[proposal.action$prop=="2ababde"] = "Trust Wallet"
    proposal.action$title[proposal.action$prop=="2eb7ddb"] = "MM RFP i2"
    proposal.action$title[proposal.action$prop=="2ef74fa"] = "DCR Comic 1"
    proposal.action$title[proposal.action$prop=="2f08f85"] = "DCR Comic 2"
    proposal.action$title[proposal.action$prop=="30822c1"] = "RFP for MM"
    proposal.action$title[proposal.action$prop=="34707d3"] = "Easyrabbit"
    proposal.action$title[proposal.action$prop=="3c02b67"] = "Latam 2"
    proposal.action$title[proposal.action$prop=="417607a"] = "DEX Development"
    proposal.action$title[proposal.action$prop=="4acb955"] = "Stickers"
    proposal.action$title[proposal.action$prop=="4affceb"] = "CoinStory"
    proposal.action$title[proposal.action$prop=="4becbe0"] = "MM Grapefruit"
    proposal.action$title[proposal.action$prop=="5226529"] = "VSP language"
    proposal.action$title[proposal.action$prop=="52ea110"] = "Ditto 2"
    proposal.action$title[proposal.action$prop=="5431da8"] = "RFP DEX"
    
    proposal.action$votes[is.na(proposal.action$votes)]=proposal.action$eligible[is.na(proposal.action$votes)]
    
    myColors = c("41bf53","2252a3","fd714b")
    
    #arrange proposals by time on x axis
    #remove early proposals with few votes
    #try it as a panel showing the top X voters
    
    proposal.action$choice = factor(proposal.action$choice, levels = c("Yes", "No", "Abstain"))
    
    #unvoted needs to take into account situations where the cluster voted for and against a proposal, by joining against a table that knows the total (grouped by proposal)
    proposal.g = proposal.action %>%
      group_by(prop) %>%
      summarize(sumvotes = sum(votes)) %>%
      collect()
    
    proposal.action = proposal.action %>%
      left_join(proposal.g, by = "prop") 
    
    proposal.action$unvoted = proposal.action$eligible - proposal.action$sumvotes
    
    
    proposal.gotspare = proposal.action %>% 
      filter(unvoted > 0)
    proposal.gotspare$choice = "Abstain"
    proposal.gotspare$votes = proposal.gotspare$unvoted
    proposal.action = bind_rows(proposal.action, proposal.gotspare)
    
    cbpallette = c("Yes"="#41bf53", "No"="#fd714b", "Abstain"="#C4CBD2")
    
    p.propvoting = ggplot(proposal.action) +
      aes(x = title, y = votes, fill = choice)+
      geom_bar(stat = "identity")+
      labs(x = "Proposal", title = paste(titletext))+
      scale_y_continuous(label=comma)+
      scale_fill_manual(values = cbpallette)+ 
      theme(axis.text.x = element_text(angle = 25, size = 6.8), axis.ticks.length=unit(.3, "cm"))
    #set colours
    #show eligible/abstain?
    
    
    ggsave(paste( titletext, ".png", sep=""), height=4.5, width=9)
    
    
  }
}


#this is the code to iterate over the clustering and plot pi votes
adr = address
  print(adr)
  address.list = data.frame(address) 
  
  do.again = 1
  countdown = 15
  problem.tickets = data.frame()
  
  
  while(do.again > 0 & countdown > 0 & nrow(problem.tickets)==0){
  
      address.list.saver = address.list
    
    
    multi1 = multi.inputs.target.basic(address.list)
    
    multi1.new.adr = multi1 %>%
      anti_join(address.list,by = "address", copy=TRUE) %>%
      group_by(address) %>%
      summarize(rows = n()) %>%
      collect()
    if(nrow(multi1.new.adr)> 0){
      address.list = multi1.new.adr %>%
        select(address) %>%
        bind_rows(address.list) 
      
      tickets.df = get.tickets(address.list)
      if(nrow(tickets.df)>0){
        politeia.votes = get.politeia.votes(tickets.df)
      if(nrow(politeia.votes)>0){
      plot.politeia.votes(politeia.votes, titletext=paste(substr(address, 0,5), " Countdown ", countdown, " multi1.new.adr"))
      }
      }
    }
    adr.tab = addresses %>%
      semi_join(address.list, by = "address", copy=TRUE) %>%
      filter(valid_mainchain == TRUE) %>%
      collect()
    
    ticket.ins = adr.tab %>%
      filter(is_funding == TRUE & tx_type == 1)
    
    #are there new tickets?
    #if so, run the function to check for harmonious voting
    
    #if the voting is harmonious, proceed
    #if the voting is not harmonious, dump the details needed to follow up
    
    
    if(nrow(ticket.ins)> 0){
      these.inputs = process.ticket.ins.getinputs(ticket.ins)
      if(nrow(these.inputs)>0){
        these.inputs.new.adr = these.inputs %>%
          anti_join(address.list,by = "address", copy=TRUE) %>%
          group_by(address) %>%
          summarize(rows = n()) %>%
          collect()
        if(nrow( these.inputs.new.adr)> 0){
          address.list = these.inputs.new.adr %>%
            select(address) %>%
            bind_rows(address.list)   
          tickets.df = get.tickets(address.list)
          politeia.votes = get.politeia.votes(tickets.df)
          if(nrow(politeia.votes)>0){
          plot.politeia.votes(politeia.votes, titletext=paste(substr(address, 0,5), " Countdown ", countdown, " these.inputs..new.adr"))
          }
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
            tickets.df = get.tickets(address.list)
            politeia.votes = get.politeia.votes(tickets.df)
            if(nrow(politeia.votes)>0){
            plot.politeia.votes(politeia.votes, titletext=paste(substr(address, 0,5), " Countdown ", countdown, " these.outputs.new.adr"))
            }
}
        }
      }
    }

    #these.inputs are inputs to tickets, take a step back and see if 1) they were mixed, 2) 
    ticket.outs = adr.tab %>%
      filter(is_funding == FALSE & tx_type == 1 & value > 100000000)
    if(nrow(ticket.outs)> 0){
      
      these.votes = addresses %>%
        semi_join(ticket.outs, by = "tx_hash", copy=TRUE) %>%
        filter(is_funding == TRUE   & mixed_broad == 0 & valid_mainchain == TRUE) %>%
        collect()
      
      these.votes.new.adr = these.votes %>%
        anti_join(address.list,by = "address", copy=TRUE) %>%
        group_by(address) %>%
        summarize(rows = n()) %>%
        collect()
      if(nrow(these.votes.new.adr )> 0){
        address.list = these.votes.new.adr %>%
          select(address) %>%
          bind_rows(address.list)   
        tickets.df = get.tickets(address.list)
        politeia.votes = get.politeia.votes(tickets.df)
        if(nrow(politeia.votes)>0){
          plot.politeia.votes(politeia.votes, titletext=paste(substr(address, 0,5), " Countdown ", countdown, " these.votes.new.adr"))  
        }
      }
    }
    #check the votes
    vote.ins = adr.tab %>%
      filter(is_funding == TRUE & tx_type == 2 & value > 100000000 & mixed_broad == 0)
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
        tickets.df = get.tickets(address.list)
        politeia.votes = get.politeia.votes(tickets.df)
        if(nrow(politeia.votes)>0){
          plot.politeia.votes(politeia.votes, titletext=paste(substr(address, 0,5), " Countdown ", countdown, " voting.addresses.new.adr"))  
        }
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
        tickets.df = get.tickets(address.list)
        politeia.votes = get.politeia.votes(tickets.df)
        if(nrow(politeia.votes)>0){
        plot.politeia.votes(politeia.votes, titletext=paste(substr(address, 0,5), " Countdown ", countdown, " type.0.tickets.new.adr"))  
        }
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
  if(exists("ticket.input.new.adr")){all.new.adr = bind_rows(all.new.adr, ticket.input.new.adr)
  ticket.input.new.adr = data.frame()}
  
  
  do.again = nrow(all.new.adr)
  countdown = countdown - 1
  print(paste("did a loop", adr, "new addresses:", do.again))
  
  }
  
  
  
  
  
  
  #removed bits
  
  
  ticket.outs = adr.tab %>%
    filter(is_funding == FALSE & tx_type == 1 & value > 100000000)
  if(nrow(ticket.outs)> 0){
    
    these.votes = addresses %>%
      semi_join(ticket.outs, by = "tx_hash", copy=TRUE) %>%
      filter(is_funding == TRUE   & mixed_broad == 0 & valid_mainchain == TRUE) %>%
      collect()
    
    these.votes.new.adr = these.votes %>%
      anti_join(address.list,by = "address", copy=TRUE) %>%
      group_by(address) %>%
      summarize(rows = n()) %>%
      collect()
    if(nrow(these.votes.new.adr )> 0){
      address.list = these.votes.new.adr %>%
        select(address) %>%
        bind_rows(address.list)   
      tickets.df = get.tickets(address.list)
      politeia.votes = get.politeia.votes(tickets.df)
      if(nrow(politeia.votes)>0){
        plot.politeia.votes(politeia.votes, titletext=paste(substr(address, 0,5), " Countdown ", countdown, " these.votes.new.adr"))  
      }
    }
  }
  #check the votes
  vote.ins = adr.tab %>%
    filter(is_funding == TRUE & tx_type == 2 & value > 100000000 & mixed_broad == 0)
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
      tickets.df = get.tickets(address.list)
      politeia.votes = get.politeia.votes(tickets.df)
      if(nrow(politeia.votes)>0){
        plot.politeia.votes(politeia.votes, titletext=paste(substr(address, 0,5), " Countdown ", countdown, " voting.addresses.new.adr"))  
      }
    }
  }
  
  
  
  #check if all the "problem clusters" have this minor issue
  
  c.adr = "DcZXFuG8tu9PTk3DSkDqvo5rPb2ShQyHEL4"
  c.adr = "DsgaTNC7266sZiSG6Ku27dap2J54g3LxcDi"
  #plot cluster addresses
  
  address.list = enhanced.clusters %>%
    filter(cluster == c.adr) %>%
    collect()
  tickets.df = get.tickets(address.list)
  politeia.votes = get.politeia.votes(tickets.df)
  plot.politeia.votes(politeia.votes, titletext=paste(substr(c.adr, 0,5), " cluster addresses"))  
  
  
  standard.cluster.checks = tbl(pcon, "cluster_checker")
  standard.cluster.problems = standard.cluster.checks %>%
    filter(result == "Problem") %>%
    collect()

  for(c.adr in standard.cluster.problems$cluster){
    
    address.list = cluster_addresses  %>%
      filter(cluster == c.adr) %>%
      collect()
    tickets.df = get.tickets(address.list)
    politeia.votes = get.politeia.votes(tickets.df)
    plot.politeia.votes(politeia.votes, titletext=paste(substr(c.adr, 0,5), " cluster addresses"))  
    
    
    
  }
  
#are these a bunch of single voting addresses voting yes/no on the same proposal? if so that's weird  
    