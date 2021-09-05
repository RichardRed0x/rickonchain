

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

#tickets.df = get.tickets(address.list)


#function which takes an address list and returns the tickets for a specific block
get.tickets.block = function(address.list){
  
  return(tickets.block.df)
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
plot.politeia.votes = function(politeia.votes, threshold = 0.05, titletext = "Proposal voting for ticket set"){
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

plot.politeia.votes.titles = function(politeia.votes, threshold = 0.01, titletext = "Proposal voting for ticket set", filename = "placeholder-name"){
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
      filter(unvoted > 0) %>%
	  distinct(prop, .keep_all = TRUE)
    proposal.gotspare$choice = "Abstain"
    proposal.gotspare$votes = proposal.gotspare$unvoted
    proposal.action = bind_rows(proposal.action, proposal.gotspare)
    
    cbpallette = c("Yes"="#41bf53", "No"="#fd714b", "Abstain"="#C4CBD2")
    
	maxeligible = max(proposal.action$eligible)
	threshold.eligibility = maxeligible*threshold
	proposal.action = proposal.action %>%
		filter(eligible >= threshold.eligibility)
	
	
    p.propvoting = ggplot(proposal.action) +
      aes(x = title, y = votes, fill = choice)+
      geom_bar(stat = "identity")+
      labs(x = "Proposal", title = paste(titletext))+
      scale_y_continuous(label=comma)+
      scale_fill_manual(values = cbpallette)+ 
      theme(axis.text.x = element_text(angle = 25, size = 6.8), axis.ticks.length=unit(.3, "cm"))
    #set colours
    #show eligible/abstain?
    
    
    ggsave(paste( filename, ".png", sep=""), height=4.5, width=9)
    
    
  }
}


over100clusteraddresses = cluster_addresses %>%
  semi_join(over100, copy=TRUE) %>%
  collect()


over100tickets = get.tickets.block(over100clusteraddresses)

over100tickets = ticket.cluster.nodupes %>%
  semi_join(over100)

over100tickets.clustertest = over100tickets %>%
  group_by(cluster) %>%
  summarise(rows=n())

over100votes = get.politeia.votes(over100tickets)
over100tickets

over100votes.l = over100votes %>%
  left_join(over100tickets, by = c("ticket"="tx_hash"))


#this doesn't work as a function necessarily but I ran it line by line to produce the long voting plots
plot.politeia.votes.titles.clusters = function(politeia.votes, threshold = 0.01, titletext = "Proposal voting for ticket set", filename = "placeholder-name"){
  #put proposal id to proposal row id
   
  
  proposal_votes.rel.wide = over100votes.l %>%
    left_join(proposals, by = c("proposals_row_id"="id"),copy=TRUE)
  
  proposal_votes.rel.wide$prop = substr(proposal_votes.rel.wide$token,0,7)
  
  p.votes = ggplot(prop.voting)+
    aes(x=prop, y = votes, fill = choice)+
    facet_grid(rows=vars(cluster))
  
  
  
  prop.voting = proposal_votes.rel.wide %>%
    group_by(prop, choice, cluster) %>%
    summarize(votes = n()) %>%
    collect()
  
  
  p.propvoting = ggplot(proposal.action) +
    aes(x = title, y = votes, fill = choice)+
    geom_bar(stat = "identity")+
    facet_grid(rows=vars(cluster))+
    labs(x = "Proposal", title = paste(titletext))+
    scale_y_continuous(label=comma)+
    scale_fill_manual(values = cbpallette)+ 
    theme(axis.text.x = element_text(angle = 25, size = 6.8), axis.ticks.length=unit(.3, "cm"))
  
  
  #eligibility
  
  eligible_proposals.rel = eligible_proposals %>%
    semi_join(proposal_votes.rel.wide, by = c("ticket"),copy=TRUE) %>%
    collect()
  

    
    eligible_proposals.rel$prop = substr(eligible_proposals.rel$proposal,0,7)
    eligible_proposals.rel$value = "Abstain"
    
    #d.eli = dcast(eligible_proposals.rel, ticket ~ prop, value.var = "value")
    
    
    
    eligible_proposals.l = eligible_proposals.rel %>%
      inner_join(proposal_votes.rel.wide, by = c("ticket", "proposal"="token"),copy=TRUE) %>%
      collect()
    
    eligible_proposals.l$choice[is.na(eligible_proposals.l$choice)]="Abstain"
    
    eligible_proposals.l$prop = substr(eligible_proposals.l$proposal,0,7)
    
    
    prop.eligible.g = eligible_proposals.l %>%
      group_by(prop, cluster) %>%
      summarize(eligible = n()) %>%
      collect()
    
    #voting

    
    
    
    prop.voting.l = prop.voting %>%
      left_join(prop.eligible.g, by = c("prop", "cluster"), copy=TRUE) %>%
      collect()
    
    prop.eligible.l = prop.eligible.g %>%
      left_join(prop.voting, by = c("prop", "cluster"), copy=TRUE) %>%
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
      group_by(prop, cluster) %>%
      summarize(sumvotes = sum(votes)) %>%
      collect()
    
    proposal.action = proposal.action %>%
      left_join(proposal.g, by = c("prop", "cluster")) 
    
    proposal.action$unvoted = proposal.action$eligible - proposal.action$sumvotes
    
    
    proposal.gotspare = proposal.action %>% 
      filter(unvoted > 0) %>%
	  distinct(prop, .keep_all = TRUE)
    proposal.gotspare$choice = "Abstain"
    proposal.gotspare$votes = proposal.gotspare$unvoted
    proposal.action = bind_rows(proposal.action, proposal.gotspare)
    
    cbpallette = c("Yes"="#41bf53", "No"="#fd714b", "Abstain"="#C4CBD2")
    
	maxeligible = max(proposal.action$eligible)
	threshold.eligibility = maxeligible*threshold
	#proposal.action = proposal.action %>%
	#	filter(eligible >= threshold.eligibility)
	
	proposal.action$cluster = substr(proposal.action$cluster,0,5)
	
    p.propvoting = ggplot(proposal.action) +
      aes(x = title, y = votes, fill = choice)+
      geom_bar(stat = "identity")+
      facet_grid(rows=vars(cluster), scales = "free_y")+
      labs(x = "Proposal", title = paste(titletext))+
      scale_y_continuous(label=comma)+
      scale_fill_manual(values = cbpallette)+ 
      theme(axis.text.x = element_text(angle = 25, size = 6.8), axis.ticks.length=unit(.3, "cm"))
    #set colours
    #show eligible/abstain?
    
    
    ggsave(paste("clusters-100-tickets-525000.png", sep=""), height=18, width=9)
    
    
  }
}

  