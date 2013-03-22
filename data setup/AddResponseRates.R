AddResponseRates = function(quest
                            ,ns
                            ,nwosCycle
                            ,special = NULL)
{
  if(nwosCycle == 11)
  {
    areaName = 'AC_WOOD'
    stateVariable = 'QUEST_STATE'
    ownVariable = 'OWNTYPE'
    famCodes = 1:4
    nonFamCodes = 5:6
  }
  if(nwosCycle == 06)
  {
    areaName = 'ACRES_IN_STATE'
    stateVariable = 'STATECD'
    ownVariable = 'OWNER_CLASS_ADJ'
    famCodes = 45
    nonFamCodes = 41:44
  }

  quest$responseRateFIA = ns$urr_fia_pf[match(quest[,stateVariable], ns$state)]
  quest$responseRateNWOS = NA
  quest$responseRateNWOS[quest[,ownVariable] %in% famCodes] = c(ns$n_ff_resp[match(quest[,stateVariable], ns$state)]/ns$n_ff[match(quest[,stateVariable], ns$state)])[quest[,ownVariable] %in% famCodes]
  quest$responseRateNWOS[quest[,ownVariable] %in% nonFamCodes] = c(ns$n_pf_nonfam_resp[match(quest[,stateVariable], ns$state)]/ns$n_pf_nonfam[match(quest[,stateVariable], ns$state)])[quest[,ownVariable] %in% nonFamCodes]
  
  assign(paste('quest_', special, nwosCycle, sep = ''), quest, .GlobalEnv)
}

