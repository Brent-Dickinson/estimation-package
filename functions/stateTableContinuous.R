stateTableContinuous = function(variableColumns
                                ,quest
                                ,stratumInfo
                                ,populationOfInterest
                                ,nwosCycle = 2011)
{
  quest = cbind(quest, populationOfInterest = populationOfInterest)
  if(nwosCycle == 2011)
  {
    areaName = 'AC_WOOD'
    stateVariable = 'QUEST_STATE'
    ownVariable = 'OWNTYPE'
    famCodes = 1:4
    nonFamCodes = 5:6
  }
  if(nwosCycle == 2006)
  {
    areaName = 'ACRES_IN_EU'
    stateVariable = 'STATECD'
    ownVariable = 'OWNER_CLASS_ADJ'
    famCodes = 45
    nonFamCodes = 41:44
  }
  quest = quest[quest[,areaName] >= 1,]
  
  # string manipulation for table names in output:
  tablesMicro = names(quest)[variableColumns]
  
  statesOut = data.frame(NULL)
  for(i in 1:nrow(stratumInfo))
  {
    t0 = Sys.time()
    # pull necessary data for each stratum:
    stateCd = stratumInfo$state[i]
    stateName = stratumInfo$state_abbr[i]
    regionCd = stratumInfo$rpa_regionCd[i]
    regionName = stratumInfo$rpa_regionNm[i]
    areaTotal = stratumInfo$A[i]
    nTotal = stratumInfo$n_h[i]
    
    questSub1 = quest[quest[,stateVariable] == stateCd,]
    # overall unit response rate:
    responseRateFIA = questSub1$responseRateFIA
    responseRateNWOS = questSub1$responseRateNWOS
    responseRateUnit = responseRateFIA*responseRateNWOS
    
    # incorporate missing OWNTYPE's into the unit response rate and pull them from sample:
    if(nwosCycle == 2011)
    {
      dNeg1 = ifelse(questSub1[,ownVariable] == -1, 0, 1)
      xNeg1 = sum(questSub1$POINT_COUNT*dNeg1/questSub1[,areaName])
      xBase = sum(questSub1$POINT_COUNT/questSub1[,areaName])
      questSub1$responseRateUnit = responseRateUnit*xNeg1/xBase
      questSub1 = questSub1[questSub1[,ownVariable] != -1,]
    }
    
    variableColumnsOut = data.frame(NULL)
    for(j in variableColumns)
    {
      if(is.numeric(questSub1[,j]) == F) stop(paste('fix make_quest_all():', names(quest)[j], 'not numeric'))
      questSub = questSub1
      # pull out -3's from sample (they were not asked that question) and create weighted item response rate:
      if(any(questSub[,j] %in% c(-3, -1))) 
      {
        # just use shortcut equations
        dNeg3 = ifelse(questSub[,j] %in% c(-3, -1), 0, 1)
        xNeg3 = sum(questSub$POINT_COUNT*dNeg3/questSub[,areaName])
        xBase = sum(questSub$POINT_COUNT/questSub[,areaName])
        questSub$responseRateItem = xNeg3/xBase
        questSub = questSub[!questSub[,j] %in% c(-3, -1),]
      }
      else
      {
        questSub$responseRateItem = rep(1, nrow(questSub))
      }
      
      # set up components using new questSub
      variable = questSub[,j]
      pointCount = questSub$POINT_COUNT
      areaIndividual = questSub[,areaName]
      variableOwnerships = rep(1, length(pointCount))
      
      table = tablesMicro[variableColumns == j]
      levelOut = data.frame(NULL)
      populationIndicator = ifelse(questSub$populationOfInterest == 'population', 1, 0)

      acresOutput = hheCore(areaTotal = areaTotal
                            ,nTotal = nTotal
                            ,responseRate = questSub$responseRateUnit*questSub$responseRateItem
                            ,pointCount = pointCount
                            ,areaIndividual = areaIndividual
                            ,variable = variable
                            ,domain = populationIndicator)
      acresInDomain = acresOutput$yHat
      varAcresInDomain = acresOutput$VyHat
      ownershipsOutput = hheCore(areaTotal = areaTotal
                                 ,nTotal = nTotal
                                 ,responseRate = questSub$responseRateUnit*questSub$responseRateItem
                                 ,pointCount = pointCount
                                 ,areaIndividual = areaIndividual
                                 ,variable = variableOwnerships
                                 ,domain = populationIndicator)
      ownershipsInDomain = ownershipsOutput$yHat
      varOwnershipsInDomain = ownershipsOutput$VyHat
      covAcresOwnershipsInDomain = cov1(areaTotal = areaTotal
                                        ,nTotal = nTotal
                                        ,responseRate = questSub$responseRateUnit*questSub$responseRateItem
                                        ,pointCount = pointCount
                                        ,areaIndividual = areaIndividual
                                        ,variable1 = variable
                                        ,variable2 = variableOwnerships
                                        ,domain1 = populationIndicator
                                        ,domain2 = populationIndicator)
      meanAcresInDomain = acresInDomain/ownershipsInDomain
      varMeanAcresInDomain = varianceRatio(acresInDomain, ownershipsInDomain, varAcresInDomain, varOwnershipsInDomain, covAcresOwnershipsInDomain)

      levelOut = rbind(levelOut, data.frame(table = table
                                            ,row = 'row_01_continuous variable'
                                            ,stateCd = stateCd
                                            ,stateNm = stateName
                                            ,regionCd = regionCd
                                            ,regionNm = regionName
                                            ,ownershipsInDomain = ownershipsInDomain
                                            ,varOwnershipsInDomain = varOwnershipsInDomain
                                            ,acresInDomain = acresInDomain
                                            ,varAcresInDomain = varAcresInDomain
                                            ,meanAcresInDomain = meanAcresInDomain
                                            ,varMeanAcresInDomain = varMeanAcresInDomain
                                            ,covAcresOwnershipsInDomain = covAcresOwnershipsInDomain
                                            ,acresInPopulation = acresInDomain
                                            ,varAcresInPopulation = varAcresInDomain
                                            ,proportionAcresInDomain = 1
                                            ,varProportionAcresInDomain = NA
                                            ,covAcresDomainPopulation = NA
                                            ,ownershipsInPopulation = ownershipsInDomain
                                            ,varOwnershipsInPopulation = varOwnershipsInDomain
                                            ,proportionOwnershipsInDomain = 1
                                            ,varProportionOwnershipsInDomain = NA
                                            ,covOwnershipsDomainPopulation = NA))
      variableColumnsOut = rbind(variableColumnsOut, levelOut)
    }
    statesOut = rbind(statesOut, variableColumnsOut)
  }
  return(statesOut)
}