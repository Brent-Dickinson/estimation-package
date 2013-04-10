stateTables = function(variableColumns
                       ,quest
                       ,stratumInfo
                       ,populationOfInterest
                       ,nwosCycle = 2011)
{
  quest = cbind(quest, populationOfInterest = populationOfInterest)
  if(nwosCycle == 2011)
  {
    areaName = 'AC_WOOD'
    variableAcreName = areaName
    stateVariable = 'QUEST_STATE'
    ownVariable = 'OWNTYPE'
    famCodes = 1:4
    nonFamCodes = 5:6
  }
  if(nwosCycle == 2006)
  {
    areaName = 'ACRES_IN_EU'
    variableAcreName = 'ACRES_IN_STATE'
    stateVariable = 'STATECD'
    ownVariable = 'OWNER_CLASS_ADJ'
    famCodes = 45
    nonFamCodes = 41:44
  }
  quest = quest[quest[,areaName] >= 1,]
  
  # string manipulation for table names in output:
  tablesMicro = names(quest)[variableColumns]
  splitTables = tablesMicro[grepl('.', tablesMicro, fixed = T)]
  tablesMicro[grepl('.', tablesMicro, fixed = T)] = substr(splitTables, 1, regexpr('.', splitTables, fixed = T) - 1)
  
  statesOut = data.frame(NULL)
  for(i in 1:nrow(stratumInfo))
  {
    t0 = Sys.time()
    # pull necessary data for each stratum:
    stateCd = stratumInfo$state[i]
    stateName = stratumInfo$state_abbr[i]
    regionCd = stratumInfo$rpa_regionCd[i]
    regionName = stratumInfo$rpa_regionNm[i]
    areaTotal = stratumInfo$AreaAcres[i]
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
    
    # get population numbers before further reducing questSub1
    pointCountP = questSub1$POINT_COUNT
    areaIndividualP = questSub1[,areaName]
    variableAcresP = questSub1[,variableAcreName]
    variableOwnershipsP = rep(1, length(pointCountP))
    
    populationIndicator = ifelse(questSub1$populationOfInterest == 'population', 1, 0)
    populationAcresOutput = hheCore(areaTotal = areaTotal
                                    ,nTotal = nTotal
                                    ,responseRate = questSub1$responseRateUnit
                                    ,pointCount = pointCountP
                                    ,areaIndividual = areaIndividualP
                                    ,variable = variableAcresP
                                    ,domain = populationIndicator)
    acresInPopulation = populationAcresOutput$yHat
    varAcresInPopulation = populationAcresOutput$VyHat
    populationOwnershipsOutput = hheCore(areaTotal = areaTotal
                                         ,nTotal = nTotal
                                         ,responseRate = questSub1$responseRateUnit
                                         ,pointCount = pointCountP
                                         ,areaIndividual = areaIndividualP
                                         ,variable = variableOwnershipsP
                                         ,domain = populationIndicator)
    ownershipsInPopulation = populationOwnershipsOutput$yHat
    varOwnershipsInPopulation = populationOwnershipsOutput$VyHat
    
    variableColumnsOut = data.frame(NULL)
    for(j in variableColumns)
    {
      if(is.factor(questSub1[,j]) == F) stop(paste('fix make_quest_all():', names(quest)[j], 'not a factor'))
      questSub = questSub1
      # pull out -3's from sample (they were not asked that question) and create weighted item response rate:
      if(any(questSub[,j] == -3)) 
      {
        # just use shortcut equations
        dNeg3 = ifelse(questSub[,j] == -3, 0, 1)
        xNeg3 = sum(questSub$POINT_COUNT*dNeg3/questSub[,areaName])
        xBase = sum(questSub$POINT_COUNT/questSub[,areaName])
        questSub$responseRateItem = xNeg3/xBase
        questSub = questSub[questSub[,j] != -3,]
      }
      else
      {
        questSub$responseRateItem = rep(1, nrow(questSub))
      }
      
      # set up components using new questSub
      domain = questSub[,j]
      allLevels = names(summary(domain))
      pointCount = questSub$POINT_COUNT
      areaIndividual = questSub[,areaName]
      variableAcres = questSub[,variableAcreName]
      variableOwnerships = rep(1, length(pointCount))
      
      table = tablesMicro[variableColumns == j]
      levelOut = data.frame(NULL)
      for(L in 1:length(allLevels))
      {
        level = allLevels[L]
        if(level != -3)
        {
          if(nrow(questSub) == 0 & level != 'ignore')
          {
            # create a dummy for inclusion in the population of interest:
            levelOut = rbind(levelOut, data.frame(table = table
                                                  ,row = level
                                                  ,stateCd = stateCd
                                                  ,stateNm = stateName
                                                  ,regionCd = regionCd
                                                  ,regionNm = regionName
                                                  ,ownershipsInDomain = 0
                                                  ,varOwnershipsInDomain = NA
                                                  ,acresInDomain = 0
                                                  ,varAcresInDomain = NA
                                                  ,acresInDomainSpecial = NA
                                                  ,varAcresInDomainSpecial = NA
                                                  ,meanAcresInDomain = NA
                                                  ,varMeanAcresInDomain = NA
                                                  ,covAcresOwnershipsInDomain = NA
                                                  ,acresInPopulation = acresInPopulation
                                                  ,varAcresInPopulation = varAcresInPopulation
                                                  ,proportionAcresInDomain = 0
                                                  ,varProportionAcresInDomain = NA
                                                  ,covAcresDomainPopulation = NA
                                                  ,ownershipsInPopulation = ownershipsInPopulation
                                                  ,varOwnershipsInPopulation = varOwnershipsInPopulation
                                                  ,proportionOwnershipsInDomain = 0
                                                  ,varProportionOwnershipsInDomain = NA
                                                  ,covOwnershipsDomainPopulation = NA))             
          }
          else
          {
            if(level != 'ignore')
            {
              populationIndicator = ifelse(questSub$populationOfInterest == 'population', 1, 0)
              # create a dummy for each domain of interest:
              domainIndicator = ifelse(domain == level, 1, 0)
              acresOutput = hheCore(areaTotal = areaTotal
                                    ,nTotal = nTotal
                                    ,responseRate = questSub$responseRateUnit*questSub$responseRateItem
                                    ,pointCount = pointCount
                                    ,areaIndividual = areaIndividual
                                    ,variable = variableAcres
                                    ,domain = populationIndicator*domainIndicator)
              acresInDomain = acresOutput$yHat
              varAcresInDomain = acresOutput$VyHat
              ownershipsOutput = hheCore(areaTotal = areaTotal
                                         ,nTotal = nTotal
                                         ,responseRate = questSub$responseRateUnit*questSub$responseRateItem
                                         ,pointCount = pointCount
                                         ,areaIndividual = areaIndividual
                                         ,variable = variableOwnerships
                                         ,domain = populationIndicator*domainIndicator)
              ownershipsInDomain = ownershipsOutput$yHat
              varOwnershipsInDomain = ownershipsOutput$VyHat
              covAcresOwnershipsInDomain = cov1(areaTotal = areaTotal
                                                ,nTotal = nTotal
                                                ,responseRate = questSub$responseRateUnit*questSub$responseRateItem
                                                ,pointCount = pointCount
                                                ,areaIndividual = areaIndividual
                                                ,variable1 = variableAcres
                                                ,variable2 = variableOwnerships
                                                ,domain1 = populationIndicator*domainIndicator
                                                ,domain2 = populationIndicator*domainIndicator)
              meanAcresInDomain = acresInDomain/ownershipsInDomain
              varMeanAcresInDomain = varianceRatio(acresInDomain, ownershipsInDomain, varAcresInDomain, varOwnershipsInDomain, covAcresOwnershipsInDomain)
              proportionAcresInDomain = acresInDomain/acresInPopulation
              covAcresDomainPopulation = cov1(areaTotal = areaTotal
                                              ,nTotal = nTotal
                                              ,responseRate = questSub$responseRateUnit*questSub$responseRateItem
                                              ,pointCount = pointCount
                                              ,areaIndividual = areaIndividual
                                              ,variable1 = variableAcres
                                              ,variable2 = variableAcres
                                              ,domain1 = populationIndicator
                                              ,domain2 = domainIndicator)
              varProportionAcresInDomain = varianceRatio(acresInDomain, acresInPopulation, varAcresInDomain, varAcresInPopulation, covAcresDomainPopulation)
              proportionOwnershipsInDomain = ownershipsInDomain/ownershipsInPopulation
              covOwnershipsDomainPopulation = cov1(areaTotal = areaTotal
                                                   ,nTotal = nTotal
                                                   ,responseRate = questSub$responseRateUnit*questSub$responseRateItem
                                                   ,pointCount = pointCount
                                                   ,areaIndividual = areaIndividual
                                                   ,variable1 = variableOwnerships
                                                   ,variable2 = variableOwnerships
                                                   ,domain1 = domainIndicator
                                                   ,domain2 = populationIndicator)
              varProportionOwnershipsInDomain = varianceRatio(ownershipsInDomain, ownershipsInPopulation, varOwnershipsInDomain, varOwnershipsInPopulation, covOwnershipsDomainPopulation)
              
              acresInDomainSpecial = NA
              varAcresInDomainSpecial = NA
              
              if(table == "table_02__all_private")
              {
                nPf = stratumInfo$n_pf[i]
                propAcres = nPf/nTotal
                responseRate = stratumInfo$urr_fia_pf[i]
                acresInDomainSpecial = propAcres*(areaTotal/responseRate)
                varAcresInDomainSpecial = (areaTotal/responseRate)^2*(propAcres*(1 - propAcres)/(nTotal*(nTotal - 1)))
              }
              
              if(table == "table_02__fam_other" & level == "row_01_Family")
              {
                nFf = stratumInfo$n_ff[i]
                propAcres = nFf/nTotal
                responseRate = stratumInfo$urr_fia_pf[i]
                acresInDomainSpecial = propAcres*(areaTotal/responseRate)
                varAcresInDomainSpecial = (areaTotal/responseRate)^2*(propAcres*(1 - propAcres)/(nTotal*(nTotal - 1)))
              }
                            
              if(table == "table_02__fam_other" & level == "row_02_Other private")
              {
                nPfNonfam = stratumInfo$n_pf_nonfam[i]
                propAcres = nPfNonfam/nTotal
                responseRate = stratumInfo$urr_fia_pf[i]
                acresInDomainSpecial = propAcres*(areaTotal/responseRate)
                varAcresInDomainSpecial = (areaTotal/responseRate)^2*(propAcres*(1 - propAcres)/(nTotal*(nTotal - 1)))
              }

              levelOut = rbind(levelOut, data.frame(table = table
                                                    ,row = level
                                                    ,stateCd = stateCd
                                                    ,stateNm = stateName
                                                    ,regionCd = regionCd
                                                    ,regionNm = regionName
                                                    ,ownershipsInDomain = ownershipsInDomain
                                                    ,varOwnershipsInDomain = varOwnershipsInDomain
                                                    ,acresInDomain = acresInDomain
                                                    ,varAcresInDomain = varAcresInDomain
                                                    ,acresInDomainSpecial = acresInDomainSpecial
                                                    ,varAcresInDomainSpecial = varAcresInDomainSpecial
                                                    ,meanAcresInDomain = meanAcresInDomain
                                                    ,varMeanAcresInDomain = varMeanAcresInDomain
                                                    ,covAcresOwnershipsInDomain = covAcresOwnershipsInDomain
                                                    ,acresInPopulation = acresInPopulation
                                                    ,varAcresInPopulation = varAcresInPopulation
                                                    ,proportionAcresInDomain = proportionAcresInDomain
                                                    ,varProportionAcresInDomain = varProportionAcresInDomain
                                                    ,covAcresDomainPopulation = covAcresDomainPopulation
                                                    ,ownershipsInPopulation = ownershipsInPopulation
                                                    ,varOwnershipsInPopulation = varOwnershipsInPopulation
                                                    ,proportionOwnershipsInDomain = proportionOwnershipsInDomain
                                                    ,varProportionOwnershipsInDomain = varProportionOwnershipsInDomain
                                                    ,covOwnershipsDomainPopulation = covOwnershipsDomainPopulation))
            }
          }
        }
      }
      variableColumnsOut = rbind(variableColumnsOut, levelOut)
    }
    statesOut = rbind(statesOut, variableColumnsOut)
    if(i == 1)
    {
      print(paste('estimated total time:', round(nrow(stratumInfo)*(Sys.time() - t0), digits = 0), 'sec.'))
    }
  }
  return(statesOut)
}

  
    
        
          
