StateTables = function(VariableColumns
                       ,Quest
                       ,PopulationOfInterest
                       ,AreaFia
                       ,NwosCycle
                       ,DomainOfInterest = 1)
{
  library(plyr)
  
  # There should only ever be 1 of 3 populations of interest:
  if(!PopulationOfInterest %in% c("Family", "AllPrivate", "Nonfam")) stop("PopulationOfInterest needs to be one of 'Family' or 'AllPrivate' or 'Nonfam'")
  
  # Tack on DomainOfInterest:
  if(length(DomainOfInterest) == 1)
  {
  }
  else
  {
    DomainOfInterest = ifelse(as.character(DomainOfInterest) == "domain", 1, 0)
  }
  
  Quest = cbind(Quest, DomainOfInterest = DomainOfInterest)
  
  # Because format of Quest depends on the NwosCycle, several variables need to be defined:
  if(NwosCycle == 2011)
  {
    AreaName = 'AC_WOOD'
    VariableAcreName = AreaName
    StateVariable = 'QUEST_STATE'
    OwnVariable = 'OWNTYPE'
    FamCodes = 1:4
    NonFamCodes = 5:6
  }
  if(NwosCycle == 2006)
  {
    AreaName = 'ACRES_IN_STATE'
    VariableAcreName = 'ACRES_IN_STATE'
    StateVariable = 'STATECD'
    OwnVariable = 'OWNER_CLASS_ADJ'
    FamCodes = 45
    NonFamCodes = 41:44
  }
  
  # We can only use observations with known ownership size and type:
  Quest = Quest[Quest[,AreaName] >= 1 & 
                  Quest[,OwnVariable] > 0 &
                  is.na(Quest[,OwnVariable]) == F,]
  
  # Subset Quest and AreaFia to just family forest ownerships if that is the population of interest:
  if(PopulationOfInterest == "Family")
  {
    Quest = Quest[Quest[,OwnVariable] %in% FamCodes,]
    AreaFiaSub = AreaFia[AreaFia$OwnerClass == "Family",]
  }
  # Otherwise it means we're doing tables 2-3 and will be using all 3 populations of interest:
  else
  {
    AreaFiaSub = ddply(AreaFia, .(StateCode = as.numeric(as.character(StateCode))), numcolwise(sum))
    AreaFiaSub$StratumName = AreaFia$StratumName[match(AreaFiaSub$StateCode, AreaFia$StateCode)]
    AreaFiaSub$RegionName = AreaFia$RegionName[match(AreaFiaSub$StateCode, AreaFia$StateCode)]
    AreaFiaFam = AreaFia[AreaFia$OwnerClass == "Family",]
    AreaFiaNonfam = AreaFia[AreaFia$OwnerClass == "PrivateNonfam",]
  }
  
  # String manipulation for Table and Row names in output:
  TablesMicro = names(Quest)[VariableColumns]
  SplitTables = TablesMicro[grepl('.', TablesMicro, fixed = T)]
  TablesMicro[grepl('.', TablesMicro, fixed = T)] = substr(SplitTables, 1, regexpr('.', SplitTables, fixed = T) - 1)
  
  # Define empty output data.frame for stratum level estimate groupings:
  StatesOut = data.frame(NULL)
  
  # Each row in AreaFiaSub represents a stratum (usually a state).
  # We will generate estimates for each one:
  for(i in 1:nrow(AreaFiaSub))
  {
    # For estimating total time of calculations:
    t0 = Sys.time()
    
    # Pull necessary info for each stratum:
    StateCode = as.character(AreaFiaSub$StateCode[i])
    StateName = as.character(AreaFiaSub$StratumName[i])
    RegionCode = as.character(AreaFiaSub$RegionCode[i])
    RegionName = as.character(AreaFiaSub$RegionName[i])
    AreaFia_i = AreaFiaSub$Area[i]
    AreaVarianceFia_i = AreaFiaSub$AreaVariance[i]
    
    # If family is not the population of interest, it meas we're doing tables 2-3.
    # We'll need to define FIA-estimated areas for each of the 3 populatoins:
    if(PopulationOfInterest != "Family")
    {
      AreaFiaNonfam_i = AreaFiaNonfam$Area[i]
      AreaVarianceFiaNonfam_i = AreaFiaNonfam$AreaVariance[i]
      AreaFiaFam_i = AreaFiaFam$Area[i]
      AreaVarianceFiaFam_i = AreaFiaFam$AreaVariance[i]
    }
    
    # Subset Quest to observations in stratum i:
    QuestSubP = Quest[Quest[,StateVariable] == StateCode,]

    # Define empty data.frame for Table-level estimate groupings:
    VariableColumnsOut = data.frame(NULL)
    
    # Characteristics of interest are levels of a factor variable.
    # Go through j factor variables from VariableColumns:
    for(j in VariableColumns)
    {
      # It needs to be a factor:
      if(is.factor(QuestSubP[,j]) == F) stop(paste('fix makeQuestAll():', names(Quest)[j], 'not a factor'))
      
      # Further subset QuestSubP to eliminate ownerships which were not asked the question:
      QuestSub = QuestSubP[QuestSubP[,j] != -3 & QuestSubP$DomainOfInterest != -3,]
      
      # Get population estimates. These will be different for different variables because of the -3 subsetting.
      # Set up estimator components using new questSub:
      Domain = QuestSub[,j]
      PointCount = QuestSub$POINT_COUNT
      AreaIndividual = QuestSub[,AreaName]
      VariableAcres = QuestSub[,VariableAcreName]
      VariableOwnerships = rep(1, length(PointCount))
            
      # Hhecore is the basic HHE function
      PopulationAcresOutput = HheCore(AreaFia = AreaFia_i
                                      ,AreaVarianceFia = AreaVarianceFia_i
                                      ,PointCount = PointCount
                                      ,AreaIndividual = AreaIndividual
                                      ,Variable = VariableAcres
                                      ,Domain = rep(1, length(PointCount)))
      AcresInPopulation = PopulationAcresOutput$yHat
      VarAcresInPopulation = PopulationAcresOutput$VyHat
      PopulationOwnershipsOutput = HheCore(AreaFia = AreaFia_i
                                           ,AreaVarianceFia = AreaVarianceFia_i
                                           ,PointCount = PointCount
                                           ,AreaIndividual = AreaIndividual
                                           ,Variable = VariableOwnerships
                                           ,Domain = rep(1, length(PointCount)))
      OwnershipsInPopulation = PopulationOwnershipsOutput$yHat
      VarOwnershipsInPopulation = PopulationOwnershipsOutput$VyHat
      
      # Define the Table of estimate groupings from string manipulation above:
      Table = TablesMicro[VariableColumns == j]
      
      # Define a vector of all the factor levels of the factor variable j:
      AllLevels = names(summary(Domain))
      
      # For each level of the jth factor variable, we need acre and ownership estimates.
      # Define empty data.frame for Row-level estimates:
      LevelOut = data.frame(NULL)
      
      # Go through L levels of the jth factor variable:
      for(L in 1:length(AllLevels))
      {
        # Define the particular level L of factor variable j:
        Level = AllLevels[L]
        
        # We won't generate estimates for those not asked the question:
        if(Level != -3)
        {
          # If the level is "ignore", we aren't interested in estimates (see MakeQuestAll).
          # In some cases, nobody will have answered the question with level L.
          # In that case we still want a Row so that Tables match up, but the estimate is 0:
          if(nrow(QuestSub) == 0 & Level != 'ignore')
          {
            LevelOut = rbind(LevelOut, data.frame(Table = Table
                                                  ,Row = Level
                                                  ,StateCode = StateCode
                                                  ,StateName = StateName
                                                  ,RegionCode = RegionCode
                                                  ,RegionName = RegionName
                                                  ,OwnershipsInDomain = 0
                                                  ,VarOwnershipsInDomain = NA
                                                  ,AcresInDomain = 0
                                                  ,VarAcresInDomain = NA
                                                  ,MeanAcresInDomain = 0
                                                  ,VarMeanAcresInDomain = NA
                                                  ,CovAcresOwnershipsInDomain = NA
                                                  ,OwnershipsInPopulation = OwnershipsInPopulation
                                                  ,VarOwnershipsInPopulation = VarOwnershipsInPopulation
                                                  ,AcresInPopulation = AcresInPopulation
                                                  ,VarAcresInPopulation = VarAcresInPopulation
                                                  ,CovAcresDomainPopulation = NA
                                                  ,CovOwnershipsDomainPopulation = NA
                                                  ,OwnNR = 0
                                                  ,vOwnNR = NA
                                                  ,AcNR = 0
                                                  ,vAcNR = NA
                                                  ,PropOwnInDomain = NA
                                                  ,VarPropOwnInDomain = NA
                                                  ,PropAcInDomain = NA
                                                  ,VarPropAcInDomain = NA
                                                  ,ContinuousInDomain = NA
                                                  ,VarContinuousInDomain = NA
                                                  ,MeanContinuousOwn = NA
                                                  ,VarMeanContinuousOwn = NA
                                                  ,CovContinuousOwn = NA
                                                  ,MeanContinuousAcre = NA
                                                  ,VarMeanContinuousAcre = NA
                                                  ,CovContinuousAcre = NA))             
          }
          
          # Otherwise, we'll proceed with regular estimation:
          else
          {
            # Again, we aren't interested in level "ignore" (see MakeQuestAll):
            if(Level != 'ignore')
            {
              # Define a default item response rate of 1 in case we get fancy later:
              QuestSub$ResponseRateItem = 1
              
              # Create a dummy indicating where factor variable j is equal to level L:
              DomainIndicator = ifelse(Domain == Level, 1, 0)
              DomainSub = QuestSub$DomainOfInterest
              
              # For Tables 2-3, we have to do some awkward maneuvering because we're utilizing 3 different FIA area estimates.
              # For the case of all private acres and ownerships:
              if(Table == "table_02__ownership_category" & Level == "row_03_Total Private")
              {
                AcresInDomain = AcresInPopulation
                VarAcresInDomain = VarAcresInPopulation
                OwnershipsOutput = HheCore(AreaFia = AreaFia_i
                                           ,AreaVarianceFia = AreaVarianceFia_i
                                           ,PointCount = PointCount
                                           ,AreaIndividual = AreaIndividual
                                           ,Variable = VariableOwnerships
                                           ,Domain = DomainIndicator*DomainSub
                                           ,ResponseRateItem = QuestSub$ResponseRateItem)              
              }
              # For the case Table 2, family:
              if(Table == "table_02__ownership_category" & Level == "row_01_Family")
              {
                AcresInDomain = AreaFiaFam_i
                VarAcresInDomain = AreaVarianceFiaFam_i
                OwnershipsOutput = HheCore(AreaFia = AreaFiaFam_i
                                           ,AreaVarianceFia = AreaVarianceFiaFam_i
                                           ,PointCount = PointCount[Domain == Level]
                                           ,AreaIndividual = AreaIndividual[Domain == Level]
                                           ,Variable = VariableOwnerships[Domain == Level]
                                           ,Domain = DomainIndicator[Domain == Level]*DomainSub[Domain == Level]
                                           ,ResponseRateItem = QuestSub$ResponseRateItem[Domain == Level])              
              }
              # For the case of Table 2, non-family other private:
              if(Table == "table_02__ownership_category" & Level == "row_02_Other private")
              {
                AcresInDomain = AreaFiaNonfam_i
                VarAcresInDomain = AreaVarianceFiaNonfam_i
                OwnershipsOutput = HheCore(AreaFia = AreaFiaNonfam_i
                                           ,AreaVarianceFia = AreaVarianceFiaNonfam_i
                                           ,PointCount = PointCount[Domain == Level]
                                           ,AreaIndividual = AreaIndividual[Domain == Level]
                                           ,Variable = VariableOwnerships[Domain == Level]
                                           ,Domain = DomainIndicator[Domain == Level]*DomainSub[Domain == Level]
                                           ,ResponseRateItem = QuestSub$ResponseRateItem[Domain == Level])              
              }
              
              # For the vast marjority of estimation (ie, Tables 4-33) the basic acres and ownerships output is:
              if(grepl("table_02", Table) == F)
              {
                AcresOutput = HheCore(AreaFia = AreaFia_i
                                      ,AreaVarianceFia = AreaVarianceFia_i
                                      ,PointCount = PointCount
                                      ,AreaIndividual = AreaIndividual
                                      ,Variable = VariableAcres
                                      ,Domain = DomainIndicator*DomainSub
                                      ,ResponseRateItem = QuestSub$ResponseRateItem)
                AcresInDomain = AcresOutput$yHat
                VarAcresInDomain = AcresOutput$VyHat
                OwnershipsOutput = HheCore(AreaFia = AreaFia_i
                                           ,AreaVarianceFia = AreaVarianceFia_i
                                           ,PointCount = PointCount
                                           ,AreaIndividual = AreaIndividual
                                           ,Variable = VariableOwnerships
                                           ,Domain = DomainIndicator*DomainSub
                                           ,ResponseRateItem = QuestSub$ResponseRateItem)              
              }
              OwnershipsInDomain = OwnershipsOutput$yHat
              VarOwnershipsInDomain = OwnershipsOutput$VyHat
              
              # We can get a conditional estimate of covariance between ownerships and acres for estimating the variance of mean size:
              CovAcresOwnershipsInDomain = CovCore(AreaFia = AreaFia_i
                                                   ,PointCount = PointCount
                                                   ,AreaIndividual = AreaIndividual
                                                   ,Variable1 = VariableAcres
                                                   ,Variable2 = VariableOwnerships
                                                   ,Domain1 = DomainIndicator*DomainSub
                                                   ,Domain2 = DomainIndicator*DomainSub)
              
              # With basic domain and population ownership and acre estimates, we can calculate estimates of mean, proportion, etc:
              MeanAcresInDomain = AcresInDomain/OwnershipsInDomain
              VarMeanAcresInDomain = VarianceRatio(AcresInDomain, OwnershipsInDomain, VarAcresInDomain, VarOwnershipsInDomain, CovAcresOwnershipsInDomain)
              ProportionAcresInDomain = AcresInDomain/AcresInPopulation
              # We have no way of estimating the covariance between acres in domain and acres in population for now:
              CovAcresDomainPopulation = 0
              VarProportionAcresInDomain = VarianceRatio(AcresInDomain, AcresInPopulation, VarAcresInDomain, VarAcresInPopulation, CovAcresDomainPopulation)
              ProportionOwnershipsInDomain = OwnershipsInDomain/OwnershipsInPopulation
              # Same as above:
              CovOwnershipsDomainPopulation = 0
              VarProportionOwnershipsInDomain = VarianceRatio(OwnershipsInDomain, OwnershipsInPopulation, VarOwnershipsInDomain, VarOwnershipsInPopulation, CovOwnershipsDomainPopulation)
              
              # Now tack all that on to LevelOut data.frame:
              LevelOut = rbind(LevelOut, data.frame(Table = Table
                                                    ,Row = Level
                                                    ,StateCode = StateCode
                                                    ,StateName = StateName
                                                    ,RegionCode = RegionCode
                                                    ,RegionName = RegionName
                                                    ,OwnershipsInDomain = OwnershipsInDomain
                                                    ,VarOwnershipsInDomain = VarOwnershipsInDomain
                                                    ,AcresInDomain = AcresInDomain
                                                    ,VarAcresInDomain = VarAcresInDomain
                                                    ,MeanAcresInDomain = MeanAcresInDomain
                                                    ,VarMeanAcresInDomain = VarMeanAcresInDomain
                                                    ,CovAcresOwnershipsInDomain = CovAcresOwnershipsInDomain
                                                    ,OwnershipsInPopulation = OwnershipsInPopulation
                                                    ,VarOwnershipsInPopulation = VarOwnershipsInPopulation
                                                    ,AcresInPopulation = AcresInPopulation
                                                    ,VarAcresInPopulation = VarAcresInPopulation
                                                    ,CovAcresDomainPopulation = CovAcresDomainPopulation
                                                    ,CovOwnershipsDomainPopulation = CovOwnershipsDomainPopulation
                                                    ,OwnNR = 0
                                                    ,vOwnNR = 0
                                                    ,AcNR = 0
                                                    ,vAcNR = 0
                                                    ,PropOwnInDomain = 0
                                                    ,VarPropOwnInDomain = 0
                                                    ,PropAcInDomain = 0
                                                    ,VarPropAcInDomain = 0
                                                    ,ContinuousInDomain = NA
                                                    ,VarContinuousInDomain = NA
                                                    ,MeanContinuousOwn = NA
                                                    ,VarMeanContinuousOwn = NA
                                                    ,CovContinuousOwn = NA
                                                    ,MeanContinuousAcre = NA
                                                    ,VarMeanContinuousAcre = NA
                                                    ,CovContinuousAcre = NA))
              if(any(grepl("row_99", LevelOut$Row)) == T)
              {
                RowNR = LevelOut[grepl("row_99", LevelOut$Row),]
                x = 1
              }
              else
              {
                RowNR = data.frame(OwnershipsInDomain = 0
                                   ,VarOwnershipsInDomain = 0
                                   ,AcresInDomain = 0
                                   ,VarAcresInDomain = 0)
                x = 0
              }
              LevelOut$OwnNR = LevelOut$OwnershipsInPopulation - RowNR$OwnershipsInDomain
              LevelOut$vOwnNR = LevelOut$VarOwnershipsInPopulation + LevelOut$VarOwnershipsInDomain - 2*LevelOut$CovOwnershipsDomainPopulation*x
              LevelOut$AcNR = LevelOut$AcresInPopulation - RowNR$AcresInDomain
              LevelOut$vAcNR = LevelOut$VarAcresInPopulation + LevelOut$VarAcresInDomain - 2*LevelOut$CovAcresDomainPopulation*x
              LevelOut$PropOwnInDomain = LevelOut$OwnershipsInDomain/LevelOut$OwnNR
              LevelOut$VarPropOwnInDomain = VarianceRatio(LevelOut$OwnershipsInDomain
                                                          ,LevelOut$OwnNR
                                                          ,LevelOut$VarOwnershipsInDomain
                                                          ,LevelOut$vOwnNR
                                                          ,LevelOut$CovOwnershipsDomainPopulation)
              LevelOut$PropAcInDomain = LevelOut$AcresInDomain/LevelOut$AcNR
              LevelOut$VarPropAcInDomain = VarianceRatio(LevelOut$AcresInDomain
                                                         ,LevelOut$AcNR
                                                         ,LevelOut$VarAcresInDomain
                                                         ,LevelOut$vAcNR
                                                         ,LevelOut$CovAcresDomainPopulation)
            }
          }
        }
      }
      VariableColumnsOut = rbind(VariableColumnsOut, LevelOut)
    }
    StatesOut = rbind(StatesOut, VariableColumnsOut)
    if(i == 1)
    {
      print(paste('estimated total time:', round(nrow(AreaFia)*(Sys.time() - t0), digits = 0), 'sec.'))
    }
  }
  return(StatesOut)
}





