StateTableContinuous = function(VariableColumns
                                ,Quest
                                ,DomainOfInterest
                                ,AreaFia
                                ,NwosCycle)
{
  library(plyr)
  
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
  
  # Tack DomainOfInterest on to Quest:
  Quest = cbind(Quest, Domain = DomainOfInterest)
  
  # We can only use observations with known ownership size and type:
  Quest = Quest[Quest[,AreaName] >= 1 & 
                  Quest[,OwnVariable] > 0 &
                  is.na(Quest[,OwnVariable]) == F,]
  
  # Subset Quest and AreaFia to just family forest ownerships:
  Quest = Quest[Quest[,OwnVariable] %in% FamCodes,]
  AreaFiaSub = AreaFia[AreaFia$OwnerClass == "Family",]

  # Define empty output data.frame for stratum level estimate groupings:
  StatesOut = data.frame(NULL)
  
  # Each row in AreaFiaSub represents a stratum (usually a state).
  # We will generate estimates for each one:
  for(i in 1:nrow(AreaFiaSub))
  {
    # Pull necessary info for each stratum:
    StateCode = as.character(AreaFiaSub$StateCode[i])
    StateName = as.character(AreaFiaSub$StratumName[i])
    RegionCode = as.character(AreaFiaSub$RegionCode[i])
    RegionName = as.character(AreaFiaSub$RegionName[i])
    AreaFia_i = AreaFiaSub$Area[i]
    AreaVarianceFia_i = AreaFiaSub$AreaVariance[i]
    
    # Subset Quest to observations in stratum i:
    QuestSubP = Quest[Quest[,StateVariable] == StateCode,]
        
    # Define empty data.frame for Table-level estimate groupings:
    VariableColumnsOut = data.frame(NULL)
    
    # Characteristics of interest are levels of a factor variable.
    # Go through j factor variables from VariableColumns:
    for(j in VariableColumns)
    {
      # Further subset QuestSubP to eliminate ownerships which were not asked the question:
      QuestSub = QuestSubP[QuestSubP[,j] >= 0,]
      
      # Define Table and Row:
      Table = ifelse(grepl("table_NA", names(QuestSub)[j]), names(QuestSub)[j]
                     ,paste("table_NA__", names(QuestSub)[j]))
      Row = "row_01_Continuous variable"
      
      # Create empty rows where nrow(QuestSub) == 0
      if(nrow(QuestSub) == 0)
      {
        VariableColumnsOut = rbind(VariableColumnsOut
                                   ,data.frame(Table = Table
                                               ,Row = Row
                                               ,StateCode = StateCode
                                               ,StateName = StateName
                                               ,RegionCode = RegionCode
                                               ,RegionName = RegionName
                                               ,OwnershipsInDomain = 0
                                               ,VarOwnershipsInDomain = NA
                                               ,AcresInDomain = 0
                                               ,VarAcresInDomain = NA
                                               ,MeanAcresInDomain = NA
                                               ,VarMeanAcresInDomain = NA
                                               ,CovAcresOwnershipsInDomain = NA
                                               ,OwnershipsInPopulation = NA
                                               ,VarOwnershipsInPopulation = NA
                                               ,AcresInPopulation = NA
                                               ,VarAcresInPopulation = NA
                                               ,CovAcresDomainPopulation = NA
                                               ,CovOwnershipsDomainPopulation = NA
                                               ,PropAcInDomain = 0
                                               ,VarPropAcInDomain = NA
                                               ,PropOwnInDomain = 0
                                               ,VarPropOwnInDomain = NA
                                               ,OwnNR = NA
                                               ,vOwnNR = NA
                                               ,AcNR = NA
                                               ,vAcNR = NA
                                               ,ContinuousInDomain = 0
                                               ,VarContinuousInDomain = NA
                                               ,MeanContinuousOwn = 0
                                               ,VarMeanContinuousOwn = NA
                                               ,CovContinuousOwn = NA
                                               ,MeanContinuousAcre = 0
                                               ,VarMeanContinuousAcre = NA
                                               ,CovContinuousAcre = NA))
      }
      else
      {
        # Set up estimator components using new questSub:
        Domain = QuestSub$Domain
        PointCount = QuestSub$POINT_COUNT
        AreaIndividual = QuestSub[,AreaName]
        VariableAcres = QuestSub[,VariableAcreName]
        VariableOwnerships = rep(1, length(PointCount))
        VariableContinuous = QuestSub[,j]
        
        # Define a default item response rate of 1 in case we get fancy later:
        QuestSub$ResponseRateItem = 1
        
        # Create a dummy indicating domain of interest:
        DomainIndicator = ifelse(Domain == "domain", 1, 0)
        
        AcresOutput = HheCore(AreaFia = AreaFia_i
                              ,AreaVarianceFia = AreaVarianceFia_i
                              ,PointCount = PointCount
                              ,AreaIndividual = AreaIndividual
                              ,Variable = VariableAcres
                              ,Domain = DomainIndicator
                              ,ResponseRateItem = QuestSub$ResponseRateItem)
        AcresInDomain = AcresOutput$yHat
        VarAcresInDomain = AcresOutput$VyHat
        OwnershipsOutput = HheCore(AreaFia = AreaFia_i
                                   ,AreaVarianceFia = AreaVarianceFia_i
                                   ,PointCount = PointCount
                                   ,AreaIndividual = AreaIndividual
                                   ,Variable = VariableOwnerships
                                   ,Domain = DomainIndicator
                                   ,ResponseRateItem = QuestSub$ResponseRateItem)              
        
        OwnershipsInDomain = OwnershipsOutput$yHat
        VarOwnershipsInDomain = OwnershipsOutput$VyHat
        
        # We can get a conditional estimate of covariance between ownerships and acres for estimating the variance of mean size:
        CovAcresOwnershipsInDomain = CovCore(AreaFia = AreaFia_i
                                             ,PointCount = PointCount
                                             ,AreaIndividual = AreaIndividual
                                             ,Variable1 = VariableAcres
                                             ,Variable2 = VariableOwnerships
                                             ,Domain1 = DomainIndicator
                                             ,Domain2 = DomainIndicator)
        
        # With basic domain and population ownership and acre estimates, we can calculate estimates of mean, proportion, etc:
        MeanAcresInDomain = AcresInDomain/OwnershipsInDomain
        VarMeanAcresInDomain = VarianceRatio(AcresInDomain, OwnershipsInDomain, VarAcresInDomain, VarOwnershipsInDomain, CovAcresOwnershipsInDomain)
        PropAcInDomain = NA
        # We have no way of estimating the covariance between acres in domain and acres in population for now:
        CovAcresDomainPopulation = NA
        VarPropAcInDomain = NA
        PropOwnInDomain = NA
        # Same as above:
        CovOwnershipsDomainPopulation = NA
        VarPropOwnInDomain = NA
        
        # Now to actually get output for the continuous variable j:
        yOutput = HheCore(AreaFia = AreaFia_i
                          ,AreaVarianceFia = AreaVarianceFia_i
                          ,PointCount = PointCount
                          ,AreaIndividual = AreaIndividual
                          ,Variable = VariableContinuous
                          ,Domain = DomainIndicator
                          ,ResponseRateItem = QuestSub$ResponseRateItem)
        ContinuousInDomain = yOutput$yHat
        VarContinuousInDomain = yOutput$VyHat
        MeanContinuousOwn = ContinuousInDomain/OwnershipsInDomain
        CovContinuousOwn = CovCore(AreaFia = AreaFia_i
                                   ,PointCount = PointCount
                                   ,AreaIndividual = AreaIndividual
                                   ,Variable1 = VariableContinuous
                                   ,Variable2 = VariableOwnerships
                                   ,Domain1 = DomainIndicator
                                   ,Domain2 = DomainIndicator)
        VarMeanContinuousOwn = VarianceRatio(ContinuousInDomain
                                             ,OwnershipsInDomain
                                             ,VarContinuousInDomain
                                             ,VarOwnershipsInDomain
                                             ,CovContinuousOwn)
        MeanContinuousAcre = ContinuousInDomain/AcresInDomain
        CovContinuousAcre = CovCore(AreaFia = AreaFia_i
                                    ,PointCount = PointCount
                                    ,AreaIndividual = AreaIndividual
                                    ,Variable1 = VariableContinuous
                                    ,Variable2 = VariableAcres
                                    ,Domain1 = DomainIndicator
                                    ,Domain2 = DomainIndicator)
        VarMeanContinuousAcre = VarianceRatio(ContinuousInDomain
                                              ,AcresInDomain
                                              ,VarContinuousInDomain
                                              ,VarAcresInDomain
                                              ,CovContinuousAcre)
        
        # Now tack all that on to VariableColumnsOut data.frame:
        VariableColumnsOut = rbind(VariableColumnsOut
                                   ,data.frame(Table = Table
                                               ,Row = Row
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
                                               ,OwnershipsInPopulation = NA
                                               ,VarOwnershipsInPopulation = NA
                                               ,AcresInPopulation = NA
                                               ,VarAcresInPopulation = NA
                                               ,CovAcresDomainPopulation = CovAcresDomainPopulation
                                               ,CovOwnershipsDomainPopulation = CovOwnershipsDomainPopulation
                                               ,PropAcInDomain = PropAcInDomain
                                               ,VarPropAcInDomain = VarPropAcInDomain
                                               ,PropOwnInDomain = PropOwnInDomain
                                               ,VarPropOwnInDomain = VarPropOwnInDomain
                                               ,OwnNR = NA
                                               ,vOwnNR = NA
                                               ,AcNR = NA
                                               ,vAcNR = NA
                                               ,ContinuousInDomain = ContinuousInDomain
                                               ,VarContinuousInDomain = VarContinuousInDomain
                                               ,MeanContinuousOwn = MeanContinuousOwn
                                               ,VarMeanContinuousOwn = VarMeanContinuousOwn
                                               ,CovContinuousOwn = CovContinuousOwn
                                               ,MeanContinuousAcre = MeanContinuousAcre
                                               ,VarMeanContinuousAcre = VarMeanContinuousAcre
                                               ,CovContinuousAcre = CovContinuousAcre))
      }
    }
    StatesOut = rbind(StatesOut, VariableColumnsOut)
  }
  return(StatesOut)
}