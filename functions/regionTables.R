RegionTables = function(StateTablesOutput, Regions = 'default')
{
  if(Regions == 'default')
  {
    StateTablesOutput = StateTablesOutput
  }
  if(Regions == 'Comparison')
  {
    StateTablesOutput = StateTablesOutput[!as.character(StateTablesOutput$StateName) %in% c('Alaska', 'Oklahoma West', 'Nevada', 'Wyoming', 'Hawaii'),]
  }
  require(plyr)
  US = ddply(StateTablesOutput, .(Table, Row), numcolwise(sum), na.rm = T)
  US = cbind(US[,1:2], StateCode = NA, StateName = NA, RegionCode = NA, RegionName = 'US', US[,3:ncol(US)])
  US$MeanAcresInDomain = US$AcresInDomain/US$OwnershipsInDomain
  US$VarMeanAcresInDomain = VarianceRatio(US$AcresInDomain, US$OwnershipsInDomain, US$VarAcresInDomain, US$VarOwnershipsInDomain, US$CovAcresOwnershipsInDomain)
  US$PropAcInDomain = US$AcresInDomain/US$AcNR
  US$VarPropAcInDomain = VarianceRatio(US$AcresInDomain, US$AcNR, US$VarAcresInDomain, US$vAcNR, US$CovAcresDomainPopulation)
  US$PropOwnInDomain = US$OwnershipsInDomain/US$OwnNR
  US$VarPropOwnInDomain = VarianceRatio(US$OwnershipsInDomain, US$OwnNR, US$VarOwnershipsInDomain, US$vOwnNR, US$CovOwnershipsDomainPopulation)
  US$MeanContinuousOwn = ifelse(grepl("Continuous", US$Row) == F, NA, US$ContinuousInDomain/US$OwnershipsInDomain)
  US$VarMeanContinuousOwn = ifelse(grepl("Continuous", US$Row) == F, NA, VarianceRatio(US$ContinuousInDomain, US$OwnershipsInDomain, US$VarContinuousInDomain, US$VarOwnershipsInDomain, US$CovContinuousOwn))
  US$MeanContinuousAcre = ifelse(grepl("Continuous", US$Row) == F, NA, US$ContinuousInDomain/US$AcresInDomain)
  US$VarMeanContinuousOwn = ifelse(grepl("Continuous", US$Row) == F, NA, VarianceRatio(US$ContinuousInDomain, US$AcresInDomain, US$VarContinuousInDomain, US$VarAcresInDomain, US$CovContinuousAcre))
  
  RegionCodes = unique(StateTablesOutput$RegionCode)
  RegionNames = unique(StateTablesOutput$RegionName)
  for(R in 1:length(RegionCodes))
  {
    RegionCode_R = RegionCodes[R]
    RegionName_R = RegionNames[R]
    RegionRoutput = StateTablesOutput[StateTablesOutput$RegionCode == RegionCode_R,]
    RegionR = ddply(RegionRoutput, .(Table, Row), numcolwise(sum), na.rm = T)
    RegionR = cbind(RegionR[,1:2], StateCode = NA, StateName = NA, RegionCode = RegionCode_R, RegionName = RegionName_R, RegionR[,3:ncol(RegionR)])
    RegionR$MeanAcresInDomain = RegionR$AcresInDomain/RegionR$OwnershipsInDomain
    RegionR$VarMeanAcresInDomain = VarianceRatio(RegionR$AcresInDomain, RegionR$OwnershipsInDomain, RegionR$VarAcresInDomain, RegionR$VarOwnershipsInDomain, RegionR$CovAcresOwnershipsInDomain)
    RegionR$PropAcInDomain = RegionR$AcresInDomain/RegionR$AcNR
    RegionR$VarPropAcInDomain = VarianceRatio(RegionR$AcresInDomain, RegionR$AcNR, RegionR$VarAcresInDomain, RegionR$vAcNR, RegionR$CovAcresDomainPopulation)
    RegionR$PropOwnInDomain = RegionR$OwnershipsInDomain/RegionR$OwnNR
    RegionR$VarPropOwnInDomain = VarianceRatio(RegionR$OwnershipsInDomain, RegionR$OwnNR, RegionR$VarOwnershipsInDomain, RegionR$vOwnNR, RegionR$CovOwnershipsDomainPopulation)
    RegionR$MeanContinuousOwn = ifelse(grepl("Continuous", RegionR$Row) == F, NA, RegionR$ContinuousInDomain/RegionR$OwnershipsInDomain)
    RegionR$VarMeanContinuousOwn = ifelse(grepl("Continuous", RegionR$Row) == F, NA, VarianceRatio(RegionR$ContinuousInDomain, RegionR$OwnershipsInDomain, RegionR$VarContinuousInDomain, RegionR$VarOwnershipsInDomain, RegionR$CovContinuousOwn))
    RegionR$MeanContinuousAcre = ifelse(grepl("Continuous", RegionR$Row) == F, NA, RegionR$ContinuousInDomain/RegionR$AcresInDomain)
    RegionR$VarMeanContinuousOwn = ifelse(grepl("Continuous", RegionR$Row) == F, NA, VarianceRatio(RegionR$ContinuousInDomain, RegionR$AcresInDomain, RegionR$VarContinuousInDomain, RegionR$VarAcresInDomain, RegionR$CovContinuousAcre))
    
    US = rbind(US, RegionR)
  }
  US = US[US$OwnershipsInDomain > 0,]
  return(US)
}