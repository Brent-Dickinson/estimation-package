regionTables = function(stateTablesOutput, regions = 'default')
{
  if(regions == 'default')
  {
    stateTablesOutput = stateTablesOutput
  }
  if(regions == 'stupid.temp.comparison.to.2006')
  {
    stateTablesOutput = stateTablesOutput[!stateTablesOutput$stateNm %in% c('AK', 'W.OK', 'W.TX', 'NV'),]
  }
  tableRows = paste(stateTablesOutput$table, stateTablesOutput$row, sep = '@')
  includeCols = names(stateTablesOutput)[grepl('mean', names(stateTablesOutput), ignore.case = T) == F &
                                           grepl('proportion', names(stateTablesOutput), ignore.case = T) == F & 
                                           (grepl('owner', names(stateTablesOutput), ignore.case = T) |
                                           grepl('acre', names(stateTablesOutput), ignore.case = T))]
  excludeCols = names(stateTablesOutput)[!names(stateTablesOutput) %in% includeCols]
  
  require(plyr)
  US = ddply(stateTablesOutput, .(table, row), numcolwise(sum))
  US = cbind(US[,1:2], stateCd = NA, stateNm = NA, regionCd = NA, regionNm = 'US', US[,4:ncol(US)])
  uniqueTableRows = unique(tableRows)
  US$meanAcresInDomain = US$acresInDomain/US$ownershipsInDomain
  US$varMeanAcresInDomain = varianceRatio(US$acresInDomain, US$ownershipsInDomain, US$varAcresInDomain, US$varOwnershipsInDomain, US$covAcresOwnershipsInDomain)
  US$proportionAcresInDomain = US$acresInDomain/US$acresInPopulation
  US$varProportionAcresInDomain = varianceRatio(US$acresInDomain, US$acresInPopulation, US$varAcresInDomain, US$varAcresInPopulation, US$covAcresDomainPopulation)
  US$proportionOwnershipsInDomain = US$ownershipsInDomain/US$ownershipsInPopulation
  US$varProportionOwnershipsInDomain = varianceRatio(US$ownershipsInDomain, US$ownershipsInPopulation, US$varOwnershipsInDomain, US$varOwnershipsInPopulation, US$covOwnershipsDomainPopulation)
  
  regionsCd = unique(stateTablesOutput$regionCd)
  regionsNm = unique(stateTablesOutput$regionNm)
  for(R in 1:length(regionsCd))
  {
    regionCd_R = regionsCd[R]
    regionNm_R = regionsNm[R]
    regionRoutput = stateTablesOutput[stateTablesOutput$regionCd == regionCd_R,]
    tableRows = paste(regionRoutput$table, regionRoutput$row, sep = '@')
    uniqueTableRows = unique(tableRows)
    regionR = ddply(regionRoutput, .(table, row), numcolwise(sum))
    regionR = cbind(regionR[,1:2], stateCd = NA, stateNm = NA, regionCd = regionCd_R, regionNm = regionNm_R, regionR[,4:ncol(regionR)])
    regionR$meanAcresInDomain = regionR$acresInDomain/regionR$ownershipsInDomain
    regionR$varMeanAcresInDomain = varianceRatio(regionR$acresInDomain, regionR$ownershipsInDomain, regionR$varAcresInDomain, regionR$varOwnershipsInDomain, regionR$covAcresOwnershipsInDomain)
    regionR$proportionAcresInDomain = regionR$acresInDomain/regionR$acresInPopulation
    regionR$varProportionAcresInDomain = varianceRatio(regionR$acresInDomain, regionR$acresInPopulation, regionR$varAcresInDomain, regionR$varAcresInPopulation, regionR$covAcresDomainPopulation)
    regionR$proportionOwnershipsInDomain = regionR$ownershipsInDomain/regionR$ownershipsInPopulation
    regionR$varProportionOwnershipsInDomain = varianceRatio(regionR$ownershipsInDomain, regionR$ownershipsInPopulation, regionR$varOwnershipsInDomain, regionR$varOwnershipsInPopulation, regionR$covOwnershipsDomainPopulation)
    US = rbind(US, regionR)
  }
  return(US)
}