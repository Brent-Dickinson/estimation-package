MakeAreaFia = function(datawd = "e:/brent/nwos/inputdata/")
{
  library(plyr)
  
  AreaNrs = read.csv(paste(datawd, 'forest_area_cnty_2011 nrs.csv', sep = ''), stringsAsFactors = F)
  AreaSrs = read.csv(paste(datawd, 'forest_area_cnty_2010 srs.csv', sep = ''), stringsAsFactors = F)
  AreaPnwrs = read.csv(paste(datawd, 'forest_area_cnty_2011 pnwrs.csv', sep = ''), stringsAsFactors = F)
  AreaRmrs = read.csv(paste(datawd, 'forest_area_cnty_2011 rmrs.csv', sep = ''), stringsAsFactors = F)
  AreaEstCounty = rbind(AreaNrs, AreaSrs, AreaPnwrs, AreaRmrs)    
  
  AreaEstSu = read.csv(paste(datawd, "forestarea.csv", sep = ""), stringsAsFactors = F)

  RefState = read.csv(paste(datawd, "refstate.csv", sep = ""), stringsAsFactors = F)
  RefCounty = read.csv(paste(datawd, 'RefCounty.csv', sep = ''), stringsAsFactors = F)
  
  AreaEstCounty$Ref = paste(AreaEstCounty$LOCATION_NM, AreaEstCounty$COUNTYCD)
  RefCounty$Ref = paste(RefCounty$State_Alpha, RefCounty$COUNTY)
  
  AreaEstCounty$SU = RefCounty$SU_Alpha[match(AreaEstCounty$Ref, RefCounty$Ref)]
  AreaEstCounty$StratumName = ifelse(AreaEstCounty$LOCATION_NM == "Texas" &
                                       AreaEstCounty$SU %in% c("Southeast", "Northeast")
                                     ,"Texas East"
                                     ,ifelse(AreaEstCounty$LOCATION_NM == "Texas" & 
                                               !AreaEstCounty$SU %in% c("Southeast", "Northeast")
                                             ,"Texas West"
                                             ,ifelse(AreaEstCounty$LOCATION_NM == "Oklahoma" &
                                                       AreaEstCounty$SU %in% c("Southeast", "Northeast")
                                                     ,"Oklahoma East"
                                                     ,ifelse(AreaEstCounty$LOCATION_NM == "Oklahoma" &
                                                               !AreaEstCounty$SU %in% c("Southeast", "Northeast")
                                                             ,"Oklahoma West"
                                                             ,AreaEstCounty$LOCATION_NM))))
  AreaEstCounty$StateCode = RefCounty$STATE[match(AreaEstCounty$Ref, RefCounty$Ref)]
  AreaEstCounty$StateCode[AreaEstCounty$StratumName == "Texas East"] = "48.1"
  AreaEstCounty$StateCode[AreaEstCounty$StratumName == "Texas West"] = "48.2"
  AreaEstCounty$StateCode[AreaEstCounty$StratumName == "Oklahoma East"] = "40.1"
  AreaEstCounty$StateCode[AreaEstCounty$StratumName == "Oklahoma West"] = "40.2"
  
  
  
  
  
  AreaEstSu$StratumName = ifelse(AreaEstSu$STATE == 48 &
                                       AreaEstSu$ESTIMATION_UNIT %in% 1:2
                                     ,"Texas East"
                                     ,ifelse(AreaEstSu$STATE == 48 & 
                                               !AreaEstSu$ESTIMATION_UNIT %in% 1:2
                                             ,"Texas West"
                                             ,ifelse(AreaEstSu$STATE == 40 &
                                                       AreaEstSu$ESTIMATION_UNIT == 12
                                                     ,"Oklahoma East"
                                                     ,ifelse(AreaEstSu$STATE == 40 &
                                                               AreaEstSu$ESTIMATION_UNIT != 12
                                                             ,"Oklahoma West"
                                                             ,RefState$STATE_NAME[match(AreaEstSu$STATE, RefState$STATECD)]))))
  AreaEstSu$StateCode = AreaEstSu$STATE
  AreaEstSu$StateCode[AreaEstSu$StratumName == "Texas East"] = "48.1"
  AreaEstSu$StateCode[AreaEstSu$StratumName == "Texas West"] = "48.2"
  AreaEstSu$StateCode[AreaEstSu$StratumName == "Oklahoma East"] = "40.1"
  AreaEstSu$StateCode[AreaEstSu$StratumName == "Oklahoma West"] = "40.2"
  
  
  
  
  
  
  CodeNameMatch = data.frame(Code = RefState$STATECD
                             ,Name = RefState$STATE_NAME)
  CodeNameMatch$RegionCode = RefState$RPA_REGIONCD[match(CodeNameMatch$Code, RefState$STATECD)]
  CodeNameMatch$RegionName = RefState$RPA_REGION_NAME[match(CodeNameMatch$Code, RefState$STATECD)]
  
  AreaEstStrata_11 = aggregate(AreaEstCounty$ESTIMATE
                            ,by = list(AreaEstCounty$StateCode
                                       ,AreaEstCounty$OWNER_CLASS)
                            ,FUN = sum
                               ,stringsAsFactors = F)
  AreaEstStrata_11$AreaVariance = aggregate(AreaEstCounty$VAR_OF_ESTIMATE_EQ_4_6
                                         ,by = list(AreaEstCounty$StateCode
                                                    ,AreaEstCounty$OWNER_CLASS)
                                         ,FUN = sum
                                            ,stringsAsFactors = F)[,3]
  colnames(AreaEstStrata_11) = c("StateCode", "OwnerClass", "Area", "AreaVariance")
  AreaEstStrata_11 = AreaEstStrata_11[grepl("Private-", AreaEstStrata_11$OwnerClass),]
  AreaEstStrata_11$OwnerClass[grepl("Private- Individual", AreaEstStrata_11$OwnerClass) == F & grepl("Private- ", AreaEstStrata_11$OwnerClass) == T] = "PrivateNonfam"
  AreaEstStrata_11$OwnerClass[grepl("Private- Individual", AreaEstStrata_11$OwnerClass)] = "Family"
  AreaEstStrata_11 = ddply(AreaEstStrata_11, .(StateCode, OwnerClass), numcolwise(sum))
  
  AreaEstStrata_11$StratumName = CodeNameMatch$Name[match(AreaEstStrata_11$StateCode, CodeNameMatch$Code)]
  AreaEstStrata_11$RegionCode = CodeNameMatch$RegionCode[match(AreaEstStrata_11$StateCode, CodeNameMatch$Code)]
  AreaEstStrata_11$RegionName = CodeNameMatch$RegionName[match(AreaEstStrata_11$StateCode, CodeNameMatch$Code)]
  
  
  AreaEstSuCombined = AreaEstSu[,c("StateCode", "FAMILY", "VAR_FAMILY", "CORP_FORIND", "VAR_CORP_FORIND", "CORP_OTHER", "VAR_CORP_OTHER", "NONCORP_OTHER", "VAR_NONCORP_OTHER")]
  AreaEstSuCombined = data.frame(StateCode = rep(AreaEstSuCombined$StateCode, 4)
                                 ,Area = c(AreaEstSuCombined$FAMILY, AreaEstSuCombined$CORP_FORIND, AreaEstSuCombined$CORP_OTHER, AreaEstSuCombined$NONCORP_OTHER)
                                 ,AreaVariance = c(AreaEstSuCombined$VAR_FAMILY, AreaEstSuCombined$VAR_CORP_FORIND, AreaEstSuCombined$VAR_CORP_OTHER, AreaEstSuCombined$VAR_NONCORP_OTHER)
                                 ,OwnerClass = c(rep("Family", nrow(AreaEstSuCombined))
                                                 ,rep("PrivateNonfam", nrow(AreaEstSuCombined)*3)))
  
  AreaStrata_06 = aggregate(AreaEstSuCombined$Area
                            ,by = list(AreaEstSuCombined$StateCode, AreaEstSuCombined$OwnerClass)
                            ,FUN = sum
                            ,stringsAsFactors = F)
  AreaStrata_06$AreaVariance = aggregate(AreaEstSuCombined$AreaVariance
                                         ,by = list(AreaEstSuCombined$StateCode, AreaEstSuCombined$OwnerClass)
                                         ,FUN = sum
                                         ,stringsAsFactors = F)[,3]
  colnames(AreaStrata_06) = c("StateCode", "OwnerClass", "Area", "AreaVariance")
  
  
  AreaEstStrata_06 = AreaStrata_06[order(as.numeric(as.character(AreaStrata_06$StateCode)), AreaStrata_06$OwnerClass),]
  AreaEstStrata_06$StratumName = CodeNameMatch$Name[match(AreaEstStrata_06$StateCode, CodeNameMatch$Code)]
  AreaEstStrata_06$RegionCode = CodeNameMatch$RegionCode[match(AreaEstStrata_06$StateCode, CodeNameMatch$Code)]
  AreaEstStrata_06$RegionName = CodeNameMatch$RegionName[match(AreaEstStrata_06$StateCode, CodeNameMatch$Code)]
  
  AreaEstStrata_06 = AreaEstStrata_06[is.na(AreaEstStrata_06$RegionCode) == F,]
  if(any(!AreaEstStrata_11$StateCode %in% AreaEstStrata_06$StateCode))
  {
    missing = unique(as.numeric(as.character(AreaEstStrata_06$StateCode[!AreaEstStrata_06$StateCode %in% AreaEstStrata_11$StateCode])))
    AreaEstStrata_11 = rbind(AreaEstStrata_11, AreaEstStrata_06[AreaEstStrata_06$StateCode %in% missing,])
  }
  AreaEstStrata_11 = AreaEstStrata_11[order(as.numeric(as.character(AreaEstStrata_11$StateCode)), AreaEstStrata_11$OwnerClass),]
  
  return(list(AreaEstStrata_11, AreaEstStrata_06))
}