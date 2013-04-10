TweakQuest = function(datawd = "e:/brent/nwos/inputdata/")
{
  ## (2a): read in the two separate quest files (???) and bind POINT_COUNT to the right quest file:
  questExtra = read.csv(paste(datawd, 'delete/quest2011a.csv', sep = ''), header = T, stringsAsFactors = F)
  quest_11 = read.csv(paste(datawd, 'delete/quest2011b.csv', sep = ''), stringsAsFactors = F)
  quest_11$POINT_COUNT = questExtra$POINT_COUNT
  quest_11$county = questExtra$county
  quest_11$POINT_COUNT[is.na(quest_11$POINT_COUNT)] = 1
  quest_11 = quest_11[quest_11$AC_WOOD >= 1 & quest_11$OWNTYPE > 0,]
  quest_06 = read.csv(paste(datawd, 'delete/quest2006.csv', sep = ''), stringsAsFactors = F)
  quest_06 = quest_06[quest_06$ACRES_IN_EU >= 1,]
  # Remove NA OWNER_CLASS_ADJ (we have no way to use those observations in estimation)
  quest_06 = quest_06[is.na(quest_06$OWNER_CLASS_ADJ) == F,]
  
  ## (2b): CT intensification issue: pull in CT point counts
  # to remove intensified sample:
  int = read.csv(paste(datawd, 'RefCtIntensified.csv', sep = ''),stringsAsFactors = F)
  int = int[,2]
  quest_11 = quest_11[-which(quest_11$QUEST_NUMBER %in% int),]
  
  ## Bring in the reference files for counties and states:
  refCounty = read.csv(paste(datawd, 'RefCounty.csv', sep = ''), stringsAsFactors = F)
  refState = read.csv(paste(datawd, 'RefState.csv', sep = ''), stringsAsFactors = F)

  # Re-code refCounty$STATE for Texas and Oklahoma into separate East and West strata:
  refCounty$STATE[refCounty$STATE == 40 & refCounty$NWOS_SU_Alpha %in% c('Northeast', 'Southeast')] = '40.1'
  refCounty$STATE[refCounty$STATE == 40 & !refCounty$NWOS_SU_Alpha %in% c('Northeast', 'Southeast')] = '40.2'
  refCounty$STATE[refCounty$STATE == 48 & refCounty$NWOS_SU_Alpha %in% c('Northeast', 'Southeast')] = '48.1'
  refCounty$STATE[refCounty$STATE == 48 & !refCounty$NWOS_SU_Alpha %in% c('Northeast', 'Southeast')] = '48.2'
  
  ## Go back to the quest and points data.frames and re-code STATECD for TX and OK.
  eastOkCounties = refCounty$COUNTY[refCounty$STATE == 40.1]  
  eastTxCounties = refCounty$COUNTY[refCounty$STATE == 48.1]
  quest_11$QUEST_STATE[quest_11$QUEST_STATE == 48 & quest_11$county %in% eastTxCounties] = "48.1"
  quest_11$QUEST_STATE[quest_11$QUEST_STATE == 48 & !quest_11$county %in% eastTxCounties] = "48.2"
  quest_11$QUEST_STATE[quest_11$QUEST_STATE == 40 & quest_11$county %in% eastOkCounties] = "40.1"
  quest_11$QUEST_STATE[quest_11$QUEST_STATE == 40 & !quest_11$county %in% eastOkCounties] = "40.2"
  quest_06$STATECD[quest_06$STATECD == 48 & quest_06$NWOS_EU %in% 1:2] = "48.1"
  quest_06$STATECD[quest_06$STATECD == 48 & !quest_06$NWOS_EU %in% 1:2] = "48.2"
  quest_06$STATECD[quest_06$STATECD == 40 & quest_06$NWOS_EU == 12] = "40.1"
  quest_06$STATECD[quest_06$STATECD == 40 & !quest_06$NWOS_EU == 12] = "40.2"
  
  dataList = list(quest_11 = quest_11
                  ,quest_06 = quest_06
                  ,refState = refState)
  dataNames = names(dataList)
  for(i in 1:length(dataList))
  {
    assign(dataNames[i], dataList[[i]], envir = .GlobalEnv)
  }
}
