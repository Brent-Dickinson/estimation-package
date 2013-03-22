
## @knitr pointsQuest
source('data setup/makePointsQuest.R')
makePointsQuest(datawd = "e:/brent/nwos/inputdata/")


## @knitr functions
functions = list("makeQuestAll"
                 ,"makeNs"
                 ,"stateTables"
                 ,"regionTables"
                 ,"hheCore"
                 ,"cov1"
                 ,"varianceRatio"
                 ,"varDifferenceRatios"
                 ,"stateTableContinuous")
functions = sapply(functions, function(x) paste("functions/", x, ".R", sep = ""))
for(i in 1:length(functions)){source(functions[i])
                              rm(i)}


## @knitr ns
ns_11 = makeNs(quest = quest_11, points = points_11, nonsampled = nonsampled_11)
ns_N11 = makeNs(quest = quest_11, points = points_11, nonsampled = nonsampled_N11)
ns_N11 = ns_N11[ns_N11$rpa_regionCd == 1,]
ns_06 = makeNs(quest = quest_06, points = points_06, nonsampled = nonsampled_06, nwosCycle = 2006)
ns_N06 = makeNs(quest = quest_06, points = points_06, nonsampled = nonsampled_N06, nwosCycle = 2006)
ns_N06 = ns_N06[ns_N06$rpa_regionCd == 1,]


## @knitr responseRates
source('data setup/AddResponseRates.R')
AddResponseRates(quest = quest_11, ns = ns_11, nwosCycle = 11)
AddResponseRates(quest = quest_06, ns = ns_06, nwosCycle = 06)


## @knitr questAll
questAll_11 = makeQuestAll(quest = quest_11, nwosCycle = 2011)


## @knitr tablesStateIgnore, cache = T
t23State_11 = stateTables(385:387
                     ,questAll_11
                     ,ns_11
                     ,factor(rep('population', nrow(questAll_11))))
t433State_11 = stateTables(c(388:ncol(questAll_11), 379:380, 382:384)
                        ,questAll_11
                        ,ns_11
                        ,questAll_11$population__family)
tAgeState_11 = stateTableContinuous(c(14, 273:274)
                                 ,questAll_11
                                 ,ns_11
                                 ,questAll_11$population__family)
t433State_11 = rbind(t433State_11, tAgeState_11)


## @knitr tablesRegionIgnore
t23Region_11 = regionTables(t23State_11[t23State_11$stateCd != 2,])
t433Region_11 = regionTables(t433State_11[t433State_11$stateCd != 2,])


## @knitr outputIgnore
outputwd = 'C:/Users/FFRC_BRENT/Dropbox/NWOS/brents nwos stuff/output data/'
write.csv(t23State_11, paste(outputwd, 'nwos tables/nwos tables 2-3 by state', substr(Sys.time(), 1, 10), '.csv', sep = ''), row.names = F)
write.csv(t23Region_11, paste(outputwd, 'nwos tables/nwos tables 2-3 by region', substr(Sys.time(), 1, 10), '.csv', sep = ''), row.names = F)
write.csv(t433State_11, paste(outputwd, 'nwos tables/nwos tables 4-33 by state', substr(Sys.time(), 1, 10), '.csv', sep = ''), row.names = F)
write.csv(t433Region_11, paste(outputwd, 'nwos tables/nwos tables 4-33 by region', substr(Sys.time(), 1, 10), '.csv', sep = ''), row.names = F)



