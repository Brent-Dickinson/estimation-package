# (1) before any NWOS analysis can proceed, a zip file with the "input data" for estimation must be obtained.
# (2) next sensitive data must be extracted from their encrypted zip files to the "~/input data/delete me/" folder.
# (2a) the quest data frame(s) are in "~/input data/NWOS Brent.rar"
# (2b) the points data frame, if in "individual_plots" format, is in "~/input data/NRS SAMPLE POINTS.rar"
# (3) purge "delete me" folder once data frames have been imported to R and do not save .RData workspace image when session is over.

# the script, "points quest.R", sets up points and quest data frames for use with make_ns() and make_quest_all().
# to do this, the working directory in that script must also be changed accordingly.
# however, points and quest can be set up separately and fed to those functions instead if desired.
source('data setup/points quest.R')

# load necessary functions for estimation:
source('functions/make_quest_all.R')
source('functions/stateTables.R')
source('functions/regionTables.R')
source('functions/hheCore.R')
source('functions/cov1.R')
source('functions/varianceRatio.R')
source('functions/varDifferenceRatios.R')
source('functions/stateTableContinuous.R')

# the quest data frame is augmented with constructed and adjusted variables:
questAll_11 = make_quest_all(quest = quest_11, nwosCycle = 2011)
questAll_06 = make_quest_all(quest = quest_06, nwosCycle = 2006)

# 2011 tables:
outputwd = 'C:/Users/FFRC_BRENT/Dropbox/NWOS/brents nwos stuff/output data/'
t23State = stateTables(385:387
                     ,questAll_11
                     ,ns_11
                     ,factor(rep('population', nrow(questAll_11))))
t23Region = regionTables(t23State
                         ,region = 'stupid.temp.comparison.to.2006')
t434State = stateTables(c(388:ncol(questAll_11), 379:380, 382:384)
                        ,questAll_11
                        ,ns_11
                        ,questAll_11$population__family)
tAgeState = stateTableContinuous(c(14, 273:274)
                                 ,questAll_11
                                 ,ns_11
                                 ,questAll_11$population__family)
tablesState = rbind(t434State, tAgeState)
tablesRegionComp = regionTables(tablesState
                            ,region = 'stupid.temp.comparison.to.2006')
tablesRegion = regionTables(tablesState)
require(xlsx)
write.xlsx2(t23State, paste(outputwd, 'nwos tables/nwos tables 2-3 by state', substr(Sys.time(), 1, 10), '.xlsx', sep = ''), row.names = F)
write.xlsx2(t23Region, paste(outputwd, 'nwos tables/nwos tables 2-3 by region', substr(Sys.time(), 1, 10), '.xlsx', sep = ''), row.names = F)
write.csv(tablesState, paste(outputwd, 'nwos tables/nwos tables 4-33 by state', substr(Sys.time(), 1, 10), '.csv', sep = ''), row.names = F)
write.xlsx2(tablesRegionComp, paste(outputwd, 'nwos tables/nwos tables 4-33 by region (minus AK, WTX, WOK)', substr(Sys.time(), 1, 10), '.xlsx', sep = ''), row.names = F)
write.xlsx2(tablesRegion, paste(outputwd, 'nwos tables/nwos tables 4-33 by region', substr(Sys.time(), 1, 10), '.xlsx', sep = ''), row.names = F)

# 2006 tables:
t23State_06 = stateTables(247:249
                       ,questAll_06
                       ,ns_06
                       ,factor(rep('population', nrow(questAll_06)))
                          ,nwosCycle = 2006)
t23Region = regionTables(t23State)
t434State = stateTables(387:ncol(questAll_11)
                        ,questAll_11
                        ,ns_11
                        ,questAll_11$population__family)
t434Region = regionTables(t434State)
