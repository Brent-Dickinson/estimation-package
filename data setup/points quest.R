######################################################################
## this script accomplishes two basic tasks necessary for nwos estimation: sets up the (1) points and (2) quest files.
## (1a): as the data are stored currently, total area by state must be brought in separately. this is census data.
## (1b): the fia plot data are stored separately by region. within each region, points are tabulated by ownership type and inventory year.
## (1c): this script brings in each region's plot data and joins them into one "points" data frame.
## (1d): next the points data frame is reduced to the appropriate inventory years given the nwos year(s) of interest, using a data frame that matches INVYR to NWOS_YEAR.
## (2a): then the nwos quest data frame is loaded and observations with AC_WOOD < 1 acre are removed.
## (2b): then the itensification issue for CT is accounted for. there are two solutions: either remove the intensified points or adjust the numbers in the points data frame to account for the intensified points.
######################################################################

## set working directory:
datawd = 'e:/brent/nwos/input data/'

## (1a): read in the A numbers:
area = read.csv(paste(datawd, 'copy of landarea.csv', sep = ''), stringsAsFactors = F)
area = area[,1:4]
areaOk = read.csv(paste(datawd, 'oklahomaArea.csv', sep = ''), stringsAsFactors = F)
areaTx = read.csv(paste(datawd, 'texasArea.csv', sep = ''), stringsAsFactors = F)

## (1b): read in the points files by region, then deal with idiosyncrasies and join them:

invyr_match_11 = read.csv(paste(datawd, 'nwos_cycle.csv', sep = ''), stringsAsFactors = F)
invyr_match_06 = read.csv(paste(datawd, 'inv_nwos_yr.csv', sep = ''), stringsAsFactors = F)
match_11 = paste(invyr_match_11$STATECD, invyr_match_11$INVYR)
match_06 = paste(invyr_match_06$state, invyr_match_06$inv_year)

source('functions/read.points.R')
points = read.points()
library(stringr)
#points_NRS = read.csv('delete me/nrs sample points.csv', header = T, stringsAsFactors = F)

## (1c & 1d): read in the NWOS_YEAR to INVYR match file, and pull out INVYR's for 2011:
## for owncd codes, see FIA DB user's guide under /my nwos/refs/
points_11 = points[paste(substr(points$STATECD, 1, 2), points$INVYR) %in% match_11,]
points_06 = points[paste(substr(points$STATECD, 1, 2), points$INVYR) %in% match_06,]

#points_NRS = points_NRS[paste(points_NRS$STATECD, points_NRS$INVYR) %in% match,]
#points_NRS$COND_NONSAMPLE_REASN_CD[is.na(points_NRS$COND_NONSAMPLE_REASN_CD)] = 0
#nonsampled_NRS = read.csv('e:/brent/nwos/input data/NRS OWN LU for NonSampled Points.csv', stringsAsFactors = F)
nonsampled_all = read.csv('e:/brent/nwos/input data/own lu for nonsampled points all regions.csv', stringsAsFactors = F)
nonsampled_all$STATECD[nonsampled_all$STATECD == 48 & nonsampled_all$ESTN_UNIT_DESCR %in% c('NORTHEAST', 'SOUTHEAST')] = '48.1'
nonsampled_all$STATECD[nonsampled_all$STATECD == 48 & !nonsampled_all$ESTN_UNIT_DESCR %in% c('NORTHEAST', 'SOUTHEAST')] = '48.2'
nonsampled_all$STATECD[nonsampled_all$STATECD == 40 & nonsampled_all$ESTN_UNIT_DESCR %in% c('NORTHEAST', 'SOUTHEAST')] = '40.1'
nonsampled_all$STATECD[nonsampled_all$STATECD == 40 & !nonsampled_all$ESTN_UNIT_DESCR %in% c('NORTHEAST', 'SOUTHEAST')] = '40.2'

nonsampled_11 = nonsampled_all[paste(substr(nonsampled_all$STATECD, 1, 2), nonsampled_all$INVYR) %in% match_11,]
nonsampled_06 = nonsampled_all[paste(substr(nonsampled_all$STATECD, 1, 2), nonsampled_all$INVYR) %in% match_06,]
## Rocky Mountain and PNW regions (RSCD) have OWNCD for non-sampled points

#su_county = read.csv('county.csv', header = T, stringsAsFactors = F)
#points_SRS$STATECD[points_SRS$STATECD == 48 & points_SRS$COUNTYCD %in% c(1,2)] = 48.12
#points_SRS$STATECD[points_SRS$STATECD == 48 & points_SRS$COUNTYCD %in% c(3:7)] = 48.34567


## (2a): read in the two separate quest files (???) and bind POINT_COUNT to the right quest file:
quest_extra = read.csv(paste(datawd, 'delete me/quest1.csv', sep = ''), header = T, stringsAsFactors = F)
quest_11 = read.csv(paste(datawd, 'delete me/x nwos_2011_quest_skip_corrected.csv', sep = ''), stringsAsFactors = F)
quest_11$POINT_COUNT = quest_extra$POINT_COUNT
quest_11$county = quest_extra$county
quest_11$POINT_COUNT[is.na(quest_11$POINT_COUNT)] = 1
quest_11 = quest_11[quest_11$AC_WOOD >= 1,]
quest_06 = read.csv(paste(datawd, 'delete me/quest.csv', sep = ''), stringsAsFactors = F)
quest_06 = quest_06[quest_06$ACRES_IN_EU >= 1,]

plot_su = read.csv(paste(datawd, 'delete me/sample_point.csv', sep = ''), stringsAsFactors = F)
quest_su = quest_extra

## (2b): CT intensification issue: pull in CT point counts
# to remove intensified sample:
int = read.csv(paste(datawd, 'ct_intens.csv', sep = ''),stringsAsFactors = F)
int = int[,2]
quest_11 = quest_11[-which(quest_11$QUEST_NUMBER %in% int),]
quest_su = quest_su[-which(quest_su$QUEST_NUMBER %in% int),]
quest_su = quest_su[-which(is.na(quest_su$OWNER)),]
quest_su = quest_su[quest_su$AC_WOOD >= 1,]
ref_su = read.csv(paste(datawd, 'state_county_su_reference.csv', sep = ''), stringsAsFactors = F)

source('functions/make_su.R')
quest_expanded = make_su()

tx_ok = read.csv(paste(datawd, 'tx.csv', sep = ''), stringsAsFactors = F)
eastTx = tx_ok$COUNTYCD[tx_ok$STATECD == 48 & tx_ok$NWOS_SU %in% 1:2]
eastOk = tx_ok$COUNTYCD[tx_ok$STATECD == 40 & tx_ok$NWOS_SU == 1]

areaTx$STATECD = ifelse(areaTx$txFips %in% eastTx, '48.1', '48.2')
areaTxAggregate = aggregate(areaTx$txAreaAcres, by = list(areaTx$STATECD), FUN = sum)
areaOk$STATECD = ifelse(areaOk$okFips %in% eastOk, '40.1', '40.2')
areaOkAggregate = aggregate(areaOk$okAreaAcres, by = list(areaOk$STATECD), FUN = sum)
areaNew = area
areaNew[(nrow(area) + 1):(nrow(area) + 4),] = NA
areaNew[(nrow(area) + 1):(nrow(area) + 2), c('STATECD', 'ACRES')] = cbind(as.character(areaTxAggregate$Group.1), areaTxAggregate$x)
areaNew[(nrow(area) + 3):(nrow(area) + 4), c('STATECD', 'ACRES')] = cbind(as.character(areaOkAggregate$Group.1), areaOkAggregate$x)
areaNew$ACRES = as.numeric(areaNew$ACRES)

quest_11$QUEST_STATE[quest_11$QUEST_STATE == 48 & quest_11$county %in% eastTx] = "48.1"
quest_11$QUEST_STATE[quest_11$QUEST_STATE == 48 & !quest_11$county %in% eastTx] = "48.2"
quest_11$QUEST_STATE[quest_11$QUEST_STATE == 40 & quest_11$county %in% eastOk] = "40.1"
quest_11$QUEST_STATE[quest_11$QUEST_STATE == 40 & !quest_11$county %in% eastOk] = "40.2"

quest_06$STATECD[quest_06$STATECD == 48 & quest_06$NWOS_EU %in% 1:2] = "48.1"
quest_06$STATECD[quest_06$STATECD == 48 & !quest_06$NWOS_EU %in% 1:2] = "48.2"
quest_06$STATECD[quest_06$STATECD == 40 & quest_06$NWOS_EU == 12] = "40.1"
quest_06$STATECD[quest_06$STATECD == 40 & !quest_06$NWOS_EU == 12] = "40.2"

quest_06 = quest_06[is.na(quest_06$OWNER_CLASS_ADJ) == F,]
# to fix intensified sample:
#pc = read.csv('c:/users/ffrc_brent/dropbox/mary ct/point count by owner.csv', header = T, stringsAsFactors = F)
#pc_imp = pc[pc$CountOfPLOT > 1,]
#nums = quest_11$QUEST_NUMBER[quest_11$QUEST_NUMBER %in% pc_imp$QUEST]
#quest_11$POINT_COUNT[quest_11$QUEST_NUMBER %in% pc_imp$QUEST] = pc_imp$CountOfPLOT[pc_imp$QUEST %in% nums]

## notes on the above fix:
# the above pc file does not contain all of the QUEST_NUMBER's from the intensified sample. the reason for this is unknown at present. 
# those ownerships from the intensified sample without a POINT_COUNT value from pc were assigned a POINT_COUNT value of 1 above in line 33.
# that approach is reasonable because the sampling intensity is about 1 point per 760 acres and all the ownerships without POINT_COUNT values from pc are under 300 acres in size.

# finally, set up states' reference data frame:
states_reference = read.csv(paste(datawd, 'state_region_reference.csv', sep = ''), stringsAsFactors = F)
txRefE = states_reference[states_reference$STATECD == 48,]
txRefE$STATECD = '48.1'
txRefE$STATE_ABBR = 'E.TX'
txRefE$STATE_NAME = 'East Texas'
txRefW = states_reference[states_reference$STATECD == 48,]
txRefW$STATECD = '48.2'
txRefW$STATE_ABBR = 'W.TX'
txRefE$STATE_NAME = 'West Texas'
okRefE = states_reference[states_reference$STATECD == 40,]
okRefE$STATECD = '40.1'
okRefE$STATE_ABBR = 'E.OK'
okRefE$STATE_NAME = 'East Oklahoma'
okRefW = states_reference[states_reference$STATECD == 40,]
okRefW$STATECD = '40.2'
okRefW$STATE_ABBR = 'W.OK'
okRefE$STATE_NAME = 'West Oklahoma'
states_reference = states_reference[!states_reference %in% c(40, 48),]
states_reference = rbind(states_reference, txRefE, txRefW, okRefE, okRefW)

# the points data frame is aggregated by stratum for estimation:
source('functions/make_ns.R')
ns_11 = make_ns(quest = quest_11, points = points_11, nonsampled = nonsampled_11)
ns_06 = make_ns(quest = quest_06, points = points_06, nonsampled = nonsampled_06, nwosCycle = 2006)

quest_11$responseRateFIA = ns_11$urr_fia_pf[match(quest_11$QUEST_STATE, ns_11$state)]
quest_11$responseRateNWOS = NA
quest_11$responseRateNWOS[quest_11$OWNTYPE %in% 1:4] = c(ns_11$n_ff_resp[match(quest_11$QUEST_STATE, ns_11$state)]/ns_11$n_ff[match(quest_11$QUEST_STATE, ns_11$state)])[quest_11$OWNTYPE %in% 1:4]
quest_11$responseRateNWOS[quest_11$OWNTYPE %in% 5:6] = c(ns_11$n_pf_nonfam_resp[match(quest_11$QUEST_STATE, ns_11$state)]/ns_11$n_pf_nonfam[match(quest_11$QUEST_STATE, ns_11$state)])[quest_11$OWNTYPE %in% 5:6]

quest_06$responseRateFIA = ns_06$urr_fia_pf[match(quest_06$STATECD, ns_06$state)]
quest_06$responseRateNWOS = NA
quest_06$responseRateNWOS[quest_06$OWNER_CLASS_ADJ == 45] = c(ns_06$n_ff_resp[match(quest_06$STATECD, ns_06$state)]/ns_06$n_ff[match(quest_06$STATECD, ns_06$state)])[quest_06$OWNTYPE %in% 1:4]
quest_06$responseRateNWOS[quest_06$OWNER_CLASS_ADJ %in% 41:44] = c(ns_06$n_pf_nonfam_resp[match(quest_06$STATECD, ns_06$state)]/ns_06$n_pf_nonfam[match(quest_06$STATECD, ns_06$state)])[quest_06$OWNER_CLASS_ADJ %in% 41:44]