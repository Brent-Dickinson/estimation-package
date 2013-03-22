plot_su = read.csv(paste(datawd, 'delete me/sample_point.csv', sep = ''), stringsAsFactors = F)
quest_su = quest_extra

quest_su = quest_su[-which(quest_su$QUEST_NUMBER %in% int),]
quest_su = quest_su[-which(is.na(quest_su$OWNER)),]
quest_su = quest_su[quest_su$AC_WOOD >= 1,]
ref_su = read.csv(paste(datawd, 'state_county_su_reference.csv', sep = ''), stringsAsFactors = F)

source('functions/makeSu.R')
quest_expanded = makeSu(quest = quest_su, plot = plot_su, ref = ref_su)

# to fix intensified sample:
#pc = read.csv('c:/users/ffrc_brent/dropbox/mary ct/point count by owner.csv', header = T, stringsAsFactors = F)
#pc_imp = pc[pc$CountOfPLOT > 1,]
#nums = quest_11$QUEST_NUMBER[quest_11$QUEST_NUMBER %in% pc_imp$QUEST]
#quest_11$POINT_COUNT[quest_11$QUEST_NUMBER %in% pc_imp$QUEST] = pc_imp$CountOfPLOT[pc_imp$QUEST %in% nums]

## notes on the above fix:
# the above pc file does not contain all of the QUEST_NUMBER's from the intensified sample. the reason for this is unknown at present. 
# those ownerships from the intensified sample without a POINT_COUNT value from pc were assigned a POINT_COUNT value of 1 above in line 33.
# that approach is reasonable because the sampling intensity is about 1 point per 760 acres and all the ownerships without POINT_COUNT values from pc are under 300 acres in size.

