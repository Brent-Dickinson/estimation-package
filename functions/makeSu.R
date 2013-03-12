makeSu = function(quest = quest_su, plot = plot_su, ref = ref_su)
{
  states = unique(quest$QUEST_STATE)
  quest_expanded = data.frame(NULL)
  for(i in 1:length(states))
  {
    state = states[i]
    quest_sub = quest[quest$QUEST_STATE == state,]
    plot_sub = plot[plot$STATECD == state, names(plot) %in% c("STATECD", "COUNTYCD", "PLOT", "OWNER")]
    quest_expanded_sub = merge(quest_sub, plot_sub, by.x = 'OWNER', by.y = 'OWNER', all.x = T)
    quest_expanded = rbind(quest_expanded, quest_expanded_sub)
  }
  matches_quest = paste(quest_expanded$QUEST_STATE, quest_expanded$COUNTYCD, sep = '.')
  matches_ref = paste(ref$STATE, ref$COUNTY, sep = '.')
  quest_expanded$su = factor(ref$SU_Alpha[match(matches_quest, matches_ref)])
  quest_expanded$POINT_COUNT = 1
  return(quest_expanded)
}