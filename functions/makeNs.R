## this function aggregates the points data frame to an "n's" data frame for use in estimation.

makeNs = function(points
                  ,nonsampled
                  ,quest
                  ,area = areaNew
                  ,ref = refState
                  ,nwosCycle = 2011
                  ,type = 'generic')
{
  if(nwosCycle == 2011)
  {
    areaName = 'AC_WOOD'
    stateVariable = 'QUEST_STATE'
    ownVariable = 'OWNTYPE'
    famCodes = 1:4
    nonFamCodes = 5:6
  }
  if(nwosCycle == 2006)
  {
    areaName = 'ACRES_IN_STATE'
    stateVariable = 'STATECD'
    ownVariable = 'OWNER_CLASS_ADJ'
    famCodes = 45
    nonFamCodes = 41:44
  }
  
  ns = data.frame(state = unique(points$STATECD[points$STATECD != 11]), stringsAsFactors = F)
  ns = data.frame(state = ns[order(as.numeric(ns$state)),], stringsAsFactors = F)
  quest = quest[quest[,areaName] >= 1,]
  
  for(i in 1:nrow(ns))
  {
    state_i = ns$state[i]
    points_i = points[points$STATECD == state_i,]
    quest_i = quest[quest[,stateVariable] == state_i,]
    nonsampled_i = nonsampled[nonsampled$STATECD == state_i,]
    
    ns$state_abbr[i] = ref$STATE_ABBR[ref$STATECD == state_i]
    ns$rpa_regionCd[i] = ref$RPA_REGIONCD[ref$STATECD == state_i]
    ns$rpa_regionNm[i] = ref$RPA_REGION_NAME[ref$STATECD == state_i]
    ns$rpa_subregion[i] = ref$RPA_SUBERGION_NAME[ref$STATECD == state_i]
    if(ns$state_abbr[i] %in% c('AK','CA','HI','OR','WA'))
    {
      if(state_i == 2) ns$RSCD[i] = 27
      else ns$RSCD[i] = 26
    }
    else
    {
      if(nrow(nonsampled_i) == 0) ns$RSCD[i] = 22
      else ns$RSCD[i] = names(summary(factor(nonsampled_i$RSCD)))  
    }
    # stratum area (see /InputData/Documentation/LandAreaCounty-Census.txt for how these data were obtained)
    ns$AreaAcres[i] = area$AreaAcres[area$state == state_i]
    # stratum total points (all FIA points, sampled or not):
    ns$n_h[i] = nrow(points_i)
    # stratum total private forest points (FIA sampled only):
    ns$n_pf[i] = nrow(points_i[points_i$OWNCD %in% c(41:45) & points_i$COND_STATUS_CD == 1,])
    # stratum total private, non-family, forest points (FIA sampled only):
    ns$n_pf_nonfam[i] = nrow(points_i[points_i$OWNCD %in% c(41:44) & points_i$COND_STATUS_CD == 1,])
    # stratum total family forest points (FIA sampled only):
    ns$n_ff[i] = nrow(points_i[points_i$OWNCD == 45 & points_i$COND_STATUS_CD == 1,])
    # stratum points from responding private forest ownerships:
    ns$n_pf_resp[i] = sum(quest_i$POINT_COUNT)
    # stratum points from responding private, non-family, forest ownerships:
    ns$n_pf_nonfam_resp[i] = sum(quest_i$POINT_COUNT[quest_i[,ownVariable] %in% nonFamCodes])
    # stratum points from responding family forest ownerships:
    ns$n_ff_resp[i] = sum(quest_i$POINT_COUNT[quest_i[,ownVariable] %in% famCodes])
    # stratum FIA undifferentiated non-sampled points:
    ns$nons[i] = nrow(points_i[points_i$COND_STATUS_CD == 5,])
    # get FIA private forest point "response rate" by region, since each region has different stratum classifications for non-sampled points:
    # Rocky Mountain:
    if(ns$RSCD[i] == 22)
    {
      # since PNW region supplies OWNCD for non-sampled points, just calculate a private ownership response rate irrespective of forest cover:
      if(type == 'generic')
      {
        ns$n_pf_nons[i] = NA
        ns$urr_fia_pf[i] = nrow(points_i[points_i$OWNCD %in% 41:45 & points_i$COND_STATUS_CD != 5,])/nrow(points_i[points_i$OWNCD %in% 41:45,])
      }
      else
      {
        ns$n_pf_nons[i] = nrow(nonsampled_i[nonsampled_i$STRATUM_DESCR == 'Green',])
        ns$urr_fia_pf[i] = ns$n_pf[i]/(ns$n_pf[i] + ns$n_pf_nons[i])        
      }
    }
    # North Central and Northeastern:
    if(ns$RSCD[i] %in% 23:34)
    {
      ns$n_pf_nons[i] = nrow(nonsampled_i[str_detect(nonsampled_i$STRATUM_DESCR, '0 -') == F &
                                            str_detect(nonsampled_i$STRATUM_DESCR, '6 -') == F &
                                            str_detect(nonsampled_i$STRATUM_DESCR, ignore.case('non')) == F &
                                            nonsampled_i$PLOT_NONSAMPLE_REASN_CD == 2,])
      ns$urr_fia_pf[i] = ns$n_pf[i]/(ns$n_pf[i] + ns$n_pf_nons[i])
    }
    # Pacific Northwest & Alaska:
    if(ns$RSCD[i] %in% 26:27)
    {
      # since PNW region supplies OWNCD for non-sampled points, just calculate a private ownership response rate irrespective of forest cover:
      if(type == 'generic')
      {
        ns$n_pf_nons[i] = NA
        ns$urr_fia_pf[i] = nrow(points_i[points_i$OWNCD %in% 41:45 & points_i$COND_STATUS_CD != 5,])/nrow(points_i[points_i$OWNCD %in% 41:45,])
      }
      else
      {
        ns$n_pf_nons[i] = nrow(nonsampled_i[str_detect(nonsampled_i$STRATUM_DESCR, ignore.case('non')) == F &
                                              str_detect(nonsampled_i$STRATUM_DESCR, ignore.case('edge')) == F &
                                              str_detect(nonsampled_i$STRATUM_DESCR, ignore.case('private')),])
        ns$n_pf_nons[i] = ns_pf_nons[i] + nrow(nonsampled_i[nonsampled_i$STRATUM_DESCR %in% c('FOREST', 'BOTTOM LANDS HARDWOOD') |
                                                              str_detect(nonsampled_i$STRATUM_DESCR, '51% -') |
                                                              str_detect(nonsampled_i$STRATUM_DESCR, '76% -'),])
        ns$urr_fia_pf[i] = ns$n_pf[i]/(ns$n_pf[i] + ns$n_pf_nons[i])
      }
    }
    # South:
    if(ns$RSCD[i] == 33)
    {
      ns$n_pf_nons[i] = nrow(nonsampled_i[(str_detect(nonsampled_i$STRATUM_DESCR, '51% -') | str_detect(nonsampled_i$STRATUM_DESCR, '76% -')) |
                                            (nonsampled_i$STRATUM_DESCR == 'FOREST' & nonsampled_i$PLOT_NONSAMPLE_REASN == 2),])
      ns$urr_fia_pf[i] = ns$n_pf[i]/(ns$n_pf[i] + ns$n_pf_nons[i])
    }    
  }
  return(ns)
}