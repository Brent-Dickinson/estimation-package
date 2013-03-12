quest_11$responseRateFIA = ns_11$urr_fia_pf[match(quest_11$QUEST_STATE, ns_11$state)]
quest_11$responseRateNWOS = NA
quest_11$responseRateNWOS[quest_11$OWNTYPE %in% 1:4] = c(ns_11$n_ff_resp[match(quest_11$QUEST_STATE, ns_11$state)]/ns_11$n_ff[match(quest_11$QUEST_STATE, ns_11$state)])[quest_11$OWNTYPE %in% 1:4]
quest_11$responseRateNWOS[quest_11$OWNTYPE %in% 5:6] = c(ns_11$n_pf_nonfam_resp[match(quest_11$QUEST_STATE, ns_11$state)]/ns_11$n_pf_nonfam[match(quest_11$QUEST_STATE, ns_11$state)])[quest_11$OWNTYPE %in% 5:6]

#quest_06$responseRateFIA = ns_06$urr_fia_pf[match(quest_06$STATECD, ns_06$state)]
#quest_06$responseRateNWOS = NA
#quest_06$responseRateNWOS[quest_06$OWNER_CLASS_ADJ == 45] = c(ns_06$n_ff_resp[match(quest_06$STATECD, ns_06$state)]/ns_06$n_ff[match(quest_06$STATECD, ns_06$state)])[quest_06$OWNTYPE %in% 1:4]
#quest_06$responseRateNWOS[quest_06$OWNER_CLASS_ADJ %in% 41:44] = c(ns_06$n_pf_nonfam_resp[match(quest_06$STATECD, ns_06$state)]/ns_06$n_pf_nonfam[match(quest_06$STATECD, ns_06$state)])[quest_06$OWNER_CLASS_ADJ %in% 41:44]