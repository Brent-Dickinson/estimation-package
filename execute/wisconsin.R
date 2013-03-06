# and then for a given list of variables from quest_all:
wisconsin = state_tables(v_cols = which(str_detect(names(quest_all), 'table')), quest = quest_all, points = ns_N[ns_N$state == 55,], print_ij = F)
write.csv(wisconsin, paste('wisconsin', substr(Sys.time(), 1, 10), 'csv', sep = '.'), row.names = F)

wi_su_fam = state_tables(v_cols = 24, quest = quest_expanded[quest_expanded$OWNTYPE %in% c(1:4),], points = ns_N[ns_N$state == 55,])
wi_su_allPriv = state_tables(v_cols = 24, quest = quest_expanded, points = ns_N[ns_N$state == 55,])

write.csv(wi_su_fam, paste('wi_su_fam', substr(Sys.time(), 1, 10), 'csv', sep = '.'), row.names = F)
write.csv(wi_su_allPriv, paste('wi_su_allPriv', substr(Sys.time(), 1, 10), 'csv', sep = '.'), row.names = F)

require(Hmisc)
wi_su_fam = wi_su_fam[,c(2,5:10)]
wi_su_fam[,c(3,5,7)] = sqrt(wi_su_fam[,c(3,5,7)])
colnames(wi_su_fam) = c("Survey Unit", "Ownerships", "St. Dev.", "Acres", "St. Dev.", "Average Size", "St. Dev.")
wi_su_fam[,2:7] = prettyNum(round(wi_su_fam[,2:7], digits = 0), big.mark = ',')
rownames(wi_su_fam) = NULL
latex(wi_su_fam, file = "", rowlabel = NULL)

wi_su_allPriv = wi_su_allPriv[,c(2,5:10)]
wi_su_allPriv[,c(3,5,7)] = sqrt(wi_su_allPriv[,c(3,5,7)])
colnames(wi_su_allPriv) = c("Survey Unit", "Ownerships", "St. Dev.", "Acres", "St. Dev.", "Average Size", "St. Dev.")
wi_su_allPriv[,2:7] = prettyNum(round(wi_su_allPriv[,2:7], digits = 0), big.mark = ',')
rownames(wi_su_allPriv) = NULL
latex(wi_su_allPriv, file = "", rowlabel = NULL)
