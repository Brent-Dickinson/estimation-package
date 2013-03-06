# for nonsampled points:
source('nonsampling/explore_nonsample.R')
nonsamp_byregion = explore_nonsample(nonsampled_11)
require(xlsx)
setwd('c:/users/ffrc_brent/dropbox/NWOS/brents nwos stuff/output data/')
write.xlsx(nonsamp_byregion[[1]], file = 'nonsamp_byregion_11.xlsx', sheetName = 'STRATUM_DESCR', row.names = F)
write.xlsx(nonsamp_byregion[[2]], file = 'nonsamp_byregion_11.xlsx', sheetName = 'PLOT_NONSAMPLE_REASN_CD', row.names = F, append = T)