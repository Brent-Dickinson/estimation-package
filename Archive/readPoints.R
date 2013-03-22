readPoints = function(datawd)
{
  keepCols = c('PLT_CN'
               ,'STATECD'
               ,'COUNTYCD'
               ,'INVYR'
               ,'COND_STATUS_CD'
               ,'COND_NONSAMPLE_REASN_CD'
               ,'OWNCD')
  pointsNrs = read.csv(paste(datawd, 'delete/points_nrs.csv', sep = ''), stringsAsFactors = F)[,keepCols]
  pointsSrs = read.csv(paste(datawd, 'delete/points_srs.csv', sep = ''), stringsAsFactors = F)[,keepCols]
  pointsPnwrs = read.csv(paste(datawd, 'delete/points_pnwrs.csv', sep = ''), stringsAsFactors = F)[,keepCols]
  pointsRmrs = read.csv(paste(datawd, 'delete/points_rmrs.csv', sep = ''), stringsAsFactors = F)[,keepCols]
  points = rbind(pointsNrs, pointsSrs, pointsPnwrs, pointsRmrs)
  return(points)
}