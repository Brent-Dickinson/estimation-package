read.points = function()
{
  points_nrs = read.csv(paste(datawd, 'nrs point count.csv', sep = ''), stringsAsFactors = F)
  points_srs = read.csv(paste(datawd, 'srs point count.csv', sep = ''), stringsAsFactors = F)
  points_srs = points_srs[!points_srs$STATECD %in% c(40, 48),]
  points_tx_ok = read.csv(paste(datawd, 'point count e_tx e_ok.csv', sep = ''), stringsAsFactors = F)
  points_tx_ok$STATECD[points_tx_ok$STATECD == 40.1] = '40.1'
  points_tx_ok$STATECD[points_tx_ok$STATECD == 40.2] = '40.2'
  points_tx_ok$STATECD[points_tx_ok$STATECD == 48.1] = '48.1'
  points_tx_ok$STATECD[points_tx_ok$STATECD == 48.2] = '48.2'
  points_pnw = read.csv(paste(datawd, 'pnwrs point count.csv', sep = ''), stringsAsFactors = F)[,1:5]
  points_rm = read.csv(paste(datawd, 'rmrs point count.csv', sep = ''), stringsAsFactors = F)
  colnames(points_nrs)[6] = 'count'
  colnames(points_srs) = colnames(points_nrs)
  colnames(points_tx_ok) = colnames(points_nrs)[2:6]
  colnames(points_rm) = colnames(points_nrs)
  colnames(points_pnw) = colnames(points_nrs)[2:6]
  points = rbind(points_nrs, points_srs, points_rm)
  points = points[,-1]
  points = rbind(points, points_tx_ok, points_pnw)
  return(points)
}