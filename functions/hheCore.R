# The following is a generic function for getting a Hansen-Hurwitz estimate of total along with its variance given an area-based, with-replacement sampling design
# What is needed:
# total area sampled (within a stratum)
# total number of points laid on the ground
# total number of points identified to fall in the population of interest
# total number of points from responding ownerships in the population of interest
# a response probability vector for the primary identification (either length 1 or length of sample). For NWOS, this is essentially the FIA's response rate
# a "POINT_COUNT" vector indicating how many points selected each ownership in the sample
# an individual area vector indicating size of each ownership in the sample
# an individual variable of interest vector
# an indivudal domain indicator

hheCore = function(areaTotal
                   ,nTotal
                   ,responseRate
                   ,pointCount
                   ,areaIndividual
                   ,variable
                   ,domain)
{
  if(length(pointCount) != length(areaIndividual) | 
       length(pointCount) != length(variable) |
       (length(pointCount) != length(domain) & length(domain) != 1))
  {
    stop('problem with hheCore(): pointCount, variable, and domain not of equal lengths')
  }
  if(!length(responseRate) %in% c(1, length(pointCount)))
  {
    stop('problem with hheCore(): responseRate is not of length 1 or that of pointCount')
  }
  
  # need to restructure vectors to reflect full sample including points outside of population of interest:
  pointCountFull = c(pointCount, rep(1, nTotal - sum(pointCount)))
  domain = c(domain, rep(0, nTotal - sum(pointCount)))
  variable = c(variable, rep(1, nTotal - sum(pointCount)))
  areaIndividual = c(areaIndividual, rep(1, nTotal - sum(pointCount)))
  responseRate = c(responseRate, rep(1, nTotal - sum(pointCount)))
  zI = domain*variable/(areaIndividual*responseRate)
  zBar = sum(pointCountFull*zI)/nTotal
  VzBar = sum(pointCountFull*(zI - zBar)^2)/(nTotal*(nTotal - 1))
  yHat = areaTotal*zBar
  VyHat = areaTotal^2*VzBar
  
  outData = data.frame(yHat = yHat
                       ,VyHat = VyHat)
  
  return(outData)  
}