cov1 = function(areaTotal
                ,nTotal
                ,responseRate
                ,pointCount
                ,areaIndividual
                ,variable1
                ,variable2
                ,domain1 = 1
                ,domain2 = 1)
{
  if(length(pointCount) != length(areaIndividual) | 
       length(pointCount) != length(variable1) |
       length(pointCount) != length(variable2) |
       (length(pointCount) != length(domain1) & length(domain1) != 1))
  {
    stop('problem with indiv1(): pointCount, variable, and domain not of equal lengths')
  }
  if(!length(responseRate) %in% c(1, length(pointCount)))
  {
    stop('problem with indiv1(): responseRate is not of length 1 or that of pointCount')
  }
  pointCountFull = c(pointCount, rep(1, nTotal - sum(pointCount)))
  domain1 = c(domain1, rep(0, nTotal - sum(pointCount)))
  domain2 = c(domain2, rep(0, nTotal - sum(pointCount)))
  variable1 = c(variable1, rep(1, nTotal - sum(pointCount)))
  variable2 = c(variable2, rep(1, nTotal - sum(pointCount)))
  areaIndividual = c(areaIndividual, rep(1, nTotal - sum(pointCount)))
  responseRate = c(responseRate, rep(1, nTotal - sum(pointCount)))
  
  z1I = domain1*variable1/(areaIndividual*responseRate)
  z2I = domain2*variable2/(areaIndividual*responseRate)

  z1Bar = sum(pointCountFull*z1I)/nTotal
  z2Bar = sum(pointCountFull*z2I)/nTotal
  
  covY1Y2 = areaTotal^2*sum(pointCountFull*(z1I - z1Bar)*(z2I - z2Bar))/(nTotal*(nTotal - 1))
  
  return(covY1Y2)
}