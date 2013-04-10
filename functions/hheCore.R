HheCore = function(AreaFia
                   ,AreaVarianceFia
                   ,PointCount
                   ,AreaIndividual
                   ,Variable
                   ,Domain
                   ,ResponseRateItem = 1)
{
  if(length(PointCount) != length(AreaIndividual) | 
       length(PointCount) != length(Variable) |
       (length(PointCount) != length(Domain) & length(Domain) != 1))
  {
    stop('problem with hheCore(): PointCount, Variable, and Domain not of equal lengths')
  }
  nTotal = sum(PointCount)
  zI = Domain*Variable/(AreaIndividual/ResponseRateItem)
  zBar = sum(PointCount*zI)/nTotal
  VzBar = sum(PointCount*(zI - zBar)^2)/(nTotal*(nTotal - 1))
  yHat = AreaFia*zBar
  VyHat = VzBar*AreaFia^2 + AreaVarianceFia*zBar^2
  
  outData = data.frame(yHat = yHat
                       ,VyHat = VyHat)
  
  return(outData)  
}