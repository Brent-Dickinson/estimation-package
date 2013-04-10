CovCore = function(AreaFia
                   ,PointCount
                   ,AreaIndividual
                   ,Variable1
                   ,Variable2
                   ,Domain1 = 1
                   ,Domain2 = 1)
{
  if(length(PointCount) != length(AreaIndividual) | 
       length(PointCount) != length(Variable1) |
       length(PointCount) != length(Variable2) |
       (length(PointCount) != length(Domain1) & length(Domain1) != 1))
  {
    stop('problem with CovCore(): PointCount, Variables, and Domains not of equal lengths')
  }
  nTotal = sum(PointCount)
  z1I = Domain1*Variable1/AreaIndividual
  z2I = Domain2*Variable2/AreaIndividual

  z1Bar = sum(PointCount*z1I)/nTotal
  z2Bar = sum(PointCount*z2I)/nTotal
  
  covY1Y2 = AreaFia^2*sum(PointCount*(z1I - z1Bar)*(z2I - z2Bar))/(nTotal*(nTotal - 1))
  
  return(covY1Y2)
}