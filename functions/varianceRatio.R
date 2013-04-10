VarianceRatio = function(numerator, denominator, varNumerator, varDenominator, covariance)
{
  variance = NULL
  for(i in 1:length(numerator))
  {
    ddNumerator_i = 1/denominator[i]
    ddDenominator_i = -numerator[i]/denominator[i]^2
    if(is.na(varNumerator[i]) == F & is.na(varDenominator[i]) == F)
    {
      if(numerator[i] == denominator[i] & varNumerator[i] == varDenominator[i])
      {
        covariance[i] = varNumerator[i]
      }      
    }
    variance_i = varNumerator[i]*ddNumerator_i^2 + 
      varDenominator[i]*ddDenominator_i^2 + 
      2*covariance[i]*ddNumerator_i*ddDenominator_i
    variance = c(variance, variance_i)
  }
  return(variance)
}