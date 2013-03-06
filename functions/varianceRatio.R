varianceRatio = function(numerator, denominator, varNumerator, varDenominator, covariance)
{
  ddNumerator = 1/denominator
  ddDenominator = -numerator/denominator^2
  variance = varNumerator*ddNumerator^2 + varDenominator*ddDenominator^2 + 2*covariance*ddNumerator*ddDenominator
  return(variance)
}