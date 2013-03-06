varDifRatiosCommon = function(numerator1
                              ,denominator
                              ,numerator2
                              ,varNum1
                              ,varDen
                              ,varNum2
                              ,covNum1Den
                              ,covNum1Num2
                              ,covDenNum2)
{
  ddNum1 = 1/denominator1
  ddDen = -numerator1/denominator^2
  ddNum2 = -1/denominator
  variance = varNum1*ddNum1^2 + 
    varDen*ddDen^2 +
    varNum2*ddNum2^2 +
    2*covNum1Den*ddNum1*ddDen +
    2*covNum1Num2*ddNum1*ddNum2 +
    2*covDenNum2*ddDen*ddNum2
  return(variance)
}