varDifferenceOfRatios = function(numerator1
                                 ,denominator1
                                 ,numerator2
                                 ,denominator2
                                 ,varNum1
                                 ,varDen1
                                 ,varNum2
                                 ,varDen2
                                 ,covNum1Den1
                                 ,covNum1Num2
                                 ,covNum1Den2
                                 ,covDen1Num2
                                 ,covDen1Den2
                                 ,covNum2Den2)
{
  ddNum1 = 1/denominator1
  ddDen1 = -numerator1/denominator1^2
  ddNum2 = -1/denominator2
  ddDen2 = numerator2/denominator2^2
  variance = varNum1*ddNum1^2 + 
    varDen1*ddDen1^2 +
    varNum2*ddNum2^2 +
    varDen2*ddDen2^2 +
    2*covNum1Den1*ddNum1*ddDen1 +
    2*covNum1Num2*ddNum1*ddNum2 +
    2*covNum1Den2*ddNum1*ddDen2 +
    2*covDen1Num2*ddDen1*ddNum2 +
    2*covDen1Den2*ddDen1*ddDen2 +
    2*covNum2Den2*ddNum2*ddDen2
  return(variance)
}