CompareRows = function(Table1
                       ,Table2
                       ,RowsOfInterest = "AllPossible"
                       ,TableVectorName = "Table"
                       ,RowVectorName = "Row")
{
  if(RowsOfInterest == "AllPossible")
  {
    Sub1 = Table1[grepl("__", Table1[,TableVectorName]) & grepl("row_99", Table1[,RowVectorName]) == F,]
    Sub2 = Table2[grepl("__", Table2[,TableVectorName]) & grepl("row_99", Table2[,RowVectorName]) == F,]
    
    Shared1 = Sub1[paste(Sub1$Table, Sub1$Row, Sub1$StateCode) %in% paste(Sub2$Table, Sub2$Row, Sub2$StateCode),]
    Shared1 = Shared1[order(as.character(Shared1$Table), as.character(Shared1$Row)), ]
    Shared2 = Sub2[paste(Sub2$Table, Sub2$Row, Sub2$StateCode) %in% paste(Sub1$Table, Sub1$Row, Sub1$StateCode),]
    Shared2 = Shared2[order(as.character(Shared2$Table), as.character(Shared2$Row)), ]
    if(nrow(Shared1) != nrow(Shared2)) stop("Problem with CompareRows(): nrow(Shared1)!=nrow(Shared2")
    
    CompShared = data.frame(Table = Shared1$Table
                            ,Row = Shared1$Row
                            ,StateCode = Shared1$StateCode
                            ,StateName = Shared1$StateName
                            ,RegionCode = Shared1$RegionCode
                            ,RegionName = Shared1$RegionName)
    
    CompShared$MeanAc1 = Shared1$MeanAcresInDomain
    CompShared$MeanAc2 = Shared2$MeanAcresInDomain
    CompShared$MeanAcDif = Shared1$MeanAcresInDomain - Shared2$MeanAcresInDomain
    CompShared$VarMeanAcDif = Shared1$VarMeanAcresInDomain + Shared2$VarMeanAcresInDomain
    CompShared$ZvalMeanAcDif = CompShared$MeanAcDif/sqrt(CompShared$VarMeanAcDif)
    CompShared$PropOwn1 = Shared1$PropOwnInDomain
    CompShared$PropOwn2 = Shared2$PropOwnInDomain
    CompShared$PropOwnDif = Shared1$PropOwnInDomain - Shared2$PropOwnInDomain
    CompShared$VarPropOwnDif = Shared1$VarPropOwnInDomain + Shared2$VarPropOwnInDomain
    CompShared$ZvalPropOwnDif = CompShared$PropOwnDif/sqrt(CompShared$VarPropOwnDif)
    CompShared$PropAc1 = Shared1$PropAcInDomain
    CompShared$PropAc2 = Shared2$PropAcInDomain
    CompShared$PropAcDif = Shared1$PropAcInDomain - Shared2$PropAcInDomain
    CompShared$VarPropAcDif = Shared1$VarPropAcInDomain + Shared2$VarPropAcInDomain
    CompShared$ZvalPropAcDif = CompShared$PropAcDif/sqrt(CompShared$VarPropAcDif)
  }
  return(CompShared)
}