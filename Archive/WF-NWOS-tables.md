Workflow for NWOS Tables
========================================================

The Raw Data Inputs
---------------------------------------------

### "POINTS_XXX"

* These are data on the individual FIA plots (points). They are pulled separately for each region from the NIMS database and are in a zip folder, under "InputData/ZipFiles/Current/Points.rar". They need to be extracted to InputData/Delete/.

* The naming protocol is "POINTS_XXX.csv", where XXX is the region abbrevation.

* We've so far used data that is summarized by count, but we have switched to a per-plot basis.

* The SQL code to generate this table is stored in "/InputData/Documentation /SQL_POINTS_XXX.sql"

* FIA's documentation on the field names of the POINTS_XXX.csv files can be found in "InputData/Documentation/FIA_Manual_5-1-2.pdf", but the important ones are:
  * STATECD - the state FIPS code
  * COUNTYCD - the county FIPS code, only critical for TX and OK (dividing into East and West)
  * INVYR - FIA inventory year, used to match to the corresponding NWOS year
  * COND_STATUS_CD - land cover code
    1. forest land
    5. non-sampled
  * COND_NONSAMPLE_REASN_CD - reason for not sampling
    2. denied access (most likely private)
    3. hazardous
  * OWNCD - the ownership type code
    41-44. private, non-family
    45. family
    
* The function, readPoints() brings in these files internally to the makePointsQuest() function.

### "Nonsampled"

* FIA plots that were not sampled by FIA - the reason for not sampling it and a descriptions of the stratum to which it belongs.

* The stratum description scheme ("STRATUM_DESCR") differs by region, but the goal is to "guess" which non-sampled points were private forest. That enables us to calculate a FIA response rate, the inverse of which we use to weight estimates. See the function makeNs() for exact details.

* For an exploration of the different STRATUM_DESCR's by region, open up the project, "nonsampling.Rproj".

### "LandArea"

* Total area of land by stratum (state, except for Texas and Oklahoma).

* Ideally, we will change the data format to by-county for the sake of consistency. Then it can be aggregated into by-strata.

* For now it is in the files "/InputData/LandArea.csv", "/InputData/LandAreaOK" and "InputData/LandAreaTX".

* The area data come from the Census Bureau, but there is no documentation for this at the moment.

### "YearMatch"

* Shows the FIA INVYR associated with each NWOSYEAR for each state. This is needed to pull out the right stratum info from POINTS_XXX for the corresponding NWOS cycle.

* Right now it's in two different files: "/InputData/YearMatch2006.csv" and "/InputData/YearMatch2011.csv".

### "QUEST"

* The NWOS questionnaire data.

* The 2006 data are in an Access database under InputData/Access/NWOS_DB_2006.accdb. The table QUEST needs to be exported to InputData/Delete/ and called "Quest2006.csv"

* Part of the 2011 data are in an Access Database under "InputData/Access/NWOS_2011.accdb". The table QUEST1 has only the first dozen or so fields from the full questionnaire data.frame. In Access, there is an update query that [already done] tacks on a "county" field so that TX and OK can be divided into East and West. QUEST1 needs to be exported to "InputData/Delete/Quest2011a". For unknown reasons, the rest of the data.frame is in a zip folder under "InputData/ZipFiles/Quest2011.rar", called "X_NWOS_2011... .csv" and should be extracted to "InputData/Delete/Quest2011b.csv"

* This system is VERY problematic and will likely be replaced in the future.

Bringing in the Raw Data
---------------------------------------------
Right now, these files are brought in with a messy function called makePointsQuest(). This needs desperately to be cleaned up, but it can be viewed on my Github site at https://github.com/Brent-Dickinson/estimation-package under /data setup/. If you have the library on your system, just give the argument datawd = "/your directory/" to makePointsQuest():


```r
source("data setup/makePointsQuest.R")
makePointsQuest(datawd = "e:/brent/nwos/inputdata/")
```


Consolidating stratum info
---------------------------------------------
Once these files are loaded, the first step is to use them to create a simplified data.frame that contains all necessary information about each stratum. The function, makeNs() does this automatically - you just have to specify the names you've given to the 5 input files. Of course, until the package actually becomes a package, you'll need to manually source all the necessary functions:


```r
functions = list("makeQuestAll", "makeNs", "stateTables", "regionTables", "hheCore", 
    "cov1", "varianceRatio", "varDifferenceRatios", "stateTableContinuous")
functions = sapply(functions, function(x) paste("functions/", x, ".R", sep = ""))
for (i in 1:length(functions)) {
    source(functions[i])
    rm(i)
}
```


Now do the makeNs() thing:


```r
ns_11 = makeNs(quest = quest_11, points = points_11, nonsampled = nonsampled_11)
ns_N11 = makeNs(quest = quest_11, points = points_11, nonsampled = nonsampled_N11)
ns_N11 = ns_N11[ns_N11$rpa_regionCd == 1, ]
ns_06 = makeNs(quest = quest_06, points = points_06, nonsampled = nonsampled_06)
```

```
## Error: undefined columns selected
```

```r
ns_N06 = makeNs(quest = quest_06, points = points_06, nonsampled = nonsampled_N06, 
    nwosCycle = 2006)
ns_N06 = ns_N06[ns_N06$rpa_regionCd == 1, ]
```


Estimate response probabilities for inverse weighting
---------------------------------------------
The next step is to decide on a way to estimate the NWOS response probabilies for all responding ownerships. At present we only have code to use the "naive" method, which is in the file construct response rates.R. Source this:


```r
source("data setup/construct response rates.R")
```


Tweak QUEST to get desired variables into an estimation-friendly format
---------------------------------------------

Next we refine the QUEST file (remove acreage non-respondents) and augment it with variables of interest created from the raw variables. The function, makeQuestAll(), does this. The raw QUEST Variables for the most part cannot be used as is because of issues like -2's, 8's, 9's, and idiosyncratic differences between 2006 and later variable coding protocols.


```r
questAll_11 = makeQuestAll(quest = quest_11, nwosCycle = 2011)
```



Using makeNs() and makeQuestAll(), we've assembled the two components needed for any kind of estimation. Now we move on to whatever it is that we want to do with the data. The most basic task weill be to create the NWOS tables from GTR-NRS-27.

Making the NWOS tables
---------------------------------------------
Whether state, regional, or national level estimates are desired, we start with the function stateTables() for categorical variables, and stateTableContinuous() for continuous variables. 

Nearly all NWOS estimators (except regression, etc) are built on the Hansen-Hurwitz estimator of total, which is what hheCore() does. Most variance estimators require covariance terms, which cov1() takes care of. stateTables() and stateTableContinuous() are built on these. Both functions require the following inputs:

1. A vector of column indexes from questAllXX that indicate the variables for which you want estimates.
2. The questAllXX data.frame.
3. The stratumInfo data.frame (constructed using makeNs).
4. A factor with length == nrow(questAllXX), with level "population" for observations in the population of interest. For family forest ownerships, this is augmented to the questAllXX data.frame itself. For all private forest ownerships, simply use factor(rep("population", nrow(questAllXX)))
5. The NWOS cycle (2006 or 2011) (This is redundant and will be changed later).

### A note on why different functions are used for categorical and continuous variables:

* For categorical variables, what we really want is number of acres and ownerships in each category of the variable. What stateTables() does is apply hheCore() separately to acres owned and to a vector of 1's to get those respective estimates. The ratio of acres and ownerhsips is the mean. stateTables() also estimates the number of acres and ownerships in the population of interest, for purposes of estimating proportions of ownerships and acres in each category of the variable. 

* For continuous variables, what we want is the total for that variable. So stateTableContinuous() applies hheCore() to the continuous variable. stateTableContinuous() also estimates the total number of ownerships in the population of interest for purposes of estimating the mean value of the continuous variable.

* In order to be able to combine output from stateTables() and stateTableContinuous() into one data.frame, they needed to have the same column names. As a result, output from stateTableContinuous() is a little awkward to interpret. Specifically:
1. ownershipsInDomain, ownershipsInPopulation and acresInPopulation mean just what they sound like. ownershipsInDomain will always equal ownershipsInPopulation.
2. But, acresInDomain refers to the estimate of total, not for acres, but for the continuous variable of interest (say, age). And meanAcresInDomain refers to the estimated mean value of that variable per ownership. Per acre mean should be incorporated at some point.
3. Proportions are obviously not meaningful, and are all set to 1. Their variances are set to "NA".

### Anyway, first build the stratum level tables:


```r
t23State_11 = stateTables(385:387, questAll_11, ns_11, factor(rep("population", 
    nrow(questAll_11))))
```

```
## [1] "estimated total time: 7 sec."
```

```r
t433State_11 = stateTables(c(388:ncol(questAll_11), 379:380, 382:384), questAll_11, 
    ns_11, questAll_11$population__family)
```

```
## [1] "estimated total time: 183 sec."
```

```r
tAgeState_11 = stateTableContinuous(c(14, 273:274), questAll_11, ns_11, questAll_11$population__family)
t433State_11 = rbind(t433State_11, tAgeState_11)
```


### Then, to make the reginal and national tables:
Use regionTables(), which simply sums rows for totals and variances and combines rows appropriately for means, proportions and their variances. The function regionTables() only requires the stateTables() output:


```r
t23Region_11 = regionTables(t23State_11[t23State_11$stateCd != 2, ])
```

```
## Loading required package: plyr
```

```r
t433Region_11 = regionTables(t433State_11[t433State_11$stateCd != 2, ])
```


### Where to send them:
Finally, you can write these to the output folder in the dropbox, after changing "outputwd" to the appropriate directory for your computer:


```r
outputwd = "C:/Users/FFRC_BRENT/Dropbox/NWOS/brents nwos stuff/output data/"
write.csv(t23State_11, paste(outputwd, "nwos tables/nwos tables 2-3 by state", 
    substr(Sys.time(), 1, 10), ".csv", sep = ""), row.names = F)
write.csv(t23Region_11, paste(outputwd, "nwos tables/nwos tables 2-3 by region", 
    substr(Sys.time(), 1, 10), ".csv", sep = ""), row.names = F)
write.csv(t433State_11, paste(outputwd, "nwos tables/nwos tables 4-33 by state", 
    substr(Sys.time(), 1, 10), ".csv", sep = ""), row.names = F)
write.csv(t433Region_11, paste(outputwd, "nwos tables/nwos tables 4-33 by region", 
    substr(Sys.time(), 1, 10), ".csv", sep = ""), row.names = F)
```

