#######################################################################
## this function adds to quest some useful factor variables constructed from quest variables. 
## it is needed to prepare quest for indiv.R.
## if there are problems with the resulting quest_all data frame, look into the internal functions below.
## in particular, this code leaves -1's and -3's as a separate category for estimation.
## however, the indiv.R function gets rid of -3's at present.
## problems may result from how the code below recodes -2's.

MakeQuestAll = function(quest = quest_11, NwosCycle = 2011)
{
  binary = function(x
                    ,label_neg3 = '-3'
                    ,label_neg2 = 'row_02_No'
                    ,label_neg1 = 'row_99_No answer'
                    ,label_2 = NULL
                    ,label_8 = 'row_98_Not applicable'
                    ,label_9 = 'row_97_Uncertain')
  {
    value_2 = 2
    value_8 = 8
    value_9 = 9
    if(is.null(label_2)) value_2 = NULL
    if(is.null(label_8)) value_8 = NULL
    if(is.null(label_9)) value_9 = NULL
    factor_var = x
    labels = c(label_neg3, label_neg2, label_neg1, 'row_02_No', 'row_01_Yes', label_2, label_8, label_9)
    values = c(-3, -2, -1, 0, 1, value_2, value_8, value_9)
    for(i in 1:length(labels))
    {
      factor_var[x == values[i]] = labels[i]
    }
    factor_var = factor(factor_var)
    if(any(is.na(factor_var))) print('warning: problem with binary(): NAs present')
    return(factor_var)
  }
  cats = function(var
                  ,labels
                  ,levels
                  ,neg2_val = 0
                  ,neg2_label = NULL
                  ,cont = F
                  ,right = T
                  ,fam = T)
  {
    if(cont == F & length(labels) != length(levels)) stop('labels and levels have different lengths')
    if(cont == F)
    {
      for(i in 1:length(levels))
      {
        var[var == levels[i]] = labels[i]
      }
      var[var == -2] = neg2_val
    }
    else
    {
      var[var == -2] = neg2_val
      var[!var %in% c(-3:-1)] = as.character(cut(as.numeric(var[!var %in% c(-3:-1)]), breaks = levels, labels = labels, right = right))
    }
    var[var == -1] = 'row_99_No answer'
    labels = c(labels)
    if(neg2_val %in% levels) var = factor(var, levels = c('-3', labels, 'row_99_No answer'))
    else var = factor(var, levels = c('-3', labels, 'row_99_No answer', neg2_label))
    return(var)
  }
  likert = function(v_names
                    ,labels = c('row_01_Important', 'row_02_Unimportant', 'row_98_Not applicable')
                    ,levels = c(0, 2, 7, Inf)
                    ,cont = F
                    ,table
                    ,neg2_label = "ignore"
                    ,val_neg2 = "ignore"
                    ,sep = ".")
  {
    likert_vars = data.frame(quest[,v_names])
    for(i in 1:ncol(likert_vars))
    {
      likert_vars[,i] = cats(likert_vars[,i]
                             ,labels = labels
                             ,levels = levels
                             ,cont = cont
                             ,neg2_val = val_neg2
                             ,neg2_label = neg2_label)
      colnames(likert_vars)[i] = paste(table, v_names[i], sep = sep)
    }
    if(any(is.na(likert_vars))) print(paste('warning: NAs produced by likert():', table))
    return(likert_vars)
  }
  any_true = function(v_names)
  {
    vars = quest[,names(quest) %in% v_names]
    value_neg1 = apply(vars, 1, function(x) all(x == -1))
    value_neg3 = apply(vars, 1, function(x) all(x == -3))
    value_8 = apply(vars, 1, function(x) all(x == 8))
    value_2 = apply(vars, 1, function(x) all(x == 2))
    value_1 = apply(vars, 1, function(x) any(x == 1))
    if(NwosCycle == 2011)
    {
      out_var = ifelse(value_neg3 == T, '-3'
                               ,ifelse(value_neg1 == T, 'row_99_No answer'
                                       ,ifelse(value_8 == T, 'row_98_Not applicable'
                                               ,ifelse(value_1 == T, 'row_01_Yes'
                                                       ,'row_02_No'))))    
    }
    if(NwosCycle == 2006)
    {
      out_var = ifelse(value_neg3 == T, '-3'
                               ,ifelse(value_neg1 == T, 'row_99_No answer'
                                       ,ifelse(value_2 == T, 'row_97_Uncertain'
                                               ,ifelse(value_1 == T, 'row_01_Yes'
                                                       ,'row_02_No'))))
    }
    out_var = factor(out_var, levels = c('-3', 'row_01_Yes', 'row_02_No', 'row_99_No answer'))
    return(out_var)
  }
  factorize = function(v_names
                       ,combine_cols = NULL
                       ,labels
                       ,levels_1 = 1
                       ,exclusive = T
                       ,neg3val = '-3'
                       ,neg2val = '0'
                       ,neg1val = 'row_99_No answer'
                       ,multi_val = NULL
                       ,table = NULL
                       ,out_neg1 = F)
  {
    vars = data.frame(NULL)
    for(i in 1:length(v_names))
    {
      vars[1:nrow(quest), i] = quest[,v_names[i]]
      colnames(vars)[i] = v_names[i]
    }
    vars[vars == -2] = neg2val
    out_var = rep(NA, nrow(quest))
    if(exclusive == T & is.null(combine_cols))
    {
      for(i in 1:length(labels))
      {
        out_var[vars[,i] %in% levels_1] = labels[i]
      }
      out_var[apply(vars, 1, function(x) any(x == -1)) == T] = neg1val
      out_var[apply(vars, 1, function(x) any(x == -3)) == T] = neg3val
      out_var[apply(vars, 1, function(x) any(x == 8)) == T] = neg3val
      out_var = factor(out_var, levels = c(neg3val, labels, multi_val, neg1val))
    }
    if(exclusive == T & is.null(combine_cols) == F)
    {
      if(length(labels) != length(combine_cols)) stop('problem with factorize: combine_cols and labels have different lengths')
      for(i in 1:length(labels))
      {
        vars_combined = data.frame(vars[,combine_cols[[i]]])
        out_var[apply(vars_combined, 1, function(x) any(x %in% levels_1))] = labels[i]
      }
      out_var[apply(vars, 1, function(x) any(x == -1)) == T] = neg1val
      out_var[apply(vars, 1, function(x) any(x == -3)) == T] = neg3val
      out_var[apply(vars, 1, function(x) any(x == 8)) == T] = neg3val
      out_var = factor(out_var, levels = c(neg3val, labels, multi_val, neg1val))
    }
    if(exclusive == F & is.null(combine_cols) & is.null(multi_val) == F)
    {
      multi = apply(vars, 1, function(x) ifelse(any(x %in% levels_1), sum(as.numeric(x)), 0))
      single = rep(0, length(multi))
      single[multi == 1] = apply(vars[multi == 1,], 1, function(x) labels[x %in% levels_1])
      out_var[single != 0] = single[single != 0]
      out_var[multi > 1] = multi_val
      out_var[apply(vars, 1, function(x) any(x == -1)) == T] = neg1val
      out_var[apply(vars, 1, function(x) any(x == -3)) == T] = neg3val
      out_var[apply(vars, 1, function(x) any(x == 8)) == T] = neg3val
      out_var[apply(vars, 1, function(x) all(x == 0)) == T] = neg1val
      out_var = factor(out_var, levels = c(neg3val, labels, multi_val, neg1val, 'ignore'))
    }
    if(exclusive == F & is.null(combine_cols) & is.null(multi_val))
    {
      out_var = data.frame(matrix(NA, nrow = nrow(vars), ncol = length(labels)))
      for(j in 1:ncol(out_var))
      {
        out_var[vars[,j] %in% levels_1, j] = labels[j]
        out_var[vars[,j] == -3, j] = neg3val
        out_var[!vars[,j] %in% c(-3, levels_1), j] = 'ignore'
        out_var[,j] = factor(out_var[,j], levels = c(neg3val, labels[j], 'ignore'))
      }
      if(out_neg1 == T)
      {
        out_neg1 = rep('ignore', nrow(vars))
        out_neg1[apply(vars, 1, function(x) any(x == -1)) == T] = neg1val
        out_neg1[apply(vars, 1, function(x) any(x == -3)) == T] = neg3val
        out_neg1 = factor(out_neg1, levels = c(neg3val, neg1val, 'ignore'))
        out_var = cbind(out_var, out_neg1)
        colnames(out_var) = c(labels, neg1val)        
      }
      else
      {
        colnames(out_var) = labels
      }
    }
    if(exclusive == F & is.null(combine_cols) == F & is.null(multi_val))
    {
      if(length(labels) != length(combine_cols)) stop('problem with factorize: combine_cols and labels have different lengths')
      out_var = data.frame(matrix('ignore', nrow = nrow(vars), ncol = length(labels)), stringsAsFactors = F)
      for(i in 1:length(labels))
      {
        vars_combined = data.frame(vars[,combine_cols[[i]]])
        out_var[apply(vars_combined, 1, function(x) any(x %in% levels_1)), i] = labels[i]
        out_var[apply(vars_combined, 1, function(x) all(x == -3)), i] = neg3val
        out_var[,i] = factor(out_var[,i], levels = c(neg3val, labels[i], 'ignore'))
      }
      if(out_neg1 == T)
      {
        out_neg1 = rep('ignore', nrow(vars))
        out_neg1[apply(vars, 1, function(x) any(x == -3)) == T] = '-3'
        out_neg1[apply(vars, 1, function(x) any(x == -1)) == T] = neg1val
        out_neg1 = factor(out_neg1, levels = c('-3', neg1val, 'ignore'))
        out_var = cbind(out_var, out_neg1)
        colnames(out_var) = c(labels, neg1val)
        
      }
      else
      {
        colnames(out_var) = labels
      }
    }
    if(exclusive == F & is.null(combine_cols) == F & is.null(multi_val) == F)
    {
      if(length(labels) != length(combine_cols)) stop('problem with factorize: combine_cols and labels have different lengths')
      out_var = data.frame(matrix('ignore', nrow = nrow(vars), ncol = length(labels)), stringsAsFactors = F)
      for(i in 1:length(labels))
      {
        vars_combined = data.frame(vars[,combine_cols[[i]]])
        out_var[apply(vars_combined, 1, function(x) any(x == -3)), i] = neg3val
        out_var[apply(vars_combined, 1, function(x) any(x == 1)), i] = labels[i]
        out_var[,i] = factor(out_var[,i], levels = c(neg3val, labels[i], 'ignore'))
      }
      out_neg1 = rep('ignore', nrow(vars))
      out_neg1[apply(vars, 1, function(x) any(x == -1)) == T] = neg1val
      out_neg1 = factor(out_neg1, levels = c(neg1val, 'ignore'))
      
      multi = apply(vars, 1, function(x) ifelse(any(x == 1), ifelse(sum(as.numeric(x)) > 1, multi_val, 'ignore')))
      multi = factor(multi, levels = c(multi_val, 'ignore'))
      out_var = cbind(out_var, multi, out_neg1)
      colnames(out_var) = c(labels, multi_val, neg1val)
    }
    if(any(is.na(as.character(out_var)))) print(paste('warning: NAs produced by factorize():', table))
    if(ncol(as.data.frame(out_var)) > 1) colnames(out_var) = paste(table, colnames(as.data.frame(out_var)), sep = '.')
    return(out_var)
  }
  if(NwosCycle == 2011)
  {
    population_family = factor(ifelse(quest$OWNTYPE %in% 1:4, 'population'
                                       ,ifelse(quest$OWNTYPE %in% 5:6, 'non_population'
                                               ,'unknown')))
    Q01__OWNTYPE_allprivate_category = factor(cats(var = quest$OWNTYPE
                              ,labels = c("row_01_Individual"
                                          ,"row_02_Joint"
                                          ,"row_03_Family partnership"
                                          ,"row_04_Trust or estate"
                                          ,"row_05_Corporation or business partnership"
                                          ,"row_05_Other")
                              ,levels = 1:6
                              ,fam = F))
    Q01__OWNTYPE_allprivate_aggregate = factor(ifelse(quest$OWNTYPE %in% c(1:4)
                                             ,'row_01_Family'
                                             ,ifelse(quest$OWNTYPE %in% c(5:6)
                                                     ,'row_02_Other private'
                                                     ,'ignore'))
                                      ,levels = c('ignore'
                                                  ,'row_01_Family'
                                                  ,'row_02_Other private')
                                      ,ordered = T)
    Q01__OWNTYPE_allprivate = factor('row_03_Total Private')
    Q02__OWNERS_NUMBER_category = cats(ifelse(quest$OWNERS_NUMBER == 0, -1, quest$OWNERS_NUMBER)
                                      ,labels = c('row_01_1', 'row_02_2', 'row_03_3-5', 'row_04_6-10', 'row_05_10+')
                                      ,levels = c(0, 1, 2, 5, 10, Inf)
                                      ,cont = T)
    Q02__OWNERS_NUMBER_continuous = quest$OWNERS_NUMBER
    percfor = quest$AC_WOOD/quest$AC_LAND
    percfor[percfor < 0 | percfor > 1 | is.na(percfor)] = -1
    Q03_ab__percent_forest = cats(percfor
                                ,labels = c('row_01_<25','row_02_25-49','row_03_50-74','row_04_75-99','row_05_100')
                                ,levels = c(-1,.24,.49,.74,.99,1)
                                ,cont = T)
    Q03_b__AC_WOOD_allprivate_category = cut(quest$AC_WOOD
                         ,breaks = c(0,9,19,49,99,199,499,999,4999,9999,Inf)
                         ,labels = c('row_01_1-9','row_02_10-19','row_03_20-49','row_04_50-99','row_05_100-199','row_06_200-499','row_07_500-999','row_08_1000-4999','row_09_5000-9999','row_10_10000+'))
    Q03_b__AC_WOOD_category = factor(ifelse(quest$OWNTYPE %in% 5:6, 'ignore'
                                   ,as.character(Q03_b__AC_WOOD_allprivate_category)))
    # different size categories for the 4x4 super graphic
    Q03_b__AC_WOOD_allprivate_category_graph = cut(quest$AC_WOOD
                               ,breaks = c(0,9,49,99,499,Inf)
                               ,labels = c('row_01_1-9', 'row_02_10-49', 'row_03_50-99', 'row_04_100-499', 'row_05_500+'))
    Q03_b__AC_WOOD_category_graph = factor(ifelse(quest$OWNTYPE %in% 5:6, 'ignore'
                                                ,as.character(Q03_b__AC_WOOD_allprivate_category_graph)))
    Q03_c__PARC_MULTI = binary(quest$PARC_MULTI
                             ,label_8 = NULL
                             ,label_9 = NULL)
    Q03_c__PARC_MULTI_NUMBER_continuous = ifelse(quest$PARC_MULTI_NUMBER == -2, 1, quest$PARC_MULTI_NUMBER)
    pmc = quest$PARC_MULTI_NUMBER
    pmc[pmc == 0] = -1
    Q03_c__PARC_MULTI_NUMBER_category = cats(pmc
                                 ,labels = c('row_01_1','row_02_2-9','row_03_10-99','row_04_100+')
                                 ,levels = c(0, 1, 9, 99, Inf)
                                 ,neg2_val = 1
                                 ,cont = T)
    # Even though there are 8's in HOME, they are all for OWNTYPE %in% 5:6 so the "Not applicable" category is irrelevant:
    Q04__HOME = binary(quest$HOME
                        ,label_9 = NULL)
    Q05__CABIN = binary(quest$CABIN
                     ,label_9 = NULL)
    Q06__FARM = binary(quest$FARM
                             ,label_9 = NULL)
    Q06__FARM_NEAR = binary(quest$FARM_NEAR
                           ,label_8 = NULL
                           ,label_9 = NULL)
    require(stringr)
    obj_names = names(quest)[grepl('^OBJ', names(quest)) &
                               grepl('.TXT', names(quest)) == F]
    obj_labels = c('row_01_To enjoy beauty or scenery'
                  ,'row_02_To protect nature or biological diversity'
                  ,'row_03_To protect water resources'
                  ,'row_04_To protect or improve wildlife habitat'
                  ,'row_05_For land investment'
                  ,'row_06_Is part of my home sit/primary residence'
                  ,'row_07_Is part of my cabin or vacation home site'
                  ,'row_08_Is part of my farm or ranch'
                  ,'row_09_For privacy'
                  ,'row_10_To raise my family'
                  ,'row_11_To pass land on to my children or other heirs'
                  ,'row_12_For firewood'
                  ,'row_13_For timber products, such as logs or pulpwood'
                  ,'row_14_For nontimber forest products, such as pine straw or berries'
                  ,'row_15_For hunting'
                  ,'row_16_For recreation, other than hunting'
                  ,'row_17_Other')
    Q07__OBJ = likert(v_names = obj_names
                      ,labels = c("row_01_Very important"
                                  ,"row_02_Important"
                                  ,"row_03_Moderately important"
                                  ,"row_04_Of little importance"
                                  ,"row_05_Not important"
                                  ,"row_98_Not applicable")
                      ,levels = c(5:1,8)
                      ,table = "Q07__"
                      ,sep = "")
    make_13 = function(vars, table, labels)
    {
      for(i in 1: ncol(vars))
      {
        vars[,i] = as.character(cats(vars[,i]
                            ,labels = c('not important', labels[i], "row_98_Not applicable")
                            ,levels = c(0, 3, 5, Inf)
                            ,cont = T))
        out_vars = vars
        out_vars[!out_vars[,i] %in% labels, i] = 'ignore'
        out_vars[,i] = factor(out_vars[,i])
      }
      colnames(out_vars) = paste(table, labels, sep = '.')
      return(out_vars)
    }
    Q07__OBJ_aggr = factorize(v_names = obj_names
                              ,labels = obj_labels
                              ,exclusive = F
                              ,table = "Q07__OBJ_aggr"
                              ,levels_1 = 4:5)
      
    Q08_a__ACQ_TYPE = factorize(v_names = names(quest)[grepl("ACQ_TYPE", names(quest)) &
                                                         grepl("TXT", names(quest)) == F]
                                         ,labels = c('row_01_Purchased'
                                                     ,'row_02_Inherited'
                                                     ,'row_03_Received as gift'
                                                     ,"row_04_Other")
                                         ,exclusive = F
                                         ,table = 'Q08_a__ACQ_TYPE')
    Q08_b__ACQ_SRC = factorize(v_names = names(quest)[grepl("ACQ_SRC", names(quest)) &
                                                        grepl("TXT", names(quest)) == F]
                                           ,labels = c("row_01_My parents and/or my spouse's parents"
                                                       ,'row_02_My spouse'
                                                       ,'row_03_Another family member'
                                                       ,"row_04_Other individual(s)"
                                                       ,"row_05_A business"
                                                       ,"row_06_A government agency"
                                                       ,"row_07_Other")
                                           ,exclusive = F
                                           ,table = 'Q08_b__ACQ_SRC')
    acq_year = ifelse(quest$ACQ_YEAR < 1900, -1
                      ,ifelse(quest$ACQ_YEAR > NwosCycle, -1, quest$ACQ_YEAR))
    Q08_c__ACQ_YEAR_category = cats(var = acq_year
                         ,labels = c("row_01_1950 or before"
                                     ,"row_02_1951-1975"
                                     ,"row_03_1976-1990"
                                     ,"row_04_1991-2000"
                                     ,"row_05_2001-present")
                         ,levels = c(0, 1950, 1975, 1990, 2000, Inf)
                         ,cont = T)
    tenure = quest$QUEST_YEAR - quest$ACQ_YEAR
    Q08_c__tenure_continuous = ifelse(quest$ACQ_YEAR < 0 | tenure < 0 | tenure > 80, -1, tenure)
    Q08_c__tenure_category = cats(ifelse(quest$ACQ_YEAR < 0 | tenure < 0, -1, tenure)
                            ,labels = c('row_01_<10', 'row_02_10-24', 'row_03_25-49', 'row_04_50+')
                            ,levels = c(0, 10, 25, 50, Inf)
                            ,cont = T
                            ,right = F)
    Q09_a__TRAN = cats(quest$TRAN
                     ,labels = c('row_01_0 (Never)','row_02_1', 'row_03_2-5', 'row_04_6+')
                     ,levels = c(0, 1, 2, 6)
                     ,cont = F)
    Q09_b__TRAN_TO = factorize(v_names = names(quest)[grepl("TRAN_TO_", names(quest)) &
                                                      grepl("TXT", names(quest)) == F]
                                              ,labels = c('row_01_My children'
                                                          ,'row_02_Another family member'
                                                          ,'row_03_Other individual(s)'
                                                          ,"row_04_A business"
                                                          ,"row_05_A government agency"
                                                          ,"row_06_Other")
                                              ,exclusive = F
                                              ,table = 'Q09_b__TRAN_TO')    
    Q09_c__TRAN_RECENT = binary(quest$TRAN_RECENT
                                        ,label_8 = NULL
                                        ,label_9 = NULL)
    Q10__MAN = factorize(v_names = names(quest)[grepl("MAN_", names(quest)) &
                                                  grepl("PLAN", names(quest)) == F &
                                                  grepl("TXT", names(quest)) == F]
                         ,labels = c("row_01_Me"
                                     ,"row_02_My spouse"
                                     ,"row_03_My children"
                                     ,"row_04_My parents"
                                     ,"row_05_Another family memeber"
                                     ,"row_06_My business partner"
                                     ,"row_07_My land manager or forester"
                                     ,"row_08_Other")
                         ,exclusive = F
                         ,table = "Q10__MAN")
    Q11__MAN_PLAN = binary(quest$MAN_PLAN
                           ,label_8 = NULL
                           ,label_9 = NULL)
    Q11__MAN_PLAN_IMPLEMENT = binary(quest$MAN_PLAN_IMPLEMENT
                                     ,label_8 = NULL
                                     ,label_9 = NULL)
    Q11__MAN_PLAN_WRITER = cats(var = quest$MAN_PLAN_WRITER
                                ,labels = c("row_01_I did"
                                            ,"row_02_Private consultant forester"
                                            ,"row_03_Forest industry forester"
                                            ,"row_04_State or local government forester"
                                            ,"row_05_Federal government forester"
                                            ,"row_06_Other")
                                ,levels = 1:6
                                ,neg2_val = "ignore"
                                ,neg2_label = "ignore")
    Q11__MAN_PLAN_NO = factorize(v_names = names(quest)[grepl("MAN_PLAN_NO_", names(quest)) &
                                                          grepl("TXT", names(quest)) == F]
                                 ,labels = c("row_01_I don't want/need one"
                                             ,"row_02_Too busy"
                                             ,"row_03_Too expensive"
                                             ,"row_04_Too complicated"
                                             ,"row_05_Other")
                                 ,exclusive = F
                                 ,table = "Q11__MAN_PLAN_NO")
    
    cut_names = names(quest)[(grepl("_OTH$", names(quest)) |
                                grepl("_PERS$", names(quest)) |
                                grepl("_SALE$", names(quest))) &
                               grepl("^CUT_", names(quest)) &
                               nchar(names(quest)) > 8]
    Q12_a__CUT_any = any_true(v_names = cut_names)
    Q12_a__CUT_type = factorize(v_names = cut_names
                               ,combine_cols = list(1:3, 4:6, 7:9, 10:12, 13:15)
                               ,labels = c('row_01_Logs'
                                           ,'row_02_Firewood'
                                           ,'row_03_Wood chips'
                                           ,'row_04_Unwanted trees'
                                           ,'row_05_Other')
                               ,exclusive = F
                               ,table = 'Q12_a__CUT_type')
    Q12_a__CUT_commercial = any_true(v_names = c("CUT_LOG_SALE", "CUT_CHIP_SALE"))
    Q12_a__CUT_personal = any_true(v_names = names(quest)[grepl("^CUT_", names(quest)) & grepl("_PERS", names(quest))])
    Q12_b__CUT_FORESTER = binary(quest$CUT_FORESTER)
    Q12_c__CUT_LOGGER = binary(quest$CUT_LOGGER)
    
    ntfp_names = names(quest)[grepl('^NTFP', names(quest)) &
                                grepl('.NO', names(quest)) == F &
                                grepl('TXT', names(quest)) == F &
                                nchar(names(quest)) > 9]
    Q13__NTFP_any = any_true(v_names = ntfp_names)
    Q13__NTFP_type = factorize(v_names = ntfp_names
                               ,combine_cols = list(1:2, 3:4, 5:6, 7:8, 9:10)
                               ,labels = c('row_01_Edibles'
                                           ,'row_02_Medicinals'
                                           ,'row_03_Landscaping'
                                           ,'row_04_Decoratives'
                                           ,'row_05_Other')
                               ,exclusive = F
                               ,table = 'Q13__NTFP_type')
    Q13__NTFP_sale_personal = factorize(v_names = c('NTFP_SALE', 'NTFP_PERS')
                                                 ,labels = c('row_01_For sale', 'row_02_For personal use')
                                                 ,exclusive = F
                                                 ,table = 'Q13__NTFP_sale_personal')
    Q14__ACT_CUT = any_true(v_names = c('ACT_CUT_SALE', 'ACT_CUT_PERS'))
    Q14__ACT_NTFP = binary(quest$ACT_NTFP
                           ,label_8 = NULL
                           ,label_9 = NULL)
    act_names = names(quest)[grepl('^ACT', names(quest)) &
                               grepl("NONE", names(quest)) == F]
    act_labels = c('row_01_Cut and/ore removed trees for sale'
                   ,'row_02_Cut and/ore removed trees for own use'
                   ,'row_03_Collected nontimber forest products'
                   ,"row_04_Reduced fire hazard"
                   ,'row_05_Controlled burn/prescribed fire'
                   ,'row_06_Eliminated or reduced invasive plants'
                   ,'row_07_Eliminated or reduced unwanted insects or diseases'
                   ,'row_08_Road construction or maintenance'
                   ,"row_09_Trail construction or maintenance"
                   ,'row_10_Improved wildlife habitat'
                   ,'row_11_Livestock grazing')
    Q14__ACT_type = factorize(v_names = act_names
                                          ,labels = act_labels
                                          ,exclusive = F
                                          ,table = 'Q14__ACT_type')
    names_futureplans = names(quest)[grepl('^FUT_', names(quest))]
    Q15__FUT_type = factorize(v_names = names_futureplans
                              ,labels = act_labels
                              ,exclusive = F
                              ,table = 'Q15__FUT_type'
                              ,levels_1 = 4:5)
    Q16__LEASE = binary(quest$LEASE
                              ,label_8 = NULL
                              ,label_9 = NULL)
    Q16_a__LEASE_ACT_type = factorize(v_names = names(quest)[grepl('^LEASE_ACT', names(quest)) &
                                                              grepl('.TXT', names(quest)) == F]
                                     ,labels = c('row_01_Hunting'
                                                 ,'row_02_Recreation (other than hunting)'
                                                 ,'row_03_Graze/pasture livestock'
                                                 ,'row_04_Land conservation'
                                                 ,'row_05_Carbon sequestration'
                                                 ,'row_06_Public water supply protection'
                                                 ,'row_07_Wildlife habitat/biodiversity'
                                                 ,'row_08_Other')
                                     ,exclusive = F
                                     ,table = 'Q16_a__LEASE_ACT_type')
    Q16_b__LEASE_5YR = binary(quest$LEASE_5YR
                             ,label_8 = NULL
                             ,label_9 = NULL)
    knowledge_labels = c("row_01_Extremely familiar"
                        ,"row_02_Moderately familiar"
                        ,"row_03_Somewhat familiar"
                        ,"row_04_Slightly familiar"
                        ,"row_05_Not at all familiar")
    Q17_a__COST_KNOW = cats(var = quest$COST_KNOW
                           ,labels = knowledge_labels
                           ,levels = 5:1)
    Q17_b__COST = binary(quest$COST
                                   ,label_8 = NULL)
    Q17_c__COST_5YR = binary(quest$COST_5YR
                                          ,label_8 = NULL)
    Q18_a__CERT_KNOW = cats(quest$CERT_KNOW
                                           ,labels = knowledge_labels
                                           ,levels = 5:1)
    Q18_b__CERT = binary(quest$CERT
                                   ,label_8 = NULL)
    Q19_a__TAX_KNOW = cats(quest$TAX_KNOW
                          ,labels = knowledge_labels
                          ,levels = 5:1)
    Q19_b__TAX = binary(quest$TAX
                       ,label_8 = NULL)
    Q20_a__EASE_KNOW = cats(quest$EASE_KNOW
                           ,labels = knowledge_labels
                           ,levels = 5:1)
    Q20_b__EASE = binary(quest$EASE
                                 ,label_8 = NULL)
    likely_labels = c("row_01_Extremely likely"
                      ,"row_02_Likely"
                      ,"row_03_Undecided"
                      ,"row_04_Unlikely"
                      ,"row_05_Extremely unlikely"
                      ,"row_98_Not applicable")
    Q20_c__EASE_5YR = likert(v_names = 'EASE_5YR'
                            ,labels = likely_labels
                            ,levels = c(5:1,8)
                            ,table = 'Q20_c__EASE_5YR')
    Q21_a__REC_WHO = factorize(v_names = names(quest)[grepl("REC_WHO", names(quest)) &
                                                       grepl("TXT", names(quest)) == F]
                              ,labels = c("row_01_Me and/or my spouse"
                                          ,"row_02_My children"
                                          ,"row_03_Other family members"
                                          ,"row_04_Friends"
                                          ,"row_05_Neighbors"
                                          ,"row_06_The general public for free"
                                          ,"row_07_The general public for a fee"
                                          ,"row_08_Other")
                              ,table = "Q21_a__REC_WHO"
                              ,exclusive = F)
    Q21_b__REC_HOW = factorize(v_names = names(quest)[grepl("REC_HOW", names(quest)) &
                                                       grepl("TXT", names(quest)) == F]
                              ,labels = c("row_01_Hunting"
                                          ,"row_02_Fishing"
                                          ,"row_03_Hiking/walking"
                                          ,"row_04_Bicycling"
                                          ,"row_05_Camping"
                                          ,"row_06_Horseback riding"
                                          ,"row_07_Skiing or snowmobiling"
                                          ,"row_08_Off-road vehicles, such as ATVs or snowmobiles"
                                          ,"row_09_Other")
                              ,table = "Q21_b__REC_HOW"
                              ,exclusive = F)
    Q22__POST = binary(quest$POST
                       ,label_8 = NULL
                       ,label_9 = NULL)
    Q22__POST_type = factorize(v_names = names(quest)[grepl("POST_", names(quest)) &
                                                        grepl("TXT", names(quest)) == F]
                               ,labels = c("row_01_Trespassing"
                                           ,"row_02_Hunting"
                                           ,"row_03_Motorized vehicles"
                                           ,"row_04_Other")
                               ,exclusive = F
                               ,table = "Q22__POST_type")
    Q23__ADVICE = binary(quest$ADVICE
                                        ,label_8 = NULL
                                        ,label_9 = NULL)
    Q23_a__ADV_TOP = factorize(v_names = names(quest)[grepl("ADV_TOP", names(quest)) &
                                                       grepl("TXT", names(quest)) == F]
                              ,labels = c("row_01_Insects or plant diseases"
                                          ,"row_02_Wildlife or wildlife habitat"
                                          ,"row_03_Timber production"
                                          ,"row_04_Land conservation"
                                          ,"row_05_Fire safety"
                                          ,"row_06_Other")
                              ,exclusive = F
                              ,table = "Q23_a__ADV_TOP")
    Q23_b__ADV_METH = factorize(v_names = names(quest)[grepl("ADV_METH", names(quest)) &
                                                        grepl("TXT", names(quest)) == F]
                               ,labels = c("row_01_Talked to someone"
                                           ,"row_02_Someone visited my land"
                                           ,"row_03_Received a brochure or other written material"
                                           ,"row_04_From the internet"
                                           ,"row_05_Attended a conference or workshop"
                                           ,"row_06_Other")
                               ,exclusive = F
                                ,table = "Q23_b__ADV_METH")
    Q23_c__ADV_SRC = factorize(v_names = c('ADV_SRC_STATE'
                                                     ,'ADV_SRC_FED'
                                                     ,'ADV_SRC_PRIV'
                                                     ,'ADV_SRC_OWNER'
                                                     ,'ADV_SRC_FAM'
                                                     ,'ADV_SRC_OTH')
                                         ,labels = c('row_01_State forestry employee'
                                                     ,'row_02_Federal employee'
                                                     ,'row_03_Private consultant'
                                                     ,'row_04_Another landowner'
                                                     ,'row_05_Family member or friend'
                                                     ,'row_06_Other')
                                         ,exclusive = F
                                         ,table = 'Q23_c__ADV_SRC')
    help_labels = c("row_01_Very helpful"
                    ,"row_02_Helpful"
                    ,"row_03_Moderately helpful"
                    ,"row_04_Of little help"
                    ,"row_05_Of no help"
                    ,"row_98_Not applicable"
                    ,"row_97_Uncertain")
    help_type_names = c("HELP_WOOD"
                        ,"HELP_TRAN"
                        ,"HELP_EASE"
                        ,"HELP_COST"
                        ,"HELP_TAX"
                        ,"HELP_TIM"
                        ,"HELP_OTH")
    Q24__HELP_type = likert(v_names = help_type_names
                            ,labels = help_labels
                            ,levels = c(5:1,8,9)
                            ,cont = F
                            ,table = "Q24__"
                            ,sep = "")
    Q24__HELP_aggr = factorize(v_names = help_type_names
                               ,labels = c("row_01_Advice on woodland management"
                                           ,"row_02_Advice on how to transfer land to the next generation"
                                           ,"row_03_Advice on selling or giving away development rights"
                                           ,"row_04_Cost-sharing for woodland management"
                                           ,"row_05_More favorable tax policies"
                                           ,"row_06_Stronger timber markets"
                                           ,"row_07_Other")
                               ,exclusive = F
                               ,table = "Q24__HELP_aggr"
                               ,levels_1 = 4:5)
    help_names = names(quest)[grepl('^HELP_TYPE', names(quest)) & grepl('.TXT', names(quest)) == F]
    Q25__HELP_TYPE = factorize(v_names = help_names
                                                ,labels = c('row_01_Talk to someone'
                                                            ,'row_02_Have someone visit my land'
                                                            ,'row_03_Written materials, such as brochures or publications'
                                                            ,'row_04_Internet'
                                                            ,'row_05_Conference or workshop'
                                                            ,'row_06_Other'
                                                            ,'row_07_Do not want/need advice or information')
                                                ,exclusive = F
                                                ,table = 'Q25__HELP_TYPE')
    
    concern_names = names(quest)[grepl('^CNC_', names(quest)) & grepl('.TXT', names(quest)) == F]
    concern_labels = c("row_01_Great concern"
                       ,"row_02_Concern"
                       ,"row_03_Moderate concern"
                       ,"row_04_Of little concern"
                       ,"row_05_No concern"
                       ,"row_98_Not applicable")
    Q26__CNC_type = likert(v_names = concern_names
                           ,labels = concern_labels
                           ,levels = c(5:1,8)
                           ,table = 'Q26__'
                           ,sep = "")
    cnc_labels = c("row_01_Air pollution"
                   ,"row_02_Damage or noise from off-road vehicles"
                   ,"row_03_Damage from animals"
                   ,"row_04_Development of nearby lands"
                   ,"row_05_Drought or lack of water"
                   ,"row_06_Global climate change"
                   ,"row_07_High property taxes"
                   ,"row_08_Invasive plant species"
                   ,"row_09_Keeping land intact for future generations"
                   ,"row_10_Misuse of wooded land, such as vandalism or dumping"
                   ,"row_11_Trespassing or poaching"
                   ,"row_12_Unwanted insects or diseases"
                   ,"row_13_Water pollution"
                   ,"row_14_Wildfire"
                   ,"row_15_Wind or ice storms"
                   ,"row_16_Other")
    Q26__CNC_aggr = factorize(v_names = concern_names
                              ,labels = cnc_labels
                              ,exclusive = F
                              ,table = "Q26__CNC_aggr"
                              ,levels_1 = 4:5)
    Q27__TRAN_FUT = likert(v_names = "TRAN_FUT"
                           ,labels = likely_labels
                           ,levels = c(5:1,8)
                           ,table = "Q27__"
                           ,sep = "")
    Q27_a__TRAN_FUT_TO_type = factorize(v_names = names(quest)[grepl("TRAN_FUT_TO_", names(quest)) &
                                                                grepl("TXT", names(quest)) == F]
                                       ,labels = c("row_01_My children"
                                                   ,"row_02_Another family member(s)"
                                                   ,"row_03_Other individual(s)"
                                                   ,"row_04_A business"
                                                   ,"row_05_A government agency"
                                                   ,"row_06_Don't know"
                                                   ,"row_07_Other")
                                       ,exclusive = F
                                       ,table = "Q27_a__TRAN_FUT_TO")
    Q27_b__TRAN_FUT_WHY_type = factorize(v_names = names(quest)[grepl("TRAN_FUT_WHY_", names(quest)) &
                                                                 grepl("TXT", names(quest)) == F]
                                        ,labels = c("row_01_I am ready to give it away"
                                                    ,"row_02_Too expensive to hold or maintain"
                                                    ,"row_03_I need the money"
                                                    ,"row_04_High market value"
                                                    ,"row_05_No longer interested in owning it"
                                                    ,"row_06_Part of investment strategy"
                                                    ,"row_07_Other")
                                        ,exclusive = F
                                        ,table = "Q27_b__TRAN_FUT_WHY")
    agree_labels = c("row_01_Strongly agree"
                     ,"row_02_Agree"
                     ,"row_03_Neither agree nor disagree"
                     ,"row_04_Disagree"
                     ,"row_05_Strongly disagree")
    Q28__ATT_type = likert(v_names = c("ATT_WOODED", "ATT_SELL")
                           ,labels = agree_labels
                           ,levels = 5:1
                           ,table = "Q28__"
                           ,sep = "")
    Q28__ATT_aggr = factorize(v_names = c("ATT_WOODED", "ATT_SELL")
                              ,labels = c("row_01_I want my wooded land to stay wooded"
                                          ,"row_02_I would sell my land if I was offered a reasonable price")
                              ,exclusive = F
                              ,table = "Q28__ATT_aggr"
                              ,levels_1 = 4:5)
    Q29__OWN1_RET = binary(quest$OWN1_RET
                           ,label_neg2 = "ignore"
                           ,label_8 = NULL
                           ,label_9 = NULL)
    Q29__OWN2_RET = binary(quest$OWN2_RET
                           ,label_neg2 = "ignore"
                           ,label_8 = NULL
                           ,label_9 = NULL)
    age_labels = c('row_01_<45','row_02_45-54','row_03_55-64','row_04_65-74','row_05_75+')
    age_levels = c(0, 44, 54, 64, 74, Inf)
    Q31__OWN1_AGE_continuous = quest$OWN1_AGE
    Q31__OWN1_AGE_category = cats(quest$OWN1_AGE
                                 ,labels = age_labels
                                 ,levels = age_levels
                                 ,cont = T)
    Q31__OWN2_AGE_continuous = quest$OWN2_AGE
    Q31__OWN2_AGE_category = cats(quest$OWN2_AGE
                                 ,labels = age_labels
                                 ,levels = age_levels
                                 ,cont = T)
    gen_labels = c('row_01_Male', 'row_02_Female')
    gen_levels = c(1, 2)
    Q32__OWN1_GENDER = cats(quest$OWN1_GENDER
                                    ,labels = gen_labels
                                    ,levels = gen_levels)
    Q32__OWN2_GENDER = cats(quest$OWN2_GENDER
                                    ,labels = gen_labels
                                    ,levels = gen_levels)
    eth_labels = c('row_01_Hispanic or Latino', 'row_02_Non-Hispanic/Latino')
    eth_levels = c(1, 0)
    Q34__OWN1_ETH = cats(quest$OWN1_ETH
                                        ,labels = eth_labels
                                        ,levels = eth_levels)
    Q34__OWN2_ETH = cats(quest$OWN2_ETH
                                        ,labels = eth_labels
                                        ,levels = eth_levels)
    race_labels = c('row_01_American Indian or Alaska Native'
                    ,'row_02_Asian'
                    ,'row_03_Black or African American'
                    ,'row_04_Native Hawaiian or other Pacific Islander'
                    ,'row_05_White')
    race1_names = names(quest)[str_detect(names(quest), '^OWN1_RACE')]
    Q35__OWN1_RACE_type = factorize(v_names = race1_names
                                        ,labels = race_labels
                                        ,exclusive = F
                                        ,multi_val = 'row_06_Two or more races'
                                        ,table = 'Q35__OWN1_RACE')
    race2_names = names(quest)[str_detect(names(quest), '^OWN2_RACE')]
    Q35__OWN2_RACE_type = factorize(v_names = race2_names
                                        ,labels = race_labels
                                        ,exclusive = F
                                        ,multi_val = 'row_06_Two or more races'
                                        ,table = 'Q35__OWN2_RACE')
    edu_labels = c('row_01_12th grade or lower'
                   ,'row_02_High school or equivalent'
                   ,'row_03_Some college'
                   ,'row_04_Associate degree'
                   ,"row_05_Bachelor's degree"
                   ,'row_06_Advanced degree')
    edu_levels = c(1:6)
    Q33__OWN1_EDU = cats(quest$OWN1_EDU
                                       ,labels = edu_labels
                                       ,levels = edu_levels)
    Q33__OWN2_EDU = cats(quest$OWN2_EDU
                                       ,labels = edu_labels
                                       ,levels = edu_levels)
    Q36__INCOME = cats(quest$INCOME
                            ,labels = c('row_01_Less than $25,000', 'row_02_$25,000-$49,999', 'row_03_$50,000-$99,999', 'row_04_$100,000-$199,999', 'row_05_$200,000 or more')
                            ,levels = c(20, 25, 50, 100, 200))
    Q37__INC_WOOD_category = cats(ifelse(quest$INC_WOOD > 100, -1, quest$INC_WOOD)
                          ,labels = c('row_01_No income from timber', 'row_02_1-4.9%', 'row_03_5-19.9%', 'row_04_20-49.9%', 'row_05_50% or more')
                          ,levels = c(0, 1, 5, 20, 50, Inf)
                          ,right = F
                          ,cont = T)
    
  }
  if(NwosCycle == 2006)
  {
    table_02__ownership_category.fam_other = factor(ifelse(quest$OWNER_CLASS_ADJ == 45
                                        ,'row_01_Family'
                                        ,ifelse(quest$OWNER_CLASS_ADJ %in% c(41:44)
                                                ,'row_02_Other private'
                                                ,'ignore'))
                                 ,levels = c('ignore'
                                             ,'row_01_Family'
                                             ,'row_02_Other private'))
    table_02__ownership_category.all_private = factor('row_03_Total Private')
    table_03__size = cut(quest$ACRES_IN_STATE
                         ,breaks = c(0,9,19,49,99,199,499,999,4999,9999,Inf)
                         ,labels = c('row_01_1-9','row_02_10-19','row_03_20-49','row_04_50-99','row_05_100-199','row_06_200-499','row_07_500-999','row_08_1000-4999','row_09_5000-9999','row_10_10000+'))
    # table 5:
    table_05 = cut(quest$ACRES_IN_STATE
                   ,breaks = c(0,9,19,49,99,199,499,999,4999,9999,Inf)
                   ,labels = c('row_01_1-9','row_02_10-19','row_03_20-49','row_04_50-99','row_05_100-199','row_06_200-499','row_07_500-999','row_08_1000-4999','row_09_5000-9999','row_10_10000+'))
    table_05__size = factor(ifelse(quest$OWNER_CLASS_ADJ %in% 41:44, 'ignore'
                                   ,as.character(table_05)))
    # table 6:
    percfor = quest$ACRES_IN_STATE/quest$ACRES_LAND_IN_STATE
    percfor[percfor < 0 | percfor > 1 | is.na(percfor)] = -1
    percfor[quest$ACRES_LAND_IN_STATE == -3] = -3
    table_06__percent_forest = cats(percfor
                                    ,labels = c('row_01_<25','row_02_25-49','row_03_50-74','row_04_75-99','row_05_100')
                                    ,levels = c(-1,.24,.49,.74,.99,1)
                                    ,cont = T)
    # table 7:
    pmc = quest$PARCELS_NUM
    pmc[pmc == 0] = -1
    table_07__num_parcels = cats(pmc
                                 ,labels = c('row_01_1','row_02_2-9','row_03_10-99','row_04_100+')
                                 ,levels = c(0, 1, 9, 99, Inf)
                                 ,neg2_val = 1
                                 ,cont = T)
    # table 8:
    table_08a__land_acq_type = factorize(v_names = names(quest)[26:29]
                                         ,combine_cols = list(1, 2, 3:4)
                                         ,labels = c('row_01_Purchase', 'row_02_Inheritance', 'row_03_Other')
                                         ,exclusive = F
                                         ,table = 'table_08a__land_acq_type')
    table_08b__land_acq_source = factorize(v_names = names(quest)[31:38]
                                           ,combine_cols = list(1, 2, 3:8)
                                           ,labels = c('row_01_Family', 'row_02_Individual', 'row_03_Other')
                                           ,exclusive = F
                                           ,table = 'table_08b__land_acq_source')
    # table 9:
    tenure = quest$NWOSYEAR - quest$ACQ_YEAR
    table_NA__tenure_continuous = ifelse(quest$ACQ_YEAR < 0 | tenure < 0 | tenure > 80, -1, tenure)
    table_09__tenure = cats(ifelse(quest$ACQ_YEAR < 0 | tenure < 0, -1, tenure)
                            ,labels = c('row_01_<10', 'row_02_10-24', 'row_03_25-49', 'row_04_50+')
                            ,levels = c(0, 10, 25, 50, Inf)
                            ,cont = T
                            ,right = F)
    # table 10:
    table_10a__transfer_frequency = cats(quest$TRANSFER
                                         ,labels = c('row_01_0 (Never)','row_02_1', 'row_03_2-5', 'row_04_6+')
                                         ,levels = c(0, 1, 2, 6)
                                         ,cont = F)
    table_10b__transfer_recipient = factorize(v_names = names(quest)[42:49]
                                              ,combine_cols = list(1, 2, 3:8)
                                              ,labels = c('row_01_Family', 'row_02_Individual', 'row_03_Other')
                                              ,exclusive = F
                                              ,table = 'table_10b__transfer_recipient')
    
    table_10c__transfer_recent = binary(quest$TRN_RECENT
                                        ,label_8 = NULL
                                        ,label_9 = NULL)
    # table 11:
    table_11__ownership_form = factorize(v_names = names(quest)[52:62]
                                         ,combine_cols = list(1, 2, 9, c(3:8, 10:11))
                                         ,labels = c('row_01_Individual or joint', 'row_02_Family partnership', 'row_03_Trust', 'row_04_Other')
                                         ,exclusive = F
                                         ,table = 'table_11__ownership_form')
    # table 12:
    table_12a__farm = binary(quest$FARM
                             ,label_9 = NULL)
    table_12b__primary_residence = binary(quest$PRIMARY_RESIDENCE
                                          ,label_8 = NULL
                                          ,label_9 = NULL)
    table_12c__vacation_home = binary(quest$SECONDARY_RESIDENCE
                                      ,label_8 = NULL
                                      ,label_9 = NULL)
    # table 13:
    require(stringr)
    obj_vars = quest[,names(quest)[str_detect(names(quest), '^OBJ_') & str_detect(names(quest), '.SPECIFY') == F & str_detect(names(quest), '.OTHER') == F]]
    obj_names = c('row_01_To enjoy beauty or scenery'
                  ,'row_02_To protect nature or biological diversity'
                  ,'row_03_For land investment'
                  ,'row_04_Part of my home or vaction home'
                  ,'row_05_Is part of my farm or ranch'
                  ,'row_06_For privacy'
                  ,'row_07_To pass land on to my children or other heirs'
                  ,'row_08_To cultivate/collect nontimber forest products'
                  ,'row_09_For production of firewood or biofuel'
                  ,'row_10_For production of sawlogs, pulpwood or other timber products'
                  ,'row_11_For hunting'
                  ,'row_12_For recreation, other than hunting')
    
    for(i in 1: ncol(obj_vars))
    {
      obj_vars[,i] = cats(obj_vars[,i]
                          ,labels = c(obj_names[i], 'not important')
                          ,levels = c(0, 3, Inf)
                          ,cont = T)
      obj_vars[obj_vars[,i] == 'not important', i] = 'ignore'
      obj_vars[,i] = factor(obj_vars[,i])
    }
    table_13__own_objectives = obj_vars
    colnames(table_13__own_objectives) = paste('table_13__own_objectives', obj_names, sep = '.')
    # table 14:
    table_14a__lease = binary(quest$LEASED
                              ,label_8 = NULL
                              ,label_9 = NULL)
    table_14b__lease_use = factorize(v_names = names(quest)[c(84, 86:90)]
                                     ,labels = c('row_01_Hunting'
                                                 ,'row_02_Recreation (other than hunting)'
                                                 ,'row_03_Graze/pasture livestock'
                                                 ,'row_04_Timber production'
                                                 ,'row_05_Cultivate/collect nontimber forest products'
                                                 ,'row_06_Other')
                                     ,exclusive = F
                                     ,table = 'table_14b__lease_use')
    table_14c__lease_recent = binary(quest$LEASE_RECENT
                                     ,label_2 = 'row_97_Uncertain'
                                     ,label_8 = NULL
                                     ,label_9 = NULL)
    # table 15:
    table_15a__easement = binary(quest$EASEMENT
                                 ,label_8 = NULL
                                 ,label_9 = NULL)
    table_15b__easement_future = cats(quest$EASEMENT_FUTURE
                                      ,labels = c('row_01_Yes'
                                                  ,'row_02_No'
                                                  ,'row_03_Maybe'
                                                  ,'row_97_Uncertain')
                                      ,levels = c(1, 0, 2, 3)
                                      ,neg2_val = "row_98_Not applicable"
                                      ,neg2_label = "row_98_Not applicable")
    # table 16:
    table_16a__green_cert_knowledge = cats(quest$CERTIFIED_KNOWLEDGE
                                           ,labels = c('row_01_Familiar', 'row_02_Not familiar')
                                           ,levels = c(1, 0))
    table_16b__green_cert = binary(quest$CERTIFIED_LAND
                                   ,label_8 = NULL
                                   ,label_9 = NULL)
    # table 17:
    table_17a__cost_share = binary(quest$COST_SHARE
                                   ,label_8 = NULL
                                   ,label_9 = NULL)
    table_17b__cost_share_recent = binary(quest$COST_SHARE_RECENT
                                          ,label_2 = 'row_97_Uncertain'
                                          ,label_8 = NULL
                                          ,label_9 = NULL)
    # table 19:
    cut_names = names(quest)[c(115:118, 120:121)]
    table_19a__trees_harvest = binary(quest$TIMBER_HARVEST
                                      ,label_8 = NULL
                                      ,label_9 = NULL)
    table_19b__trees_harvest_type = factorize(v_names = cut_names
                                              ,labels = c('row_01_Sawlogs'
                                                          ,'row_02_Veneer logs'
                                                          ,'row_03_Pulp wood'
                                                          ,'row_04_Firewood'
                                                          ,'row_05_Posts or poles'
                                                          ,'row_06_Other')
                                              ,exclusive = F
                                              ,table = 'table_19b__trees_harvest_type')
    table_19c__professional = binary(quest$HRV_CONSULT
                                     ,label_2 = 'row_97_Uncertain'
                                     ,label_8 = NULL
                                     ,label_9 = NULL)
    table_19d__trees_harvest_recent = binary(quest$HRV_RECENT
                                             ,label_2 = 'row_97_Uncertain'
                                             ,label_8 = NULL
                                             ,label_9 = NULL)
    table_19e__commercial_harvest = any_true(c('HRV_SAW', 'HRV_VENEER', 'HRV_PULP'))
    table_19f__reason_for_harvest = factorize(names(quest)[124:134]
                                              ,labels = c('row_01_Part of management plan'
                                                          ,'row_02_Trees were mature'
                                                          ,'row_03_Clear land'
                                                          ,'row_04_Needed money'
                                                          ,'row_05_Wood for personal use'
                                                          ,'row_06_Price was right'
                                                          ,'row_07_Improve hunting'
                                                          ,'row_08_Improve recreation'
                                                          ,'row_09_Remove trees damaged by natural catastrophes'
                                                          ,'row_10_Improve quality of remaining trees'
                                                          ,'row_11_Other')
                                              ,exclusive = F
                                              ,table = 'table_19f__reason_for_harvest')
    # table 20:
    ntfp_names = names(quest)[141:145]
    table_20a__ntfp_harvest = binary(quest$NTFP_COLLECT
                                     ,label_8 = NULL
                                     ,label_9 = NULL)
    table_20b__ntfp_harvest_type = factorize(v_names = ntfp_names
                                             ,labels = c('row_01_Edibles'
                                                         ,'row_02_Medicinals'
                                                         ,'row_03_Decoratives'
                                                         ,'row_04_Cultural'
                                                         ,'row_05_Other')
                                             ,exclusive = F
                                             ,table = 'table_20b__ntfp_harvest_type')
    table_20c__ntfp_personal_or_sale = factorize(v_names = c('NTFP_SALE', 'NTFP_PERSONAL')
                                                 ,labels = c('row_01_For sale', 'row_02_For personal use')
                                                 ,exclusive = F
                                                 ,table = 'table_20c__ntfp_personal_or_sale')
    table_20d__ntfp_harvest_recent = binary(quest$NTFP_RECENT
                                            ,label_2 = 'row_97_Uncertain'
                                            ,label_8 = NULL
                                            ,label_9 = NULL)
    # table 21:
    table_21__management_plan = binary(quest$MANAGEMENT_PLAN
                                       ,label_2 = 'row_97_Uncertain'
                                       ,label_8 = NULL
                                       ,label_9 = NULL)
    # table 22: 
    act_names = names(quest)[c(114, 151, 140, 152:160)]
    table_22__forestry_recent = factorize(v_names = act_names
                                          ,labels = c('row_01_Timber harvest'
                                                      ,'row_02_Collection of NTFPs'
                                                      ,'row_03_Site preparation'
                                                      ,'row_04_Tree planting'
                                                      ,'row_05_Fire hazard reduction'
                                                      ,'row_06_Application of chemicals'
                                                      ,'row_07_Road/trail maintenance'
                                                      ,'row_08_Wildlife habitat improvement'
                                                      ,'row_09_Posting land'
                                                      ,'row_10_Private recreation'
                                                      ,'row_11_Public recration'
                                                      ,'row_12_None of the above')
                                          ,exclusive = F
                                          ,table = 'table_22__forestry_recent')
    # table 23:
    table_23a__advice_received = binary(quest$ADVICE
                                        ,label_8 = NULL
                                        ,label_9 = NULL)
    table_23b__advice_source = factorize(v_names = names(quest)[c(166:172, 174, 173, 176)]
                                         ,combine_cols = list(1, 2, 3, 4, 5, 6, 7, 8, 9:10)
                                         ,labels = c('row_01_State forestry agency'
                                                     ,'row_02_Extension'
                                                     ,'row_03_Other state agency'
                                                     ,'row_04_Federal agency'
                                                     ,'row_05_Private consultant'
                                                     ,'row_06_Forest industry'
                                                     ,'row_07_Logger'
                                                     ,'row_08_Another landowner'
                                                     ,'row_09_Other')
                                         ,exclusive = F
                                         ,table = 'table_23b__advice_source')
    # table 24:
    help_names = names(quest)[178:188]
    table_24__info_source_preferred = factorize(v_names = help_names
                                                ,labels = c('row_01_Publications, books, or pamphlets'
                                                            ,'row_02_Newsletters, magazines, or newspapers'
                                                            ,'row_03_Internet/web'
                                                            ,'row_04_Conferences, workshops, or video conferences'
                                                            ,'row_05_Video tapes for home viewing'
                                                            ,'row_06_Television or radio programs'
                                                            ,'row_07_Visiting other woodlands or field trips'
                                                            ,'row_08_Talking with a forester or other natural resource professional'
                                                            ,'row_09_Talking with other woodland owners'
                                                            ,'row_10_Talking with a logging contractor'
                                                            ,'row_11_Membership in a land owner organization')
                                                ,exclusive = F
                                                ,table = 'table_24__advice_source_preferred')
    # table 25:
    concern_names = names(quest)[190:200]
    table_25__concerns = factorize(v_names = concern_names
                                   ,labels = c('row_01_Dealing with an endangered species'
                                               ,'row_02_High property taxes'
                                               ,'row_03_Keeping land intact for heirs'
                                               ,'row_04_Lawsuits'
                                               ,'row_05_Regulations that restrict harvesting'
                                               ,'row_06_Development of nearby lands'
                                               ,'row_07_Damage or noise from motorized vehicles'
                                               ,'row_08_Tresspassing or poaching'
                                               ,'row_09_Timber theft'
                                               ,'row_10_Misuse of wooded land, such as vandalism or dumping')
                                   ,exclusive = F
                                   ,table = 'table_25__concerns')
    # table 27: 
    names_futureplans = names(quest)[c(216:226, 214:215, 227)]
    table_27__future_plans = factorize(v_names = names_futureplans
                                       ,labels = c('row_01_Leave it as is - no activity'
                                                   ,'row_02_Minimal activity to maintain forest land'
                                                   ,'row_03_Harvest firewood'
                                                   ,'row_04_Harvest sawlogs or pulpwood'
                                                   ,'row_05_Collect nontimber forest products'
                                                   ,'row_06_Sell some or all of their forest land'
                                                   ,'row_07_Give some or all of their forest land to heirs'
                                                   ,'row_08_Subdivide some or all of their forest land and sell subdivisions'
                                                   ,'row_09_Buy more forest land'
                                                   ,'row_10_Convert some or all of their forest land to another use'
                                                   ,'row_11_Convert another land use to forest land'
                                                   ,'row_12_No current plans'
                                                   ,'row_13_Unknown'
                                                   ,'row_14_Other')
                                       ,exclusive = F
                                       ,table = 'table_27__future_plans')
    # table 29:
    age_labels = c('row_01_<45','row_02_45-54','row_03_55-64','row_04_65-74','row_05_75+')
    age_levels = c(0, 41, 51, 61, 71, Inf)
    table_29a__owner1_age = cats(quest$AGE
                                 ,labels = age_labels
                                 ,levels = age_levels
                                 ,cont = T)
    # table 30:
    edu_labels = c('row_01_12th grade or lower','row_02_High school or equivalent','row_03_Some college','row_04_Associate degree',"row_05_Bachelor's degree",'row_06_Advanced degree')
    edu_levels = c(1:6)
    table_30a__owner1_education = cats(quest$EDUCATION
                                       ,labels = edu_labels
                                       ,levels = edu_levels)
    # table 31:
    table_31__income = cats(quest$INCOME
                            ,labels = c('row_01_Less than $25,000', 'row_02_$25,000-$49,999', 'row_03_$50,000-$99,999', 'row_04_$100,000-$199,999', 'row_05_$200,000 or more')
                            ,levels = c(20, 25, 50, 100, 200))
    # table 32:
    gen_labels = c('row_01_Male', 'row_02_Female', 'row_03_Both')
    gen_levels = c(1:2, 0)
    table_32a__owner1_gender = cats(quest$GENDER
                                    ,neg2_val = 'ignore'
                                    ,labels = gen_labels
                                    ,levels = gen_levels)
    # table 33:
    eth_labels = c('row_01_Hispanic or Latino', 'row_02_Non-Hispanic/Latino')
    eth_levels = c(1, 0)
    table_33a1__owner1_ethnicity = cats(quest$HISPANIC
                                        ,labels = eth_labels
                                        ,levels = eth_levels)
    race_labels = c('row_01_American Indian or Alaska Native', 'row_02_Asian', 'row_03_Black or African American', 'row_04_Native Hawaiian or other Pacific Islander', 'row_05_White')
    race1_names = names(quest)[str_detect(names(quest), '^RACE')]
    table_33b1__owner1_race = factorize(v_names = race1_names
                                        ,labels = race_labels
                                        ,exclusive = F
                                        ,multi_val = 'row_06_Two or more races'
                                        ,table = '33b.1__owner1_race')
    table_NA__size_graph = cut(quest$ACRES_IN_STATE
                      ,breaks = c(0,9,49,99,499,Inf)
                      ,labels = c('row_01_1-9', 'row_02_10-49', 'row_03_50-99', 'row_04_100-499', 'row_05_500+'))
  }

  SffiOut = CalcSffi(quest_df = quest, NwosCycle = NwosCycle)
  table_NA__sffi_prime = SffiOut[[1]]$PRIME_PROSPECTS_GROUP
  table_NA__sffi_att = SffiOut[[2]]$ATTITUDINAL_GROUP

  quest_all = quest
  pull_names = sort(ls(pattern = '__'))
  for(i in 1:length(pull_names))
  {
    quest_all = cbind(quest_all, eval(as.name(pull_names[i])))
    if(ncol(as.data.frame(eval(as.name(pull_names[i])))) == 1)
    {
      names(quest_all)[ncol(quest_all)] = pull_names[i]
    }
  }
  
  return(quest_all)
}
