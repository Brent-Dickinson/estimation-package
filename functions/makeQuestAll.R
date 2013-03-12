#######################################################################
## this function adds to quest some useful factor variables constructed from quest variables. 
## it is needed to prepare quest for indiv.R.
## if there are problems with the resulting quest_all data frame, look into the internal functions below.
## in particular, this code leaves -1's and -3's as a separate category for estimation.
## however, the indiv.R function gets rid of -3's at present.
## problems may result from how the code below recodes -2's.

makeQuestAll = function(quest = quest_11, nwosCycle = 2011)
{
  binary = function(x
                    ,label_neg3 = '-3'
                    ,label_neg2 = 'row_02_No'
                    ,label_neg1 = 'row_99_No response'
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
    if(nwosCycle == 2011)
    {
      x[quest$OWNTYPE %in% 5:6] = 'ignore'      
    }
    if(nwosCycle == 2006)
    {
      x[quest$OWNER_CLASS_ADJ %in% 41:44] = 'ignore'
    }
    labels = c(label_neg3, label_neg2, label_neg1, 'row_02_No', 'row_01_Yes', label_2, label_8, label_9, 'ignore')
    values = c(-3, -2, -1, 0, 1, value_2, value_8, value_9, 'ignore')
    for(i in 1:length(labels))
    {
      factor_var[x == values[i]] = labels[i]
    }
    factor_var = factor(factor_var, levels = sort(unique(labels)))
    if(any(is.na(factor_var))) print('warning: problem with binary(): NAs present')
    return(factor_var)
  }
  cats = function(var
                  ,labels
                  ,levels
                  ,neg2_val = 0
                  ,neg2_label = NULL
                  ,cont = F
                  ,right = T)
  {
    if(cont == F & length(labels) != length(levels)) stop('labels and levels have different lengths')
    if(cont == F)
    {
      for(i in 1:length(levels))
      {
        var[var == levels[i]] = labels[i]
      }
      var[var == 8] = -2
      var[var == -2] = neg2_val
    }
    else
    {
      var[var == -2] = neg2_val
      var[!var %in% c(-3:-1)] = as.character(cut(as.numeric(var[!var %in% c(-3:-1)]), breaks = levels, labels = labels, right = right))
    }
    var[var == -1] = 'row_99_No answer'
    if(nwosCycle == 2011)
    {
      var[quest$OWNTYPE %in% 5:6] = 'ignore'      
    }
    if(nwosCycle == 2006)
    {
      var[quest$OWNER_CLASS_ADJ %in% 41:44] = 'ignore'
    }
    
    labels = c(labels, 'ignore')
    if(neg2_val %in% levels) var = factor(var, levels = c('-3', labels, 'row_99_No answer'))
    else var = factor(var, levels = c('-3', labels, 'row_99_No answer', neg2_label))
    return(var)
  }
  likert = function(v_names, labels = c('row_01_Important', 'row_02_Unimportant', 'row_98_Not applicable'), levels = c(0, 2, 7, Inf), table)
  {
    likert_vars = data.frame(quest[,names(quest) %in% v_names])
    for(i in 1:ncol(likert_vars))
    {
      likert_vars[likert_vars[,i] == -2, i] = 8
      likert_vars[,i] = cats(likert_vars[,i]
                             ,labels = labels
                             ,levels = levels
                             ,cont = T)
      colnames(likert_vars)[i] = paste(table, i, sep = '.')
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
    if(nwosCycle == 2011)
    {
      out_var = ifelse(quest$OWNTYPE %in% 5:6, 'ignore'
                       ,ifelse(value_neg3 == T, '-3'
                               ,ifelse(value_neg1 == T, 'row_99_No answer'
                                       ,ifelse(value_8 == T, 'row_98_Not applicable'
                                               ,ifelse(value_1 == T, 'row_01_Yes'
                                                       ,'row_02_No')))))      
    }
    if(nwosCycle == 2006)
    {
      out_var = ifelse(quest$OWNER_CLASS_ADJ %in% 41:44, 'ignore'
                       ,ifelse(value_neg3 == T, '-3'
                               ,ifelse(value_neg1 == T, 'row_99_No answer'
                                       ,ifelse(value_2 == T, 'row_97_Uncertain'
                                               ,ifelse(value_1 == T, 'row_01_Yes'
                                                       ,'row_02_No')))))
    }
    out_var = factor(out_var, levels = c('-3', 'row_01_Yes', 'row_02_No', 'row_99_No answer', 'ignore'))
    return(out_var)
  }
  factorize = function(v_names
                       ,combine_cols = NULL
                       ,labels
                       ,exclusive = T
                       ,neg3val = '-3'
                       ,neg2val = '0'
                       ,neg1val = 'row_99_No answer'
                       ,multi_val = NULL
                       ,table = NULL)
  {
    vars = data.frame(NULL)
    for(i in 1:length(v_names))
    {
      vars[1:nrow(quest), i] = quest[,names(quest) == v_names[i]]
      colnames(vars)[i] = v_names[i]
    }
    vars[vars == -2] = neg2val
    out_var = rep(NA, nrow(quest))
    if(exclusive == T & is.null(combine_cols))
    {
      for(i in 1:length(labels))
      {
        out_var[vars[,i] == 1] = labels[i]
        if(nwosCycle == 2006)
        {
          out_var[quest$OWNER_CLASS_ADJ %in% 41:44] = 'ignore'
        }
        if(nwosCycle == 2011)
        {
          out_var[quest$OWNTYPE %in% 5:6] = 'ignore' 
        }
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
        out_var[apply(vars_combined, 1, function(x) any(x == 1))] = labels[i]
      }
      out_var[apply(vars, 1, function(x) any(x == -1)) == T] = neg1val
      out_var[apply(vars, 1, function(x) any(x == -3)) == T] = neg3val
      out_var[apply(vars, 1, function(x) any(x == 8)) == T] = neg3val
      if(nwosCycle == 2006)
      {
        out_var[quest$OWNER_CLASS_ADJ %in% 41:44] = 'ignore'
      }
      if(nwosCycle == 2011)
      {
        out_var[quest$OWNTYPE %in% 5:6] = 'ignore' 
      }
      out_var = factor(out_var, levels = c(neg3val, labels, multi_val, neg1val, 'ignore'))
    }
    if(exclusive == F & is.null(combine_cols) & is.null(multi_val) == F)
    {
      multi = apply(vars, 1, function(x) ifelse(any(x == 1), sum(as.numeric(x)), 0))
      single = rep(0, length(multi))
      single[multi == 1] = apply(vars[multi == 1,], 1, function(x) labels[x == 1])
      out_var[single != 0] = single[single != 0]
      out_var[multi > 1] = multi_val
      out_var[apply(vars, 1, function(x) any(x == -1)) == T] = neg1val
      out_var[apply(vars, 1, function(x) any(x == -3)) == T] = neg3val
      out_var[apply(vars, 1, function(x) any(x == 8)) == T] = neg3val
      out_var[apply(vars, 1, function(x) all(x == 0)) == T] = neg1val
      if(nwosCycle == 2006)
      {
        out_var[quest$OWNER_CLASS_ADJ %in% 41:44] = 'ignore'
      }
      if(nwosCycle == 2011)
      {
        out_var[quest$OWNTYPE %in% 5:6] = 'ignore' 
      }
      out_var = factor(out_var, levels = c(neg3val, labels, multi_val, neg1val, 'ignore'))
    }
    if(exclusive == F & is.null(combine_cols) & is.null(multi_val))
    {
      out_var = data.frame(matrix(NA, nrow = nrow(vars), ncol = length(labels)))
      for(j in 1:ncol(out_var))
      {
        out_var[vars[,j] == 1, j] = labels[j]
        out_var[vars[,j] == -3, j] = neg3val
        out_var[!vars[,j] %in% c(-3, 1), j] = 'ignore'
        if(nwosCycle == 2006)
        {
          out_var[quest$OWNER_CLASS_ADJ %in% 41:44, j] = 'ignore'
        }
        if(nwosCycle == 2011)
        {
          out_var[quest$OWNTYPE %in% 5:6, j] = 'ignore' 
        }
        out_var[,j] = factor(out_var[,j], levels = c(neg3val, labels[j], 'ignore'))
      }
      out_neg1 = rep('ignore', nrow(vars))
      out_neg1[apply(vars, 1, function(x) any(x == -1)) == T] = neg1val
      out_neg1[apply(vars, 1, function(x) any(x == -3)) == T] = neg3val
      out_neg1 = factor(out_neg1, levels = c(neg3val, neg1val, 'ignore'))
      out_var = cbind(out_var, out_neg1)
      colnames(out_var) = c(labels, neg1val)
    }
    if(exclusive == F & is.null(combine_cols) == F & is.null(multi_val))
    {
      if(length(labels) != length(combine_cols)) stop('problem with factorize: combine_cols and labels have different lengths')
      out_var = data.frame(matrix('ignore', nrow = nrow(vars), ncol = length(labels)), stringsAsFactors = F)
      for(i in 1:length(labels))
      {
        vars_combined = data.frame(vars[,combine_cols[[i]]])
        out_var[apply(vars_combined, 1, function(x) any(x == 1)), i] = labels[i]
        out_var[apply(vars_combined, 1, function(x) any(x == -3)), i] = neg3val
        if(nwosCycle == 2006)
        {
          out_var[quest$OWNER_CLASS_ADJ %in% 41:44, i] = 'ignore'
        }
        if(nwosCycle == 2011)
        {
          out_var[quest$OWNTYPE %in% 5:6, i] = 'ignore' 
        }
        out_var[,i] = factor(out_var[,i], levels = c(neg3val, labels[i], 'ignore'))
      }
      out_neg1 = rep('ignore', nrow(vars))
      out_neg1[apply(vars, 1, function(x) any(x == -3)) == T] = '-3'
      out_neg1[apply(vars, 1, function(x) any(x == -1)) == T] = neg1val
      out_neg1 = factor(out_neg1, levels = c('-3', neg1val, 'ignore'))
      out_var = cbind(out_var, out_neg1)
      colnames(out_var) = c(labels, neg1val)
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
        if(nwosCycle == 2006)
        {
          out_var[quest$OWNER_CLASS_ADJ %in% 41:44, i] = 'ignore'
        }
        if(nwosCycle == 2011)
        {
          out_var[quest$OWNTYPE %in% 5:6, i] = 'ignore' 
        }
        out_var[,i] = factor(out_var[,i], levels = c(neg3val, labels[i], 'ignore'))
      }
      out_neg1 = rep('ignore', nrow(vars))
      out_neg1[apply(vars, 1, function(x) any(x == -1)) == T] = neg1val
      out_neg1 = factor(out_neg1, levels = c(neg1val, 'ignore'))
      
      multi = apply(vars, 1, function(x) ifelse(any(x == 1), ifelse(sum(as.numeric(x)) > 1, multi_val, 'ignore')))
      if(nwosCycle == 2006)
      {
        multi[quest$OWNER_CLASS_ADJ %in% 41:44] = 'ignore'
      }
      if(nwosCycle == 2011)
      {
        multi[quest$OWNTYPE %in% 5:6] = 'ignore' 
      }
      multi = factor(multi, levels = c(multi_val, 'ignore'))
      out_var = cbind(out_var, multi, out_neg1)
      colnames(out_var) = c(labels, multi_val, neg1val)
    }
    if(any(is.na(as.character(out_var)))) print(paste('warning: NAs produced by factorize():', table))
    if(ncol(as.data.frame(out_var)) > 1) colnames(out_var) = paste(table, colnames(as.data.frame(out_var)), sep = '.')
    return(out_var)
  }
  calc.sffi.attitude <- function(TIM='OBJ_TIM', INV='OBJ_INV', AES='OBJ_BEA',
                                 NAT='OBJ_NAT', PRI='OBJ_PRI', HUN='OBJ_HUNT',
                                 REC='OBJ_REC')
  {
    tim <- ifelse(quest[,TIM] %in% c(4,5), 1, ifelse(quest[,TIM] %in% c(1,2,3), 0, 'row_98_Not applicable'))
    inv <- ifelse(quest[,INV] %in% c(4,5), 1, ifelse(quest[,INV] %in% c(1,2,3), 0, 'row_98_Not applicable'))
    aes <- ifelse(quest[,AES] %in% c(4,5), 1, ifelse(quest[,AES] %in% c(1,2,3), 0, 'row_98_Not applicable'))
    nat <- ifelse(quest[,NAT] %in% c(4,5), 1, ifelse(quest[,NAT] %in% c(1,2,3), 0, 'row_98_Not applicable'))
    pri <- ifelse(quest[,PRI] %in% c(4,5), 1, ifelse(quest[,PRI] %in% c(1,2,3), 0, 'row_98_Not applicable'))
    hun <- ifelse(quest[,HUN] %in% c(4,5), 1, ifelse(quest[,HUN] %in% c(1,2,3), 0, 'row_98_Not applicable'))
    rec <- ifelse(quest[,REC] %in% c(4,5), 1, ifelse(quest[,REC] %in% c(1,2,3), 0, 'row_98_Not applicable'))
    
    fin <- ifelse((is.na(tim) & is.na(inv)), 'row_98_Not applicable', pmax(tim, inv, na.rm=T))
    ame <- ifelse((is.na(aes) & is.na(nat) & is.na(pri) & is.na(hun) & is.na(rec)), 'row_98_Not applicable', pmax(aes, nat, pri, hun, rec, na.rm=T))
    
    if(nwosCycle == 2006)
    {
      att <- factor(ifelse(quest$OWNER_CLASS_ADJ %in% 41:44, 'ignore'
                           ,ifelse(fin==0 & ame==1, 'row_01_1',    # Woodland Retreat
                                   ifelse(fin==1 & ame==1, 'row_02_2',    # Working the Land
                                          ifelse(fin==1 & ame==0, 'row_03_3', 		# Supplemental Income
                                                 ifelse(fin==0 & ame==0, 'row_04_4', 'row_98_Not applicable')))))
                    ,levels = c('row_01_1', 'row_02_2', 'row_03_3', 'row_04_4', 'row_98_Not applicable', 'ignore'))	# Uninvolved      
    }
    if(nwosCycle == 2011)
    {
      att <- factor(ifelse(quest$OWNTYPE %in% 5:6, 'ignore'
                           ,ifelse(fin==0 & ame==1, 'row_01_1',    # Woodland Retreat
                                   ifelse(fin==1 & ame==1, 'row_02_2',    # Working the Land
                                          ifelse(fin==1 & ame==0, 'row_03_3', 		# Supplemental Income
                                                 ifelse(fin==0 & ame==0, 'row_04_4', 'row_98_Not applicable')))))
                    ,levels = c('row_01_1', 'row_02_2', 'row_03_3', 'row_04_4', 'row_98_Not applicable', 'ignore'))	# Uninvolved
      
    }
    return(att)
  }
  calc.sffi.prime.prospect <- function(FORESTER='CUT_FORESTER',
                                       PLAN='MAN_PLAN',
                                       NAT='OBJ_NAT', NTFP='OBJ_NTFP',FIRE='OBJ_FIRE',
                                       TIM='OBJ_TIM', HUN='OBJ_HUNT', REC='OBJ_REC',
                                       TIM.FUT='FUT_CUT_SALE', EASE.FUT='EASE_5YR',
                                       EASE = 'EASE', COST = 'COST', ADVICE = 'ADVICE',
                                       CERT = 'CERT')
  {
    # Calculate Engagement Index
    eng.ease <- ifelse(quest[,EASE] == 1, 1, 0)
    eng.cost <- ifelse(quest[,COST] == 1, 1, 0)
    eng.forester <- ifelse(quest[,FORESTER] == 1, 1, 0)
    eng.plan <- ifelse(quest[,PLAN] == 1, 1, 0 )
    eng.adv <- ifelse(quest[,ADVICE] == 1, 1, 0 )
    eng.cert <- ifelse(quest[,CERT] == 1, 1, 0)
    eng <- eng.ease + eng.cost + eng.forester + eng.plan + eng.adv + (2*eng.cert)
    eng.lev <- ifelse(eng>=2, 2, eng)
    
    # Calculate interest index  
    int.nat <- ifelse(quest[,NAT] %in% c(4,5), 1, 0)
    int.ntfp <- ifelse(quest[,NTFP] %in% c(4,5), 1, 0)
    int.fire <- ifelse(quest[,FIRE] %in% c(4,5), 1, 0)
    int.tim <- ifelse(quest[,TIM] %in% c(4,5), 1, 0)
    int.hun <- ifelse(quest[,HUN] %in% c(4,5), 1, 0)
    int.rec <- ifelse(quest[,REC] %in% c(4,5), 1, 0)
    int.tim.fut <- ifelse(quest[,TIM.FUT] == 1, 1, 0) # FUT_CUT_SALE
    int.ease.fut <- ifelse(quest[,EASE.FUT] %in% c(5,4), 1, 0) # EASE_5YR
    
    int <- int.nat + int.ntfp + int.fire + int.tim + int.hun + int.rec + int.tim.fut + int.ease.fut
    int.lev <- ifelse(int>=3, 2, ifelse(int %in% c(1,2), 1, 0))
    
    if(nwosCycle == 2011)
    {
      pp <- factor(  ifelse(quest$OWNTYPE %in% 5:6, 'ignore'
                            ,ifelse(eng.lev==2 & int.lev==2, 'row_01_1',      ## model owner
                                    ifelse(eng.lev==1 & int.lev==2, 'row_02_2',  		## prime prospect
                                           ifelse(eng.lev==1 & int.lev==1, 'row_02_2',			## prime prospect
                                                  ifelse(eng.lev==0 & int.lev==2, 'row_02_2',			## prime prospect
                                                         ifelse(eng.lev==0 & int.lev==1, 'row_02_2',			## prime prospect
                                                                ifelse(eng.lev==2 & int.lev==1, 'row_03_3',			## potential defector
                                                                       ifelse(eng.lev==2 & int.lev==0, 'row_03_3',			## potential defector
                                                                              ifelse(eng.lev==1 & int.lev==0, 'row_03_3',			## potential defector
                                                                                     ifelse(eng.lev==0 & int.lev==0, 'row_04_4', 'row_98_Not applicable'))))))))))
                     ,levels = c('row_01_1', 'row_02_2', 'row_03_3', 'row_04_4', 'row_98_Not applicable', 'ignore'))	## write-off  
    }
    if(nwosCycle == 2006)
    {
      pp <- factor(  ifelse(quest$OWNER_CLASS_ADJ %in% 41:44, 'ignore'
                            ,ifelse(eng.lev==2 & int.lev==2, 'row_01_1',      ## model owner
                                    ifelse(eng.lev==1 & int.lev==2, 'row_02_2',  		## prime prospect
                                           ifelse(eng.lev==1 & int.lev==1, 'row_02_2',			## prime prospect
                                                  ifelse(eng.lev==0 & int.lev==2, 'row_02_2',			## prime prospect
                                                         ifelse(eng.lev==0 & int.lev==1, 'row_02_2',			## prime prospect
                                                                ifelse(eng.lev==2 & int.lev==1, 'row_03_3',			## potential defector
                                                                       ifelse(eng.lev==2 & int.lev==0, 'row_03_3',			## potential defector
                                                                              ifelse(eng.lev==1 & int.lev==0, 'row_03_3',			## potential defector
                                                                                     ifelse(eng.lev==0 & int.lev==0, 'row_04_4', 'row_98_Not applicable'))))))))))
                     ,levels = c('row_01_1', 'row_02_2', 'row_03_3', 'row_04_4', 'row_98_Not applicable', 'ignore'))	## write-off      
    }
    return(pp)
  }
  if(nwosCycle == 2011)
  {
    table_02__fam_other = factor(ifelse(quest$OWNTYPE %in% c(1:4)
                                        ,'row_01_Family'
                                        ,ifelse(quest$OWNTYPE %in% c(5:6)
                                                ,'row_02_Other private'
                                                ,'ignore'))
                                 ,levels = c('ignore'
                                             ,'row_01_Family'
                                             ,'row_02_Other private'))
    table_02__all_private = factor('row_03_Total Private')
    table_03__size = cut(quest$AC_WOOD
                         ,breaks = c(0,9,19,49,99,199,499,999,4999,9999,Inf)
                         ,labels = c('row_01_1-9','row_02_10-19','row_03_20-49','row_04_50-99','row_05_100-199','row_06_200-499','row_07_500-999','row_08_1000-4999','row_09_5000-9999','row_10_10000+'))
    # table 5:
    table_05 = cut(quest$AC_WOOD
                   ,breaks = c(0,9,19,49,99,199,499,999,4999,9999,Inf)
                   ,labels = c('row_01_1-9','row_02_10-19','row_03_20-49','row_04_50-99','row_05_100-199','row_06_200-499','row_07_500-999','row_08_1000-4999','row_09_5000-9999','row_10_10000+'))
    table_05__size = factor(ifelse(quest$OWNTYPE %in% 5:6, 'ignore'
                                   ,as.character(table_05)))
    # table 6:
    percfor = quest$AC_WOOD/quest$AC_LAND
    percfor[percfor < 0 | percfor > 1 | is.na(percfor)] = -1
    table_06__percent_forest = cats(percfor
                                    ,labels = c('row_01_<25','row_02_25-49','row_03_50-74','row_04_75-99','row_05_100')
                                    ,levels = c(-1,.24,.49,.74,.99,1)
                                    ,cont = T)
    # table 7:
    pmc = quest$PARC_MULTI_NUMBER
    pmc[pmc == 0] = -1
    table_07__num_parcels = cats(pmc
                                 ,labels = c('row_01_1','row_02_2-9','row_03_10-99','row_04_100+')
                                 ,levels = c(0, 1, 9, 99, Inf)
                                 ,neg2_val = 1
                                 ,cont = T)
    # table 8:
    table_08a__land_acq_type = factorize(v_names = names(quest)[41:44]
                                         ,combine_cols = list(1, 2, 3:4)
                                         ,labels = c('row_01_Purchase', 'row_02_Inheritance', 'row_03_Other')
                                         ,exclusive = F
                                         ,table = 'table_08a__land_acq_type')
    table_08b__land_acq_source = factorize(v_names = names(quest)[46:52]
                                           ,combine_cols = list(1:3, 4, 5:7)
                                           ,labels = c('row_01_Family', 'row_02_Individual', 'row_03_Other')
                                           ,exclusive = F
                                           ,table = 'table_08b__land_acq_source')
    # table 9:
    tenure = quest$QUEST_YEAR - quest$ACQ_YEAR
    table_09__tenure = cats(ifelse(quest$ACQ_YEAR < 0 | tenure < 0, -1, tenure)
                            ,labels = c('row_01_<10', 'row_02_10-24', 'row_03_25-49', 'row_04_50+')
                            ,levels = c(0, 10, 25, 50, Inf)
                            ,cont = T
                            ,right = F)
    # table 10:
    table_10a__transfer_frequency = cats(quest$TRAN
                                         ,labels = c('row_01_0 (Never)','row_02_1', 'row_03_2-5', 'row_04_6+')
                                         ,levels = c(0, 1, 2, 6)
                                         ,cont = F)
    table_10b__transfer_recipient = factorize(v_names = names(quest)[56:61]
                                              ,combine_cols = list(1:2, 3, 4:6)
                                              ,labels = c('row_01_Family', 'row_02_Individual', 'row_03_Other')
                                              ,exclusive = F
                                              ,table = 'table_10b__transfer_recipient')
    
    table_10c__transfer_recent = binary(quest$TRAN_RECENT
                                        ,label_8 = NULL
                                        ,label_9 = NULL)
    # table 11: just use "quest$OWNTYPE" as is
    # table 12:
    table_12a__farm = binary(quest$FARM
                             ,label_9 = NULL)
    table_12b__primary_residence = binary(quest$HOME
                                          ,label_9 = NULL)
    table_12c__vacation_home = binary(quest$CABIN
                                      ,label_9 = NULL)
    # table 13:
    require(stringr)
    obj_vars = quest[,str_detect(names(quest), '^OBJ') & str_detect(names(quest), '.TXT') == F]
    obj_names = c('row_01_To enjoy beauty or scenery'
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
    
    obj_vars[obj_vars %in% c(-1,8)] = -3
    for(i in 1: ncol(obj_vars))
    {
      obj_vars[!obj_vars[,i] %in% 4:5, i] = 'ignore'
      obj_vars[obj_vars[,i] %in% 4:5, i] = obj_names[i]
      obj_vars[,i] = factor(obj_vars[,i], levels = c(obj_names[i], 'ignore'))
    }
    table_13__own_objectives = obj_vars
    colnames(table_13__own_objectives) = paste('table_13__own_objectives', obj_names, sep = '.')
    # table 14:
    table_14a__lease = binary(quest$LEASE
                              ,label_8 = NULL
                              ,label_9 = NULL)
    table_14b__lease_use = factorize(v_names = names(quest)[str_detect(names(quest), '^LEASE_ACT') & str_detect(names(quest), '.TXT') == F]
                                     ,labels = c('row_01_Hunting'
                                                 ,'row_02_Recreation (other than hunting)'
                                                 ,'row_03_Graze/pasture livestock'
                                                 ,'row_04_Land conservation'
                                                 ,'row_05_Carbon sequestration'
                                                 ,'row_06_Public water supply protection'
                                                 ,'row_07_Wildlife habitat/biodiversity'
                                                 ,'row_08_Other')
                                     ,exclusive = F
                                     ,table = 'table_14b__lease_use')
    table_14c__lease_recent = binary(quest$LEASE_5YR
                                     ,label_8 = NULL
                                     ,label_9 = NULL)
    # table 15:
    table_15a__easement = binary(quest$EASE
                                 ,label_8 = NULL)
    table_15b__easement_future = likert(v_names = 'EASE_5YR', table = '15b__easement_future')
    # table 16:
    table_16a__green_cert_knowledge = likert(v_names = 'CERT_KNOW'
                                             ,labels = c('row_01_Familiar', 'row_02_Not familiar', 'row_03_Not Applicable')
                                             ,table = 'table_16a__green_cert_knowledge')
    table_16b__green_cert = binary(quest$CERT
                                   ,label_8 = NULL)
    # table 17:
    table_17a__cost_share = binary(quest$COST
                                   ,label_8 = NULL)
    table_17b__cost_share_recent = binary(quest$COST_5YR
                                          ,label_8 = NULL)
    # table 18??????
    # table 19:
    cut_names = names(quest)[c(83:85, 87:89, 91:93, 95:97, 99:101)]
    table_19a__trees_harvest = any_true(v_names = cut_names)
    table_19b__trees_harvest_type = factorize(v_names = cut_names
                                              ,combine_cols = list(1:3, 4:6, 7:9, 10:12, 13:15)
                                              ,labels = c('row_01_Firewood'
                                                          ,'row_02_Logs'
                                                          ,'row_03_Wood chips'
                                                          ,'row_04_Unwanted trees'
                                                          ,'row_05_Other')
                                              ,exclusive = F
                                              ,table = 'table_19b__trees_harvest_type')
    table_19c__professional = binary(quest$CUT_FORESTER)
    table_19d__trees_harvest_recent = any_true(v_names = c('ACT_CUT_SALE', 'ACT_CUT_PERS'))
    table_19e__trees_harvest_commercial = binary(quest$CUT_LOG_SALE
                                                 ,label_8 = NULL
                                                 ,label_9 = NULL)
    # last part of table 19??????
    # table 20:
    ntfp_names = names(quest)[str_detect(names(quest), '^NTFP') & str_detect(names(quest), '.NO') == F & str_detect(names(quest), 'TXT') == F][-c(11:12)]
    if(length(ntfp_names) != 10) stop('reformulate ntfp_names')
    table_20a__ntfp_harvest = any_true(v_names = ntfp_names)
    table_20b__ntfp_harvest_type = factorize(v_names = ntfp_names
                                             ,combine_cols = list(1:2, 3:4, 5:6, 7:8, 9:10)
                                             ,labels = c('row_01_Edibles'
                                                         ,'row_02_Medicinals'
                                                         ,'row_03_Landscaping'
                                                         ,'row_04_Decoratives'
                                                         ,'row_05_Other')
                                             ,exclusive = F
                                             ,table = 'table_20b__ntfp_harvest_type')
    table_20c__ntfp_personal_or_sale = factorize(v_names = c('NTFP_SALE', 'NTFP_PERS')
                                                 ,labels = c('row_01_For sale', 'row_02_For personal use')
                                                 ,exclusive = F
                                                 ,table = 'table_20c__ntfp_personal_or_sale')
    table_20d__ntfp_harvest_recent = binary(quest$ACT_NTFP
                                            ,label_8 = NULL
                                            ,label_9 = NULL)
    # table 21:
    table_21__management_plan = binary(quest$MAN_PLAN
                                       ,label_8 = NULL
                                       ,label_9 = NULL)
    # table 22: 
    act_names = names(quest)[str_detect(names(quest), '^ACT')]
    table_22__forestry_recent = factorize(v_names = act_names
                                          ,combine_cols = list(1:2, 3, 4, 5, 6, 7, 8:9, 10, 11, 12)
                                          ,labels = c('row_01_Timber harvest'
                                                      ,'row_02_Collection of NTFPs'
                                                      ,'row_03_Fire hazard reduction'
                                                      ,'row_04_Controlled burn'
                                                      ,'row_05_Invasive species removal'
                                                      ,'row_06_Unwanted insect removal'
                                                      ,'row_06_Road/trail maintenance'
                                                      ,'row_08_Wildlife habitat improvement'
                                                      ,'row_09_Livestock grazing'
                                                      ,'row_10_None of the above')
                                          ,exclusive = F
                                          ,table = 'table_22__forestry_recent')
    # table 23:
    table_23a__advice_received = binary(quest$ADVICE
                                        ,label_8 = NULL
                                        ,label_9 = NULL)
    table_23b__advice_source = factorize(v_names = c('ADV_SRC_STATE'
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
                                         ,table = 'table_23b__advice_source')
    # table 24:
    help_names = names(quest)[str_detect(names(quest), '^HELP_TYPE') & str_detect(names(quest), '.TXT') == F]
    table_24__info_source_preferred = factorize(v_names = help_names
                                                ,labels = c('row_01_Talk to someone'
                                                            ,'row_02_Have someone visit my land'
                                                            ,'row_03_Written materials, such as brochures or publications'
                                                            ,'row_04_Internet'
                                                            ,'row_05_Conference or workshop'
                                                            ,'row_06_Other'
                                                            ,'row_07_Do not want/need advice or information')
                                                ,exclusive = F
                                                ,table = 'table_24__advice_source_preferred')
    # table 25:
    concern_names = names(quest)[str_detect(names(quest), '^CNC_') & str_detect(names(quest), '.TXT') == F]
    table_25__concerns = factorize(v_names = concern_names
                                   ,labels = c('row_01_Air pollution'
                                               ,'row_02_Damage or noise from off-road vehicles'
                                               ,'row_03_Damage from animals'
                                               ,'row_04_Development of nearby lands'
                                               ,'row_05_Drought or lack of water'
                                               ,'row_06_Global climate change'
                                               ,'row_07_High property taxes'
                                               ,'row_08_Invasive plant species'
                                               ,'row_09_Keeping land intact for future generations'
                                               ,'row_10_Misuse of wooded land, such as vandalism or dumping'
                                               ,'row_11_Trespassing or poaching'
                                               ,'row_12_Unwanted insects or diseases'
                                               ,'row_13_Water pollution'
                                               ,'row_14_Wildfire'
                                               ,'row_15_Wind or ice storms'
                                               ,'row_16_Other')
                                   ,exclusive = F
                                   ,table = 'table_25__concerns')
    # table 26: ???????????
    # table 27: 
    names_futureplans = names(quest)[str_detect(names(quest), '^FUT_')]
    table_27__future_plans = factorize(v_names = names_futureplans
                                       ,labels = c('row_01_Cut and/or remove trees for sale'
                                                   ,'row_02_Cut and/or remove trees for own use'
                                                   ,'row_03_Collect nontimber forest products'
                                                   ,'row_04_Reduce fire hazard'
                                                   ,'row_05_Controlled burn/prescribed fire'
                                                   ,'row_06_Eliminate or reduce invasive species'
                                                   ,'row_07_Eliminate or reduce unwanted insects or dieseases'
                                                   ,'row_08_Road construction or maintenance'
                                                   ,'row_09_Trail construction or maintenance'
                                                   ,'row_10_Improve wildlife habitat'
                                                   ,'row_11_Livestock grazing')
                                       ,exclusive = F
                                       ,table = 'table_27__future_plans')
    # table 28: skipping for now (occupation)
    # table 29:
    age_labels = c('row_01_<45','row_02_45-54','row_03_55-64','row_04_65-74','row_05_75+')
    age_levels = c(0, 44, 54, 64, 74, Inf)
    table_29a__owner1_age = cats(quest$OWN1_AGE
                                 ,labels = age_labels
                                 ,levels = age_levels
                                 ,cont = T)
    table_29b__owner2_age = cats(quest$OWN2_AGE
                                 ,labels = age_labels
                                 ,levels = age_levels
                                 ,cont = T)
    # table 30:
    edu_labels = c('row_01_12th grade or lower','row_02_High school or equivalent','row_03_Some college','row_04_Associate degree',"row_05_Bachelor's degree",'row_06_Advanced degree')
    edu_levels = c(1:6)
    table_30a__owner1_education = cats(quest$OWN1_EDU
                                       ,labels = edu_labels
                                       ,levels = edu_levels)
    table_30b__owner2_education = cats(quest$OWN2_EDU
                                       ,labels = edu_labels
                                       ,levels = edu_levels)
    # table 31:
    table_31__income = cats(quest$INCOME
                            ,labels = c('row_01_Less than $25,000', 'row_02_$25,000-$49,999', 'row_03_$50,000-$99,999', 'row_04_$100,000-$199,999', 'row_05_$200,000 or more')
                            ,levels = c(20, 25, 50, 100, 200))
    # table 32:
    gen_labels = c('row_01_Male', 'row_02_Female')
    gen_levels = c(1, 2)
    table_32a__owner1_gender = cats(quest$OWN1_GENDER
                                    ,labels = gen_labels
                                    ,levels = gen_levels)
    table_32b__owner2_gender = cats(quest$OWN2_GENDER
                                    ,labels = gen_labels
                                    ,levels = gen_levels)
    # table 33:
    eth_labels = c('row_01_Hispanic or Latino', 'row_02_Non-Hispanic/Latino')
    eth_levels = c(1, 0)
    table_33a1__owner1_ethnicity = cats(quest$OWN1_ETH
                                        ,labels = eth_labels
                                        ,levels = eth_levels)
    table_33a2__owner2_ethnicity = cats(quest$OWN2_ETH
                                        ,labels = eth_labels
                                        ,levels = eth_levels)
    race_labels = c('row_01_American Indian or Alaska Native', 'row_02_Asian', 'row_03_Black or African American', 'row_04_Native Hawaiian or other Pacific Islander', 'row_05_White')
    race1_names = names(quest)[str_detect(names(quest), '^OWN1_RACE')]
    table_33b1__owner1_race = factorize(v_names = race1_names
                                        ,labels = race_labels
                                        ,exclusive = F
                                        ,multi_val = 'row_06_Two or more races'
                                        ,table = '33b.1__owner1_race')
    race2_names = names(quest)[str_detect(names(quest), '^OWN2_RACE')]
    table_33b1__owner2_race = factorize(v_names = race2_names
                                        ,labels = race_labels
                                        ,exclusive = F
                                        ,multi_val = 'row_06_Two or more races'
                                        ,table = '33b.2__owner2_race')
    # table 34: ????????
    # assorted variables not in Butler, 2008:
    INC_WOOD__nice = cats(ifelse(quest$INC_WOOD > 100, -1, quest$INC_WOOD)
                          ,labels = c('row_01_No income from timber', 'row_02_1-4.9%', 'row_03_5-19.9%', 'row_04_20-49.9%', 'row_05_50% or more')
                          ,levels = c(0, 1, 5, 20, 50, Inf)
                          ,right = F
                          ,cont = T)
    OWNERS_NUMBER__agg = cats(ifelse(quest$OWNERS_NUMBER == 0, -1, quest$OWNERS_NUMBER)
                              ,labels = c('row_01_1', 'row_02_2-5', 'row_03_6-10', 'row_04_10+')
                              ,levels = c(0, 1, 5, 10, Inf)
                              ,cont = T)
    sffi__att = calc.sffi.attitude()
    sffi__prime = calc.sffi.prime.prospect()
    # different size categories for the 4x4 super graphic
    size__graph = cut(quest$AC_WOOD
                      ,breaks = c(0,9,49,99,499,Inf)
                      ,labels = c('row_01_1-9', 'row_02_10-49', 'row_03_50-99', 'row_04_100-499', 'row_05_500+'))
    population__family = factor(ifelse(quest$OWNTYPE %in% 1:4, 'population'
                                       ,ifelse(quest$OWNTYPE %in% 5:6, 'non_population'
                                               ,'unknown')))
  }
  if(nwosCycle == 2006)
  {
    table_02__fam_other = factor(ifelse(quest$OWNER_CLASS_ADJ == 45
                                        ,'row_01_Family'
                                        ,ifelse(quest$OWNER_CLASS_ADJ %in% c(41:44)
                                                ,'row_02_Other private'
                                                ,'ignore'))
                                 ,levels = c('ignore'
                                             ,'row_01_Family'
                                             ,'row_02_Other private'))
    table_02__all_private = factor('row_03_Total Private')
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
                                          ,label_9 = NULL)
    table_12c__vacation_home = binary(quest$SECONDARY_RESIDENCE
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
                                      ,levels = c(1, 0, 2, 3))
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
  }
  quest_all = quest
  for(i in 1:length(ls(pattern = '__')))
  {
    quest_all = cbind(quest_all, eval(as.name(ls(pattern = '__')[i])))
    if(ncol(as.data.frame(eval(as.name(ls(pattern = '__')[i])))) == 1)
    {
      names(quest_all)[ncol(quest_all)] = ls(pattern = '__')[i]
    }
  }
  
  return(quest_all)
}
