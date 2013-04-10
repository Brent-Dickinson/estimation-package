########################################################################################################
##   USDA Forest Service, National Woodland Owner Survey 								##
##	Written by: Brett J Butler (bbutler01@fs.fed.us)								##
##																	##
##	Note: This program only calculates the new SFFI variables.  It is expected that the resulting 	##
##		variables will be merged with the full dataset in MS Access						##
##																	##
##	Codes:															##
##		Prime Prospects													##
##			1	Model Owner												##
##			2	Prime prospect											##
##			3	Potential defector										##
##			4	Write-off												##
##		Attitudinal														##
##			1	Woodland Retreat											##
##			2	Workign the land											##
##			3	Supplemental Income										##
##			4	Uninvolved												##
##																	##
########################################################################################################
CalcSffi = function(quest_df, NwosCycle)
{
  tweak = function(x
                  ,make_0 = 0
                  ,make_1 = 1
                  ,make_2 = NULL
                   ,val_Neg1 = -1
                  ,val_Neg2 = 0)
  {
    if(all(x%in%make_0 | x%in%make_1 | x%in%make_2 | x == -1 | x == -2 | x == -3) == F) stop("problem with tweak() in CalcSffi(): category not covered")
    out = ifelse(x %in% make_0, 0
                 ,ifelse(x %in% make_1, 1
                         ,ifelse(x %in% make_2, 2
                                 ,ifelse(x == -1, val_Neg1
                                         ,ifelse(x == -2, val_Neg2, -3)))))
    return(out)
  }
  anytrue = function(data, fun = sum, margin = 1)
  {
    out = apply(data, 1, function(x)ifelse(any(x == -3), -3, ifelse(any(x == -1), -1, fun(x))))
    return(out)
  }
  
  ## PRIME PROSPECTS SEGMENTATION FUNCTION
  prime.prospects.segmentation = function(quest_df, NwosCycle)
  {
    if(NwosCycle == 2006)
    {
      CE = tweak(quest_df$RESTRICT_LAND_USE)
      CG = tweak(quest_df$CERTIFIED_LAND, make_1 = NULL, make_2 = 1)
      CS = tweak(quest_df$COST_SHARE)
      PF = tweak(quest_df$HRV_CONSULT, make_0 = c(0,2))
      MP = tweak(quest_df$MANAGEMENT_PLAN, make_0 = c(0,2))
      AD = tweak(quest_df$ADVICE)
      
      OBJNAT = tweak(quest_df$OBJ_BIODIVERSITY, make_1 = 1:3, make_0 = 4:7)
      OBJNTF = tweak(quest_df$OBJ_NTFP, make_1 = 1:3, make_0 = 4:7)
      OBJFIR = tweak(quest_df$OBJ_FIREWOOD, make_1 = 1:3, make_0 = 4:7)
      OBJTIM = tweak(quest_df$OBJ_TIMBER, make_1 = 1:3, make_0 = 4:7)
      OBJHUN = tweak(quest_df$OBJ_HUNT, make_1 = 1:3, make_0 = 4:7)
      OBJREC = tweak(quest_df$OBJ_RECREATE, make_1 = 1:3, make_0 = 4:7)
      PLNCE = tweak(quest_df$EASEMENT_FUTURE, make_0 = c(-2, 0, 2, 3))
      PLNGC = tweak(quest_df$CERTIFIED_FUTURE, make_0 = c(-2, 0, 2, 3))
      PLNTIM = tweak(quest_df$FUTURE_SAWLOGS)
      
      OwnVar = "OWNER_CLASS_ADJ"
      NonFamCodes = 41:44
      
    }
    if(NwosCycle == 2011)
    {
      # Calculate Engagement Index
      CE = tweak(quest_df$EASE, make_0 = c(0,9))
      CG = tweak(quest_df$CERT, make_0 = c(0,9))
      CS = tweak(quest_df$COST, make_0 = c(0,9))
      PF = tweak(quest_df$CUT_FORESTER, make_0 = c(0,8,9))
      MP = tweak(quest_df$MAN_PLAN)
      AD = tweak(quest_df$ADVICE)
      
      # Calculate interest index
      OBJNAT = tweak(quest_df$OBJ_NAT, make_1 = 4:5, make_0 = c(1:3, 8))
      OBJNTF = tweak(quest_df$OBJ_NTFP, make_1 = 4:5, make_0 = c(1:3, 8))
      OBJFIR = tweak(quest_df$OBJ_FIRE, make_1 = 4:5, make_0 = c(1:3, 8))
      OBJTIM = tweak(quest_df$OBJ_TIM, make_1 = 4:5, make_0 = c(1:3, 8))
      OBJHUN = tweak(quest_df$OBJ_HUNT, make_1 = 4:5, make_0 = c(1:3, 8))
      OBJREC = tweak(quest_df$OBJ_REC, make_1 = 4:5, make_0 = c(1:3, 8))
      PLNTIM = tweak(quest_df$FUT_CUT_SALE, make_1 = 4:5, make_0 = c(1:3, 8))
      PLNGC = NULL
      PLNCE = tweak(quest_df$EASE_5YR, make_1 = 4:5, make_0 = c(1:3, 8)) 
      
      OwnVar = "OWNTYPE"
      NonFamCodes = 5:6
    }
    
    ENGAGEMENT = anytrue(cbind(CE,CG,CS,PF,MP,AD))
    ENGAGEMENT_LEVEL = ifelse(ENGAGEMENT>=2, 2, ENGAGEMENT)
    
    INTEREST = anytrue(cbind(OBJNAT,OBJNTF,OBJFIR,OBJTIM,OBJHUN,OBJREC,PLNCE,PLNGC,PLNTIM))
    INTEREST_LEVEL = ifelse(INTEREST>=3, 2,
                            ifelse(INTEREST %in% c(1,2), 1, INTEREST))
    
    PRIME_PROSPECTS_GROUP = factor(ifelse(quest_df[,OwnVar] %in% NonFamCodes, 'ignore'
                                          ,ifelse(ENGAGEMENT_LEVEL == -3 | INTEREST_LEVEL == -3, '-3'
                                                  ,ifelse(ENGAGEMENT_LEVEL == -1 | INTEREST_LEVEL  == -1, 'row_99_No answer'
                                                          ,ifelse(ENGAGEMENT_LEVEL==2 & INTEREST_LEVEL==2, 'row_01_Model owner',      ## MODEL OWNER
                                                                  ifelse(ENGAGEMENT_LEVEL==1 & INTEREST_LEVEL==2, 'row_02_Prime prospect',      ## PRIME PROSPECT
                                                                         ifelse(ENGAGEMENT_LEVEL==1 & INTEREST_LEVEL==1, 'row_02_Prime prospect',      ## PRIME PROSPECT
                                                                                ifelse(ENGAGEMENT_LEVEL==0 & INTEREST_LEVEL==2, 'row_02_Prime prospect',    	## PRIME PROSPECT
                                                                                       ifelse(ENGAGEMENT_LEVEL==0 & INTEREST_LEVEL==1, 'row_02_Prime prospect',			## PRIME PROSPECT
                                                                                              ifelse(ENGAGEMENT_LEVEL==2 & INTEREST_LEVEL==1, 'row_03_Potential defector',			## POTENTIAL DEFECTOR
                                                                                                     ifelse(ENGAGEMENT_LEVEL==2 & INTEREST_LEVEL==0, 'row_03_Potential defector',			## POTENTIAL DEFECTOR
                                                                                                            ifelse(ENGAGEMENT_LEVEL==1 & INTEREST_LEVEL==0, 'row_03_Potential defector',			## POTENTIAL DEFECTOR
                                                                                                                   'row_04_Write off')))))))))))
                                   ,levels = c('row_01_Model owner', 'row_02_Prime prospect', 'row_03_Potential defector', 'row_04_Write off', 'row_99_No answer', 'ignore', '-3')
                                   ,ordered = T)
    
    pp_df = data.frame(ENGAGEMENT = ENGAGEMENT
                       ,ENGAGEMENT_LEVEL = ENGAGEMENT_LEVEL
                       ,INTEREST = INTEREST
                       ,INTEREST_LEVEL = INTEREST_LEVEL
                       ,PRIME_PROSPECTS_GROUP = PRIME_PROSPECTS_GROUP)      
    
    return(pp_df)
  }
  sffi__prime = prime.prospects.segmentation(quest_df = quest_df, NwosCycle = NwosCycle)
  
  ## ATTITUDINAL SEGMENTATION FUNCTION
  attitudinal.segmentation = function(quest_df, NwosCycle)
  {
    if(NwosCycle == 2006)
    {
      TIM = tweak(quest_df$OBJ_TIMBER, make_1 = 1:3, make_0 = 4:7, val_Neg1 = NA)
      INV = tweak(quest_df$OBJ_INVEST, make_1 = 1:3, make_0 = 4:7, val_Neg1 = NA)
      AES = tweak(quest_df$OBJ_AESTHETICS, make_1 = 1:3, make_0 = 4:7, val_Neg1 = NA)
      BIO = tweak(quest_df$OBJ_BIODIVERSITY, make_1 = 1:3, make_0 = 4:7, val_Neg1 = NA)
      PRI = tweak(quest_df$OBJ_PRIVACY, make_1 = 1:3, make_0 = 4:7, val_Neg1 = NA)
      HUN = tweak(quest_df$OBJ_HUNT, make_1 = 1:3, make_0 = 4:7, val_Neg1 = NA)
      REC = tweak(quest_df$OBJ_RECREATE, make_1 = 1:3, make_0 = 4:7, val_Neg1 = NA)
      
      FIN = ifelse(is.na(TIM) & is.na(INV), NA, pmax(TIM, INV, na.rm=T))
      AME = ifelse(is.na(AES) & is.na(BIO) & is.na(PRI) & is.na(HUN) & is.na(REC), NA, pmax(AES, BIO, PRI, HUN, REC, na.rm=T))
      
      ATT = factor(ifelse(quest_df$OWNER_CLASS_ADJ %in% 41:44, 'ignore'
                           ,ifelse(is.na(FIN) | is.na(AME), "row_99_No answer"
                                   ,ifelse(FIN==0 & AME==1, 'row_01_Woodland retreat',    ## Woodland Retreat
                                           ifelse(FIN==1 & AME==1, 'row_02_Working the land',    ## Working the Land
                                                  ifelse(FIN==1 & AME==0, 'row_03_Supplemental income',   	## Supplemental Income
                                                         'row_04_Uninvolved')))))
                    ,levels = c('row_01_Woodland retreat', 'row_02_Working the land', 'row_03_Supplemental income', 'row_04_Uninvolved', 'row_99_No answer', 'ignore')
                    ,ordered = T)
      att_df = data.frame(FINANCIAL=FIN, AMENITY=AME, ATTITUDINAL_GROUP=ATT)      
    }
    if(NwosCycle == 2011)
    {
      tim = ifelse(quest_df$OBJ_TIM %in% c(4,5), 1, ifelse(quest_df$OBJ_TIM %in% c(1,2,3), 0, NA))
      inv = ifelse(quest_df$OBJ_INV %in% c(4,5), 1, ifelse(quest_df$OBJ_INV %in% c(1,2,3), 0, NA))
      aes = ifelse(quest_df$OBJ_BEA %in% c(4,5), 1, ifelse(quest_df$OBJ_BEA %in% c(1,2,3), 0, NA))
      nat = ifelse(quest_df$OBJ_NAT %in% c(4,5), 1, ifelse(quest_df$OBJ_NAT %in% c(1,2,3), 0, NA))
      pri = ifelse(quest_df$OBJ_PRI %in% c(4,5), 1, ifelse(quest_df$OBJ_PRI %in% c(1,2,3), 0, NA))
      hun = ifelse(quest_df$OBJ_HUNT %in% c(4,5), 1, ifelse(quest_df$OBJ_HUNT %in% c(1,2,3), 0, NA))
      rec = ifelse(quest_df$OBJ_REC %in% c(4,5), 1, ifelse(quest_df$OBJ_REC %in% c(1,2,3), 0, NA))
      
      fin = ifelse((is.na(tim) & is.na(inv)), NA, pmax(tim, inv, na.rm=T))
      ame = ifelse((is.na(aes) & is.na(nat) & is.na(pri) & is.na(hun) & is.na(rec)), NA, pmax(aes, nat, pri, hun, rec, na.rm=T))
      
      att = factor(ifelse(quest_df$OWNTYPE %in% 5:6, 'ignore'
                          ,ifelse(is.na(fin) | is.na(ame), "row_99_No answer"
                                  ,ifelse(fin==0 & ame==1, 'row_01_Woodland retreat',    # Woodland Retreat
                                          ifelse(fin==1 & ame==1, 'row_02_Working the land',    # Working the Land
                                                 ifelse(fin==1 & ame==0, 'row_03_Supplemental income',
                                                        'row_04_Uninvolved')))))
                   ,levels = c('row_01_Woodland retreat', 'row_02_Working the land', 'row_03_Supplemental income', 'row_04_Uninvolved', 'row_99_No answer', 'ignore')
                   ,ordered = T)
      att_df = data.frame(FINANCIAL=fin, AMENITY=ame, ATTITUDINAL_GROUP=att)      
    }
    return(att_df)
  }
  sffi__att = attitudinal.segmentation(quest_df = quest_df, NwosCycle = NwosCycle)
  return(list(sffi__prime, sffi__att))
}

