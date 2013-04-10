########################################################################################################
##   USDA Forest Service, National Woodland Owner Survey   							##
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
  teak = function(x
                  ,make_0 = 0
                  ,make_1 = 1
                  ,make_2 = NULL
                  ,val_Neg1 = -1
                  ,val_Neg2 = 0
                  ,val_Neg3 = -3)
  {
    out = ifelse(x %in% make_0, 0
                 ,ifelse(x %in% make_1, 1
                         ,ifelse(x %in% make_2, 2
                                 ,ifelse(x == -1, val_Neg1
                                         ,ifelse(x == -2, val_Neg2
                                                 ,ifelse(x == -3, val_Neg3
                                                         ,stop("problem with tweak() in CalcSffi(): category not covered")))))))
    return(out)
  }
  ## PRIME PROSPECTS SEGMENTATION FUNCTION
  prime.prospects.segmentation <- function(quest_df, NwosCycle)
  {
    if(NwosCycle == 2006)
    {
      CE = tweak(quest_df$RESTRICT_LAND_USE)
      CE <- ifelse(quest_df$RESTRICT_LAND_USE %in% c(1), 1,
                   ifelse(quest_df$RESTRICT_LAND_USE %in% c(-2, 0), 0, NA))
      GC <- ifelse(quest_df$CERTIFIED_LAND %in% c(1), 2,
                   ifelse(quest_df$CERTIFIED_LAND %in% c(-2, 0), 0, NA))
      CS <- ifelse(quest_df$COST_SHARE %in% c(1), 1,
                   ifelse(quest_df$COST_SHARE %in% c(-2, 0), 0, NA))
      PF <- ifelse(quest_df$HRV_CONSULT %in% c(1), 1,
                   ifelse(quest_df$HRV_CONSULT %in% c(-2, 0, 2), 0, NA))
      MP <- ifelse(quest_df$MANAGEMENT_PLAN %in% c(1), 1,
                   ifelse(quest_df$MANAGEMENT_PLAN %in% c(-2, 0, 2), 0, NA))
      AD <- ifelse(quest_df$ADVICE %in% c(1), 1,
                   ifelse(quest_df$ADVICE %in% c(-2, 0), 0, NA))
      
      ENGAGEMENT <- CE + GC + CS + PF + MP + AD 
      ENGAGEMENT_LEVEL <- ifelse(ENGAGEMENT>=2, 2, ENGAGEMENT)
      
      OBJNAT <- ifelse(quest_df$OBJ_BIODIVERSITY %in% c(1, 2), 1, 
                       ifelse(quest_df$OBJ_BIODIVERSITY %in% c(3, 4, 5, 6, 7), 0, NA))
      OBJNTF <- ifelse(quest_df$OBJ_NTFP %in% c(1,2), 1,
                       ifelse(quest_df$OBJ_NTFP %in% c(3, 4, 5, 6, 7), 0, NA))
      OBJFIR <- ifelse(quest_df$OBJ_FIREWOOD %in% c(1,2), 1,
                       ifelse(quest_df$OBJ_FIREWOOD %in% c(3, 4, 5, 6, 7), 0, NA))
      OBJTIM <- ifelse(quest_df$OBJ_TIMBER %in% c(1,2), 1,
                       ifelse(quest_df$OBJ_TIMBER %in% c(3, 4, 5, 6, 7), 0, NA))
      OBJHUN <- ifelse(quest_df$OBJ_HUNT %in% c(1,2), 1,
                       ifelse(quest_df$OBJ_HUNT %in% c(3, 4, 5, 6, 7), 0, NA))
      OBJREC <- ifelse(quest_df$OBJ_RECREATE %in% c(1,2), 1,
                       ifelse(quest_df$OBJ_RECREATE %in% c(3, 4, 5, 6, 7), 0, NA))
      PLNCE <- ifelse(quest_df$EASEMENT_FUTURE %in% c(1), 1,
                      ifelse(quest_df$EASEMENT_FUTURE %in% c(-2, 0, 2, 3), 0, NA))
      PLNGC <- ifelse(quest_df$CERTIFIED_FUTURE %in% c(1), 1,
                      ifelse(quest_df$CERTIFIED_FUTURE %in% c(-2, 0, 2, 3), 0, NA))
      PLNTIM <- ifelse(quest_df$FUTURE_SAWLOGS %in% c(1), 1,
                       ifelse(quest_df$FUTURE_SAWLOGS %in% c(0), 0, NA))
      
      INTEREST <- OBJNAT + OBJNTF + OBJFIR + OBJTIM + OBJHUN + OBJREC + PLNCE + PLNGC + PLNTIM
      INTEREST_LEVEL <- ifelse(INTEREST>=3, 2,
                               ifelse(INTEREST %in% c(1,2), 1, INTEREST))
      
      PRIME_PROSPECTS_GROUP <- factor(ifelse(quest_df$OWNER_CLASS_ADJ %in% 41:44, 'ignore'
                                             ,ifelse(ENGAGEMENT_LEVEL==2 & INTEREST_LEVEL==2, 'row_01_Model owner',      ## MODEL OWNER
                                                     ifelse(ENGAGEMENT_LEVEL==1 & INTEREST_LEVEL==2, 'row_02_Prime prospect',    	## PRIME PROSPECT
                                                            ifelse(ENGAGEMENT_LEVEL==1 & INTEREST_LEVEL==1, 'row_02_Prime prospect',			## PRIME PROSPECT
                                                                   ifelse(ENGAGEMENT_LEVEL==0 & INTEREST_LEVEL==2, 'row_02_Prime prospect',			## PRIME PROSPECT
                                                                          ifelse(ENGAGEMENT_LEVEL==0 & INTEREST_LEVEL==1, 'row_02_Prime prospect',			## PRIME PROSPECT
                                                                                 ifelse(ENGAGEMENT_LEVEL==2 & INTEREST_LEVEL==1, 'row_03_Potential defector',			## POTENTIAL DEFECTOR
                                                                                        ifelse(ENGAGEMENT_LEVEL==2 & INTEREST_LEVEL==0, 'row_03_Potential defector',			## POTENTIAL DEFECTOR
                                                                                               ifelse(ENGAGEMENT_LEVEL==1 & INTEREST_LEVEL==0, 'row_03_Potential defector',			## POTENTIAL DEFECTOR
                                                                                                      'row_04_Write off')))))))))
                                      ,levels = c('row_01_Model owner', 'row_02_Prime prospect', 'row_03_Potential defector', 'row_04_Write off', 'ignore')
                                      ,ordered = T)
      pp_df <- data.frame(ENGAGEMENT = ENGAGEMENT
                          ,ENGAGEMENT_LEVEL = ENGAGEMENT_LEVEL
                          ,INTEREST = INTEREST
                          ,INTEREST_LEVEL = INTEREST_LEVEL
                          ,PRIME_PROSPECTS_GROUP = PRIME_PROSPECTS_GROUP)      
    }
    if(NwosCycle == 2011)
    {
      # Calculate Engagement Index
      eng.ease <- ifelse(quest_df$EASE == 1, 1,
                         ifelse(quest_df$EASE == -1, NA, 0))
      eng.cost <- ifelse(quest_df$COST == 1, 1,
                         ifelse(quest_df$COST == -1, NA, 0))
      eng.forester <- ifelse(quest_df$CUT_FORESTER == 1, 1,
                             ifelse(quest_df$CUT_FORESTER == -1, NA, 0))
      eng.plan <- ifelse(quest_df$MAN_PLAN == 1, 1, 0 )
      eng.adv <- ifelse(quest_df$ADVICE == 1, 1, 0 )
      eng.cert <- ifelse(quest_df$CERT == 1, 1, 0)
      eng <- eng.ease + eng.cost + eng.forester + eng.plan + eng.adv + (2*eng.cert)
      eng.lev <- ifelse(eng>=2, 2, eng)
      
      # Calculate interest index  
      int.nat <- ifelse(quest_df$OBJ_NAT %in% c(4,5), 1, 0)
      int.ntfp <- ifelse(quest_df$OBJ_NTFP %in% c(4,5), 1, 0)
      int.fire <- ifelse(quest_df$OBJ_FIRE %in% c(4,5), 1, 0)
      int.tim <- ifelse(quest_df$OBJ_TIM %in% c(4,5), 1, 0)
      int.hun <- ifelse(quest_df$OBJ_HUNT %in% c(4,5), 1, 0)
      int.rec <- ifelse(quest_df$OBJ_REC %in% c(4,5), 1, 0)
      int.tim.fut <- ifelse(quest_df$FUT_CUT_SALE == 1, 1, 0) # FUT_CUT_SALE
      int.ease.fut <- ifelse(quest_df$EASE_5YR %in% c(5,4), 1, 0) # EASE_5YR
      
      int <- int.nat + int.ntfp + int.fire + int.tim + int.hun + int.rec + int.tim.fut + int.ease.fut
      int.lev <- ifelse(int>=3, 2, ifelse(int %in% c(1,2), 1, 0))
      
      pp <- factor(  ifelse(quest_df$OWNTYPE %in% 5:6, 'ignore'
                            ,ifelse(eng.lev==2 & int.lev==2, 'row_01_Model owner',      ## model owner
                                    ifelse(eng.lev==1 & int.lev==2, 'row_02_Prime prospect',      ## prime prospect
                                           ifelse(eng.lev==1 & int.lev==1, 'row_02_Prime prospect',			## prime prospect
                                                  ifelse(eng.lev==0 & int.lev==2, 'row_02_Prime prospect',			## prime prospect
                                                         ifelse(eng.lev==0 & int.lev==1, 'row_02_Prime prospect',			## prime prospect
                                                                ifelse(eng.lev==2 & int.lev==1, 'row_03_Potential defector',			## potential defector
                                                                       ifelse(eng.lev==2 & int.lev==0, 'row_03_Potential defector',			## potential defector
                                                                              ifelse(eng.lev==1 & int.lev==0, 'row_03_Potential defector',			## potential defector
                                                                                     'row_04_Write off')))))))))
                     ,levels = c('row_01_Model owner', 'row_02_Prime prospect', 'row_03_Potential defector', 'row_04_Write off', 'ignore')
                     ,ordered = T)
      pp_df <- data.frame(ENGAGEMENT = eng
                          ,ENGAGEMENT_LEVEL = eng.lev
                          ,INTEREST = int
                          ,INTEREST_LEVEL = int.lev
                          ,PRIME_PROSPECTS_GROUP = pp)      
    }
    return(pp_df)
  }
  sffi__prime = prime.prospects.segmentation(quest_df = quest_df, NwosCycle = NwosCycle)
  
  ## ATTITUDINAL SEGMENTATION FUNCTION
  attitudinal.segmentation <- function(quest_df, NwosCycle)
  {
    if(NwosCycle == 2006)
    {
      TIM_Q1 <- quantile(quest_df$OBJ_TIMBER[quest_df$OBJ_TIMBER>=1], 0.25)
      INV_Q1 <- quantile(quest_df$OBJ_INVEST[quest_df$OBJ_INVEST>=1], 0.25)
      AES_Q1 <- quantile(quest_df$OBJ_AESTHETICS[quest_df$OBJ_AESTHETICS>=1], 0.25)
      BIO_Q1 <- quantile(quest_df$OBJ_BIODIVERSITY[quest_df$OBJ_BIODIVERSITY>=1], 0.25)
      PRI_Q1 <- quantile(quest_df$OBJ_PRIVACY[quest_df$OBJ_PRIVACY>=1], 0.25)
      HUN_Q1 <- quantile(quest_df$OBJ_HUNT[quest_df$OBJ_HUNT>=1], 0.25)
      REC_Q1 <- quantile(quest_df$OBJ_RECREATE[quest_df$OBJ_RECREATE>=1], 0.25)
      
      TIM <- ifelse(quest_df$OBJ_TIMBER %in% c(-1), NA, ifelse(quest_df$OBJ_TIMBER<=TIM_Q1, 1, 0))
      INV <- ifelse(quest_df$OBJ_INVEST %in% c(-1), NA, ifelse(quest_df$OBJ_INVEST<=INV_Q1, 1, 0))
      AES <- ifelse(quest_df$OBJ_AESTHETICS %in% c(-1), NA, ifelse(quest_df$OBJ_AESTHETICS<=AES_Q1, 1, 0))
      BIO <- ifelse(quest_df$OBJ_BIODIVERSITY %in% c(-1), NA, ifelse(quest_df$OBJ_BIODIVERSITY<=BIO_Q1, 1, 0))
      PRI <- ifelse(quest_df$OBJ_PRIVACY %in% c(-1), NA, ifelse(quest_df$OBJ_PRIVACY<=PRI_Q1, 1, 0))
      HUN <- ifelse(quest_df$OBJ_HUNT %in% c(-1), NA, ifelse(quest_df$OBJ_HUNT<=HUN_Q1, 1, 0))
      REC <- ifelse(quest_df$OBJ_RECREATE %in% c(-1), NA, ifelse(quest_df$OBJ_RECREATE<=REC_Q1, 1, 0))
      
      FIN <- ifelse(is.na(TIM) & is.na(INV), NA, pmax(TIM, INV, na.rm=T))
      AME <- ifelse(is.na(AES) & is.na(BIO) & is.na(PRI) & is.na(HUN) & is.na(REC), NA, pmax(AES, BIO, PRI, HUN, REC, na.rm=T))
      
      ATT <- factor(ifelse(quest_df$OWNER_CLASS_ADJ %in% 41:44, 'ignore'
                           ,ifelse(is.na(FIN) | is.na(AME), "row_99_No response"
                                   ,ifelse(FIN==0 & AME==1, 'row_01_Woodland retreat',    ## Woodland Retreat
                                           ifelse(FIN==1 & AME==1, 'row_02_Working the land',    ## Working the Land
                                                  ifelse(FIN==1 & AME==0, 'row_03_Supplemental income',   	## Supplemental Income
                                                         'row_04_Uninvolved')))))
                    ,levels = c('row_01_Woodland retreat', 'row_02_Working the land', 'row_03_Supplemental income', 'row_04_Uninvolved', 'row_99_No response', 'ignore')
                    ,ordered = T)
      att_df <- data.frame(FINANCIAL=FIN, AMENITY=AME, ATTITUDINAL_GROUP=ATT)      
    }
    if(NwosCycle == 2011)
    {
      tim <- ifelse(quest_df$OBJ_TIM %in% c(4,5), 1, ifelse(quest_df$OBJ_TIM %in% c(1,2,3), 0, NA))
      inv <- ifelse(quest_df$OBJ_INV %in% c(4,5), 1, ifelse(quest_df$OBJ_INV %in% c(1,2,3), 0, NA))
      aes <- ifelse(quest_df$OBJ_BEA %in% c(4,5), 1, ifelse(quest_df$OBJ_BEA %in% c(1,2,3), 0, NA))
      nat <- ifelse(quest_df$OBJ_NAT %in% c(4,5), 1, ifelse(quest_df$OBJ_NAT %in% c(1,2,3), 0, NA))
      pri <- ifelse(quest_df$OBJ_PRI %in% c(4,5), 1, ifelse(quest_df$OBJ_PRI %in% c(1,2,3), 0, NA))
      hun <- ifelse(quest_df$OBJ_HUNT %in% c(4,5), 1, ifelse(quest_df$OBJ_HUNT %in% c(1,2,3), 0, NA))
      rec <- ifelse(quest_df$OBJ_REC %in% c(4,5), 1, ifelse(quest_df$OBJ_REC %in% c(1,2,3), 0, NA))
      
      fin <- ifelse((is.na(tim) & is.na(inv)), NA, pmax(tim, inv, na.rm=T))
      ame <- ifelse((is.na(aes) & is.na(nat) & is.na(pri) & is.na(hun) & is.na(rec)), NA, pmax(aes, nat, pri, hun, rec, na.rm=T))
      
      att <- factor(ifelse(quest_df$OWNTYPE %in% 5:6, 'ignore'
                           ,ifelse(is.na(fin) | is.na(ame), "row_99_No response")
                           ,ifelse(fin==0 & ame==1, 'row_01_Woodland retreat',    # Woodland Retreat
                                   ifelse(fin==1 & ame==1, 'row_02_Working the land',    # Working the Land
                                          ifelse(fin==1 & ame==0, 'row_03_Supplemental income',
                                                 'row_04_Uninvolved'))))
                    ,levels = c('row_01_Woodland retreat', 'row_02_Working the land', 'row_03_Supplemental income', 'row_04_Uninvolved', 'row_99_No response', 'ignore')
                    ,ordered = T)
      att_df <- data.frame(FINANCIAL=fin, AMENITY=ame, ATTITUDINAL_GROUP=att)      
    }
    return(att_df)
  }
  sffi__att = attitudinal.segmentation(quest_df = quest_df, NwosCycle = NwosCycle)
  return(list(sffi__prime, sffi__att))
}
