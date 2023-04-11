#### 05_analyze_questionnaires.R ####

#' Load and pre-process behavioral data,
#' load pre-processed questionnaire data,
#' test whether questionnaire scores moderate attentional (cue position) effects on responses.
#' Uses pre-processed questionnaire data (either use the files provided or created them via executing 01_prepare_questionnaires.R).
#' Adjust root directory to your own folder structure before running script.

# ================================================================================================================================================ #
#### Directories: ####

rootDir       <- "/project/2420133.01/" # adjust to your own folder structure before running script

codeDir       <- paste0(rootDir,"analyses/onlineSample/")

dataDir       <- paste0(rootDir,"data/onlineSample/") 
rawDir        <- paste0(dataDir,"rawData/")
processedDir  <- paste0(dataDir,"processedData/")

modelDir <- paste0(dataDir,"models/")
if (!dir.exists(modelDir)) {dir.create(modelDir)}

# ================================================================================================================================================ #
#### Load packages and custom functions: #

source(paste0(codeDir,"package_manager.R")); # Load packages, set factor coding
source(paste0(codeDir,"00_functions_analyze.R")); # Load functions

# ================================================================================================================================================ #
#### Load raw GNG task data: ####

cat("Load raw data of task\n")
rawData <- do.call("rbind", lapply(list.files(rawDir, pattern="task",full=TRUE), 
                                   read.csv, header=T))

# ================================================================================================================================================ #
#### Select GNG task data: ####

## Select by vector of strings:
variables <- c("Participant.Public.ID","Reaction.Time","Response","Correct","Incorrect",
               "LeftActionCue","RightActionCue","LeftOutcome","RightOutcome","ITI","Reward","ISI","Punishment",
               "LeftCatch","MiddleCatch","RightCatch","CatchAnswer","RewAction",
               "SpreadsheetID","MaxTrial","Trialnr","Blocknr",
               "LeftOutcomeNum","RightOutcomeNum","Decoy","RewardNum","PunishmentNum","CuePosition","CueOutcome","CueOutcomeNum","ReqAction","Validity")

## Apply function:
GNGData <- preprocess_task_data(rawData, variables)

# ================================================================================================================================================ #
#### Load questionnaire data: ####

SCSData <- read.csv(paste0(processedDir, "MGNGSearch_SCS.csv"))
BISBASData <- read.csv(paste0(processedDir, "MGNGSearch_BISBAS.csv"))
footballData <- read.csv(paste0(processedDir, "MGNGSearch_Football.csv"))

# ================================================================================================================================================ #
#### 1) Effect of SCS: ####

## Merge data:
mergeData <- merge(GNGData, SCSData, by = "Participant.Public.ID")

## Select data:
modData <- mergeData
# noLearners <- c(6, 11, 13)
# modData <- subset(mergeData, !(subject %in% noLearners))
table(modData$subject)
length(unique(modData$subject))

## Z-standardize:
modData$SCS_Mean_z <- as.numeric(scale(modData$SCS_Mean))

## Formula:
formula = "Response_n ~ CuePosition_f * SCS_Mean_z + (CuePosition_f * SCS_Mean_z|subject)"

## Fit model:
mod <- glmer(formula = formula, 
             modData, family = binomial(), 
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod); beep()
#                           Estimate Std. Error z value         Pr(>|z|)    
# (Intercept)                0.17922    0.02579   6.949 0.00000000000367 ***
# CuePosition_f1             0.12617    0.03366   3.748         0.000178 ***
# SCS_Mean_z                 0.01740    0.02951   0.590         0.555330    
# CuePosition_f1:SCS_Mean_z -0.02811    0.03262  -0.862         0.388823     XXX

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                          Df   Chisq Chi Df Pr(>Chisq)    
# CuePosition_f            13 11.2522      1  0.0007953 ***
# SCS_Mean_z               13  0.3326      1  0.5641421     
# CuePosition_f:SCS_Mean_z 13  0.7008      1  0.4025187 XXX

# ================================================================================================================================================ #
#### 2) Effect of BIS/BAS scales: ####

## Merge data:
mergeData <- merge(GNGData, BISBASData, by = "Participant.Public.ID")

## Select data:
modData <- mergeData
# noLearners <- c(6, 11, 13)
# modData <- subset(mergeData, !(subject %in% noLearners))
table(modData$subject)
length(unique(modData$subject))

## Z-standardize:
modData$BIS_z <- as.numeric(scale(modData$BIS))
modData$BAS_Drive_z <- as.numeric(scale(modData$BAS_Drive))
modData$BAS_FunSeeking_z <- as.numeric(scale(modData$BAS_FunSeeking))
modData$BAS_RewResp_z <- as.numeric(scale(modData$BAS_RewResp))

# -------------------------------------------------------------------------- #
#### 2A) BIS: ####

## Formula:
formula = "Response_n ~ CuePosition_f * BIS_z + (CuePosition_f * BIS_z|subject)"

## Fit model:
mod <- glmer(formula = formula, 
             modData, family = binomial(), 
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod); beep()
#                      Estimate Std. Error z value       Pr(>|z|)    
# (Intercept)           0.16905    0.02618   6.458 0.000000000106 ***
# CuePosition_f1        0.13470    0.03168   4.252 0.000021216650 ***
# BIS_z                -0.05993    0.02867  -2.091         0.0365 *  
# CuePosition_f1:BIS_z -0.06935    0.03196  -2.170         0.0300 * XXX

plot(effect("CuePosition_f:BIS_z",mod)) # --> cue position effect only in people with low BIS scores

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                     Df   Chisq Chi Df Pr(>Chisq)    
# CuePosition_f       13 14.7869      1  0.0001204 ***
# BIS_z               13  4.1221      1  0.0423269 *  
# CuePosition_f:BIS_z 13  4.3197      1  0.0376736 *  XXX

# -------------------------------------------------------------------------- #
#### 2B) BAS Drive: ####

## Formula:
formula = "Response_n ~ CuePosition_f * BAS_Drive_z + (CuePosition_f * BAS_Drive_z|subject)"

## Fit model:
mod <- glmer(formula = formula, 
             modData, family = binomial(), 
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod); beep()
#                            Estimate Std. Error z value          Pr(>|z|)    
# (Intercept)                 0.18173    0.02451   7.415 0.000000000000122 ***
# CuePosition_f1              0.12203    0.02960   4.122 0.000037591276703 ***
# BAS_Drive_z                 0.05139    0.02805   1.832            0.0669 .  
# CuePosition_f1:BAS_Drive_z -0.03942    0.03639  -1.083            0.2786    XXX

plot(effect("CuePosition_f:BAS_Drive_z",mod)) # 

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                           Df   Chisq Chi Df Pr(>Chisq)    
# CuePosition_f             13 12.1187      1  0.0004992 ***
# BAS_Drive_z               13  0.9784      1  0.3225829    
# CuePosition_f:BAS_Drive_z 13  1.0304      1  0.3100620  XXX

# -------------------------------------------------------------------------- #
#### 2C) BAS Fun Seeking: ####

## Formula:
formula = "Response_n ~ CuePosition_f * BAS_FunSeeking_z + (CuePosition_f * BAS_FunSeeking_z|subject)"

## Fit model:
mod <- glmer(formula = formula, 
             modData, family = binomial(), 
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod); beep()
#                                 Estimate Std. Error z value        Pr(>|z|)    
# (Intercept)                      0.17962    0.02748   6.536 0.0000000000632 ***
# CuePosition_f1                   0.12938    0.03267   3.961 0.0000747334376 ***
# BAS_FunSeeking_z                 0.02325    0.02848   0.816          0.4143    
# CuePosition_f1:BAS_FunSeeking_z -0.07489    0.03228  -2.320          0.0204 * XXX   

plot(effect("CuePosition_f:BAS_FunSeeking_z",mod)) # --> cue position effect only in people with low BIS scores

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                                Df   Chisq Chi Df Pr(>Chisq)    
# CuePosition_f                  13 12.7714      1   0.000352 ***
# BAS_FunSeeking_z               13  0.6152      1   0.432846    
# CuePosition_f:BAS_FunSeeking_z 13  4.6362      1   0.031304 *  XXX

# -------------------------------------------------------------------------- #
#### 2D) BAS Reward Responsiveness: ####

## Formula:
formula = "Response_n ~ CuePosition_f * BAS_RewResp_z + (CuePosition_f * BAS_RewResp_z|subject)"

## Fit model:
mod <- glmer(formula = formula, 
             modData, family = binomial(), 
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod); beep()
#                              Estimate Std. Error z value        Pr(>|z|)    
# (Intercept)                   0.18057    0.02733   6.607 0.0000000000393 ***
# CuePosition_f1                0.13120    0.03168   4.142 0.0000344936192 ***
# BAS_RewResp_z                -0.01648    0.02909  -0.567           0.571    
# CuePosition_f1:BAS_RewResp_z -0.01476    0.03440  -0.429           0.668 XXX

plot(effect("CuePosition_f:BAS_RewResp_z",mod)) # --> cue position effect only in people with low BIS scores

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                             Df   Chisq Chi Df Pr(>Chisq)    
# CuePosition_f               13 12.8033      1   0.000346 ***
# BAS_RewResp_z               13  0.3003      1   0.583717    
# CuePosition_f:BAS_RewResp_z 13  0.0969      1   0.755606  XXX

# ================================================================================================================================================ #
#### 3) Effect of football ratings: ####

## Merge data:
mergeData <- merge(GNGData, footballData, by = "Participant.Public.ID")

## Select data:
modData <- mergeData
# noLearners <- c(6, 11, 13)
# modData <- subset(mergeData, !(subject %in% noLearners))
table(modData$subject)
length(unique(modData$subject))

## Create factors:
modData$footballWinRegret_f <- factor(modData$footballWinRegret, levels = c(1,2), labels = c("Shift","Stay"))
modData$footballLossRegret_f <- factor(modData$footballLossRegret, levels = c(1,2), labels = c("Shift","Stay"))

## Z-standardize:
modData$footballWinResponsibilityDif_z <- as.numeric(scale(modData$footballWinResponsibilityDif))
modData$footballLossResponsibilityDif_z <- as.numeric(scale(modData$footballLossResponsibilityDif))

# -------------------------------------------------------------------------- #
#### 3A) Regret Win condition: ####

## Formula:
formula = "Response_n ~ CuePosition_f * footballWinRegret_f + (CuePosition_f * footballWinRegret_f|subject)"

## Fit model:
mod <- glmer(formula = formula, 
             modData, family = binomial(), 
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod); beep()
#                                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          0.15695    0.04266   3.679 0.000234 ***
# CuePosition_f1                       0.14059    0.04493   3.129 0.001756 ** 
# footballWinRegret_f1                 0.02664    0.04266   0.624 0.532415    
# CuePosition_f1:footballWinRegret_f1 -0.01679    0.04493  -0.374 0.708692  XXX

plot(effect("CuePosition_f:footballWinRegret_f",mod)) #

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                                   Df  Chisq Chi Df Pr(>Chisq)  
# CuePosition_f                     13 5.5802      1    0.01816 *
# footballWinRegret_f               13 0.3871      1    0.53385  
# CuePosition_f:footballWinRegret_f 13 0.1381      1    0.71013 XXX 

# -------------------------------------------------------------------------- #
#### 3B) Responsibility Win condition: ####

## Formula:
formula = "Response_n ~ CuePosition_f * footballWinResponsibilityDif_z + (CuePosition_f * footballWinResponsibilityDif_z|subject)"

## Fit model:
mod <- glmer(formula = formula, 
             modData, family = binomial(), 
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod); beep()
#                                               Estimate Std. Error z value        Pr(>|z|)    
# (Intercept)                                    0.17837    0.02645   6.742 0.0000000000156 ***
# CuePosition_f1                                 0.12353    0.03357   3.679        0.000234 ***
# footballWinResponsibilityDif_z                -0.01646    0.02937  -0.560        0.575299    
# CuePosition_f1:footballWinResponsibilityDif_z  0.02111    0.03335   0.633        0.526741   XXX

plot(effect("CuePosition_f:footballWinResponsibilityDif_z",mod)) #

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                                              Df   Chisq Chi Df Pr(>Chisq)   
# CuePosition_f                                13 10.8243      1   0.001002 **
# footballWinResponsibilityDif_z               13  0.3112      1   0.576936   
# CuePosition_f:footballWinResponsibilityDif_z 13  0.3915      1   0.531521 XXX

# -------------------------------------------------------------------------- #
#### 3C) Regret Loss condition: ####

## Formula:
formula = "Response_n ~ CuePosition_f * footballLossRegret_f + (CuePosition_f * footballLossRegret_f|subject)"

## Fit model:
mod <- glmer(formula = formula, 
             modData, family = binomial(), 
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod); beep()
#                                      Estimate Std. Error z value         Pr(>|z|)    
# (Intercept)                           0.19206    0.02713   7.079 0.00000000000145 ***
# CuePosition_f1                        0.12415    0.03221   3.855         0.000116 ***
# footballLossRegret_f1                 0.04225    0.02713   1.557         0.119435    
# CuePosition_f1:footballLossRegret_f1 -0.01027    0.03221  -0.319         0.749862 XXX

plot(effect("CuePosition_f:footballLossRegret_f",mod)) #

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                                    Df   Chisq Chi Df Pr(>Chisq)    
# CuePosition_f                      13 13.2965      1  0.0002659 ***
# footballLossRegret_f               13  2.3934      1  0.1218515    
# CuePosition_f:footballLossRegret_f 13  0.1013      1  0.7502732   XXX

# -------------------------------------------------------------------------- #
#### 3D) Responsibility Loss condition: ####

## Formula:
formula = "Response_n ~ CuePosition_f * footballLossResponsibilityDif_z + (CuePosition_f * footballLossResponsibilityDif_z|subject)"

## Fit model:
mod <- glmer(formula = formula, 
             modData, family = binomial(), 
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod); beep()
#                                                 Estimate Std. Error z value        Pr(>|z|)    
# (Intercept)                                     0.175850   0.026776   6.567 0.0000000000512 ***
# CuePosition_f1                                  0.111975   0.030814   3.634        0.000279 ***
# footballLossResponsibilityDif_z                 0.028833   0.027340   1.055        0.291607    
# CuePosition_f1:footballLossResponsibilityDif_z -0.003537   0.040269  -0.088        0.930007  XXX

plot(effect("CuePosition_f:footballLossResponsibilityDif_z",mod)) #

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                                               Df   Chisq Chi Df Pr(>Chisq)    
# CuePosition_f                                 13 11.1744      1  0.0008293 ***
# footballLossResponsibilityDif_z               13  0.9029      1  0.3419963    
# CuePosition_f:footballLossResponsibilityDif_z 13  0.0070      1  0.9331602  XXX 

# END