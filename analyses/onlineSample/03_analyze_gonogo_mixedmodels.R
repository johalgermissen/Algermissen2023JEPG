#### 03_analyze_gonogo_mixedmodels.R ####

#' Load and pre-process behavioral data,
#' test effects of attention (cue position), instructions (Instruction), and stakes difference (PointDif) on responses and RTs.
#' Plots are created in 04_plot_gonogo.R
#' Moderation of effects by questionnaires are tested in 05_analyze_questionnaires.R.
#' Adjust root directory to your own folder structure before running script.

# ================================================================================================================================================ #
#### Set directories: ####

rootDir       <- "/project/2420133.01/" # adjust to your own folder structure before running script

codeDir       <- paste0(rootDir,"analyses/onlineSample/")

dataDir       <- paste0(rootDir,"data/onlineSample/") 
rawDir        <- paste0(dataDir,"rawData/")
processedDir  <- paste0(dataDir,"processedData/")

modelDir <- paste0(dataDir,"models/")
if (!dir.exists(modelDir)) {dir.create(modelDir)}
plotDir <- paste0(dataDir,"plots/")
if (!dir.exists(plotDir)) {dir.create(plotDir)}

# ================================================================================================================================================ #
#### Load packages and custom functions: #

source(paste0(codeDir,"package_manager.R")); # Load packages, set factor coding
source(paste0(codeDir,"00_functions_analyze.R")); # Load functions

# ================================================================================================================================================ #
#### Load raw data: ####

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

## Overview all variables:
str(GNGData)
head(GNGData)
round(stat.desc(GNGData))
# subject                 = numeric, participant ID (1-34).
# Sub_Trialnr             = string, subject ID + underscore + trial number (note that 15 practice trials were skipped in pre-processing).
# Participant.Public.ID   = string, participant's public ID on Prolific, used for merging with questionnaires.
# Reaction.Time           = numeric, participant's reaction time on this trial.
# Response                = factor with two levels "Go" and "NoGo", participant's response on this trial.
# Correct                 = integer, 1 if correct response to this cue on this trial, 0 if incorrect response.
# Incorrect               = integer, 1 if incorrect response to this cue on this trial, 0 if correct response.
# LeftActionCue           = string, image file name of cue displayed on the left side that requires Go/ NoGo response; A-D (blocks 1-4) + 1-4 (cue 1-4) or "Transparent" (no cue displayed).
# RightActionCue          = string, image file name of cue displayed on the right side that requires Go/ NoGo response; A-D (blocks 1-4) + 1-4 (cue 1-4) or "Transparent" (no cue displayed).
# LeftOutcome             = string, image file name of potential outcome displayed on the left side; p (positive) or m (negative) + 10-90 (potential outcome magnitude).
# RightOutcome            = string, image file name of potential outcome displayed on the right side; p (positive) or m (negative) + 10-90 (potential outcome magnitude).
# ITI                     = numeric, inter-trial-interval in ms (uniformly distributed 1100 1200 1300 1400 1500 1600 1700 1800 1900).
# Reward                  = string, image file name of potential reward; p (positive) + 10-90 (potential reward magnitude).
# ISI                     = numeric, inter-stimulus-interval in ms (uniformly distributed 300 400 500 600 800 900 1000 1100)
# Punishment              = string, image file name of potential punishment; m (negative) + 10-90 (potential reward magnitude).
# LeftCatch               = numeric, number presented as the left choice option on catch trials.
# MidCatch                = numeric, number presented as the middle choice option on catch trials.
# RightCatch              = numeric, number presented as the right choice option on catch trials.
# CatchAnswer             = numeric, correct answer on catch trials.
# RewAction               = factor with two levels "Go" and "NoGo", action that will be rewarded on this trial (given Correct and Valid).
# SpredsheetID            = numeric, ID of spreadsheet used for this participant.
# MaxTrial                = numeric, maximal trial index (= total number of trials) per participant.
# Trialnr                 = numeric, trial index (incrementing).
# Blocknr                 = numeric, block index (incrementing).
# LeftOutcomeNum          = numeric, potential outcome magnitude displayed on the left side (10-90).
# RightOutcomeNum         = numeric, potential outcome magnitude displayed on the left side (10-90).
# Decoy                   = numeric, on catch trials: number that was neither the possible reward magnitude nor the possible punishment magnitude.
# RewardNum               = numeric, potential reward magnitude on this trial.
# PunishmentNum           = numeric, potential punishment magnitude on this trial.
# CuePosition             = factor with two levels "Punishment" and "Reward", whether Go/NoGo cue was presented next to potential reward magnitude or potential punishment magnitude.
# CueOutcome              = string, image file name of outcome next to which the Go/NoGo cue was presented.
# CueOutcomeNum           = numeric, magnitude of outcome next to which the Go/NoGo cue was presented.
# ReqAction               = factor with two levels "Go" and "NoGo", action required for this cue.
# Validity                = factor with two levels "Invalid" and "Valid", "Valid" gives reward for correct action and punishment for incorrect action, "Invalid" gives reverse.
# RewardPosition          = factor with two levels "Left" and "Right", on which side the possible reward magnitude was presented.
# CuePosition_n           = numeric, whether the Go/NoGo cue was displayed next to the left (1) or right (-1) outcome magnitude.
# CueOutcome_n            = numeric, magnitude of the possible outcome next to which the Go/ NoGo cue was presented.
# Reaction.Time_cleaned   = numeric, reaction time in seconds with RTs < 0.3 sec deleted (see function preprocess_task_data()).
# Outcome                 = numeric, magnitude of outcome obtained.
# OutcomeVal              = factor with two levels "Positive" and "Negative", valence of outcome obtained.
# OutcomeVal_n            = numeric, whether obtained outcome was positive (1) or negative (-1).
# PointDif                = numeric, difference between possible outcome magnitude presented on the left minus on the right.
# CueValence_n            = numeric, whether the possible reward magnitude was bigger than the possible punishment magnitude (1) or not.
# CueValence_f            = factor with two levels "Positive" and "Negative", whether possible reward magnitude was bigger than the possible punishment magnitude ("Positive") or not ("Negative").
# Instruction             = factor with two levels "Before" and "After", whether participant performed trial before getting additional instructions ("Before", blocks 1 & 2) or afterwards ("After", blocks 3 & 4).
# Response_n              = numeric, whether participant performed Go response (1) on this trial or not (0).
# Accuracy_n              = numeric, whether participant performed correct response (1) on this trial or not (0).
# Accuracy_f              = factor with two levels "Correct" and "Incorrect", whether participant performed correct response on this trial or not.
# Trialnr_Block           = numeric, trial number within block.
# Trialnr_within_Block    = numeric, index of mini-block of 14 trials (1-4) within block.
# ActionCue               = string, image file name of cue displayed on this trial; A-D (blocks 1-4) + 1-4 (cue 1-4).
# ReqAction_n             = numeric, numeric version of ReqAction.
# Validity_n              = numeric, numeric version of Validity.
# CuePosition_f           = factor, factor version of CuePosition.
# RewardPosition_f        = factor, factor version of RewardPosition.
# ReqAction_f             = factor, factor version of ReqAction.
# RewAction_f             = factor, factor version of RewAction.
# Validity_f              = factor, factor version of Validity.
# Instruction_f           = factor, factor version of Instruction.
# Condition_f             = factor with 4 levels "Go2Reward", "Go2Punishment", NoGo2Reward", "NoGo2Punishment", four conditions obtained by concatenating ReqAction and CueValence.
# CuePosition_o           = ordered factor, ordered factor version of CuePosition.
# RewardPosition_o        = ordered factor, ordered factor version of RewardPosition.
# ReqAction_o             = ordered factor, ordered factor version of ReqAction.
# RewAction_o             = ordered factor, ordered factor version of RewAction.
# Validity_o              = ordered factor, ordered factor version of Validity.
# Instruction_o           = ordered factor, ordered factor version of Instruction.
# Condition_o             = ordered factor, ordered factor version of Condition_f.
# Accuracy_o              = ordered factor, ordered factor version of Accuracy.
# Reaction.Time_z         = numeric, z-standardized reaction times.
# Outcome_z               = numeric, z-standardized version of outcome magnitude.
# RewardNum_z             = numeric, z-standardized version of possible reward magnitude.
# PunishmentNum_z         = numeric, z-standardized version of possible punishment magnitude.
# CueOutcomeNum_z         = numeric, z-standardized version of possible outcome magnitude next to which the Go/ NoGo cue is presented.
# PointDif_z              = numeric, z-standardized version of PointDif.

# ================================================================================================================================================ #
#### Select catch task data: ####

variables <- c("Participant.Public.ID","Participant.Private.ID","Trial.Number",
               "Reaction.Time","Response","Correct","Incorrect",
               "LeftCatch","MiddleCatch","RightCatch")

catchData <- preprocess_catch_data(rawData, variables)

# ================================================================================================================================================ #
#### Data dimensions: ####

table(GNGData$subject) # 34 subjects
table(GNGData$Trialnr) # 224 trials per subject

# ================================================================================================================================================ #
#### 0A) Go/NoGo Accuracy: ####

## Aggregate per subject:
GNGACCvec <- as.numeric(tapply(GNGData$Accuracy_n,GNGData$subject,mean))
round(stat.desc(GNGACCvec),3)
# nbr.val     nbr.null       nbr.na          min          max        range          sum       median 
#  34.000        0.000        0.000        0.527        0.942        0.415       26.790        0.833 
#    mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
#   0.788        0.021        0.043        0.015        0.124        0.157 

## Determine what is significantly above chance level:
nTrialGNG <- sum(GNGData$subject==1) # 224
binom.test(round(nTrialGNG*0.56), nTrialGNG, p = 0.5, alternative = "greater")
# 0.55: number of successes = 123, number of trials = 224, p-value = 0.08022
# 0.56: number of successes = 125, number of trials = 224, p-value = 0.04731

## Subjects with accuracy not significantly above chance:
which(GNGACCvec < 0.56) # 6 11 13
noLearners <- c(6, 11, 13)
# --> Consider excluding subjects 6, 11, 13 (< 55% accuracy)

## Simple logistic regression per subject with ReqAction_f as sole regressor:
for (iSub in sort(unique(GNGData$subject))){ # iSub <- 1
  subdata <- subset(GNGData, subject == iSub)
  mod <- glm(Response_n ~ ReqAction_f, subdata, family = binomial())
  print(paste0("Subject ",iSub,", p = ",round(summary(mod)$"coefficients"[2,4],5)))
} # 6, 11, 13 have p > 0.05
noLearners <- c(6, 11, 13)

# ================================================================================================================================================ #
#### 0B) Catch Trial Accuracy: ####

## Aggregate per subject:
round(tapply(catchData$Accuracy_n,catchData$subject,mean),2)
catchACCvec <- as.numeric(tapply(catchData$Accuracy_n,catchData$subject,mean))
round(stat.desc(catchACCvec),3)
# nbr.val     nbr.null       nbr.na          min          max        range          sum       median 
#  33.000        0.000        1.000        0.250        0.917        0.667       21.917        0.750 
#    mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
#   0.664        0.032        0.066        0.034        0.186        0.280  

## Determine what is significantly above chance level:
nTrialCatch <- sum(catchData$subject==1) # 12
binom.test(round(nTrialCatch*0.63), nTrialCatch, p = 1/3, alternative = "greater")
# 0.62: number of successes = 7, number of trials = 12, p-value = 0.06645
# 0.63: number of successes = 8, number of trials = 12, p-value = 0.01876

length(which(catchACCvec >= 0.63)) # 24 subjects above chance
length(which(catchACCvec < 0.63)) # 9 subjects not above chance
which(catchACCvec < 0.63) #  5  7 11 13 15 18 25 29 33

# ================================================================================================================================================ #
#### 0C) Go/NoGo RTs: ####

# -------------------------------------- #
## NoGos and NAs on RTs:
sum(is.na(GNGData$Reaction.Time)) # 3475
sum(is.na(GNGData$Reaction.Time_cleaned)) # 3514
table(GNGData$Response)
#   Go NoGo 
# 4141 3475 

# -------------------------------------- #
## Raw RTs:

densityplot(GNGData$Reaction.Time)
densityplot(~Reaction.Time, data = GNGData, group = CuePosition_f, auto.key = T)
round(stat.desc(GNGData$Reaction.Time),3)
#  nbr.val     nbr.null       nbr.na          min          max        range          sum       median 
# 4141.000        0.000     3475.000        0.007        1.487        1.480     3429.093        0.803 
#     mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
#    0.828        0.004        0.007        0.054        0.232        0.280 

# -------------------------------------- #
## Cleaned RTs:

densityplot(GNGData$Reaction.Time_cleaned)
densityplot(~Reaction.Time_cleaned, data = GNGData, group = CuePosition_f, auto.key = T)
round(stat.desc(GNGData$Reaction.Time_cleaned),3)
#  nbr.val     nbr.null       nbr.na          min          max        range          sum       median 
# 4102.000        0.000     3514.000        0.316        1.487        1.171     3423.361        0.804 
#     mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
#    0.835        0.003        0.007        0.050        0.223        0.268 

# ================================================================================================================================================ #
#### 0D) Catch trial RTs: ####

densityplot(catchData$Reaction.Time)
round(stat.desc(catchData$Reaction.Time),2)

# ===================================================================================================================================== # 
# ===================================================================================================================================== # 
#### Hypothesis 1A) Responses ~ cue position: ####

## Select data:
modData <- GNGData
# noLearners <- c(6, 11, 13)
# modData <- subset(GNGData, !(subject %in% noLearners))
table(modData$subject)
length(unique(modData$subject))

## Formula:
formula = "Response_n ~ ReqAction_f * CuePosition_f + (ReqAction_f * CuePosition_f|subject)"

## Fit model:
mod <- glmer(formula = formula, 
             modData, family = binomial(), 
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod); beep()
#                             Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)                  0.30128    0.04261   7.071     0.00000000000154 ***
# ReqAction_f1                 1.50548    0.12909  11.662 < 0.0000000000000002 ***
# CuePosition_f1               0.18906    0.04675   4.044     0.00005253279441 *** XXX
# ReqAction_f1:CuePosition_f1  0.04418    0.03096   1.427                0.154   
plot(effect("ReqAction_f",mod))
plot(effect("CuePosition_f",mod))
plot(effect("ReqAction_f:CuePosition_f",mod))

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                           Df   Chisq Chi Df            Pr(>Chisq)    
# ReqAction_f               13 65.7085      1 0.0000000000000005228 ***
# CuePosition_f             13 12.5781      1             0.0003903 ***
# ReqAction_f:CuePosition_f 13  1.6927      1             0.1932398     

# ===================================================================================================================================== # 
#### Hypothesis 2A) Responses ~ cue position * instructions: ####

## Formula:
formula = "Response_n ~ ReqAction_f * CuePosition_f * Instruction_f + (ReqAction_f * CuePosition_f  * Instruction_f|subject)"

## Fit model:
mod <- glmer(formula = formula, 
             modData, family = binomial(), 
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod); beep()
#                                             Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)                                 0.336366   0.045192   7.443   0.0000000000000984 ***
# ReqAction_f1                                1.601958   0.139371  11.494 < 0.0000000000000002 ***
# CuePosition_f1                              0.188585   0.049388   3.818             0.000134 *** XXX
# Instruction_f1                             -0.041862   0.040487  -1.034             0.301154    
# ReqAction_f1:CuePosition_f1                 0.034332   0.033531   1.024             0.305886    
# ReqAction_f1:Instruction_f1                -0.376066   0.065642  -5.729   0.0000000101014378 *** XXX
# CuePosition_f1:Instruction_f1              -0.031170   0.036923  -0.844             0.398563     XXX
# ReqAction_f1:CuePosition_f1:Instruction_f1  0.005253   0.034641   0.152             0.879476

## Save:
# saveRDS(mod, paste0(modelDir,"pGo_ReqActionCuePositionInstruction.rds"))
# mod <- readRDS(paste0(modelDir,"pGo_ReqActionCuePositionInstruction.rds"))

plot(effect("ReqAction_f",mod))
plot(effect("CuePosition_f",mod))
plot(effect("Instruction_f",mod))
plot(effect("ReqAction_f:CuePosition_f",mod))
plot(effect("ReqAction_f:Instruction_f",mod))
plot(effect("CuePosition_f:Instruction_f",mod)) # --> if anything stronger afterwards..?

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                                         Df   Chisq Chi Df         Pr(>Chisq)    
# ReqAction_f                             43 54.5371      1 0.0000000000001525 ***
# CuePosition_f                           43 10.9039      1          0.0009596 ***
# Instruction_f                           43  0.8927      1          0.3447408    
# ReqAction_f:CuePosition_f               43  0.7673      1          0.3810579    
# ReqAction_f:Instruction_f               43 29.2784      1 0.0000000626897074 ***
# CuePosition_f:Instruction_f             43  0.5505      1          0.4581215    
# ReqAction_f:CuePosition_f:Instruction_f 43  1.7793      1          0.1822413  

## Save:
# saveRDS(mod_LRT, paste0(modelDir,"pGo_ReqActionCuePositionInstruction_LRT.rds"))
# mod_LRT <- readRDS(paste0(modelDir,"pGo_ReqActionCuePositionInstruction_LRT.rds"))

# ===================================================================================================================================== # 
# ===================================================================================================================================== # 
#### Hypothesis 1B) RTs ~ cue position: ####

modData <- GNGData
# noLearners <- c(6, 11, 13)
# modData <- subset(GNGData, !(subject %in% noLearners))
table(modData$subject)
length(unique(modData$subject))

## Formula:
formula = "Reaction.Time_cleaned ~ ReqAction_f * CuePosition_f + (ReqAction_f * CuePosition_f|subject)"

## Fit model:
mod <- lmer(formula = formula, 
             modData,
             control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod); beep()
#                              Estimate Std. Error        df t value             Pr(>|t|)    
# (Intercept)                  0.860969   0.013361 33.106231  64.438 < 0.0000000000000002 ***
# ReqAction_f1                -0.039461   0.006678 30.277009  -5.909          0.000001737 ***
# CuePosition_f1              -0.033208   0.005456 37.809916  -6.086          0.000000442 *** XXX
# ReqAction_f1:CuePosition_f1  0.005193   0.004189 99.139786   1.240                0.218 

plot(effect("ReqAction_f",mod))
plot(effect("CuePosition_f",mod))
plot(effect("ReqAction_f:CuePosition_f",mod))

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                           Df  Chisq Chi Df   Pr(>Chisq)    
# ReqAction_f               14 25.027      1 0.0000005653 ***
# CuePosition_f             14 26.155      1 0.0000003151 ***
# ReqAction_f:CuePosition_f 14  1.505      1       0.2199 

# ===================================================================================================================================== # 
#### Hypothesis 2B) RTs ~ cue position * instructions: ####

## Formula:
formula = "Reaction.Time_cleaned ~ ReqAction_f * CuePosition_f * Instruction_f + (ReqAction_f * CuePosition_f  * Instruction_f|subject)"

## Fit model:
mod <- lmer(formula = formula, 
             modData, 
             control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod); beep()
#                                               Estimate  Std. Error          df t value             Pr(>|t|)    
# (Intercept)                                  0.8613144   0.0136232  32.2466912  63.224 < 0.0000000000000002 ***
# ReqAction_f1                                -0.0380856   0.0062403  32.7926740  -6.103           0.00000073 ***
# CuePosition_f1                              -0.0337448   0.0055414  38.9646895  -6.090           0.00000039 ***
# Instruction_f1                               0.0189583   0.0069676  31.3722984   2.721               0.0105 *  
# ReqAction_f1:CuePosition_f1                  0.0059641   0.0041130 130.9582464   1.450               0.1494    
# ReqAction_f1:Instruction_f1                  0.0011683   0.0054932  27.6911212   0.213               0.8331    
# CuePosition_f1:Instruction_f1                0.0066698   0.0050108  39.2121093   1.331               0.1908    
# ReqAction_f1:CuePosition_f1:Instruction_f1  -0.0003644   0.0045420  51.8775977  -0.080               0.9364 

plot(effect("ReqAction_f",mod))
plot(effect("CuePosition_f",mod))
plot(effect("Instruction_f",mod))
plot(effect("ReqAction_f:CuePosition_f",mod))
plot(effect("ReqAction_f:Instruction_f",mod))
plot(effect("CuePosition_f:Instruction_f",mod))

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                                         Df   Chisq Chi Df   Pr(>Chisq)    
# ReqAction_f                             44 26.2095      1 0.0000003063 ***
# CuePosition_f                           44 25.6999      1 0.0000003988 ***
# Instruction_f                           44  6.9342      1     0.008456 ** 
# ReqAction_f:CuePosition_f               44  1.9444      1     0.163187    
# ReqAction_f:Instruction_f               44  0.0408      1     0.839924    
# CuePosition_f:Instruction_f             44  1.6520      1     0.198692    
# ReqAction_f:CuePosition_f:Instruction_f 44  0.0043      1     0.947545 

# ===================================================================================================================================== # 
# ===================================================================================================================================== # 
#### Exploratory analysis 1A) Responses ~ point difference: ####

## Select data:
modData <- GNGData
# noLearners <- c(6, 11, 13)
# modData <- subset(GNGData, !(subject %in% noLearners))
table(modData$subject)
length(unique(modData$subject))

## Formula:
# formula = "Response_n ~ PointDif + (PointDif|subject)"
formula = "Response_n ~ PointDif_z + (PointDif_z|subject)"
# formula = "Response_n ~ PointDif_z * CuePosition_f + (PointDif_z * CuePosition_f|subject)"

## Fit model:
mod <- glmer(formula = formula, 
             modData, family = binomial(), 
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod); beep()
## UNSTANDARDIZED:
#              Estimate Std. Error z value        Pr(>|z|)    
# (Intercept) 0.1769898  0.0269702   6.562 0.0000000000529 ***
# PointDif    0.0022799  0.0007484   3.046         0.00232 ** 
plot(effect("PointDif",mod))
## STANDARDIZED:
#             Estimate Std. Error z value        Pr(>|z|)    
# (Intercept)  0.17595    0.02695   6.528 0.0000000000665 ***
# PointDif_z   0.07511    0.02465   3.047         0.00231 ** 
plot(effect("PointDif_z",mod))
## WITH CUEPOSITION:
#                           Estimate Std. Error z value       Pr(>|z|)    
# (Intercept)               0.178255   0.027787   6.415 0.000000000141 ***
# PointDif_z                0.075569   0.023864   3.167       0.001542 ** 
# CuePosition_f1            0.128000   0.033691   3.799       0.000145 ***
# PointDif_z:CuePosition_f1 0.005305   0.025575   0.207       0.835663   
plot(effect("PointDif_z:CuePosition_f",mod)) # similarly strong

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#          Df  Chisq Chi Df Pr(>Chisq)   
# PointDif  4 8.1485      1    0.00431 **
#            Df  Chisq Chi Df Pr(>Chisq)   
# PointDif_z  4 8.1485      1    0.00431 **

# ===================================================================================================================================== # 
#### Exploratory analysis 1B) RTs ~ point difference: ####

## Select data:
modData <- GNGData
# noLearners <- c(6, 11, 13)
# modData <- subset(GNGData, !(subject %in% noLearners))
table(modData$subject)
length(unique(modData$subject))

## Formula:
# formula = "Reaction.Time_cleaned ~ PointDif + (PointDif|subject)"
formula = "Reaction.Time_cleaned ~ PointDif_z + (PointDif_z|subject)"
# formula = "Reaction.Time_cleaned ~ PointDif_z * CuePosition_f + (PointDif_z * CuePosition_f|subject)"

## Fit model:
mod <- lmer(formula = formula,
            modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
## UNSTANDARDIZED:
#               Estimate Std. Error         df t value            Pr(>|t|)    
# (Intercept)  0.8347663  0.0136741 33.0408322  61.047 <0.0000000000000002 ***
# PointDif    -0.0001297  0.0001301 36.2293417  -0.997               0.326  
plot(effect("PointDif",mod))
## STANDARDIZED:
#              Estimate Std. Error        df t value            Pr(>|t|)    
# (Intercept)  0.834833   0.013672 33.040776  61.061 <0.0000000000000002 ***
# PointDif_z  -0.004272   0.004286 36.229000  -0.997               0.326    
plot(effect("PointDif_z",mod))
## WITH CUEPOSITION:
#                             Estimate Std. Error         df t value             Pr(>|t|)    
# (Intercept)                 0.837238   0.013975  32.991780  59.911 < 0.0000000000000002 ***
# PointDif_z                 -0.003665   0.004262  35.727945  -0.860               0.3956    
# CuePosition_f1             -0.028900   0.004818  31.383772  -5.998           0.00000118 ***
# PointDif_z:CuePosition_f1  -0.005524   0.003263 451.251783  -1.693               0.0911 . 
plot(effect("PointDif_z:CuePosition_f",mod)) # speeding through stakes only when on reward side..?

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
## UNSTANDARDIZED:
#          Df  Chisq Chi Df Pr(>Chisq)
# PointDif  5 1.0055      1      0.316
## STANDARDIZED:
#            Df  Chisq Chi Df Pr(>Chisq)
# PointDif_z  5 1.0055      1      0.316
## WITH CUEPOSITION:
#                          Df   Chisq Chi Df  Pr(>Chisq)    
# PointDif_z               14  0.7495      1     0.38664    
# CuePosition_f            14 25.1440      1 0.000000532 ***
# PointDif_z:CuePosition_f 14  2.8555      1     0.09106 .

## END
