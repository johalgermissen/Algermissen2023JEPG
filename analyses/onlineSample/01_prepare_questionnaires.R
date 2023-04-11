#### 01_prepare_questionnaire.R ####

#' Load questionnaire data, pre-process (compute mean indices), 
#' check attention checks, 
#' test for condition difference in football scenarios; merge football data sets;
#' save all data sets as pre-processed data.
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

source(paste0(codeDir,"package_manager.R")); # Load functions
source(paste0(codeDir,"00_functions_analyze.R")); # Load functions

# ================================================================================================================================================ #
#### Load SCS: ####

SCSdata <- read.csv(paste0(rawDir,"data_exp_16464-v2_questionnaire-3bu2.csv")) # SQS
SCSdata <- reshape_questionnaire(SCSdata)
head(SCSdata)

# --------------------------------------------- #
## Mean score:
# Scale from 1 to 5.
# items in correct order are 1,6,8,11
# reversed are items 2,3,4,5,7,9,10,12,13

SCSdata$SCS_Sum <- SCSdata$selfControlQ01 + SCSdata$selfControlQ06 + SCSdata$selfControlQ08 + SCSdata$selfControlQ11 + 
  6 - SCSdata$selfControlQ02 + 6 - SCSdata$selfControlQ03 + 6 - SCSdata$selfControlQ04 + 6 - SCSdata$selfControlQ05 + 6 - SCSdata$selfControlQ07 +
  6 - SCSdata$selfControlQ09 + 6 - SCSdata$selfControlQ10 + 6 - SCSdata$selfControlQ12 + 6 - SCSdata$selfControlQ13
SCSdata$SCS_Mean <- SCSdata$SCS_Sum/13

# --------------------------------------------- #
## Attention check 0!: should indicate "very much" = 5
SCSdata$attentionCheck01 # everyone but 2 subjects
SCSdata[which(SCSdata$attentionCheck01!=5), "Participant.Public.ID"]
# [1] a1s3f48e 6wih2pi3
SCSdata[which(SCSdata$attentionCheck01!=5),] # still variability in responses

# --------------------------------------------- #
## Save:
write.csv(SCSdata,paste0(processedDir, "MGNGSearch_SCS.csv"), row.names = F)

# ================================================================================================================================================ #
#### Load BIS/BAS: ####

BISBASdata <- read.csv(paste0(rawDir, "data_exp_16464-v2_questionnaire-23ew.csv")) # BIS/BAS
BISBASdata <- reshape_questionnaire(BISBASdata)

# --------------------------------------------- #
## Mean scores:
# Scale from 1 to 4.
# Items other than 2 and 22 are reverse-scored.
# BAS Drive: 3, 9, 12, 21
# BAS Fun Seeking: 5, 10, 15, 20
# BAS Reward Responsiveness: 4, 7, 14, 18, 23
# BIS: 2, 8, 13, 16, 19, 22, 24 (2 and 22 reversed)
# Items 1, 6, 11, 17, are fillers.
BISBASdata$BAS_Drive <- (BISBASdata$BISBASQ03 + BISBASdata$BISBASQ09 + BISBASdata$BISBASQ12 + BISBASdata$BISBASQ21)/4
BISBASdata$BAS_FunSeeking <- (BISBASdata$BISBASQ05 + BISBASdata$BISBASQ10 + BISBASdata$BISBASQ15 + BISBASdata$BISBASQ20)/4
BISBASdata$BAS_RewResp <- (BISBASdata$BISBASQ04 + BISBASdata$BISBASQ07 + BISBASdata$BISBASQ14 + BISBASdata$BISBASQ18 + BISBASdata$BISBASQ23)/5
BISBASdata$BIS <- (5 - BISBASdata$BISBASQ02 + BISBASdata$BISBASQ08 + BISBASdata$BISBASQ13 + BISBASdata$BISBASQ16 + 
                     BISBASdata$BISBASQ19 + 5 - BISBASdata$BISBASQ22 + BISBASdata$BISBASQ24)/7

rcor.test(BISBASdata[,c("BAS_Drive","BAS_FunSeeking","BAS_RewResp","BIS")])
#                BAS_Drive BAS_FunSeeking BAS_RewResp BIS   
# BAS_Drive       *****     0.352          0.489      -0.246
# BAS_FunSeeking  0.041     *****          0.449      -0.022
# BAS_RewResp     0.003     0.008          *****       0.107
# BIS             0.161     0.902          0.548       *****
# --> BAS scales moderately correlated, with BIS not so much...

# --------------------------------------------- #
## Attention check 02: should indicate "very true" = 1
BISBASdata$attentionCheck02 # everyone but one
BISBASdata[which(BISBASdata$attentionCheck02!=1),"Participant.Public.ID"]
# a1s3f48e
BISBASdata[which(BISBASdata$attentionCheck02!=1),] # still variability in responses...

# --------------------------------------------- #
## Save:
write.csv(BISBASdata,paste0(processedDir,"MGNGSearch_BISBAS.csv"), row.names = F)

# ================================================================================================================================================ #
#### Football Win Scenario: ####

footballWinData <- read.csv(paste0(rawDir, "data_exp_16464-v2_questionnaire-clcu.csv"))
footballWinData <- reshape_questionnaire(footballWinData)

## Responsibility ratings:
footballWinData$footballWinResponsibilityAction <- as.numeric(footballWinData$footballWinResponsibilityAction)
footballWinData$footballWinResponsibilityInaction <- as.numeric(footballWinData$footballWinResponsibilityInaction)

## Descriptives:
stat.desc(footballWinData[,c("footballWinResponsibilityAction","footballWinResponsibilityInaction")])

## Difference:
footballWinData$footballWinResponsibilityDif <- footballWinData$footballWinResponsibilityAction - footballWinData$footballWinResponsibilityInaction
densityplot(footballWinData$footballWinResponsibilityDif)

## Choice who has higher regret:
table(footballWinData$footballWinRegret) # 30 4

## T-test:
t.test(footballWinData$footballWinResponsibilityAction, footballWinData$footballWinResponsibilityInaction, 
       paired = T) # t = 7.2751, df = 33, p-value = 0.00000002396
cohen.d(footballWinData$footballWinResponsibilityAction, footballWinData$footballWinResponsibilityInaction, 
        paired = T) # d estimate: 1.721684 (large)

# ================================================================================================================================================ #
#### Football Loss Scenario: ####

footballLossData <- read.csv(paste0(rawDir, "data_exp_16464-v2_questionnaire-59ym.csv")) # FootballLoss
footballLossData <- reshape_questionnaire(footballLossData)

## Responsibility ratings:
footballLossData$footballLossResponsibilityAction <- as.numeric(footballLossData$footballLossResponsibilityAction) 
footballLossData$footballLossResponsibilityInaction <- as.numeric(footballLossData$footballLossResponsibilityInaction) 

## Descriptives:
stat.desc(footballLossData[,c("footballLossResponsibilityAction","footballLossResponsibilityInaction")])

## Difference:
footballLossData$footballLossResponsibilityDif <- footballLossData$footballLossResponsibilityAction - footballLossData$footballLossResponsibilityInaction
densityplot(footballLossData$footballLossResponsibilityDif)

## Choice who has higher regret:
table(footballLossData$footballLossRegret) # 11 23

## Responsibility:
t.test(footballLossData$footballLossResponsibilityAction, footballLossData$footballLossResponsibilityInaction, 
       paired = T) # t = -2.4484, df = 33, p-value = 0.01983
cohen.d(footballLossData$footballLossResponsibilityAction, footballLossData$footballLossResponsibilityInaction, 
        paired = T) # d estimate: -0.6192635 (medium)

# ================================================================================================================================================ #
#### Combine football data sets: ####

## Merge:
footballData <- merge(footballWinData, footballLossData, by="Participant.Public.ID")

## Save:
write.csv(footballData,paste0(processedDir, "MGNGSearch_Football.csv"), row.names = F)

# END