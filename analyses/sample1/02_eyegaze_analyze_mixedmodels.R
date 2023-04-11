#### 02_eyegaze_analyze_mixedmodels.R ####

#' Sample 1:
#' Perform mixed-effects linear and logistic regression models to obtain all results
#' reported in the main text and supplementary material for Sample 1 only.
#' For creating plots featured in the main text and supplementary material, see the sample2/script 03_plots_paper.R. 
#' For between-subjects correlations across both samples, see the script sample2/04_between_subjects.R.
#' Adjust root directory to your own folder structure before running script.

# ================================================================================================================= #
#### Initialize directories: ####

rootDir       <- "/project/2420133.01/" # adjust to your own folder structure before running script

codeDir       <- paste0(rootDir,"analyses/functions/")

dataDir       <- paste0(rootDir,"data/sample1/") 
rawDir        <- paste0(dataDir,"rawData/")
processedDir  <- paste0(dataDir,"processedData/")
behavDataDir  <- paste0(rawDir,"behavData/")
eyeDataDir    <- paste0(processedDir,"eyeDataAggr")

modelDir <- paste0(dataDir,"models/")
if (!dir.exists(modelDir)) {dir.create(modelDir)}
plotDir <- paste0(dataDir,"plots/")
if (!dir.exists(plotDir)) {dir.create(plotDir)}

# ================================================================================================================= #
#### Load packages and custom functions: ####

source(paste0(codeDir,"package_manager.R")) # Load functions
source(paste0(codeDir,"00_functions_analyze.R")) # Load functions

# ================================================================================================================= #
# ================================================================================================================= #
# ================================================================================================================= #
# ================================================================================================================= #
#### Alternative A: Read previously pre-processed data data: ####

eyeData <- read.csv(paste0(processedDataDir,"eyeData_processed.csv"))
eyeData <- wrapper_preprocessing(eyeData) # refresh factors
noLearners <- c(9, 10, 11, 16, 25)

# ================================================================================================================= #
#### Alternative B: Load and combine aggregated eye-tracking files: ####

# -------------------------------------- #
## Load eye-tracking data:
eyeData <- do.call("rbind", lapply(list.files(eyeDataDir, pattern="aggr.csv",full=TRUE), read.csv, header=T))
table(eyeData$subject)
length(table(eyeData$subject))

# -------------------------------------- #
## Load behavioral data:
allData <- lapply(list.files(behavDataDir, pattern=".csv",full=TRUE), read.csv, header=T)
# Account for invalid column headers for 23-35 (just take from 22 and insert for 23-25):
for (i in 23:length(allData)){
  names(allData[[i]]) <- names(allData[[22]])
}
behavData <- do.call("rbind",allData)
table(behavData$subject)
length(table(behavData$subject))

## Add counter of cue number:
behavData <- as.data.frame(group_by(behavData,subject,cue) %>%  mutate(counter = row_number()))
table(behavData$counter)

## Set dwell time to NA if no first fixation:
eyeData[is.na(eyeData$firstfix_out_f),
        c("dwell_left_abs","dwell_right_abs","dwell_left_rel","dwell_right_rel",
          "dwell_rew_abs","dwell_pun_abs","dwell_rew_rel","dwell_pun_rel")] <- NA

# -------------------------------------- #
#### Preprocess: ####

eyeData <- wrapper_preprocessing(eyeData)

# -------------------------------------- #
#### Fit Q-values: ####

tmp <- wrapper_Qvalues(eyeData)
eyeData <- tmp[[1]]
tapply(eyeData$QGo,eyeData$reqAction_f,mean)
tapply(eyeData$QNoGo,eyeData$reqAction_f,mean)

alphaBest <- tmp[[2]]
betaBest <- tmp[[3]]
logLikBest <- tmp[[4]]

# -------------------------------------- #
#### Save pre-processed data: ####

write.csv(eyeData, paste0(processedDataDir, "eyeData_processed.csv"))

# ======================================================================================================================== #
#### 1A) Demographics: #### 

demoData <- subset(behavData, trialNr == 1)

## Age:
round(stat.desc(demoData$age), 1)
#      nbr.val     nbr.null       nbr.na          min          max        range          sum       median         mean      SE.mean 
#        35.00         0.00         0.00        18.00        58.00        40.00       863.00        23.00        24.66         1.19 
# CI.mean.0.95          var      std.dev     coef.var 
#         2.42        49.82         7.06         0.29

## Only young people:
round(stat.desc(demoData$age[demoData$age <= 35]),1)
#      nbr.val     nbr.null       nbr.na          min          max        range          sum       median         mean      SE.mean 
#   34.0000000    0.0000000    0.0000000   18.0000000   35.0000000   17.0000000  805.0000000   23.0000000   23.6764706    0.6997843 
# CI.mean.0.95          var      std.dev     coef.var 
#    1.4237218   16.6497326    4.0804084    0.1723402

selVar <- demoData$age; nRound <- 2
selVar <- demoData$age[demoData$age <= 35]; nRound <- 2
cat(paste0("M = ",round(mean(selVar),nRound),", SD = ",round(sd(selVar),nRound),", range ",round(min(selVar),nRound)," - ",round(max(selVar),nRound),"\n"))
# M = 24.66, SD = 7.06, range 18 - 58
# M = 23.68, SD = 4.08, range 18 - 35

## Gender:
table(demoData$gender)
# female   male 
#     27      8 

## Handedness;
table(demoData$hand)
# left_hand right_hand 
#         5         30

## Eye dominance:
table(demoData$eye)
# left_eye right_eye 
#       14        21

# ======================================================================================================================== #
#### 1B) Accuracy Go/NoGo Task: ####

## Select only GNG task:
taskData <- subset(eyeData,!is.na(outcome))
table(taskData$subject) # 240 GNG trials per subject

## Aggregate accuracy per subject:
GNGACCvec <- as.numeric(tapply(taskData$ACC,taskData$subject,mean)) # save
round(tapply(taskData$ACC,taskData$subject,mean),2) # plot
#    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24   25   26 
# 0.58 0.72 0.75 0.71 0.65 0.75 0.78 0.65 0.53 0.51 0.50 0.63 0.87 0.72 0.70 0.51 0.83 0.77 0.83 0.80 0.68 0.65 0.73 0.80 0.54 0.71 
#   27   28   29   30   31   32   33   34   35 
# 0.83 0.81 0.75 0.61 0.67 0.80 0.82 0.67 0.63 

## Descriptives:
round(stat.desc(GNGACCvec),3)
selVar <- GNGACCvec; nRound <- 3
cat(paste0("M = ",round(mean(selVar),nRound),", SD = ",round(sd(selVar),nRound),", range ",round(min(selVar),nRound)," - ",round(max(selVar),nRound),"\n"))
# M = 0.7, SD = 0.104, range 0.5 - 0.871

## Binomial test:
nTrialGNG <- sum(taskData$subject==1)
binom.test(round(nTrialGNG*0.55), nTrialGNG, p = 0.5, alternative = "greater")
# 0.55: number of successes = 132, number of trials = 240, p-value = 0.06874
# 0.56: number of successes = 134, number of trials = 240, p-value = 0.04057

## Identify subjects who did not learn significantly above chance:
noLearners <- as.numeric(which(round(tapply(taskData$ACC,taskData$subject,mean),2) <= .55))
noLearners # --> subs 9, 10, 11, 16, 25 have ACC <= .55
rbind(noLearners,GNGACCvec[noLearners])
noLearners <- c(9, 10, 11, 16, 25)

# ======================================================================================================================== #
#### 1C) Accuracy Catch Task: ####

## Select only catch trials:
catchData <- subset(eyeData,is.na(outcome))
table(catchData$subject) # 24 catch trials per subject

## Aggregate accuracy per subject:
catchACCvec <- as.numeric(tapply(catchData$ACC,catchData$subject,mean,na.rm=T)) # save
round(tapply(catchData$ACC,catchData$subject,mean,na.rm=T),2) # plot
#    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24   25   26 
# 0.57 0.88 0.88 0.96 0.79 0.92 0.92 0.77 0.71 0.79 0.61 1.00 0.87 0.92 0.88 0.88 0.96 0.88 1.00 0.92 0.83 0.75 0.88 0.83 0.96 0.67 
#   27   28   29   30   31   32   33   34   35 
# 0.96 0.88 0.83 0.88 0.92 0.88 0.92 0.92 0.88 

# --------------------------------------------- #
### Descriptives:
round(stat.desc(catchACCvec),3)
selVar <- catchACCvec; nRound <- 3
cat(paste0("M = ",round(mean(selVar),nRound),", SD = ",round(sd(selVar),nRound),", range ",round(min(selVar),nRound)," - ",round(max(selVar),nRound),"\n"))
# M = 0.858, SD = 0.101, range 0.565 - 1

# --------------------------------------------- #
### Binomial test:
nTrial <- sum(catchData$subject==1)
binom.test(round(nTrial*0.69), nTrial, p = 0.5, alternative = "greater")
# 0.68: number of successes = 132, number of trials = 240, p-value = 0.07579
# 0.69: number of successes = 132, number of trials = 240, p-value = 0.03196

## Identify subjects who did not perform catch task significantly above chance:
noCatchers <- as.numeric(which(round(tapply(catchData$ACC,catchData$subject,mean,na.rm=T),2) < .69))
noCatchers # # --> subs 1, 11, 26 have ACC < .69
catchACCvec[noCatchers]
noCatchers <- c(1, 11, 26)

# ======================================================================================================================== #
#### 1D) Bonus: ####

## Total points obtained (based on outcome):
totalPoints <- tapply(behavData$outcome,behavData$subject,sum,na.rm = T)
totalPoints[totalPoints < 20] <- 20 # minimum possible is 20 points

## Possible points: reward is valid, punishment if invalid
behavData$optimPoints <- ifelse(behavData$validity==1,behavData$rewMag,behavData$punMag*-1)
behavData$optimPoints[is.na(behavData$outcome)] <- NA # don't count catch trials
possiblePoints <- tapply(behavData$optimPoints,behavData$subject,sum,na.rm = T)

## Extra settings:
requiredPoints <- 0
maxPayoff <- 2

## Compute bonus per subject:
euros <- round((totalPoints - requiredPoints)/(possiblePoints-requiredPoints)*maxPayoff,2)
euros[euros > maxPayoff] <- maxPayoff # crop maximum to 2 euros
euros
#    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24   25   26 
# 0.46 0.58 0.86 0.72 0.59 0.88 1.30 0.50 0.10 0.10 0.09 0.54 1.58 0.95 0.83 0.11 1.07 1.17 1.23 1.22 0.66 0.54 1.17 1.22 0.14 0.69 
#   27   28   29   30   31   32   33   34   35 
# 1.33 1.25 1.23 0.26 0.35 1.36 1.06 0.61 0.33

## Descriptives:
stat.desc(euros)
selVar <- euros; nRound <- 2
cat(paste0("M = ",round(mean(selVar),nRound),", SD = ",round(sd(selVar),nRound),", range ",round(min(selVar),nRound)," - ",round(max(selVar),nRound),"\n"))
# M = 0.77, SD = 0.43, range 0.09 - 1.58

# ======================================================================================================================== #
#### 1E) Trials with no fixation: ####

nrow(eyeData) # 9240

## Index of whether no fixations made:
eyeData$noFix_n <- ifelse(is.na(eyeData$dwell_left_abs) & is.na(eyeData$dwell_right_abs), 1, 0)
table(eyeData$noFix_n)
#    0    1 
# 7781  403 

## Number trials with/without fixation per subject:
table(eyeData$noFix_n, eyeData$subject)
#     1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35
# 0 256 263 227 264 256 264 261 264 230 262 174 262 245 251 218 251 263 264 264 224 262 263 263 257 264 196 264 262 264 260 263 255 263 261 264
# 1   8   1  37   0   8   0   3   0  34   2  90   2  19  13  46  13   1   0   0  40   2   1   1   7   0  68   0   2   0   4   1   9   1   3   0
# --> subs 11 (15, 20) and 26 have quite a few (> 40) missing trials
round(table(eyeData$noFix_n,eyeData$subject) / length(unique(eyeData$trialnr)) * 100)

## Descriptives:
noFix <- as.numeric(tapply(eyeData$noFix_n, eyeData$subject, sum, na.rm = T) / length(unique(eyeData$trialnr))) * 100
round(stat.desc(noFix),1)
# nbr.val     nbr.null       nbr.na          min          max        range          sum       median 
#    35.0          9.0          0.0          0.0         34.1         34.1        157.6          0.8 
#    mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
#     4.5          1.3          2.7         63.4          8.0          1.8 

# ======================================================================================================================== #
#### 1F) Trials with only one fixation: ####

nrow(eyeData) # 9240

## Index of only one fixation made:
eyeData$oneFix_n <- ifelse(eyeData$dwell_left_abs==0 & eyeData$dwell_right_abs > 0|eyeData$dwell_left_abs > 0 & eyeData$dwell_right_abs == 0, 1, 0)

table(eyeData$oneFix_n)
#    0    1 
# 7810 1014
table(eyeData$oneFix_n)/nrow(eyeData)
#         0         1 
# 0.8452381 0.1097403

## Number of trials with only one fixation per subject:
table(eyeData$oneFix_n,eyeData$subject) 
#     1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35
# 0 181 252 177 262 132 252 239 259 117 220  12 254 206 204 177 235 263 264 262 197 258 259 262 255 259 133 264 236 261 250 246 248 262 238 214
# 1  75  11  50   2 124  12  22   5 113  42 162   8  39  47  41  16   0   0   2  27   4   4   1   2   5  63   0  26   3  10  17   7   1  23  50
# --> sub 5, 9, 11 often (> 100 times) have only 1 fixation
round(table(eyeData$oneFix_n,eyeData$subject) / length(unique(eyeData$trialnr)) * 100)

oneFix <- as.numeric(tapply(eyeData$oneFix_n, eyeData$subject, sum, na.rm = T) / length(unique(eyeData$trialnr))) * 100
round(stat.desc(oneFix),1)
# nbr.val     nbr.null       nbr.na          min          max        range          sum       median 
#    35.0          3.0          0.0          0.0         61.4         61.4        384.1          4.5 
#    mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
#    11.0          2.5          5.0        212.4         14.6          1.3 

# ===================================================================================================================================== #
#### 1G) RT inspection: ####

## Select data:
modData <- subset(eyeData, isCatch == 0)
# modData <- subset(eyeData, isCatch == 0 & !(subject %in% noLearners)) # exclude subjects who did not learn
length(unique(modData$subject))
table(modData$subject)

## Density of all RTs:
densityplot(modData$RT, xlim = c(0.25,1))

## Too early responses:
sum(modData$RT < 0.200, na.rm = T) # 9 early
tooEarly <- as.numeric(tapply(modData$RT < 0.200, modData$subject, sum, na.rm = T)) / length(unique(modData$trialnr)) * 100
round(stat.desc(tooEarly),1)
# nbr.val     nbr.null       nbr.na          min          max        range          sum       median 
#    35.0         31.0          0.0          0.0          1.5          1.5          3.4          0.0 
#    mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
#     0.1          0.1          0.1          0.1          0.3          3.2

## Too late responses:
sum(modData$RT > 0.800, na.rm = T) # 79 very late 
tooLate <- as.numeric(tapply(modData$RT > 0.800, modData$subject, sum, na.rm = T)) / length(unique(modData$trialnr)) * 100
round(stat.desc(tooLate),1)
# nbr.val     nbr.null       nbr.na          min          max        range          sum       median 
#    64.0         37.0          0.0          0.0         14.0         14.0         34.8          0.0 
#    mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
#     0.5          0.2          0.4          3.2          1.8          3.3 

## Plot density of raw RTs:
densityplot(modData$RT)
sum(!is.na(modData$RT)) # 4947
nrow(modData) # 8400

## Delete < 200 and > 800 ms:
densityplot(modData$RT_cleaned) # more normal, still some right skew
sum(is.na(modData$RT_cleaned)) - sum(is.na(modData$RT)) # excluded 88 data points (< 200 and > 800)

# ===================================================================================================================================== #
#### 2) EFFECTS OF REQUIRED ACTION (LEARNING): ####

# ===================================================================================================================================== #
#### 2A) Responses ~ required action (learning): ####

## Select data:
modData <- subset(eyeData, isCatch == 0)
# modData <- subset(eyeData, isCatch == 0 & !(subject %in% noLearners)) # exclude subjects who did not learnin
length(unique(modData$subject))
table(modData$subject)

## Formula:
formula <- "response_cleaned ~ reqAction_f + rewLeft_f + (reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#              Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)   0.49474    0.06820   7.254    0.000000000000403 ***
# reqAction_f1  1.07536    0.09667  11.124 < 0.0000000000000002 *** XXX
# rewLeft_f1    0.02085    0.02715   0.768                0.443

## Plot:
plot(effect("reqAction_f",mod))
print_effect(mod,"reqAction_f")
plot(effect("rewLeft_f",mod))
print_effect(mod,"rewLeft_f")

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT)
#             Df   Chisq Chi Df         Pr(>Chisq)    
# reqAction_f  8 53.1906      1 0.0000000000003027 ***
# rewLeft_f    8  0.5626      1             0.453

# --------------------------------------- #
### Automatized raincloud bar plot:
png(paste0(plotDir,"raincloud_bar_response_cleaned_reqAction.png"),width = 480, height = 480)
plot_raincloud(modData, x = "reqAction_f", y = "response_cleaned", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0.25,0.90),
               color = c("blue","red"), Labels = c("Go","NoGo"), 
               xLab = "Required Action", yLab = "p(Go)", Main = "Response as a function of \nrequired action")
dev.off()
png(paste0(plotDir,"raincloud_bar_response_cleaned_reqAction_full.png"),width = 480, height = 480)
plot_raincloud(modData, x = "reqAction_f", y = "response_cleaned", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
               color = c("blue","red"), Labels = c("Go","NoGo"), 
               xLab = "Required Action", yLab = "p(Go)", Main = "Response as a function of \nrequired action")
dev.off()

# --------------------------------------- #
### Regression bar plot:
modData <- subset(eyeData, isCatch == 0)

## Formula:
formula <- "response_cleaned ~ reqAction_f + rewLeft_f + (reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
plot(effect("reqAction_f",mod)) # raw scale suggested
plot(effect("reqAction_f",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Regression-based bar plot:
png(paste0(plotDir,"regression_response_cleaned_reqAction.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "reqAction_f", # 
                    yLim = c(0.25,0.90),
                    # yLim = c(0.4,0.9), 
                    color = c("blue","red"), Labels = c("Go","NoGo"), 
                    xLab = "Required Action", yLab = "p(Go)", Main = "Response as a function of \nrequired action")
dev.off()

png(paste0(plotDir,"regression_response_cleaned_reqAction_full.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "reqAction_f", # 
                    yLim = c(0,1),
                    # yLim = c(0.4,0.9), 
                    color = c("blue","red"), Labels = c("Go","NoGo"), 
                    xLab = "Required Action", yLab = "p(Go)", Main = "Response as a function of \nrequired action")
dev.off()

# ===================================================================================================================================== #
#### 2B) RTs ~ required action (control): ####

## Select data:
modData <- subset(eyeData, isCatch == 0)
# modData <- subset(eyeData, isCatch == 0 & !(subject %in% noLearners)) # exclude subjects who did not learnin
length(unique(modData$subject))
table(modData$subject)

## Formula:
formula <- "RT_cleaned_z ~ reqAction_f + (reqAction_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)

#              Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)   0.04946    0.07242 33.91992   0.683    0.499    
# reqAction_f1 -0.08516    0.01635 31.82500  -5.209 0.000011 ***

plot(effect("reqAction_f",mod)) # faster for (correct) Go than (incorrect) NoGo
print_effect(mod,"reqAction_f")

# ------------------------------------------------ #
### Automatized raincloud bar plot:
png(paste0(plotDir,"raincloud_bar_RT_cleaned_reqAction.png"),width = 480, height = 480)
plot_raincloud(modData, x = "reqAction_f", y = "RT_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = c(0.36, 0.65),
               color = c("blue","red"), Labels = c("Go","NoGo"), 
               xLab = "Required Action", yLab = "RT (in sec.)", Main = "Reaction times as a function of \nrequired action")
dev.off()

# ===================================================================================================================================== #
#### 3) RESPONSES ~ STAKES (BIASES): ####

## Select data:
modData <- subset(eyeData, isCatch == 0)
# modData <- subset(eyeData, isCatch == 0 & !(subject %in% noLearners))
# modData <- subset(eyeData, isCatch == 0 & dwell_rew_abs > 0 & dwell_pun_abs > 0) # only trials with both stakes fixated

length(unique(modData$subject))
table(modData$subject)

## Compute difference and re-standardize:
modData$difMag <- modData$rewMag - modData$punMag
modData$difMag_z <- as.numeric(scale(modData$difMag))
modData$rewMag_z <- as.numeric(scale(modData$rewMag))
modData$punMag_z <- as.numeric(scale(modData$punMag))
modData$difMag_z <- as.numeric(scale(modData$difMag))

## Indices of time within task:
modData$trialnr_z <- as.numeric(scale(modData$trialnr))
modData$trialNrBlock_z <- as.numeric(scale(modData$trialNrBlock))
modData$cueRep_z <- as.numeric(scale(modData$cueRep))
modData$blockNr_z <- as.numeric(scale(modData$blockNr))

## Q-value difference:
modData$Qdif <- modData$QGo - modData$QNoGo
modData$Qdif_z <- as.numeric(scale(modData$Qdif))

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 3A) Stakes difference: ####

### Formula:

## Baseline model:
formula <- "response_cleaned ~ difMag_z + reqAction_f + rewLeft_f + (difMag_z + reqAction_f + rewLeft_f|subject)"
# formula <- "response_cleaned ~ difMag_z + Qdif_z + rewLeft_f + (difMag_z + Qdif_z + rewLeft_f|subject)"

## Change over time:
formula <- "response_cleaned ~ difMag_z * trialnr_z + reqAction_f + rewLeft_f + (difMag_z * trialnr_z + reqAction_f + rewLeft_f|subject)"
formula <- "response_cleaned ~ difMag_z * trialNrBlock_z + reqAction_f + rewLeft_f + (difMag_z * trialNrBlock_z + reqAction_f + rewLeft_f|subject)"
formula <- "response_cleaned ~ difMag_z * cueRep_z + reqAction_f + rewLeft_f + (difMag_z * cueRep_z + reqAction_f + rewLeft_f|subject)"
formula <- "response_cleaned ~ difMag_z * blockNr_z + reqAction_f + rewLeft_f + (difMag_z * blockNr_z + reqAction_f + rewLeft_f|subject)"

### Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)

## Baseline model:
#              Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)   0.49611    0.06832   7.262    0.000000000000382 ***
# difMag_z      0.11677    0.02747   4.252    0.000021225381711 *** XXX
# reqAction_f1  1.07854    0.09684  11.137 < 0.0000000000000002 ***
# rewLeft_f1    0.01891    0.02730   0.693                0.489
## --> GOES INTO PAPER!!!

## Variation over time:
#                    Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)         0.50169    0.06883   7.289    0.000000000000312 ***
# difMag_z            0.11667    0.02895   4.030    0.000055755980507 ***
# trialnr_z          -0.14162    0.04040  -3.506             0.000456 ***
# reqAction_f1        1.09496    0.09962  10.991 < 0.0000000000000002 ***
# rewLeft_f1          0.01914    0.02849   0.672             0.501679    
# difMag_z:trialnr_z -0.03624    0.02921  -1.241             0.214737 

#                         Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)              0.50413    0.06943   7.261    0.000000000000385 ***
# difMag_z                 0.11024    0.02944   3.744             0.000181 ***
# trialNrBlock_z          -0.17954    0.04494  -3.995    0.000064572535698 ***
# reqAction_f1             1.10232    0.09889  11.147 < 0.0000000000000002 ***
# rewLeft_f1               0.01914    0.03041   0.629             0.529148    
# difMag_z:trialNrBlock_z -0.02306    0.02864  -0.805             0.420706    

#                   Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)        0.50306    0.06920   7.270    0.000000000000359 ***
# difMag_z           0.11098    0.02962   3.747             0.000179 ***
# cueRep_z          -0.17024    0.04288  -3.970    0.000071806020782 ***
# reqAction_f1       1.09989    0.09931  11.075 < 0.0000000000000002 ***
# rewLeft_f1         0.01895    0.03028   0.626             0.531439    
# difMag_z:cueRep_z -0.01551    0.02870  -0.540             0.588977

#                    Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)         0.49968    0.06860   7.285    0.000000000000323 ***
# difMag_z            0.11772    0.02796   4.211    0.000025407127941 ***
# blockNr_z          -0.08770    0.03855  -2.275               0.0229 *  
# reqAction_f1        1.08894    0.09857  11.048 < 0.0000000000000002 ***
# rewLeft_f1          0.01937    0.02788   0.695               0.4872    
# difMag_z:blockNr_z -0.03038    0.02788  -1.090               0.2759    

plot(effect("difMag_z",mod)) # the higher rewards: the more Go
print_effect(mod,"difMag_z") # b = 0.117, se = 0.027, z = 4.252, p < .001

### p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()

## Baseline model:
#             Df   Chisq Chi Df         Pr(>Chisq)    
# difMag_z    13 15.3201      1 0.0000907481942880 ***
# reqAction_f 13 53.2598      1 0.0000000000002922 ***
# rewLeft_f   13  0.4585      1             0.4983 
# --> GOES INTO PAPER!!!

## Variation over time:
#                    Df   Chisq Chi Df         Pr(>Chisq)    
# difMag_z           26 13.6969      1          0.0002148 ***
# trialnr_z          26 10.8981      1          0.0009626 ***
# reqAction_f        26 52.4892      1 0.0000000000004326 ***
# rewLeft_f          26  1.4254      1          0.2325101    
# difMag_z:trialnr_z 26  2.5499      1          0.1103030  

#                         Df   Chisq Chi Df         Pr(>Chisq)    
# difMag_z                26 12.1008      1          0.0005040 ***
# trialNrBlock_z          26 13.5639      1          0.0002306 ***
# reqAction_f             26 53.3354      1 0.0000000000002812 ***
# rewLeft_f               26  0.3818      1          0.5366590    
# difMag_z:trialNrBlock_z 26  0.6149      1          0.4329639  

#                   Df   Chisq Chi Df         Pr(>Chisq)    
# difMag_z          26 12.1350      1          0.0004949 ***
# cueRep_z          26 13.4061      1          0.0002508 ***
# reqAction_f       26 52.9472      1 0.0000000000003426 ***
# rewLeft_f         26  0.3773      1          0.5390259    
# difMag_z:cueRep_z 26  0.2790      1          0.5973807    

#                    Df   Chisq Chi Df        Pr(>Chisq)    
# difMag_z           26 14.7096      1         0.0001254 ***
# blockNr_z          26  4.9001      1         0.0268556 *  
# reqAction_f        26 52.7753      1 0.000000000000374 ***
# rewLeft_f          26  0.4617      1         0.4968097    
# difMag_z:blockNr_z 26  1.1396      1         0.2857394   

# --------------------------------------- #  
### Automatized raincloud plot:
png(paste0(plotDir,"raincloud_bar_response_cleaned_difMag.png"),width = 480, height = 480)
plot_raincloud(modData, x = "difMag", y = "response_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = c(0.08, 0.83),
               color = "#FFC000", # Labels = c,
               xLab = "Reward minus punishment stakes", yLab = "p(Go)", Main = "Response as a function of \nstake difference")
dev.off()

# --------------------------------------- #  
### Plot based on regression:
# modData <- subset(eyeData, isCatch == 0)

## Formula:
formula <- "response_cleaned ~ difMag + (difMag|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("difMag",mod)) # raw scale suggested
plot(effect("difMag",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_response_cleaned_difMag.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "difMag", 
                     xLim = c(-4,4),
                     yLim = c(0.25, 0.90),
                     # yLim = c(0, 1),
                     color = "#FFC000", # Labels = c(1:5),
                     xLab = "stakes Difference", yLab = "p(Go)", Main = "Response as a function of \nstakes difference")
dev.off()
png(paste0(plotDir,"regression_response_cleaned_difMag_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "difMag", 
                     xLim = c(-4,4),
                     # yLim = c(0.24, 0.82),
                     yLim = c(0, 1),
                     color = "#FFC000", # Labels = c(1:5),
                     xLab = "stakes Difference", yLab = "p(Go)", Main = "Response as a function of \nstakes difference")
dev.off()

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 3B) Reward stakes only: ####

## Formula:
formula <- "response_cleaned ~ rewMag_z + reqAction_f + rewLeft_f + (rewMag_z + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#              Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)   0.49694    0.06840   7.265    0.000000000000373 ***
# rewMag_z      0.13450    0.02764   4.867    0.000001132666268 *** XXX
# reqAction_f1  1.07978    0.09706  11.124 < 0.0000000000000002 ***
# rewLeft_f1    0.01928    0.02752   0.701                0.484  
# --> GOES INTO PAPER

plot(effect("rewMag_z",mod)) # the higher rewards: the more Go
print_effect(mod,"rewMag_z") # b = 0.135, se = 0.028, z = 4.867, p < .001

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III", # all_fit = T,
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#             Df  Chisq Chi Df         Pr(>Chisq)    
# rewMag_z    13 20.791      1 0.0000051224715125 ***
# reqAction_f 13 53.213      1 0.0000000000002993 ***
# rewLeft_f   13  0.470      1              0.493  
# --> GOES INTO PAPER

# ------------------------------------------- #
## Automatized raincloud plot:
png(paste0(plotDir,"raincloud_bar_response_cleaned_rewMag.png"),width = 480, height = 480)
plot_raincloud(modData,x = "rewMag", y = "response_cleaned", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = c(0.24, 0.82),
               color = "#009933", Labels = "1",
               xLab = "Reward stakes", yLab = "p(Go)", Main = "Response as a function of \nreward stakes")
dev.off()

## Line plot:
png(paste0(plotDir,"raincloud_line_response_cleaned_rewMag.png"),width = 480, height = 480)
plot_raincloud(modData,x = "rewMag", y = "response_cleaned", 
               isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T, 
               # yLim = c(0, 1),
               yLim = c(0.24, 0.82),
               color = "#009933", Labels = "1",
               xLab = "Reward stakes", yLab = "p(Go)", Main = "Response as a function of \nreward stakes")
dev.off()

# ------------------------------------------- #
## Plot based on regression:
# modData <- subset(eyeData, isCatch == 0)

## Formula:
formula <- "response_cleaned ~ rewMag + (rewMag|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("rewMag",mod)) # raw scale suggested
plot(effect("rewMag",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_response_cleaned_rewMag.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "rewMag", 
                     xLim = c(1,5),
                     yLim = c(0.25, 0.90),
                     # yLim = c(0, 1),
                     color = "#009933", # Labels = c(1:5),
                     xLab = "Reward stakes", yLab = "p(Go)", Main = "Response as a function of \nreward stakes")
dev.off()

png(paste0(plotDir,"regression_response_cleaned_stakes_rewMag_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "rewMag", 
                     xLim = c(1,5),
                     # yLim = c(0.24, 0.82),
                     yLim = c(0, 1),
                     color = "#009933", # Labels = c(1:5),
                     xLab = "Reward stakes", yLab = "p(Go)", Main = "Response as a function of \nreward stakes")
dev.off()

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 3C) Punishment stakes only: ####

## Formula:
formula <- "response_cleaned ~ punMag_z + reqAction_f + rewLeft_f + (punMag_z + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#              Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)   0.49468    0.06821   7.253    0.000000000000409 ***
# punMag_z     -0.05132    0.02616  -1.961               0.0498 * XXX 
# reqAction_f1  1.07584    0.09663  11.133 < 0.0000000000000002 ***
# rewLeft_f1    0.02002    0.02711   0.739               0.4601    
# --> REPORT IN PAPER!!!

plot(effect("punMag_z",mod)) # the higher punishments: the less Go
print_effect(mod,"punMag_z") # b = -0.051, se = 0.026, z = -1.961, p = 0.0498

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#             Df   Chisq Chi Df         Pr(>Chisq)    
# punMag_z    13  3.3010      1            0.06924 .  
# reqAction_f 13 53.0887      1 0.0000000000003188 ***
# rewLeft_f   13  0.3657      1            0.54536  
# --> REPORT IN PAPER!!!

# ------------------------------------------- #
## Automatized raincloud plot:
png(paste0(plotDir,"raincloud_bar_response_cleaned_punMag.png"),width = 480, height = 480)
plot_raincloud(modData, x = "punMag", y = "response_cleaned", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = c(0.24, 0.82),
               color = "#CC0000", Labels = "1",
               xLab = "Punishment stakes", yLab = "p(Go)", Main = "Response as a function of \npunishment stakes")
dev.off()

## Line plot:
png(paste0(plotDir,"raincloud_line_response_cleaned_punMag.png"),width = 480, height = 480)
plot_raincloud(modData, x = "punMag", y = "response_cleaned", 
               isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T, 
               # yLim = c(0, 1),
               yLim = c(0.24, 0.82),
               color = "#CC0000", Labels = "1",
               xLab = "Punishment stakes", yLab = "p(Go)", Main = "Response as a function of \npunishment stakes")
dev.off()

# ------------------------------------------- #
## Plot based on regression:
# modData <- subset(eyeData, isCatch == 0)

## Formula:
formula <- "response_cleaned ~ punMag + (punMag|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("punMag",mod)) # raw scale suggested
plot(effect("punMag",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_response_cleaned_punMag.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "punMag", 
                     xLim = c(1,5),
                     yLim = c(0.25, 0.90),
                     # yLim = c(0, 1),
                     color = "#CC0000", 
                     xLab = "Punishment stakes", yLab = "p(Go)", Main = "Response as a function of \npunishment stakes")
dev.off()

png(paste0(plotDir,"regression_response_cleaned_punMag_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "punMag", 
                     xLim = c(1,5),
                     # yLim = c(0.24, 0.82),
                     yLim = c(0, 1),
                     color = "#CC0000", 
                     xLab = "Punishment stakes", yLab = "p(Go)", Main = "Response as a function of \npunishment stakes")
dev.off()

# ===================================================================================================================================== #
#### 4) RT ~ STAKES: ####

## Select data:
modData <- subset(eyeData, isCatch == 0)
# modData <- subset(eyeData, isCatch == 0 & !(subject %in% noLearners)) # exclude subjects who did not learnin
# modData <- subset(eyeData, isCatch == 0 & dwell_rew_abs > 0 & dwell_pun_abs > 0) # only trials with both stakes fixated

length(unique(modData$subject))
table(modData$subject)

# Re-standardize:
modData$RT_cleaned_z <- as.numeric(scale(modData$RT_cleaned))
modData$rewMag_z <- as.numeric(scale(modData$rewMag))
modData$punMag_z <- as.numeric(scale(modData$punMag))
modData$difMag_z <- as.numeric(scale(modData$difMag))

## Q-value difference:
modData$Qdif <- modData$QGo - modData$QNoGo
modData$Qdif_z <- as.numeric(scale(modData$Qdif))

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 4A) Stakes difference: ####

## Formula:
formula <- "RT_cleaned_z ~ difMag_z + reqAction_f + rewLeft_f + (punMag_z +  reqAction_f + rewLeft_f|subject)"
# formula <- "RT_cleaned_z ~ difMag_z + Qdif_z + rewLeft_f + (difMag_z + Qdif_z + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#               Estimate Std. Error        df t value   Pr(>|t|)    
# (Intercept)    0.04911    0.07015  36.82209   0.700     0.4883    
# difMag_z      -0.04105    0.01505 140.96585  -2.727     0.0072 ** XXX
# reqAction_f1  -0.08552    0.01676  52.34766  -5.104 0.00000472 ***
# rewLeft_f1    -0.01340    0.01611  33.89667  -0.831     0.4116 
# --> GOES INTO PAPER!!!

plot(effect("difMag_z",mod)) # the higher rewards: the more Go
print_effect(mod,"difMag_z")

## p-values with LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#             Df   Chisq Chi Df Pr(>Chisq)    
# difMag_z    14  7.3230      1    0.006808 ** XXX
# reqAction_f 14 22.7133      1 0.000001881 ***
# rewLeft_f   14  0.7039      1    0.401465  
# --> GOES INTO PAPER!!!

# -------------------------------------------- #
### Automatized raincloud plot:
png(paste0(plotDir,"raincloud_RT_cleaned_difMag.png"),width = 480, height = 480)
plot_raincloud(modData, x = "difMag", y = "RT_cleaned", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               # yLim = c(0.35, 0.67),
               yLim = c(0.35, 0.70),
               color = "orchid",
               xLab = "Reward minus Punishment stakes", yLab = "RT (in sec.)", Main = "Reaction times as a function of \nstake difference")
dev.off()

# -------------------------------------------- #
### Plot based on regression:
# modData <- subset(eyeData, isCatch == 0)

## Formula:
formula <- "RT_cleaned ~ difMag + (difMag|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("difMag",mod)) # raw scale suggested
plot(effect("difMag",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_RT_cleaned_difMag.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "difMag", 
                     xLim = c(-4,4),
                     yLim = c(0.25, 0.70),
                     color = "#FFC000", # Labels = c(1:5),
                     xLab = "Stakes difference", yLab = "Reaction time", Main = "Response as a function of \nstakes difference")
dev.off()

png(paste0(plotDir,"regression_RT_cleaned_difMag_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "difMag", 
                     xLim = c(-4,4),
                     yLim = c(0, 1),
                     color = "#FFC000", # Labels = c(1:5),
                     xLab = "Stakes difference", yLab = "Reaction time", Main = "Response as a function of \nstakes difference")
dev.off()

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 4B) Reward stakes only: ####

## Formula:
formula <- "RT_cleaned_z ~ rewMag_z + rewLeft_f + (rewMag_z + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
             control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#             Estimate Std. Error       df t value Pr(>|t|)  
# (Intercept)  0.01905    0.07195 33.77006   0.265   0.7927  
# rewMag_z    -0.02819    0.01382 32.49211  -2.039   0.0496 *
# rewLeft_f1  -0.01415    0.01607 32.22919  -0.880   0.3852  
### --> GOES INTO PAPER!!!

plot(effect("rewMag_z",mod)) # the higher rewards: the more Go
print_effect(mod,"rewMag_z") # b = -0.028, se = 0.014, t(32.492) = -2.039, p = 0.0496

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT)
#           Df  Chisq Chi Df Pr(>Chisq)  
# rewMag_z   9 3.9826      1    0.04597 *
# rewLeft_f  9 0.7764      1    0.37824 
### --> GOES INTO PAPER!!!

# ------------------------------------- #
### Automatized raincloud plot:
png(paste0(plotDir,"raincloud_line_RT_cleaned_rewMag.png"),width = 480, height = 480)
plot_raincloud(modData, x = "rewMag", y = "RT_cleaned",
               isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T, 
               # yLim = c(0, 1),
               yLim = c(0.35, 0.70),
               color = "#009933", Labels = "1",
               xLab = "Reward stakes", yLab = "RT (in sec.)", Main = "Reaction times as a function of \nreward stakes")
dev.off()

# ------------------------------------- #
### Plot based on regression:
# modData <- subset(eyeData, isCatch == 0)

## Formula:
formula <- "RT_cleaned ~ rewMag + (rewMag|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa"))); summary(mod)
plot(effect("rewMag",mod)) # raw scale suggested
plot(effect("rewMag",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_RT_cleaned_rewMag.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "rewMag", 
                     xLim = c(1,5),
                     yLim = c(0.25, 0.70),
                     color = "#009933", margin = c(0,1,0,0),
                     xLab = "Reward stakes", yLab = "Reaction time", Main = "Reaction time as a function of \n reward stakes")
dev.off()
png(paste0(plotDir,"regression_RT_cleaned_rewMag_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "rewMag", 
                     xLim = c(1,5),
                     yLim = c(0, 1),
                     color = "#009933", margin = c(0,1,0,0),
                     xLab = "Reward stakes", yLab = "Reaction time", Main = "Reaction time as a function of \n reward stakes")
dev.off()

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 4C) Punishment stakes only: ####

## Formula:
formula <- "RT_cleaned_z ~ punMag_z + rewLeft_f + (punMag_z + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
             control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#             Estimate Std. Error       df t value Pr(>|t|)  
# (Intercept)  0.01865    0.07204 33.76706   0.259   0.7973  
# punMag_z     0.03388    0.01681 28.01398   2.016   0.0535 .
# rewLeft_f1  -0.01341    0.01615 32.26701  -0.830   0.4127
### --> GOES INTO PAPER!!!

plot(effect("punMag_z",mod)) # the higher punishments: the less Go
print_effect(mod,"punMag_z") # b = 0.034, se = 0.017, t(28.014) = 2.016, p = 0.0535

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT)

## RT_cleaned:
#           Df  Chisq Chi Df Pr(>Chisq)  
# punMag_z   9 4.0124      1    0.04517 *
# rewLeft_f  9 0.7041      1    0.40143  
### --> GOES INTO PAPER!!!

# ------------------------------------- #
### Automatized raincloud plot:
png(paste0(plotDir,"raincloud_line_RT_cleaned_punMag.png"),width = 480, height = 480)
plot_raincloud(modData, x = "punMag", y = "RT_cleaned",
               isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T, 
               # yLim = c(0, 1),
               # yLim = c(0.35, 0.66),
               yLim = c(0.35, 0.70),
               color = "#CC0000", Labels = "1",
               xLab = "Punishment stakes", yLab = "RT (in sec.)", Main = "Reaction times as a function of \npunishment stakes")
dev.off()

# ------------------------------------- #
## Plot based on regression:
# modData <- subset(eyeData, isCatch == 0)

## Formula:
formula <- "RT_cleaned ~ punMag + (punMag|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa"))); summary(mod)
plot(effect("punMag",mod)) # raw scale suggested
plot(effect("punMag",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_RT_cleaned_punMag.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "punMag", 
                     xLim = c(1,5),
                     yLim = c(0.25, 0.70),
                     color = "#CC0000", margin = c(0,1,0,0),
                     xLab = "Punishment stakes", yLab = "Reaction time", Main = "Reaction time as a function of \n punishment stakes")
dev.off()
png(paste0(plotDir,"regression_RT_cleaned_punMag_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "punMag", 
                     xLim = c(1,5),
                     yLim = c(0, 1),
                     color = "#CC0000", margin = c(0,1,0,0),
                     xLab = "Punishment stakes", yLab = "Reaction time", Main = "Reaction time as a function of \n punishment stakes")
dev.off()

# ===================================================================================================================================== #
#### 5A): FIRST FIXATION OUTCOME  ~ REQUIRED ACTION: ####

## Select data:
modData <- eyeData 
# modData <- subset(eyeData, !(subject %in% noLearners)) # exclude subjects who did not learn
# modData <- subset(eyeData, dwell_rew_abs > 0 & dwell_pun_abs > 0) # only trials with both stakes fixated
length(unique(modData$subject))
table(modData$subject)

## Formula:
formula <- "firstfix_out_f ~ reqAction_f + rewLeft_f + (reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#              Estimate Std. Error z value       Pr(>|z|)    
# (Intercept)   0.67471    0.10862   6.212 0.000000000524 ***
# reqAction_f1  0.11307    0.03512   3.220        0.00128 ** XXX
# rewLeft_f1    0.44353    0.45446   0.976        0.32909  
## --> GOES INTO PAPER!!!

plot(effect("reqAction_f",mod)) # right direction
print_effect(mod,"reqAction_f", ) # b = 0.113, se = 0.035, z = 3.220, p = 0.0013

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#             Df   Chisq Chi Df Pr(>Chisq)    
# reqAction_f  8 13.9152      1  0.0001912 *** XXX
# rewLeft_f    8  0.9375      1  0.3329208    
## --> GOES INTO PAPER!!!

# ------------------------------------------ #
### Automatized raincloud plot:

## Bar plot:
png(paste0(plotDir,"raincloud_bar_firstfixout_reqAction.png"),width = 480, height = 480)
plot_raincloud(modData, x = "reqAction_f", y = "firstfix_out_n", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F,
               # yLim = c(0, 1),
               yLim = c(0.39, 0.87),
               color = c("blue","red"), Labels = c("Go","NoGo"),
               xLab = "Required Action", yLab = "p(first fixation on reward)", Main = "First fixation as a function of \nrequired action")
dev.off()

## Violin plot:
png(paste0(plotDir,"raincloud_violin_firstfixout_reqAction.png"),width = 480, height = 480)
plot_raincloud(modData, x = "reqAction_f", y = "firstfix_out_n", 
               isBar = F, isPoint = T, isViolin = T, isBoxplot = T, isMean = F,
               # yLim = c(0, 1),
               yLim = c(0.0, 1),
               color = c("blue","red"), Labels = c("Go","NoGo"),
               xLab = "Required Action", yLab = "p(first fixation on reward)", Main = "First fixation as a function of \nrequired action")
dev.off()

## Beeswarm plot:
png(paste0(plotDir,"beeswarm_firstfixout_reqAction.png"),width = 480, height = 480)
plot_beeswarm(modData, x = "reqAction_f", y = "firstfix_out_n",
               yLim = c(0, 1),
               color = c("blue","red"), Labels = c("Go","NoGo"),
               xLab = "Required Action", yLab = "p(first fixation on reward)", Main = "First fixation as a function of \nrequired action")
dev.off()

# ------------------------------------------ #
### Regression bar plot:
# modData <- eyeData

## Formula:
formula <- "firstfix_out_f ~ reqAction_f + (reqAction_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
plot(effect("reqAction_f",mod)) # raw scale suggested 
plot(effect("reqAction_f",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

png(paste0(plotDir,"regression_firstfixout_reqAction.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "reqAction_f", # 
                    # yLim = c(0,1), 
                    yLim = c(0.25,0.9), 
                    color = c("blue","red"), Labels = c("Go","NoGo"), z = 1.96,
                    xLab = "Required Action", yLab = "p(first fix. reward)", Main = "First fixation as a function of \nrequired action")
dev.off()

png(paste0(plotDir,"regression_firstfixout_reqAction_full.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "reqAction_f", # 
                    yLim = c(0,1),
                    color = c("blue","red"), Labels = c("Go","NoGo"), z = 1.96,
                    xLab = "Required Action", yLab = "p(first fix. reward)", Main = "First fixation as a function of \nrequired action")
dev.off()

# ===================================================================================================================================== #
#### 5B): FIRST FIXATION ~ Q-VALUES: ####

## Select data:
modData <- eyeData 
# modData <- subset(eyeData, !(subject %in% noLearners)) # exclude subjects who did not learn
# modData <- subset(eyeData, dwell_rew_abs > 0 & dwell_pun_abs > 0) # only trials with both stakes fixated

length(unique(modData$subject))
table(modData$subject)

## Re-standardize:
modData$Qdif_z <- as.numeric(scale(modData$Qdif_z))
mean(modData$Qdif_z)
sd(modData$Qdif_z)

## Formula:
formula <- "firstfix_out_n ~ Qdif_z + rewLeft_f + (Qdif_z + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#             Estimate Std. Error z value       Pr(>|z|)    
# (Intercept)  0.67215    0.10800   6.224 0.000000000486 ***
# Qdif_z       0.09051    0.03255   2.781        0.00543 ** XXX
# rewLeft_f1   0.43624    0.45300   0.963        0.33554 
## ---> GOES INTO PAPER!!!

plot(effect("Qdif_z",mod)) 
print_effect(mod,"Qdif_z") # b = 0.093, se = 0.033, z = 2.845, p = 0.0044

## p-values With LRTs:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#           Df  Chisq Chi Df Pr(>Chisq)   
# Qdif_z     8 8.3980      1   0.003756 ** XXX
# rewLeft_f  8 0.9133      1   0.339244
## --> GOES INTO PAPER!!!

# ------------------------------------ #
### Automatized raincloud plot:
# Prepare data:
nPerc <- 3; yLim <- c(0.30, 1);
nPerc <- 4; yLim <- c(0, 1);
nPerc <- 5; yLim <- c(0, 1);

selVar <- "Qdif"; percVar <- paste0(selVar,"_",nPerc,"Perc");
modData <- create_percentiles(modData,nPerc=nPerc,selVar,perSub=F)
tapply(modData$firstfix_out_n,modData[,percVar],mean,na.rm=T)

## Bar plot:
png(paste0(plotDir,"raincloud_bar_firstfixout_",percVar,".png"),width = 480, height = 480)
plot_raincloud(modData, x = percVar, y = "firstfix_out_n", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F,
               yLim = yLim,
               color = "orchid", Labels = "1",
               xLab = " Percentiles(Q(Go) - Q(NoGo))", yLab = "p(first fixation on reward)", Main = "First fixation as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()
png(paste0(plotDir,"raincloud_bar_firstfixout_",percVar,"_full.png"),width = 480, height = 480)
plot_raincloud(modData, x = percVar, y = "firstfix_out_n", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F,
               yLim = c(0, 1),
               color = "orchid", Labels = "1",
               xLab = " Percentiles(Q(Go) - Q(NoGo))", yLab = "p(first fixation on reward)", Main = "First fixation as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()

## Line pLot:
png(paste0(plotDir,"raincloud_line_firstfixout_",percVar,".png"),width = 480, height = 480)
plot_raincloud(modData, x = percVar, y = "firstfix_out_n", 
               isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T, 
               # yLim = c(0,1),
               yLim = yLim,
               color = "orchid", Labels = "1",
               xLab = " Percentiles(Q(Go) - Q(NoGo))", yLab = "p(first fixation on reward)", Main = "First fixation as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()
png(paste0(plotDir,"raincloud_line_firstfixout_",percVar,"_full.png"),width = 480, height = 480)
plot_raincloud(modData, x = percVar, y = "firstfix_out_n", 
               isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T, 
               yLim = c(0,1),
               color = "orchid", Labels = "1",
               xLab = " Percentiles(Q(Go) - Q(NoGo))", yLab = "p(first fixation on reward)", Main = "First fixation as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()

# ------------------------------------ #
### Plot based on regression:
# modData <- eyeData

## Formula:
formula <- "firstfix_out_n ~ Qdif_z + rewLeft_f + (Qdif_z + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("Qdif_z",mod)) # raw scale suggested
plot(effect("Qdif_z",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_firstfixout_nQdifz.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif_z", 
                     xLim = c(-2,2),
                     yLim = c(0.25,0.95), 
                     color = "orchid", 
                     xLab = "Q(Go) - Q(NoGo) (z)", yLab = "p(first fix. reward)", Main = "First fixation as a function of \n Q(Go) - Q(NoGo)")
dev.off()

png(paste0(plotDir,"regression_firstfixout_nQdifz_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif_z", 
                     # xLim = c(-2,2),
                     yLim = c(0, 1), 
                     # yLim = c(0,1), 
                     color = "orchid", 
                     xLab = "Q(Go) - Q(NoGo) (z)", yLab = "p(first fix. reward)", Main = "First fixation as a function of \n Q(Go) - Q(NoGo)")
dev.off()

# ------------------------------------ #
### Un-standardized Q-value difference:

## Formula:
formula <- "firstfix_out_n ~ Qdif + rewLeft_f + (Qdif + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("Qdif",mod)) # raw scale suggested

## Plot regression line:
png(paste0(plotDir,"regression_firstfixout_nQdif.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif", 
                     xLim = c(-2,2),
                     yLim = c(0.3,0.95), 
                     color = "orchid", 
                     xLab = "Q(Go) - Q(NoGo)", yLab = "p(first fixation on reward)", Main = "First fixation as a function of \n Q(Go) - Q(NoGo)")
dev.off()

png(paste0(plotDir,"regression_firstfixout_nQdif_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif", 
                     xLim = c(-2,2),
                     yLim = c(0,1), 
                     color = "orchid", 
                     xLab = "Q(Go) - Q(NoGo)", yLab = "p(first fixation on reward)", Main = "First fixation as a function of \n Q(Go) - Q(NoGo)")
dev.off()

# ===================================================================================================================================== #
#### 6A): RESPONSE ~ FIRST FIXATION OUTCOME: #####

## Select data:
modData <- subset(eyeData, isCatch == 0)
# modData <- subset(eyeData, isCatch == 0 & !(subject %in% noLearners)) # exclude subjects who did not learn
# modData <- subset(eyeData, isCatch == 0 & dwell_rew_abs > 0 & dwell_pun_abs > 0) # only trials with both stakes fixated

length(unique(modData$subject))
table(modData$subject)

## Formula:
formula <- "response_cleaned ~ firstfix_out_f + rewLeft_f + (firstfix_out_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#                 Estimate Std. Error z value       Pr(>|z|)    
# (Intercept)      0.34993    0.05558   6.296 0.000000000305 ***
# firstfix_out_f1 -0.05309    0.02484  -2.137         0.0326 *  XXX
# rewLeft_f1       0.01335    0.02444   0.546         0.5849  

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                Df  Chisq Chi Df Pr(>Chisq)  
# firstfix_out_f  8 4.4947      1     0.0340 * XXX
# rewLeft_f       8 0.2968      1     0.5859

# ------------------------------------------ #
### Automatized raincloud plot:
plotdata <- subset(eyeData, isCatch == 0 & firstfix_out_f %in% c("reward","punishment"))
png(paste0(plotDir,"raincloud_bar_response_cleaned_firstfixout.png"),width = 480, height = 480)
plot_raincloud(plotdata, x = "firstfix_out_f", y = "response_cleaned", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
               # yLim = c(0.24, 0.92),
               color = c("#CC0000","#009933"), Labels = c("Punishment","Reward"),
               xLab = "First fixation", yLab = "p(Go)", Main = "Proportion Go response as a function of \nfirst fixation")
dev.off()

# ===================================================================================================================================== #
#### 6B) RT ~ FIRST FIXATION OUTCOME: ####

## Select data:
modData <- subset(eyeData, isCatch == 0)
# modData <- subset(eyeData, isCatch == 0 & !(subject %in% noLearners)) # exclude subjects who did not learn
# modData <- subset(eyeData, isCatch == 0 & dwell_rew_abs > 0 & dwell_pun_abs > 0) # only trials with both stakes fixated

length(unique(modData$subject))
table(modData$subject)

## Re-standardize:
modData$RT_cleaned_z <- as.numeric(scale(modData$RT_cleaned))

## Formula:
formula <- "RT_cleaned_z ~ firstfix_out_f + rewLeft_f + (firstfix_out_f + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#                  Estimate Std. Error        df t value  Pr(>|t|)    
# (Intercept)      0.038302   0.072299 33.864208   0.530     0.600    
# firstfix_out_f1 -0.009693   0.016511 53.774205  -0.587     0.560 XXX     
# reqAction_f1    -0.084436   0.017416 31.117773  -4.848 0.0000329 ***
# rewLeft_f1      -0.015851   0.016751 28.262689  -0.946     0.352  

plot(effect("firstfix_out_f",mod))
print_effect(mod,"firstfix_out_f") # b = -0.010, se = 0.017, t(53.774) = -0.587, p = 0.5596

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                Df   Chisq Chi Df Pr(>Chisq)    
# firstfix_out_f 14  0.2566      1     0.6124 XXX   
# reqAction_f    14 18.8274      1 0.00001431 ***
# rewLeft_f      14  1.5977      1     0.2062

# ------------------------------------------ #
### Automatized raincloud plot:
plotdata <- subset(eyeData, isCatch == 0 & firstfix_out_f %in% c("reward","punishment"))
png(paste0(plotDir,"raincloud_bar_RT_cleaned_firstfixout.png"),width = 480, height = 480)
plot_raincloud(plotdata, x = "firstfix_out_f", y = "RT_cleaned", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = c(0.35, 0.70),
               color = c("#CC0000","#009933"), Labels = c("Punishment","Reward"),
               xLab = "First fixation", yLab = "RT (in sec.)", Main = "Reaction times as a function of \nfirst fixation")
dev.off()

# ===================================================================================================================================== #
#### 7A) DWELL TIME ~ FIRST FIXATION: ####

## Select data:
modData <- eyeData 
# modData <- subset(eyeData, !(subject %in% noLearners)) # exclude subjects who did not learn
# modData <- subset(eyeData, dwell_rew_abs > 0 & dwell_pun_abs > 0) # only trials with both stakes fixated

length(unique(modData$subject))
table(modData$subject)

## Compute difference and re-standardize:
modData$dwell_out_dif <- modData$dwell_rew_abs - modData$dwell_pun_abs
modData$dwell_out_dif_z <- as.numeric(scale(modData$dwell_out_dif))
modData$dwell_rew_rel_z <- as.numeric(scale(modData$dwell_rew_rel))

## Formula:
formula <- "dwell_out_dif_z ~ firstfix_out_f + rewLeft_f + (firstfix_out_f + rewLeft_f|subject)"
formula <- "dwell_rew_rel_z ~ firstfix_out_f + rewLeft_f + (firstfix_out_f + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
## Difference:
#                 Estimate Std. Error       df t value   Pr(>|t|)    
# (Intercept)     -0.04587    0.03742 34.05094  -1.226    0.22874    
# firstfix_out_f1 -0.18387    0.05877 34.20902  -3.129    0.00358 ** XXX
# rewLeft_f1      -0.21182    0.03762 33.93590  -5.631 0.00000262 ***
## --> GOES INTO PAPER!!!

## Ratio:
#                 Estimate Std. Error       df t value   Pr(>|t|)    
# (Intercept)     -0.06960    0.03361 33.91885  -2.071   0.046028 *  
# firstfix_out_f1 -0.29100    0.07722 34.66925  -3.769   0.000612 *** XXX
# rewLeft_f1      -0.20148    0.03514 32.87962  -5.733 0.00000214 ***

plot(effect("firstfix_out_f",mod)) # 
print_effect(mod,"firstfix_out_f") # 

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT)
## Difference:
#                Df   Chisq Chi Df Pr(>Chisq)    
# firstfix_out_f  9  8.8116      1   0.002993 ** XXX
# rewLeft_f       9 22.9302      1 0.00000168 ***
## --> GOES INTO PAPER!!!
## Ratio:
#                Df  Chisq Chi Df  Pr(>Chisq)    
# firstfix_out_f  9 12.190      1   0.0004804 *** XXX
# rewLeft_f       9 23.404      1 0.000001313 ***

# ===================================================================================================================================== #
#### 7B) DWELL TIME ~ STAKES: ####

## Select data:
modData <- eyeData 
# modData <- subset(eyeData, !(subject %in% noLearners)) # exclude subjects who did not learn
# modData <- subset(eyeData, dwell_rew_abs > 0 & dwell_pun_abs > 0) # only trials with both stakes fixated

length(unique(modData$subject))
table(modData$subject)

## Compute difference and re-standardize:
modData$difMag <- modData$rewMag - modData$punMag
modData$rewMag_z <- as.numeric(scale(modData$rewMag))
modData$punMag_z <- as.numeric(scale(modData$punMag))
modData$difMag_z <- as.numeric(scale(modData$difMag))

modData$dwell_out_dif <- modData$dwell_rew_abs - modData$dwell_pun_abs
modData$dwell_out_dif_z <- as.numeric(scale(modData$dwell_out_dif))

## Formula:
formula <- "dwell_out_dif_z ~ difMag_z + rewLeft_f + (difMag_z + rewLeft_f|subject)"
formula <- "dwell_rew_rel_z ~ difMag_z + rewLeft_f + (difMag_z + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
## Difference:
#               Estimate Std. Error         df t value  Pr(>|t|)    
# (Intercept)  0.0001827  0.0462835 34.0508354   0.004  0.996873    
# difMag_z     0.0880127  0.0194302 34.4654211   4.530 0.0000677 *** XXX
# rewLeft_f1  -0.2291895  0.0533785 34.0001484  -4.294  0.000138 ***
## --> GOES INTO PAPER!!!
## Ratio:
#              Estimate Std. Error        df t value  Pr(>|t|)    
# (Intercept)  0.002281   0.045802 34.012666   0.050  0.960571    
# difMag_z     0.074675   0.015504 34.546735   4.816 0.0000287 ***
# rewLeft_f1  -0.213906   0.050027 33.832611  -4.276  0.000147 ***
print_effect(mod,"difMag_z") # b = -0.082, se = 0.017, t(34.234) = -4.842, p < .001

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT)
## Difference:
#           Df  Chisq Chi Df Pr(>Chisq)    
# difMag_z   9 16.488      1 0.00004897 *** XXX
# rewLeft_f  9 15.163      1 0.00009862 ***
## --> GOES INTO PAPER!!!
## Ratio:
#           Df  Chisq Chi Df Pr(>Chisq)    
# difMag_z   9 18.222      1 0.00001966 ***
# rewLeft_f  9 15.064      1  0.0001039 ***

# ===================================================================================================================================== #
#### 8A) DWELL TIME ~ REQUIRED ACTION: ####

## Select data:
modData <- eyeData 
# modData <- subset(eyeData, !(subject %in% noLearners)) # exclude subjects who did not learn
# modData <- subset(eyeData, dwell_rew_abs > 0 & dwell_pun_abs > 0) # only trials with both stakes fixated

length(unique(modData$subject))
table(modData$subject)

## Re-standardize:
modData$dwell_out_dif <- modData$dwell_rew_abs - modData$dwell_pun_abs
modData$dwell_out_dif_z <- as.numeric(scale(modData$dwell_out_dif))
modData$dwell_rew_rel_z <- as.numeric(scale(modData$dwell_rew_rel))

## Formula:
formula <- "dwell_out_dif_z ~ reqAction_f + rewLeft_f + (reqAction_f + rewLeft_f|subject)"
formula <- "dwell_out_dif_z ~ reqAction_f + rewLeft_f + firstfix_out_f + (reqAction_f + rewLeft_f + firstfix_out_f|subject)"
formula <- "dwell_out_dif_z ~ reqAction_f + rewLeft_f + firstfix_out_f + difMag_z + (reqAction_f + rewLeft_f + firstfix_out_f + difMag_z|subject)"

formula <- "dwell_rew_rel_z ~ reqAction_f + rewLeft_f + (reqAction_f + rewLeft_f|subject)"
formula <- "dwell_rew_rel_z ~ reqAction_f + rewLeft_f + firstfix_out_f + (reqAction_f + rewLeft_f + firstfix_out_f|subject)"
formula <- "dwell_rew_rel_z ~ reqAction_f + rewLeft_f + firstfix_out_f + difMag_z + (reqAction_f + rewLeft_f + firstfix_out_f + difMag_z|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
## DIFFERENCE:
#                Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   0.0000956  0.0462663 34.0511686   0.002 0.998363    
# reqAction_f1  0.0361604  0.0117717 33.0098510   3.072 0.004242 **  XXX
# rewLeft_f1   -0.2285842  0.0535633 33.9975997  -4.268 0.000149 ***
#                 Estimate Std. Error       df t value   Pr(>|t|)    
# (Intercept)     -0.04538    0.03746 34.04834  -1.211    0.23413    
# reqAction_f1     0.03081    0.01022 34.76644   3.015    0.00477 ** XXX
# rewLeft_f1      -0.21210    0.03760 33.95745  -5.641 0.00000253 ***
# firstfix_out_f1 -0.18276    0.05864 34.22370  -3.117    0.00369 ** 
#                 Estimate Std. Error       df t value   Pr(>|t|)    
# (Intercept)     -0.04566    0.03740 34.04524  -1.221   0.230504    
# reqAction_f1     0.03028    0.01023 34.79783   2.959   0.005514 ** XXX
# rewLeft_f1      -0.21254    0.03730 34.47160  -5.698 0.00000203 ***
# firstfix_out_f1 -0.18031    0.05874 34.02577  -3.069   0.004194 ** 
# difMag_z         0.08178    0.01953 34.30301   4.187   0.000186 ***
## RATIO:
#               Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)   0.002324   0.045796 34.011538   0.051 0.959827    
# reqAction_f1  0.037121   0.012610 29.407577   2.944 0.006275 ** XXX
# rewLeft_f1   -0.213077   0.050092 33.833186  -4.254 0.000157 ***
#                  Estimate Std. Error        df t value   Pr(>|t|)    
# (Intercept)     -0.069130   0.033662 33.917024  -2.054   0.047786 *  
# reqAction_f1     0.026312   0.009508 33.080287   2.767   0.009185 ** XXX
# rewLeft_f1      -0.201289   0.035081 33.055324  -5.738 0.00000208 ***
# firstfix_out_f1 -0.289669   0.077202 34.655713  -3.752   0.000642 ***
#                  Estimate Std. Error        df t value   Pr(>|t|)    
# (Intercept)     -0.069324   0.033652 33.917980  -2.060   0.047135 *  
# reqAction_f1     0.025940   0.009483 33.065081   2.736   0.009937 **  XXX
# rewLeft_f1      -0.201825   0.034840 33.168140  -5.793 0.00000174 ***
# firstfix_out_f1 -0.287787   0.077233 34.632227  -3.726   0.000691 ***
# difMag_z         0.067831   0.015379 34.433276   4.411 0.00009633 ***

print_effect(mod,"reqAction_f") # b = 0.026, se = 0.009, t(33.065) = 2.736, p = 0.0099
plot(effect("reqAction_f",mod)) # required Go: more time spend looking at reward, right direction...

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
## DIFFERENCE:
#             Df   Chisq Chi Df Pr(>Chisq)    
# reqAction_f  9  8.5816      1  0.0033957 ** XXX
# rewLeft_f    9 15.0135      1  0.0001067 ***
#                Df   Chisq Chi Df  Pr(>Chisq)    
# reqAction_f    14  8.2652      1   0.0040412 ** XXX
# rewLeft_f      14 23.0298      1 0.000001595 ***
# firstfix_out_f 14 11.0798      1   0.0008727 ***
#                Df   Chisq Chi Df  Pr(>Chisq)    
# reqAction_f    20  4.7106      1   0.0299771 *  XXX
# rewLeft_f      20 23.5106      1 0.000001242 ***
# firstfix_out_f 20  5.2348      1   0.0221389 *  
# difMag_z       20 11.2142      1   0.0008117 ***
## RATIO:
#             Df   Chisq Chi Df Pr(>Chisq)    
# reqAction_f  9  7.9262      1  0.0048724 ** XXX  
# rewLeft_f    9 14.9380      1  0.0001111 ***
#                Df   Chisq Chi Df  Pr(>Chisq)    
# reqAction_f    14  7.0372      1   0.0079836 ** XXX
# rewLeft_f      14 23.4691      1 0.000001269 ***
# firstfix_out_f 14 12.1036      1   0.0005032 ***
#                Df   Chisq Chi Df   Pr(>Chisq)    
# reqAction_f    20  6.8958      1    0.0086400 **  XXX
# rewLeft_f      20 25.9589      1 0.0000003488 ***
# firstfix_out_f 20 14.0989      1    0.0001734 ***
# difMag_z       20 15.7942      1 0.0000706201 ***

# ------------------------------------ #
### Automatized raincloud plot:

# ------------- #
## Difference:
png(paste0(plotDir,"raincloud_bar_dwelloutdif_reqAction.png"),width = 480, height = 480)
plot_raincloud(modData, xVar = "reqAction_f", yVar = "dwell_out_dif", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = c(0.48, 0.76),
               color = c("blue","red"), Labels = c("Go","NoGo"),
               xLab = "Required action", yLab = "Dwell time difference", Main = "Dwell time difference as a function of \nrequired action")
dev.off()

# ------------- #
## Ratio:
png(paste0(plotDir,"raincloud_bar_dwellrewrel_reqAction.png"),width = 480, height = 480)
plot_raincloud(modData, xVar = "reqAction_f", yVar = "dwell_rew_rel", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = c(0.48, 0.76),
               color = c("blue","red"), Labels = c("Go","NoGo"),
               xLab = "Required action", yLab = "Relative dwell time on rewards", Main = "Relative dwell time on rewards as a function of \nrequired action")
dev.off()

# ------------------------------------ #
### Plot based on regression:

# ------------- #
## Difference:

## Formula:
formula <- "dwell_out_dif ~ reqAction_f + (reqAction_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData, 
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
plot(effect("reqAction_f",mod)) # raw scale suggestedplot(effect("reqAction_f",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_dwelloutdif_reqAction.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "reqAction_f", # 
                      yLim = c(40,85), 
                      color = c("blue","red"), Labels = c("Go","NoGo"), z = 1.96,
                      xLab = "Required Action", yLab = "Dwell time difference")
dev.off()

png(paste0(plotDir,"regression_dwelloutdif_reqAction_full.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "reqAction_f", # 
                      yLim = c(0,100),
                      color = c("blue","red"), Labels = c("Go","NoGo"), z = 1.96,
                      xLab = "Required Action", yLab = "Dwell time difference")
dev.off()

# ------------- #
## Ratio:

modData$dwell_rew_rel100 <- modData$dwell_rew_rel * 100

## Formula:
formula <- "dwell_rew_rel100 ~ reqAction_f + (reqAction_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData, 
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
plot(effect("reqAction_f",mod)) # raw scale suggestedplot(effect("reqAction_f",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_dwellrewrel_reqAction.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "reqAction_f", # 
                    yLim = c(40,85), 
                    color = c("blue","red"), Labels = c("Go","NoGo"), z = 1.96,
                    xLab = "Required Action", yLab = "Relative reward dwell time (%)")
dev.off()

png(paste0(plotDir,"regression_dwellrewrel_reqAction_full.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "reqAction_f", # 
                    yLim = c(0,100),
                    color = c("blue","red"), Labels = c("Go","NoGo"), z = 1.96,
                    xLab = "Required Action", yLab = "Relative reward dwell time (%)")
dev.off()

# ===================================================================================================================================== #
#### 8B) DWELL TIME ~ Q-values: ####

## Select data:
modData <- eyeData 
# modData <- subset(eyeData, !(subject %in% noLearners)) # exclude subjects who did not learn
# modData <- subset(eyeData, dwell_rew_abs > 0 & dwell_pun_abs > 0) # only trials with both stakes fixated

length(unique(modData$subject))
table(modData$subject)

## Re-standardize:
modData$Qdif_z <- as.numeric(scale(modData$Qdif))

## Re-standardize:
modData$dwell_out_dif_z <- as.numeric(scale(modData$dwell_out_dif))
modData$dwell_rew_rel_z <- as.numeric(scale(modData$dwell_rew_rel))

## Formula:
formula <- "dwell_out_dif_z ~ Qdif_z + rewLeft_f + firstfix_out_f + difMag_z + (Qdif_z + rewLeft_f + firstfix_out_f + difMag_z|subject)"
formula <- "dwell_rew_rel_z ~ Qdif_z + rewLeft_f + firstfix_out_f + difMag_z + (Qdif_z + rewLeft_f + firstfix_out_f + difMag_z|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
## Difference:
#                 Estimate Std. Error       df t value   Pr(>|t|)    
# (Intercept)     -0.04568    0.03742 34.04297  -1.221   0.230639    
# Qdif_z           0.02604    0.01046 26.16263   2.489   0.019480 *  XXX
# rewLeft_f1      -0.21141    0.03727 34.54941  -5.672 0.00000218 ***
# firstfix_out_f1 -0.17966    0.05895 33.97084  -3.048   0.004443 ** 
# difMag_z         0.08161    0.01955 34.31309   4.174   0.000194 ***

## Ratio:
#                 Estimate Std. Error       df t value   Pr(>|t|)    
# (Intercept)     -0.06950    0.03363 33.90948  -2.067   0.046485 *  
# Qdif_z           0.01577    0.01082 14.99404   1.457   0.165700 XXX   
# rewLeft_f1      -0.20092    0.03481 33.09124  -5.771 0.00000187 ***
# firstfix_out_f1 -0.28782    0.07739 34.61524  -3.719   0.000705 ***
# difMag_z         0.06779    0.01540 34.43732   4.401 0.00009899 ***
plot(effect("Qdif_z",mod)) # if Go: look longer at rewards
print_effect(mod,"Qdif_z") # b = 0.016, se = 0.011, t(15.466) = 1.445, p = 0.1684

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III", all_fit=TRUE,
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()

## Difference:
#                Df   Chisq Chi Df Pr(>Chisq)    
# Qdif_z         20  4.3610      1  0.0367699 *  XXX
# rewLeft_f      20 23.3650      1 0.00000134 ***
# firstfix_out_f 20  8.4076      1  0.0037365 ** 
# difMag_z       20 14.4259      1  0.0001458 ***
## Ratio:
#                Df   Chisq Chi Df  Pr(>Chisq)    
# Qdif_z         20  0.9606      1    0.327041    XXX
# rewLeft_f      20 23.6506      1 0.000001155 ***
# firstfix_out_f 20 11.9112      1    0.000558 ***
# difMag_z       20 15.7528      1 0.000072181 ***

# ------------------------------------ #
### Plot based on regression:

# ----------------- #
## Difference:

## Formula:
formula <- "dwell_rew_rel100 ~ Qdif + rewLeft_f + (Qdif + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("Qdif",mod)) # raw scale suggested

# Plot regression line:
png(paste0(plotDir,"regression_dwelloutdif_Qdif.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif", 
                       xLim = c(-2,2),
                       yLim = c(40,85), 
                       color = "orchid", 
                       xLab = "Q(Go) - Q(NoGo)", yLab = "Dwell time difference")
dev.off()

png(paste0(plotDir,"regression_dwelloutdif_Qdif_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif", 
                       xLim = c(-2,2),
                       yLim = c(0,100), 
                       color = "orchid", 
                       xLab = "Q(Go) - Q(NoGo)", yLab = "Dwell time difference")
dev.off()

# ----------------- #
## Ratio:

## Formula:
formula <- "dwell_rew_rel100 ~ Qdif + rewLeft_f + (Qdif + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("Qdif",mod)) # raw scale suggested

# Plot regression line:
png(paste0(plotDir,"regression_dwellrewrel_Qdif.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif", 
                     xLim = c(-2,2),
                     yLim = c(40,85), 
                     color = "orchid", 
                     xLab = "Q(Go) - Q(NoGo)", yLab = "Relative reward dwell time (%)")
dev.off()

png(paste0(plotDir,"regression_dwellrewrel_Qdif_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif", 
                     xLim = c(-2,2),
                     yLim = c(0,100), 
                     color = "orchid", 
                     xLab = "Q(Go) - Q(NoGo)", yLab = "Relative reward dwell time (%)")
dev.off()

# ===================================================================================================================================== #
#### 9) RESPONSE ~ DWELL TIME OUTCOME: ####

## Select data:
modData <- subset(eyeData, isCatch == 0)
# modData <- subset(eyeData, isCatch == 0 & !(subject %in% noLearners)) # exclude subjects who did not learn
# modData <- subset(eyeData, isCatch == 0 & dwell_rew_abs > 0 & dwell_pun_abs > 0) # only trials with both stakes fixated

length(unique(modData$subject))
table(modData$subject)

## Compute difference and  re-standardize:
modData$dwell_out_dif <- modData$dwell_rew_abs - modData$dwell_pun_abs
modData$dwell_out_dif_z <- as.numeric(scale(modData$dwell_out_dif))
modData$dwell_rew_rel_z <- as.numeric(scale(modData$dwell_rew_rel))
modData$dwell_rew_abs_z <- as.numeric(scale(modData$dwell_rew_abs))
modData$dwell_pun_abs_z <- as.numeric(scale(modData$dwell_pun_abs))
modData$dwell_rew_rel_z <- as.numeric(scale(modData$dwell_rew_rel))

## Q-value difference:
modData$Qdif <- modData$QGo - modData$QNoGo
modData$Qdif_z <- as.numeric(scale(modData$Qdif))

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 9A) Difference reward minus punishment dwell times: ####

## Formula:
formula <- "response_cleaned ~ dwell_out_dif_z + reqAction_f + rewLeft_f + (dwell_out_dif_z + reqAction_f + rewLeft_f|subject)"
# formula <- "response_cleaned ~ dwell_out_dif_z + Qdif_z + rewLeft_f + (dwell_out_dif_z + Qdif_z + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#                 Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)      0.51681    0.06836   7.561   0.0000000000000401 ***
# dwell_out_dif_z  0.13225    0.03410   3.879             0.000105 *** XXX
# reqAction_f1     1.08140    0.09766  11.073 < 0.0000000000000002 ***
# rewLeft_f1       0.04746    0.03079   1.541             0.123220  

plot(effect("dwell_out_dif_z",mod)) # more time, more Go
print_effect(mod,"dwell_out_dif_z") # b = 0.132, se = 0.034, z = 3.879, p < .001

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                 Df   Chisq Chi Df         Pr(>Chisq)    
# dwell_out_dif_z 13 12.2029      1          0.0004772 *** XXX
# reqAction_f     13 52.8423      1 0.0000000000003614 ***
# rewLeft_f       13  2.2364      1          0.1347916   

# -------------------------------------------------- # 
### Plot based on regression:

## Formula:
formula <- "response_cleaned ~ dwell_out_dif + (dwell_out_dif|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("dwell_out_dif",mod)) # raw scale suggested
plot(effect("dwell_out_dif",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_response_cleaned_dwelloutdif.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_out_dif", # not it's understandardized
                       xLim = c(-1500,1500),
                       c(0.07, 0.90),
                       # yLim = c(0, 1),
                       color = "#7B30A0", margin = c(0,1,0,0),
                       xLab = "Dwell time difference", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n absolute dwell time on rewards (percentiles)")
dev.off()
png(paste0(plotDir,"regression_response_cleaned_dwelloutdif_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_out_dif", 
                       xLim = c(-1500,1500),
                       yLim = c(0, 1),
                       color = "#7B30A0", margin = c(0,1,0,0),
                       xLab = "Dwell Time Difference", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n absolute dwell time on rewards (percentiles)")
dev.off()

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 9B) Relative: Reward / (reward + punishment) ####

## Formula:
formula <- "response_cleaned ~ dwell_rew_rel_z + reqAction_f + rewLeft_f + (dwell_rew_rel_z + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#                 Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)      0.51680    0.06876   7.517   0.0000000000000562 ***
# dwell_rew_rel_z  0.14041    0.03184   4.410   0.0000103502722013 *** XXX
# reqAction_f1     1.08090    0.09770  11.064 < 0.0000000000000002 ***
# rewLeft_f1       0.04772    0.03009   1.586                0.113    
## --> GOES INTO PAPER!!!

plot(effect("dwell_rew_rel_z",mod)) # more time, more Go
print_effect(mod,"dwell_rew_rel_z") # b = 0.140, se = 0.032, z = 4.410, p < .001

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                 Df   Chisq Chi Df         Pr(>Chisq)    
# dwell_rew_rel_z 13 15.3311      1 0.0000902182130572 *** XXX
# reqAction_f     13 52.7934      1 0.0000000000003705 ***
# rewLeft_f       13  2.3642      1             0.1242 
## --> GOES INTO PAPER!!!

# ---------------------------------------------------- #
### Automatized raincloud plot:
nPerc <- 3; selVar <- "dwell_rew_rel"; percVar <- paste0(selVar,"_",nPerc,"Perc");
nPerc <- 4; selVar <- "dwell_rew_rel"; percVar <- paste0(selVar,"_",nPerc,"Perc");
nPerc <- 5; selVar <- "dwell_rew_rel"; percVar <- paste0(selVar,"_",nPerc,"Perc");

## Prepare data:
modData <- create_percentiles(modData,nPerc=nPerc,selVar)
round(tapply(modData$response,modData[,percVar],mean,na.rm=T), 3) #
plotData <- modData[!(is.na(modData[,percVar])),] # remove NAs in percVar

## Raincloud bar plot:
png(paste0(plotDir,"raincloud_bar_response_cleaned_",percVar,"_full.png"),width = 480, height = 480)
plot_raincloud(plotData, xVar = percVar, yVar = "response_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
               # yLim = c(0.31, 0.91),
               color = "orchid", 
               xLab = " Percentiles(Relative dwell time on rewards)", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n relative dwell time on rewards (percentiles)")
dev.off()

# ---------------------------------------------------- #
### Plot based on regression (UNSTANDARDIZED):

## Formula:
formula <- "response_cleaned ~ dwell_rew_rel + (dwell_rew_rel|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("dwell_rew_rel",mod)) # raw scale suggested
plot(effect("dwell_rew_rel",mod), rescale.axis = F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_response_cleaned_dwell_rew_rel.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_rel", 
                     xLim = c(-0,1),
                     yLim = c(0.07, 0.90),
                     # yLim = c(0, 1),
                     color = "#7B30A0", margin = c(0,1,0,0),
                     xLab = "Dwell time ratio (%)", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n dwell time ratio")
dev.off()
png(paste0(plotDir,"regression_response_cleaned_dwell_rew_rel_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_rel", 
                     xLim = c(-0,1),
                     yLim = c(0, 1),
                     color = "#7B30A0", margin = c(0,1,0,0),
                     xLab = "Dwell time ratio (%)", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n dwell time ratio")
dev.off()

# ---------------------------------------------------- #
### Plot based on regression (STANDARDIZED):

## Formula:
formula <- "response_cleaned ~ dwell_rew_rel_z + (dwell_rew_rel_z|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("dwell_rew_rel_z",mod)) # raw scale suggested
plot(effect("dwell_rew_rel_z",mod), rescale.axis = F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_response_cleaned_dwell_rew_rel_z.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_rel_z", 
                     xLim = c(-0,1),
                     yLim = c(0.07, 0.90),
                     # yLim = c(0, 1),
                     color = "#7B30A0", margin = c(0,1,0,0),
                     xLab = "Dwell time ratio (z)", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n dwell time ratio")
dev.off()
png(paste0(plotDir,"regression_response_cleaned_dwell_rew_rel_z_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_rel_z", 
                     xLim = c(-0,1),
                     yLim = c(0, 1),
                     color = "#7B30A0", margin = c(0,1,0,0),
                     xLab = "Dwell time ratio (z)", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n dwell time ratio")
dev.off()

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 9C) Reward dwell times only: ####

## Formula:
formula <- "response_cleaned ~ dwell_rew_abs_z + reqAction_f + rewLeft_f + (dwell_rew_abs_z + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#                 Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)      0.50993    0.06936   7.352    0.000000000000195 ***
# dwell_rew_abs_z  0.03491    0.03435   1.016                0.309     XXX
# reqAction_f1     1.08287    0.09808  11.041 < 0.0000000000000002 ***
# rewLeft_f1       0.02468    0.03129   0.789                0.430 

plot(effect("dwell_rew_abs_z",mod)) # more time, more Go: correct direction, but too weak
print_effect(mod,"dwell_rew_abs_z") # b = 0.035, se = 0.034, z = 1.016, p = 0.3094

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                 Df   Chisq Chi Df         Pr(>Chisq)    
# dwell_rew_abs_z 13  0.9446      1             0.3311 XXX  
# reqAction_f     13 52.6762      1 0.0000000000003933 ***
# rewLeft_f       13  0.5956      1             0.4403

# ---------------------------------------------- # 
### Automatized raincloud plot:

nPerc <- 3; yLim = c(0.15, 1); 
nPerc <- 4; yLim = c(0.15, 0.83);
nPerc <- 5; yLim = c(0.15, 0.90);

# Prepare data:
selVar <- "dwell_rew_abs"; percVar <- paste0(selVar,"_",nPerc,"Perc");
modData <- create_percentiles(modData,nPerc=nPerc,selVar)
round(tapply(modData$response,modData[,percVar],mean,na.rm=T),3)
plotData <- modData[!(is.na(modData[,percVar])),] # exclude NAs

## Bar plot:
png(paste0(plotDir,"raincloud_bar_response_cleaned_",percVar,".png"),width = 480, height = 480)
plot_raincloud(plotData, xVar = percVar, yVar = "response_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F,
               # yLim = c(0, 1),
               yLim = yLim,
               color = "#009933", Labels = "1",
               xLab = " Percentiles \n(Abs. dwell time on rewards)", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n absolute dwell time on rewards (percentiles)")
dev.off()
png(paste0(plotDir,"raincloud_bar_response_cleaned_",percVar,"_full.png"),width = 480, height = 480)
plot_raincloud(plotData, xVar = percVar, yVar = "response_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F,
               yLim = c(0, 1),
               color = "#009933", Labels = "1",
               xLab = " Percentiles \n(Abs. dwell time on rewards)", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n absolute dwell time on rewards (percentiles)")
dev.off()

## Line plot:
png(paste0(plotDir,"raincloud_line_response_cleaned_",percVar,".png"),width = 480, height = 480)
plot_raincloud(modData, xVar = percVar, yVar = "response_cleaned",
               isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T,
               # yLim = c(0, 1),
               yLim = yLim,
               color = "#009933", Labels = "1",
               xLab = " Percentiles \n(Abs. dwell time on rewards)", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n absolute dwell time on rewards (percentiles)")
dev.off()
png(paste0(plotDir,"raincloud_line_response_cleaned_",percVar,"_full.png"),width = 480, height = 480)
plot_raincloud(modData, xVar = percVar, yVar = "response_cleaned",
               isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T,
               yLim = c(0, 1),
               color = "#009933", Labels = "1",
               xLab = " Percentiles \n(Abs. dwell time on rewards)", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n absolute dwell time on rewards (percentiles)")
dev.off()

# ---------------------------------------------- # 
### Plot based on regression:
# modData <- subset(eyeData, isCatch == 0)

## Formula:
formula <- "response_cleaned ~ dwell_rew_abs + (dwell_rew_abs|subject)"
# formula <- "response_cleaned ~ dwell_rew_abs + reqAction_f + rewLeft_f + (dwell_rew_abs + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("dwell_rew_abs",mod)) # raw scale suggested
plot(effect("dwell_rew_abs",mod), rescale.axis = F, ylim = c(0,1)) # 0-1 enforced

# Plot regression line:
png(paste0(plotDir,"regression_response_cleaned_dwellrewabs.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_abs", 
                     xLim = c(0,1500),
                     yLim = c(0.07, 0.90), # yLim = c(0.25, 0.90),
                     color = "#009933", margin = c(0,1,0,0),
                     xLab = "Reward dwell time", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n absolute dwell time on rewards (percentiles)")
dev.off()
png(paste0(plotDir,"regression_response_cleaned_dwellrewabs_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_abs", 
                     xLim = c(0,1500),
                     yLim = c(0, 1),
                     color = "#009933", margin = c(0,1,0,0),
                     xLab = "Reward dwell time", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n absolute dwell time on rewards (percentiles)")
dev.off()

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 9D) Punishment dwell times only: ####

## Formulas:
formula <- "response_cleaned ~ dwell_pun_abs_z + reqAction_f + rewLeft_f + (dwell_pun_abs_z + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod);
#                 Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)      0.52535    0.06873   7.643   0.0000000000000212 ***
# dwell_pun_abs_z -0.18487    0.03740  -4.943   0.0000007685733713 *** XXX
# reqAction_f1     1.07920    0.09754  11.064 < 0.0000000000000002 ***
# rewLeft_f1       0.05423    0.03020   1.795               0.0726 .

plot(effect("dwell_pun_abs_z",mod)) # more time, less Go: correct direction, but too weak
print_effect(mod,"dwell_pun_abs_z") # b = -0.185, se = 0.037, z = -4.943, p < .001

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III", # all_fit = TRUE,
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                 Df   Chisq Chi Df         Pr(>Chisq)    
# dwell_pun_abs_z 13 18.0417      1 0.0000216114796415 *** XXX
# reqAction_f     13 52.7916      1 0.0000000000003709 ***
# rewLeft_f       13  3.0137      1            0.08256 .

# ----------------------------------------------- #
## Automatized raincloud plot:
nPerc <- 3; yLim = c(0.15, 1); 
nPerc <- 4; yLim = c(0.15, 0.83);
nPerc <- 5; yLim = c(0.15, 0.90);

## Prepare data:
selVar <- "dwell_pun_abs"; percVar <- paste0(selVar,"_",nPerc,"Perc");
modData <- create_percentiles(modData,nPerc=nPerc,selVar)
tapply(modData$response,modData[,percVar],mean,na.rm=T)
plotData <- modData[!(is.na(modData[,percVar])),]

## Bar plot:
png(paste0(plotDir,"raincloud_bar_response_cleaned_",percVar,".png"),width = 480, height = 480)
plot_raincloud(modData, xVar = percVar, yVar = "response_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = yLim, # c(0.31, 0.91),
               color = "#CC0000", Labels = "1",
               xLab = " Percentiles \n(Abs. dwell time on punishments)", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n absolute dwell time on punishments (percentiles)")
dev.off()
png(paste0(plotDir,"raincloud_bar_response_cleaned_",percVar,"_full.png"),width = 480, height = 480)
plot_raincloud(modData, xVar = percVar, yVar = "response_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
               color = "#CC0000", Labels = "1",
               xLab = " Percentiles \n(Abs. dwell time on punishments)", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n absolute dwell time on punishments (percentiles)")
dev.off()

## Line plot:
png(paste0(plotDir,"raincloud_line_response_cleaned_",percVar,".png"),width = 480, height = 480)
plot_raincloud(modData, xVar = percVar, yVar = "response_cleaned",
               isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T, 
               # yLim = c(0, 1),
               yLim = yLim, # c(0.31, 0.91),
               color = "#CC0000", Labels = "1",
               xLab = " Percentiles \n(Abs. dwell time on punishments)", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n absolute dwell time on punishments (percentiles)")
dev.off()
png(paste0(plotDir,"raincloud_line_response_cleaned_",percVar,"_full.png"),width = 480, height = 480)
plot_raincloud(modData, xVar = percVar, yVar = "response_cleaned",
               isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T, 
               yLim = c(0, 1),
               color = "#CC0000", Labels = "1",
               xLab = " Percentiles \n(Abs. dwell time on punishments)", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n absolute dwell time on punishments (percentiles)")
dev.off()

# ----------------------------------------------- #
### Plot based on regression:
# modData <- subset(eyeData, isCatch == 0)

## Formula:
formula <- "response_cleaned ~ dwell_pun_abs + (dwell_pun_abs|subject)"

## Fir model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("dwell_pun_abs",mod)) # raw scale suggested
plot(effect("dwell_pun_abs",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced
plot(effect("dwell_pun_abs",mod), rescale.axis=F, ylim =  c(0.07, 0.90)) # 

## Plot regression line:
png(paste0(plotDir,"regression_response_cleaned_dwellpunabs.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_pun_abs", # useEffect = F, xVec = c(0, 5000),
                     xLim = c(0,1500),
                     yLim = c(0.07, 0.90),
                     color = "#CC0000", margin = c(0,1,0,0),
                     xLab = " Punishment dwell time", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n absolute dwell time on punishments (percentiles)")
dev.off()
png(paste0(plotDir,"regression_response_cleaned_dwellpunabs_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_pun_abs", # useEffect = F, xVec = c(0, 5000),
                     xLim = c(0,1500),
                     yLim = c(0, 1),
                     color = "#CC0000", margin = c(0,1,0,0),
                     xLab = " Punishment dwell time", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n absolute dwell time on punishments (percentiles)")
dev.off()

# ===================================================================================================================================== #
#### 10) RT ~ DWELL TIME OUTCOME: ####

## Select data:
modData <- subset(eyeData, isCatch == 0)
# modData <- subset(eyeData, isCatch == 0 & !(subject %in% noLearners)) # exclude subjects who did not learn
# modData <- subset(eyeData, isCatch == 0 & dwell_rew_abs > 0 & dwell_pun_abs > 0) # only trials with both stakes fixated

length(unique(modData$subject))
table(modData$subject)

## Compute difference and re-standardize:
modData$dwell_out_dif <- modData$dwell_rew_abs - modData$dwell_pun_abs
modData$dwell_out_dif_z <- as.numeric(scale(modData$dwell_out_dif))
modData$dwell_rew_abs_z <- as.numeric(scale(modData$dwell_rew_abs))
modData$dwell_pun_abs_z <- as.numeric(scale(modData$dwell_pun_abs))
modData$dwell_rew_rel_z <- as.numeric(scale(modData$dwell_rew_rel))

modData$RT_cleaned_z <- as.numeric(scale(modData$RT_cleaned))

## Q-value difference:
modData$Qdif <- modData$QGo - modData$QNoGo
modData$Qdif_z <- as.numeric(scale(modData$Qdif))

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 10A) Dwell time difference: ####

## Formula:
formula <- "RT_cleaned_z ~ dwell_out_dif_z + reqAction_f + rewLeft_f + (dwell_out_dif_z + reqAction_f + rewLeft_f|subject)"
# formula <- "RT_cleaned_z ~ dwell_out_dif_z + Qdif_z + rewLeft_f + (dwell_out_dif_z + Qdif_z + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#                 Estimate Std. Error       df t value  Pr(>|t|)    
# (Intercept)      0.03562    0.07260 33.85473   0.491     0.627    
# dwell_out_dif_z -0.03561    0.02580 34.30433  -1.380     0.176 XXX  
# reqAction_f1    -0.08347    0.01738 30.66438  -4.802 0.0000388 ***
# rewLeft_f1      -0.02134    0.01598 44.65389  -1.335     0.189   

print_effect(mod,"dwell_out_dif_z")
plot(effect("dwell_out_dif_z",mod)) # if anything: more looking at reward: faster

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                 Df   Chisq Chi Df Pr(>Chisq)    
# dwell_out_dif_z 14  1.8979      1     0.1683 XXX     
# reqAction_f     14 18.5524      1 0.00001653 ***
# rewLeft_f       14  1.7839      1     0.1817     

# ----------------------------------------- #
### Plot based on regression:
# modData <- subset(eyeData, isCatch == 0)

## Formula:
formula <- "RT_cleaned ~ dwell_out_dif + (dwell_out_dif|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("dwell_out_dif",mod)) # raw scale suggested
plot(effect("dwell_out_dif",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_RT_cleaned_dwelloutdif.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_out_dif", 
                       xLim = c(-1500,1500),
                       # yLim = c(0.24, 0.82),
                       yLim = c(0.25, 0.70),
                       # yLim = c(0, 1),
                       color = "#7B30A0", margin = c(0,1,0,0),
                       xLab = "Dwell time difference", yLab = "Reaction time", Main = "RTs as a function of \n absolute dwell time on rewards (percentiles)")
dev.off()
png(paste0(plotDir,"regression_RT_cleaned_dwelloutdif_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_out_dif", 
                       xLim = c(-1500,1500),
                       yLim = c(0, 1),
                       color = "#7B30A0", margin = c(0,1,0,0),
                       xLab = "Dwell time difference", yLab = "Reaction time", Main = "RTs as a function of \n absolute dwell time on rewards (percentiles)")
dev.off()

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 10B) Relative dwell time: #####

## Formula:
formula <- "RT_cleaned_z ~ dwell_rew_rel_z + rewLeft_f + reqAction_f + (dwell_rew_rel_z + rewLeft_f + reqAction_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#                 Estimate Std. Error       df t value  Pr(>|t|)    
# (Intercept)      0.03550    0.07253 33.78080   0.489     0.628    
# dwell_rew_rel_z -0.03167    0.02587 32.29649  -1.225     0.230 XXX    
# rewLeft_f1      -0.02033    0.01561 30.74910  -1.302     0.203    
# reqAction_f1    -0.08475    0.01717 30.71270  -4.936 0.0000264 ***
## --> GOES INTO PAPER!!!

plot(effect("dwell_rew_rel_z",mod)) # more time on reward: faster
print_effect(mod,"dwell_rew_rel_z") # 

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT)
#                 Df   Chisq Chi Df Pr(>Chisq)    
# dwell_rew_rel_z 14  1.4894      1     0.2223 XXX    
# rewLeft_f       14  1.6884      1     0.1938    
# reqAction_f     14 19.3920      1 0.00001065 ***
## --> GOES INTO PAPER!!!

## ---------------------------------------------- #
### Automatized raincloud plot:
nPerc <- 3; selVar <- "dwell_rew_rel"; percVar <- paste0(selVar,"_",nPerc,"Perc");
nPerc <- 4; selVar <- "dwell_rew_rel"; percVar <- paste0(selVar,"_",nPerc,"Perc");
nPerc <- 5; selVar <- "dwell_rew_rel"; percVar <- paste0(selVar,"_",nPerc,"Perc");

## Prepare data:
modData <- create_percentiles(modData,nPerc=nPerc,selVar)
tapply(modData$RT,modData[,percVar],mean,na.rm=T)
plotData <- modData[!(is.na(modData[,percVar])),]

## Raincloud bar plot:
png(paste0(plotDir,"raincloud_bar_RT_cleaned_",percVar,".png"),width = 480, height = 480)
plot_raincloud(modData, xVar = percVar, yVar = "RT_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = c(0.35, 0.74),
               color = "orchid", Labels = "1",
               xLab = " Percentiles(Relative dwell time on rewards)", yLab = "RT (in sec.)", Main = "Reaction times as a function of \n relative dwell time on rewards (percentiles)")
dev.off()

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 10C) Reward dwell time only: ####

## Formula:
formula <- "RT_cleaned_z ~ dwell_rew_abs_z + reqAction_f + rewLeft_f + (dwell_rew_abs_z + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#                 Estimate Std. Error       df t value  Pr(>|t|)    
# (Intercept)      0.03376    0.07205 33.94778   0.469     0.642    
# dwell_rew_abs_z -0.03371    0.02659 31.48400  -1.268     0.214 XXX    
# reqAction_f1    -0.08354    0.01744 30.50772  -4.791 0.0000405 ***
# rewLeft_f1      -0.01777    0.01546 31.69923  -1.149     0.259

plot(effect("dwell_rew_abs_z",mod)) # more time on reward: faster
print_effect(mod,"dwell_rew_abs_z") # b = -0.034, se = 0.027, t(31.484) = -1.268, p = 0.2142

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                 Df   Chisq Chi Df Pr(>Chisq)    
# dwell_rew_abs_z 14  1.6187      1     0.2033 XXX    
# reqAction_f     14 18.4274      1 0.00001765 ***
# rewLeft_f       14  1.3336      1     0.2482    

# ------------------------------------------------------------ #
### Automatized raincloud plot:

nPerc <- 3; selVar <- "dwell_rew_abs"; percVar <- paste0(selVar,"_",nPerc,"Perc"); 
nPerc <- 4; selVar <- "dwell_rew_abs"; percVar <- paste0(selVar,"_",nPerc,"Perc"); 
nPerc <- 5; selVar <- "dwell_rew_abs"; percVar <- paste0(selVar,"_",nPerc,"Perc"); 
yLim = c(0.35, 0.80);

## Prepare data:
modData <- create_percentiles(modData,nPerc=nPerc,selVar)
round(tapply(modData$RT,modData[,percVar],mean,na.rm=T), 3)
plotData <- modData[!(is.na(modData[,percVar])),]

## Raincloud bar plot:
png(paste0(plotDir,"raincloud_bar_RT_cleaned_",percVar,".png"),width = 480, height = 480)
plot_raincloud(plotData, xVar = percVar, yVar = "RT_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = c(0.35, 0.74),
               color = "#009933", Labels = "1",
               xLab = " Percentiles (Absolute dwell time on rewards)", yLab = "RT (in sec.)", Main = "Reaction times as a function of \n absolute dwell time on rewards (percentiles)")
dev.off()
png(paste0(plotDir,"raincloud_bar_RT_cleaned_",percVar,"_full.png"),width = 480, height = 480)
plot_raincloud(plotData, xVar = percVar, yVar = "RT_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
               color = "#009933", Labels = "1",
               xLab = " Percentiles (Absolute dwell time on rewards)", yLab = "RT (in sec.)", Main = "Reaction times as a function of \n absolute dwell time on rewards (percentiles)")
dev.off()

# ------------------------------------------------------------ #
### Plot based on regression:
# modData <- subset(eyeData, isCatch == 0)

## Formula:
formula <- "RT_cleaned ~ dwell_rew_abs + (dwell_rew_abs|subject)"

# Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa"))); summary(mod)
plot(effect("dwell_rew_abs",mod)) # raw scale suggested
plot(effect("dwell_rew_abs",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_RTcleaned_dwellrewabs.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_abs", 
                     xLim = c(0,1500),
                     yLim = c(0.25, 0.70),
                     # yLim = c(0, 1),
                     color = "#009933", margin = c(0,1,0,0),
                     xLab = "Reward dwell time", yLab = "Reaction time", Main = "Reaction time as a function of \n reward dwell time")
dev.off()
png(paste0(plotDir,"regression_RTcleaned_dwellrewabs_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_abs", 
                     xLim = c(0,1500),
                     yLim = c(0, 1),
                     color = "#009933", margin = c(0,1,0,0),
                     xLab = "Reward dwell time", yLab = "Reaction time", Main = "Reaction time as a function of \n reward dwell time")
dev.off()

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 10D) Punishment dwell time only: ####

## Formula:
formula <- "RT_cleaned_z ~ dwell_pun_abs_z + reqAction_f + rewLeft_f + (dwell_pun_abs_z + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#                 Estimate Std. Error       df t value  Pr(>|t|)    
# (Intercept)      0.03243    0.07408 33.77124   0.438     0.664    
# dwell_pun_abs_z  0.02714    0.02813 34.57813   0.965     0.341 XXX    
# reqAction_f1    -0.08403    0.01703 30.57935  -4.935 0.0000267 ***
# rewLeft_f1      -0.02045    0.01570 31.50030  -1.302     0.202 

plot(effect("dwell_pun_abs_z",mod)) # more time on punishment: faster
print_effect(mod,"dwell_pun_abs_z") #  = 0.027, se = 0.028, t(34.578) = 0.965, p = 0.3414

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                 Df   Chisq Chi Df Pr(>Chisq)    
# dwell_pun_abs_z 14  0.9385      1     0.3327 XXX    
# reqAction_f     14 19.3753      1 0.00001074 ***
# rewLeft_f       14  1.6902      1     0.1936 

# ------------------------------------------------- #
### Automatized raincloud plot:
nPerc <- 3; selVar <- "dwell_pun_abs"; percVar <- paste0(selVar,"_",nPerc,"Perc");
nPerc <- 4; selVar <- "dwell_pun_abs"; percVar <- paste0(selVar,"_",nPerc,"Perc");
nPerc <- 5; selVar <- "dwell_pun_abs"; percVar <- paste0(selVar,"_",nPerc,"Perc");
yLim = c(0.35, 0.80)

## Prepare data:
modData <- create_percentiles(modData,nPerc=nPerc,selVar)
tapply(modData$RT,modData[,percVar],mean,na.rm=T)
plotData <- modData[!(is.na(modData[,percVar])),]

## Raincloud bar plot:
png(paste0(plotDir,"raincloud_bar_RT_cleaned_",percVar,".png"),width = 480, height = 480)
plot_raincloud(plotData, xVar = percVar, yVar = "RT_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0.35, 0.80),
               color = "#CC0000", Labels = "1",
               xLab = " Percentiles (Absolute dwell time on punishments)", yLab = "RT (in sec.)", Main = "Reaction times as a function of \n absolute dwell time on punishments (percentiles)")
dev.off()
png(paste0(plotDir,"raincloud_bar_RT_cleaned_",percVar,"_full.png"),width = 480, height = 480)
plot_raincloud(plotData, xVar = percVar, yVar = "RT_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
               color = "#CC0000", Labels = "1",
               xLab = " Percentiles (Absolute dwell time on punishments)", yLab = "RT (in sec.)", Main = "Reaction times as a function of \n absolute dwell time on punishments (percentiles)")
dev.off()

# ------------------------------------------------- #
### Plot based on regression:
# modData <- subset(eyeData, isCatch == 0)

## Formula:
formula <- "RT_cleaned ~ dwell_pun_abs + (dwell_pun_abs|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa"))); summary(mod)
plot(effect("dwell_pun_abs",mod)) # raw scale suggested
plot(effect("dwell_pun_abs",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_RTcleaned_dwellpunabs.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_pun_abs", 
                     xLim = c(0,1500),
                     yLim = c(0.25, 0.70),
                     # yLim = c(0.25, 0.90),
                     # yLim = c(0, 1),
                     color = "#CC0000", margin = c(0,1,0,0),
                     xLab = "Punishment dwell time", yLab = "Reaction time", Main = "Reaction time as a function of \n punishment dwell time")
dev.off()
png(paste0(plotDir,"regression_RTcleaned_dwellpunabs_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_pun_abs", 
                     xLim = c(0,1500),
                     yLim = c(0, 1),
                     color = "#CC0000", margin = c(0,1,0,0),
                     xLab = "Punishment dwell time", yLab = "Reaction time", Main = "Reaction time as a function of \n punishment dwell time")
dev.off()

# ===================================================================================================================================== #
#### 11): RESPONSE ~ INTERACTION STAKES AND DWELL TIMES: ####

## Select data:
modData <- subset(eyeData, isCatch == 0)
# modData <- subset(eyeData, isCatch == 0 & !(subject %in% noLearners)) # exclude subjects who did not learn
# modData <- subset(eyeData, isCatch == 0 & dwell_rew_abs > 0 & dwell_pun_abs > 0) # only trials with both stakes fixated

length(unique(modData$subject))
table(modData$subject)

## Recompute difference and re-standardize:
modData$difMag <- modData$rewMag - modData$punMag
modData$difMag_z <- as.numeric(scale(modData$difMag))
modData$dwell_out_dif <- modData$dwell_rew_abs - modData$dwell_pun_abs
modData$dwell_out_dif_z <- as.numeric(scale(modData$dwell_out_dif))
modData$dwell_rew_rel_z <- as.numeric(scale(modData$dwell_rew_rel))

## Formula:
formula <- "response_cleaned ~ difMag_z * dwell_out_dif_z + reqAction_f + rewLeft_f + (difMag_z * dwell_out_dif_z + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod); beep()
#                          Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)               0.52046    0.06860   7.586   0.0000000000000329 ***
# difMag_z                  0.10590    0.03063   3.458             0.000545 ***
# dwell_out_dif_z           0.12268    0.03509   3.496             0.000473 ***
# reqAction_f1              1.08756    0.09828  11.066 < 0.0000000000000002 ***
# rewLeft_f1                0.04035    0.03144   1.283             0.199397    
# difMag_z:dwell_out_dif_z -0.02698    0.02830  -0.954             0.340321 XXX
# --> negative effect: antagonistic: either strong stakes effect or strong dwell time effect, but not both?

plot(effect("difMag_z:dwell_out_dif_z",mod)) # positive stakes effect for attention to punishments; gets weaker for attention to rewards??? 
print_effect(mod,"difMag_z:dwell_out_dif_z") # 

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                          Df   Chisq Chi Df         Pr(>Chisq)    
# difMag_z                 26 10.2682      1           0.001353 ** 
# dwell_out_dif_z          26  9.8594      1           0.001690 ** 
# reqAction_f              26 52.8074      1 0.0000000000003679 ***
# rewLeft_f                26  1.5508      1           0.213020    
# difMag_z:dwell_out_dif_z 26  0.7740      1           0.378981 XXX

# END