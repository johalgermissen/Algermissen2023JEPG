#### 02_eyegaze_analyze_mixedmodels.R ####

#' Sample 2:
#' Perform mixed-effects linear and logistic regression models to obtain all results
#' reported in the main text and supplementary material for Sample 2 only.
#' For creating plots featured in the main text and supplementary material, see the script 03_plots_paper.R. 
#' For between-subjects correlations across both samples, see the script 04_between_subjects.R.
#' Adjust root directory to your own folder structure before running script.

# ================================================================================================================= #
#### Initialize directories: ####

rootDir       <- "/project/2420133.01/" # adjust to your own folder structure before running script

codeDir       <- paste0(rootDir,"analyses/functions/")

dataDir       <- paste0(rootDir,"data/sample2/") 
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
noLearners <- c(16, 20, 23, 35, 47, 60, 64)

# ================================================================================================================= #
#### Alternative B: Load and combine aggregated eye-tracking files: ####

# -------------------------------------- #
## Load eye-tracking data:
eyeData <- do.call("rbind", lapply(list.files(eyeDataDir, pattern="aggr.csv",full=TRUE), read.csv, header=T))
table(eyeData$subject)
length(table(eyeData$subject))

# -------------------------------------- #
## Load behavioral data:
allData <- lapply(list.files(behavDataDir,pattern=".csv",full=TRUE), read.csv, header=T)
behavData <- do.call("rbind",allData)
# Account for double subject numbers 33 and 56 in behavioral data::
behavData[which(behavData$subject == 33 & behavData$age == 20),"subject"] <- 34
behavData[which(behavData$subject == 56 & behavData$age == 19),"subject"] <- 57
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
#        64.00         0.00         0.00        18.00        34.00        16.00      1375.00        21.00        21.48         0.38 
# CI.mean.0.95          var      std.dev     coef.var 
#         0.76         9.14         3.02         0.14

selVar <- demoData$age; nRound <- 2
cat(paste0("M = ",round(mean(selVar),nRound),", SD = ",round(sd(selVar),nRound),", range ",round(min(selVar),nRound)," - ",round(max(selVar),nRound),"\n"))
# M = 21.48, SD = 3.02, range 18 - 34

## Gender:
table(demoData$gender)
# female   male  other 
#     50     13      1 

## Handedness;
table(demoData$hand)
# left_hand right_hand 
#         2         62 

## Eye dominance:
table(demoData$eye)
# left_eye right_eye 
#       23        41

# ======================================================================================================================== #
#### 1B) Accuracy Go/NoGo Task: ####

## Select only GNG task:
taskData <- subset(eyeData,!is.na(outcome))
table(taskData$subject) # 240 GNG trials per subject

## Aggregate accuracy per subject:
GNGACCvec <- as.numeric(tapply(taskData$ACC,taskData$subject,mean)) # save
round(tapply(taskData$ACC,taskData$subject,mean),2) # plot
#    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24   25   26   27 
# 0.56 0.67 0.81 0.70 0.87 0.91 0.78 0.62 0.77 0.80 0.77 0.75 0.77 0.86 0.84 0.43 0.92 0.82 0.81 0.48 0.83 0.76 0.36 0.80 0.81 0.56 0.72 
#   28   29   30   31   32   33   34   35   36   37   38   39   40   41   42   43   44   45   46   47   48   49   50   51   52   53   54 
# 0.76 0.63 0.62 0.61 0.86 0.82 0.90 0.40 0.85 0.86 0.81 0.74 0.76 0.59 0.88 0.63 0.65 0.80 0.86 0.53 0.81 0.84 0.78 0.65 0.88 0.68 0.88 
#   55   56   57   58   59   60   61   62   63   64 
# 0.75 0.83 0.63 0.87 0.75 0.52 0.68 0.80 0.75 0.55 

## Descriptives:
round(stat.desc(GNGACCvec),3)
selVar <- GNGACCvec; nRound <- 3
cat(paste0("M = ",round(mean(selVar),nRound),", SD = ",round(sd(selVar),nRound),", range ",round(min(selVar),nRound)," - ",round(max(selVar),nRound),"\n"))
# M = 0.734, SD = 0.132, range 0.362 - 0.917

## Binomial test:
nTrialGNG <- sum(taskData$subject==1)
binom.test(round(nTrialGNG*0.55), nTrialGNG, p = 0.5, alternative = "greater")
# 0.55: number of successes = 132, number of trials = 240, p-value = 0.06874
# 0.56: number of successes = 134, number of trials = 240, p-value = 0.04057
# pbinom(240*0.55, 240, 0.50, lower.tail = FALSE)

## Identify subjects who did not learn significantly above chance:
noLearners <- as.numeric(which(round(tapply(taskData$ACC,taskData$subject,mean),2) <= .55))
noLearners # --> subs 16 20 23 35 47 60 have ACC <= .55
rbind(noLearners,GNGACCvec[noLearners])
noLearners <- c(16, 20, 23, 35, 47, 60, 64)

# ======================================================================================================================== #
#### 1C) Accuracy Catch Task: ####

## Select only catch trials:
catchData <- subset(eyeData,is.na(outcome))
table(catchData$subject) # 24 catch trials per subject

## Aggregate accuracy per subject:
catchACCvec <- as.numeric(tapply(catchData$ACC,catchData$subject,mean,na.rm=T)) # save
round(tapply(catchData$ACC,catchData$subject,mean,na.rm=T),2) # plot
#    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24   25   26   27 
# 0.96 0.79 0.83 1.00 0.92 1.00 0.96 0.92 1.00 0.92 1.00 0.96 0.88 0.29 0.92 0.92 0.96 0.92 0.92 0.46 1.00 0.92 0.25 0.87 0.96 0.83 0.88 
#   28   29   30   31   32   33   34   35   36   37   38   39   40   41   42   43   44   45   46   47   48   49   50   51   52   53   54 
# 0.96 1.00 0.79 0.92 0.92 0.96 0.96 0.54 0.92 0.87 0.75 1.00 0.96 0.79 1.00 0.92 0.83 0.96 0.79 0.83 1.00 0.92 1.00 0.71 0.75 0.67 0.92 
#   55   56   57   58   59   60   61   62   63   64 
# 0.67 0.88 0.87 1.00 0.71 0.88 1.00 0.79 0.75 0.83 

# --------------------------------------------- #
### Descriptives:
round(stat.desc(catchACCvec),3)
selVar <- catchACCvec; nRound <- 3
cat(paste0("M = ",round(mean(selVar),nRound),", SD = ",round(sd(selVar),nRound),", range ",round(min(selVar),nRound)," - ",round(max(selVar),nRound),"\n"))
# M = 0.862, SD = 0.155, range 0.25 - 1

# --------------------------------------------- #
### Binomial test:
nTrial <- sum(catchData$subject==1)
binom.test(round(nTrial*0.69), nTrial, p = 0.5, alternative = "greater")
# 0.68: number of successes = 132, number of trials = 240, p-value = 0.07579
# 0.69: number of successes = 132, number of trials = 240, p-value = 0.03196

## Identify subjects who did not perform catch task significantly above chance:
noCatchers <- as.numeric(which(round(tapply(catchData$ACC,catchData$subject,mean,na.rm=T),2) <= .69))
noCatchers # # --> subs 1, 11, 26 have ACC < .69
rbind(noCatchers,catchACCvec[noCatchers])
noCatchers <- c(14, 20, 23, 35, 53, 55)

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
#    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24   25   26   27 
# 0.11 0.87 1.04 0.75 1.45 1.62 1.07 0.72 0.84 1.01 1.07 0.80 0.97 1.55 1.25 0.10 1.60 1.24 1.16 0.10 1.65 1.29 0.10 0.93 1.28 0.13 0.83 
#   28   29   30   31   32   33   34   35   36   37   38   39   40   41   42   43   44   45   46   47   48   49   50   51   52   53   54 
# 0.97 0.49 0.37 0.43 1.35 0.87 1.67 0.10 1.26 1.21 1.10 0.80 0.96 0.46 1.37 0.56 0.70 1.19 1.55 0.10 0.96 1.33 0.94 0.58 1.41 0.73 1.49 
#   55   56   57   58   59   60   61   62   63   64 
# 1.03 1.22 0.15 1.57 0.93 0.10 0.67 1.21 0.79 0.14 

## Descriptives:
stat.desc(euros)
selVar <- euros; nRound <- 2
cat(paste0("M = ",round(mean(selVar),nRound),", SD = ",round(sd(selVar),nRound),", range ",round(min(selVar),nRound)," - ",round(max(selVar),nRound),"\n"))
# M = 0.91, SD = 0.47, range 0.1 - 1.67

# ======================================================================================================================== #
#### 1E) Trials with no fixation: ####

nrow(eyeData) # 16896

## Index of whether no fixations made:
eyeData$noFix_n <- ifelse(is.na(eyeData$dwell_left_abs) & is.na(eyeData$dwell_right_abs), 1, 0)
table(eyeData$noFix_n)
#     0     1 
# 16305   591

## Number trials with/without fixation per subject:
table(eyeData$noFix_n, eyeData$subject)
#     1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33
# 0 254 264 264 264 264 262 264 262 263 261 261 263 227 218 241 264 262 258 264 264 260 264 250 263 254 259 264 264 263 247 264 262 264
# 1  10   0   0   0   0   2   0   2   1   3   3   1  37  46  23   0   2   6   0   0   4   0  14   1  10   5   0   0   1  17   0   2   0
# 
#    34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64
# 0 264 125 264 260 264 264 261 263 262 264 263 248 212 261 263 264 264 260 238 259 264 251 264 242 264 257 262 264 212 248 232
# 1   0 139   0   4   0   0   3   1   2   0   1  16  52   3   1   0   0   4  26   5   0  13   0  22   0   7   2   0  52  16  32
# --> subs 14, 35, 46, and 62 have quite a few (> 40) missing trials

## Descriptives:
noFix <- as.numeric(tapply(eyeData$noFix_n, eyeData$subject, sum, na.rm = T) / length(unique(eyeData$trialnr))) * 100
round(stat.desc(noFix),1)
# nbr.val     nbr.null       nbr.na          min          max        range          sum       median 
#    64.0         24.0          0.0          0.0         52.7         52.7        223.9          0.8 
#    mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
#     3.5          1.0          2.0         61.7          7.9          2.2 

# ======================================================================================================================== #
#### 1F) Trials with only one fixation: ####

nrow(eyeData) # 16896

## Index of only one fixation made:
eyeData$oneFix_n <- ifelse(eyeData$dwell_left_abs==0 & eyeData$dwell_right_abs > 0|eyeData$dwell_left_abs > 0 & eyeData$dwell_right_abs == 0, 1, 0)
table(eyeData$oneFix_n)
#     0     1 
# 14621  1684 
round(table(eyeData$oneFix_n)/nrow(eyeData),3)
#     0     1 
# 0.865 0.100

## Number of trials with only one fixation per subject:
table(eyeData$oneFix_n,eyeData$subject) 
#     1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33
# 0 193 264 263 262 263 259 264 260 262 249 252 262 151 155 204 247 257 236 258 150 259 263 236 250 232 239 263 264 262 114 264 244 264
# 1  61   0   1   2   1   3   0   2   1  12   9   1  76  63  37  17   5  22   6 114   1   1  14  13  22  20   1   0   1 133   0  18   0
# 
#    34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64
# 0 263  14 263 233 264 264 257 255 232 264 262 238 181 249 263 263 264 151 152 236 234 164 240 152 263 208 262 264  95 202 104
# 1   1 111   1  27   0   0   4   8  30   0   1  10  31  12   0   1   0 109  86  23  30  87  24  90   1  49   0   0 117  46 128

# --> sub 20, 30, 35, 51, 62, 64 often (> 100 times) have only 1 fixation

## Descriptives:
oneFix <- as.numeric(tapply(eyeData$oneFix_n, eyeData$subject, sum, na.rm = T) / length(unique(eyeData$trialnr))) * 100
round(stat.desc(oneFix),1)
# nbr.val     nbr.null       nbr.na          min          max        range          sum       median 
#    64.0         12.0          0.0          0.0         50.4         50.4        637.9          3.2 
#    mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
#    10.0          1.8          3.6        206.4         14.4          1.4

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
sum(modData$RT < 0.200, na.rm = T) # 27 early
tooEarly <- as.numeric(tapply(modData$RT < 0.200, modData$subject, sum, na.rm = T))
round(stat.desc(tooEarly),1)

## Too late:
sum(modData$RT > 0.800, na.rm = T) # 92 very late 
tooLate <- as.numeric(tapply(modData$RT > 0.800, modData$subject, sum, na.rm = T))
round(stat.desc(tooLate),2)

## Plot density of raw RTs:
densityplot(modData$RT)
sum(!is.na(modData$RT)) # 8679
nrow(modData) # 15360

## Delete < 200 and > 800 ms:
densityplot(modData$RT_cleaned) # more normal, still some right skew
sum(is.na(modData$RT_cleaned)) - sum(is.na(modData$RT)) # excluded 120 data points (< 200 and > 800)

# ===================================================================================================================================== #
#### 2) EFFECTS OF REQUIRED ACTION (LEARNING): ####

# ------------------------------------------------------------------------------------------------------------------------------------- #
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
#               Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)   0.396893   0.051203   7.751  0.00000000000000909 ***
# reqAction_f1  1.264939   0.091344  13.848 < 0.0000000000000002 ***
# rewLeft_f1   -0.003389   0.021492  -0.158                0.875 

## Plot:
plot(effect("reqAction_f",mod))
print_effect(mod,"reqAction_f") # b = 1.265, se = 0.091, z = 13.848, p < .001

plot(effect("rewLeft_f",mod))
print_effect(mod,"rewLeft_f") # b = -0.003, se = 0.021, z = -0.158, p = 0.8747

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT)
#             Df   Chisq Chi Df          Pr(>Chisq)    
# reqAction_f  8 89.1895      1 <0.0000000000000002 ***
# rewLeft_f    8  0.0233      1              0.8788 

# --------------------------------------- #
### Automatized raincloud bar plot:
png(paste0(plotDir,"raincloud_bar_response_cleaned_reqAction.png"),width = 480, height = 480)
plot_raincloud(modData, x = "reqAction_f", y = "response_cleaned", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
               color = c("blue","red"), Labels = c("Go","NoGo"), 
               xLab = "Required Action", yLab = "p(Go)", Main = "Response as a function of \nrequired action")
dev.off()
which(tapply(modData$ACC,modData$subject,mean) < 0.50) # 16, 20, 23, 35
tmpData <- subset(modData, subject == 23)
tapply(tmpData$response_cleaned,tmpData$reqAction_f,mean, na.rm = T)

# --------------------------------------- #
### Regression bar plot:
# modData <- subset(eyeData, isCatch == 0)

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
                    color = c("blue","red"), Labels = c("Go","NoGo"), 
                    xLab = "Required Action", yLab = "p(Go)", Main = "Response as a function of \nrequired action")
dev.off()

png(paste0(plotDir,"regression_response_cleaned_reqAction_full.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "reqAction_f", # 
                    yLim = c(0,1),
                    color = c("blue","red"), Labels = c("Go","NoGo"), 
                    xLab = "Required Action", yLab = "p(Go)", Main = "Response as a function of \nrequired action")
dev.off()

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 2B) RT ~ required action (control): ####

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
#              Estimate Std. Error       df t value     Pr(>|t|)    
# (Intercept)   0.05251    0.04674 63.04881   1.123        0.266    
# reqAction_f1 -0.09332    0.01435 56.22052  -6.501 0.0000000226 ***

plot(effect("reqAction_f",mod)) # faster for (correct) Go than (incorrect) NoGo
print_effect(mod,"reqAction_f") # b = -0.093, se = 0.014, t(56.221) = -6.501, p < .001

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
png(paste0(plotDir,"raincloud_bar_RT_cleaned_reqAction_full.png"),width = 480, height = 480)
plot_raincloud(modData, x = "reqAction_f", y = "RT_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
               color = c("blue","red"), Labels = c("Go","NoGo"), 
               xLab = "Required Action", yLab = "RT (in sec.)", Main = "Reaction times as a function of \nrequired action")
dev.off()

# ===================================================================================================================================== #
#### 3) RESPONSES ~ STAKES (BIASES): ####

## Select data:
modData <- subset(eyeData, isCatch == 0)
# modData <- subset(eyeData, isCatch == 0 & !(subject %in% noLearners)) # exclude subjects who did not learnin
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
#               Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)   0.401216   0.051680   7.763  0.00000000000000826 ***
# difMag_z      0.091691   0.031092   2.949              0.00319 ** XXX
# reqAction_f1  1.273056   0.091525  13.909 < 0.0000000000000002 ***
# rewLeft_f1   -0.006102   0.022104  -0.276              0.78250   
## --> GOES INTO PAPER!!!

## Variation over time:
#                     Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)         0.411322   0.053425   7.699   0.0000000000000137 ***
# difMag_z            0.090991   0.031739   2.867              0.00415 ** 
# trialnr_z          -0.214318   0.039640  -5.407   0.0000000642224838 ***
# reqAction_f1        1.301148   0.093637  13.896 < 0.0000000000000002 ***
# rewLeft_f1         -0.007057   0.022356  -0.316              0.75224    
# difMag_z:trialnr_z -0.025889   0.020522  -1.262              0.20711

#                          Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)              0.409645   0.052792   7.760 0.000000000000008518 ***
# difMag_z                 0.084038   0.031464   2.671              0.00756 ** 
# trialNrBlock_z          -0.262117   0.032507  -8.064 0.000000000000000741 ***
# reqAction_f1             1.308644   0.095442  13.711 < 0.0000000000000002 ***
# rewLeft_f1              -0.007119   0.022266  -0.320              0.74919    
# difMag_z:trialNrBlock_z -0.024128   0.022227  -1.086              0.27767   

#                   Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)        0.41042    0.05299   7.745  0.00000000000000956 ***
# difMag_z           0.08333    0.03146   2.649              0.00808 ** 
# cueRep_z          -0.26872    0.03353  -8.014  0.00000000000000111 ***
# reqAction_f1       1.30872    0.09558  13.693 < 0.0000000000000002 ***
# rewLeft_f1        -0.00693    0.02236  -0.310              0.75658    
# difMag_z:cueRep_z -0.02168    0.02219  -0.977              0.32868  

#                     Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)         0.411178   0.053461   7.691   0.0000000000000146 ***
# difMag_z            0.094074   0.031712   2.967             0.003011 ** 
# blockNr_z          -0.136312   0.041398  -3.293             0.000992 ***
# reqAction_f1        1.294272   0.093001  13.917 < 0.0000000000000002 ***
# rewLeft_f1         -0.007051   0.022342  -0.316             0.752324    
# difMag_z:blockNr_z -0.016185   0.020945  -0.773             0.439681   

plot(effect("difMag_z",mod)) # the higher rewards: the more Go
print_effect(mod,"difMag_z") # b = 0.092, se = 0.031, z = 2.949, p = 0.0032

### p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()

## Baseline model:
#             Df   Chisq Chi Df            Pr(>Chisq)   
# difMag_z    13  7.9162      1              0.004899 ** XXX
# reqAction_f 13 89.6033      1 < 0.00000000000000022 ***
# rewLeft_f   13  0.0716      1              0.789011 
# --> GOES INTO PAPER!!!

## Variation over time:
#                         Df   Chisq Chi Df            Pr(>Chisq)    
# difMag_z                26  6.5414      1               0.01054 *  
# trialNrBlock_z          26 44.8258      1      0.00000000002154 ***
# reqAction_f             26 88.1176      1 < 0.00000000000000022 ***
# rewLeft_f               26  0.0955      1               0.75732    
# difMag_z:trialNrBlock_z 26  1.1056      1               0.29305 

#                    Df   Chisq Chi Df            Pr(>Chisq)    
# difMag_z           26  7.5021      1              0.006163 ** 
# trialnr_z          26 24.2086      1          0.0000008645 ***
# reqAction_f        26 89.4791      1 < 0.00000000000000022 ***
# rewLeft_f          26  0.0934      1              0.759909    
# difMag_z:trialnr_z 26  1.4887      1              0.222422   

#                    Df   Chisq Chi Df            Pr(>Chisq)    
# difMag_z           26  8.0112      1              0.004649 ** 
# blockNr_z          26  9.9598      1              0.001600 ** 
# reqAction_f        26 89.6250      1 < 0.00000000000000022 ***
# rewLeft_f          26  0.0934      1              0.759860    
# difMag_z:blockNr_z 26  0.5592      1              0.454584  

#                   Df   Chisq Chi Df            Pr(>Chisq)    
# difMag_z          26  6.4380      1               0.01117 *  
# cueRep_z          26 44.2775      1       0.0000000000285 ***
# reqAction_f       26 87.9926      1 < 0.00000000000000022 ***
# rewLeft_f         26  0.0898      1               0.76449    
# difMag_z:cueRep_z 26  0.8926      1               0.34478  

# --------------------------------------- #  
### Automatized raincloud plot:
png(paste0(plotDir,"raincloud_bar_response_cleaned_difMag.png"),width = 480, height = 480)
plot_raincloud(modData, x = "difMag", y = "response_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = c(0.24, 0.82),
               color = "#FFC000", # Labels = c,
               xLab = "Reward minus punishment stakes", yLab = "p(Go)", Main = "Response as a function of \nstake difference")
dev.off()
png(paste0(plotDir,"raincloud_bar_response_cleaned_difMag_full.png"),width = 480, height = 480)
plot_raincloud(modData, x = "difMag", y = "response_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
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
                     xLab = "Stakes difference", yLab = "p(Go)", Main = "Response as a function of \nstakes difference")
dev.off()
png(paste0(plotDir,"regression_response_cleaned_difMag_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "difMag", 
                     xLim = c(-4,4),
                     # yLim = c(0.24, 0.82),
                     yLim = c(0, 1),
                     color = "#FFC000", # Labels = c(1:5),
                     xLab = "Stakes difference", yLab = "p(Go)", Main = "Response as a function of \nstakes difference")
dev.off()

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 3B) Reward stakes only: ####

## Formula:
formula <- "response_cleaned ~ rewMag_z + reqAction_f + rewLeft_f + (rewMag_z + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#               Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)   0.399820   0.051564   7.754  0.00000000000000891 ***
# rewMag_z      0.081253   0.026858   3.025              0.00248 ** XXX
# reqAction_f1  1.269986   0.091531  13.875 < 0.0000000000000002 ***
# rewLeft_f1   -0.005981   0.022041  -0.271              0.78611 
# --> GOES INTO PAPER

plot(effect("rewMag_z",mod)) # the higher rewards: the more Go
print_effect(mod,"rewMag_z") #

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#             Df   Chisq Chi Df            Pr(>Chisq)    
# rewMag_z    13  8.1514      1              0.004303 **  XXX
# reqAction_f 13 89.3660      1 < 0.00000000000000022 ***
# rewLeft_f   13  0.0692      1              0.792549   
# --> GOES INTO PAPER

# ------------------------------------------- #
### Automatized raincloud plot:
png(paste0(plotDir,"raincloud_bar_response_cleaned_rewMag.png"),width = 480, height = 480)
plot_raincloud(modData,x = "rewMag", y = "response_cleaned", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = c(0.24, 0.82),
               color = "#009933", Labels = "1",
               xLab = "Reward stakes", yLab = "p(Go)", Main = "Response as a function of \nreward stakes")
dev.off()
png(paste0(plotDir,"raincloud_bar_response_cleaned_rewMag_full.png"),width = 480, height = 480)
plot_raincloud(modData,x = "rewMag", y = "response_cleaned", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
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
png(paste0(plotDir,"raincloud_line_response_cleaned_rewMag_full.png"),width = 480, height = 480)
plot_raincloud(modData,x = "rewMag", y = "response_cleaned", 
               isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T, 
               yLim = c(0, 1),
               color = "#009933", Labels = "1",
               xLab = "Reward stakes", yLab = "p(Go)", Main = "Response as a function of \nreward stakes")
dev.off()

# ------------------------------------------- #
### Plot based on regression:
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
#               Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)   0.399688   0.051481   7.764  0.00000000000000824 ***
# punMag_z     -0.063498   0.028297  -2.244               0.0248 * XXX  
# reqAction_f1  1.270865   0.091572  13.878 < 0.0000000000000002 ***
# rewLeft_f1   -0.004502   0.022036  -0.204               0.8381      
# --> REPORT IN PAPER!!!

plot(effect("punMag_z",mod)) # the higher punishments: the less Go
print_effect(mod,"punMag_z") # b = -0.063, se = 0.028, z = -2.244, p = 0.0248

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#             Df   Chisq Chi Df           Pr(>Chisq)    
# punMag_z    13  4.7071      1              0.03004 *  
# reqAction_f 13 89.3913      1 < 0.0000000000000002 ***
# rewLeft_f   13  0.0392      1              0.84307 
# --> REPORT IN PAPER!!!

# ------------------------------------- # 
### Automatized raincloud plot:
png(paste0(plotDir,"raincloud_bar_response_cleaned_punMag.png"),width = 480, height = 480)
plot_raincloud(modData, x = "punMag", y = "response_cleaned", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = c(0.24, 0.82),
               color = "#CC0000", Labels = "1",
               xLab = "Punishment stakes", yLab = "p(Go)", Main = "Response as a function of \npunishment stakes")
dev.off()
png(paste0(plotDir,"raincloud_bar_response_cleaned_punMag_full.png"),width = 480, height = 480)
plot_raincloud(modData, x = "punMag", y = "response_cleaned", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
               color = "#CC0000", Labels = "1",
               xLab = "Punishment stakes", yLab = "p(Go)", Main = "Response as a function of \npunishment stakes")
dev.off()

### Line plot:
png(paste0(plotDir,"raincloud_line_response_cleaned_punMag.png"),width = 480, height = 480)
plot_raincloud(modData, x = "punMag", y = "response_cleaned", 
               isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T, 
               # yLim = c(0, 1),
               yLim = c(0.24, 0.82),
               color = "#CC0000", Labels = "1",
               xLab = "Punishment stakes", yLab = "p(Go)", Main = "Response as a function of \npunishment stakes")
dev.off()
png(paste0(plotDir,"raincloud_line_response_cleaned_punMag_full.png"),width = 480, height = 480)
plot_raincloud(modData, x = "punMag", y = "response_cleaned", 
               isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T, 
               yLim = c(0, 1),
               color = "#CC0000", Labels = "1",
               xLab = "Punishment stakes", yLab = "p(Go)", Main = "Response as a function of \npunishment stakes")
dev.off()

# ------------------------------------- # 
### Plot based on regression:
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
# modData <- subset(eyeData, isCatch == 0 & !(subject %in% noLearners)) # exclude subjects who did not learn
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
formula <- "RT_cleaned_z ~ difMag_z + reqAction_f + rewLeft_f + (difMag_z +  reqAction_f + rewLeft_f|subject)"
# formula <- "RT_cleaned_z ~ difMag_z + Qdif_z + rewLeft_f + (difMag_z + Qdif_z + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#              Estimate Std. Error       df t value     Pr(>|t|)    
# (Intercept)   0.05336    0.04678 63.04006   1.140       0.2584    
# difMag_z     -0.02620    0.01111 85.12116  -2.358       0.0207 * XXX  
# reqAction_f1 -0.09354    0.01448 59.29901  -6.459 0.0000000217 ***
# rewLeft_f1   -0.01844    0.01067 82.62766  -1.728       0.0878 
# --> GOES INTO PAPER!!!

plot(effect("difMag_z",mod)) # the higher rewards: the more Go
print_effect(mod,"difMag_z") # b = -0.025, se = 0.011, t(254.021) = -2.374, p = 0.0184

## p-values with LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#             Df   Chisq Chi Df    Pr(>Chisq)    
# difMag_z    14  6.3134      1       0.01198 * XXX 
# reqAction_f 14 32.7786      1 0.00000001033 ***
# rewLeft_f   14  2.9703      1       0.08481 . 
# --> GOES INTO PAPER!!!

# -------------------------------------------- #
## Automatized raincloud plot:
png(paste0(plotDir,"raincloud_bar_RT_cleaned_difMag.png"),width = 480, height = 480)
plot_raincloud(modData, x = "difMag", y = "RT_cleaned", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               # yLim = c(0.35, 0.67),
               yLim = c(0.35, 0.70),
               color = "#FFC000",
               xLab = "Stakes difference", yLab = "RT (in sec.)", Main = "Reaction times as a function of \nstake difference")
dev.off()
png(paste0(plotDir,"raincloud_bar_RT_cleaned_difMag_full.png"),width = 480, height = 480)
plot_raincloud(modData, x = "difMag", y = "RT_cleaned", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
               color = "#FFC000",
               xLab = "Stakes difference", yLab = "RT (in sec.)", Main = "Reaction times as a function of \nstake difference")
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
formula <- "RT_cleaned_z ~ rewMag_z + reqAction_f + rewLeft_f + (rewMag_z +  reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
             control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#                Estimate Std. Error         df t value     Pr(>|t|)    
# (Intercept)     0.05269    0.04674   63.03826   1.127       0.2639    
# rewMag_z       -0.01189    0.01013 2277.47640  -1.174       0.2406  XXX  
# reqAction_f1   -0.09339    0.01436   55.24105  -6.503 0.0000000241 ***
# rewLeft_f1     -0.01839    0.01049   64.03171  -1.753       0.0843 
### --> GOES INTO PAPER!!!

plot(effect("rewMag_z",mod)) # the higher rewards: the more Go
print_effect(mod,"rewMag_z") # b = -0.012, se = 0.010, t(2277.476) = -1.174, p = 0.2406

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT)
#             Df   Chisq Chi Df    Pr(>Chisq)    
# rewMag_z    14  0.0307      1        0.8610 XXX   
# reqAction_f 14 31.5116      1 0.00000001983 ***
# rewLeft_f   14  1.6976      1        0.1926    
### --> GOES INTO PAPER!!!

# ------------------------------------- #
### Automatized raincloud plot:
png(paste0(plotDir,"raincloud_bar_RT_cleaned_rewMag.png"),width = 480, height = 480)
plot_raincloud(modData, x = "rewMag", y = "RT_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = c(0.35, 0.70),
               color = "#009933", Labels = "1",
               xLab = "Reward stakes", yLab = "RT (in sec.)", Main = "Reaction times as a function of \nreward stakes")
dev.off()
png(paste0(plotDir,"raincloud_bar_RT_cleaned_rewMag_full.png"),width = 480, height = 480)
plot_raincloud(modData, x = "rewMag", y = "RT_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
               color = "#009933", Labels = "1",
               xLab = "Reward stakes", yLab = "RT (in sec.)", Main = "Reaction times as a function of \nreward stakes")
dev.off()

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
formula <- "RT_cleaned_z ~ punMag_z + reqAction_f + rewLeft_f + (punMag_z +  reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
             control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#              Estimate Std. Error       df t value     Pr(>|t|)    
# (Intercept)   0.05292    0.04677 63.04860   1.132      0.26209    
# punMag_z      0.02928    0.01084 92.60505   2.702      0.00821 **  XXX
# reqAction_f1 -0.09324    0.01444 58.40817  -6.456 0.0000000233 ***
# rewLeft_f1   -0.01834    0.01055 73.25169  -1.737      0.08654 .  

### --> GOES INTO PAPER!!!

plot(effect("punMag_z",mod)) # the higher punishments: the less Go
print_effect(mod,"punMag_z") # b = 0.029, se = 0.011, t(92.605) = 2.702, p = 0.0082

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT)
#             Df   Chisq Chi Df   Pr(>Chisq)    
# punMag_z    14  7.4114      1     0.006481 ** XXX 
# reqAction_f 14 32.6912      1 0.0000000108 ***
# rewLeft_f   14  2.9997      1     0.083278 .  
### --> GOES INTO PAPER!!!

# ------------------------------------- #
### Automatized raincloud plot:
png(paste0(plotDir,"raincloud_bar_RT_cleaned_punMag.png"),width = 480, height = 480)
plot_raincloud(modData, x = "punMag", y = "RT_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               # yLim = c(0.35, 0.66),
               yLim = c(0.35, 0.70),
               color = "#CC0000", Labels = "1",
               xLab = "Punishment stakes", yLab = "RT (in sec.)", Main = "Reaction times as a function of \npunishment stakes")
dev.off()
png(paste0(plotDir,"raincloud_bar_RT_cleaned_punMag_full.png"),width = 480, height = 480)
plot_raincloud(modData, x = "punMag", y = "RT_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
               color = "#CC0000", Labels = "1",
               xLab = "Punishment stakes", yLab = "RT (in sec.)", Main = "Reaction times as a function of \npunishment stakes")
dev.off()

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
### Plot based on regression:
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
#### 5A) FIRST FIXATION OUTCOME  ~ REQUIRED ACTION: ####

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
#              Estimate Std. Error z value          Pr(>|z|)    
# (Intercept)   0.71202    0.09681   7.355 0.000000000000191 ***
# reqAction_f1  0.08999    0.02789   3.226           0.00126 ** XXX
# rewLeft_f1    0.87661    0.33111   2.647           0.00811 ** 
## --> GOES INTO PAPER!!!

plot(effect("reqAction_f",mod)) # right direction
print_effect(mod,"reqAction_f", ) # b = 0.090, se = 0.028, z = 3.226, p = 0.0013

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#             Df  Chisq Chi Df Pr(>Chisq)   
# reqAction_f  8 7.8815      1   0.004994 ** XXX
# rewLeft_f    8 6.6270      1   0.010044 * 
## --> GOES INTO PAPER!!!

# ------------------------------------------ #
### Automatized raincloud plot:

## Bar plot:
png(paste0(plotDir,"raincloud_bar_firstfixout_reqAction.png"),width = 480, height = 480)
plot_raincloud(modData, x = "reqAction_f", y = "firstfix_out_n", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F,
               # yLim = c(0, 1),
               yLim = c(0.39, 0.95),
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

# ----------------------------------------- #
### Regression bar plot:
# modData <- eyeData

## Formula:
formula <- "firstfix_out_f ~ reqAction_f + (reqAction_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
plot(effect("reqAction_f",mod)) # raw scale suggestedplot(effect("reqAction_f",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Line plot:
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
#### 5B) FIRST FIXATION ~ Q-VALUES: ####

## Select data:
modData <- subset(eyeData)
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
#             Estimate Std. Error z value          Pr(>|z|)    
# (Intercept)  0.71384    0.09699   7.360 0.000000000000184 ***
# Qdif_z       0.13164    0.03934   3.346          0.000819 *** XXX
# rewLeft_f1   0.88089    0.33205   2.653          0.007981 ** 
## ---> GOES INTO PAPER!!!

plot(effect("Qdif_z",mod)) 
print_effect(mod,"Qdif_z") # b = 0.133, se = 0.039, z = 3.400, p < .001

## p-values With LRTs:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#           Df  Chisq Chi Df Pr(>Chisq)   
# Qdif_z     8 9.1713      1   0.002458 ** XXX
# rewLeft_f  8 6.6540      1   0.009893 **
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
               yLim = c(0.39, 0.95),
               # yLim = yLim,
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
                     xLab = "Q(Go) - Q(NoGo) (z)", yLab = "p(first fix. reward)", Main = "First fixation as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()

png(paste0(plotDir,"regression_firstfixout_nQdifz_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif_z", 
                     # xLim = c(-2,2),
                     yLim = c(0, 1), 
                     # yLim = c(0,1), 
                     color = "orchid", 
                     xLab = "Q(Go) - Q(NoGo) (z)", yLab = "p(first fix. reward)", Main = "First fixation as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()

# ------------------------------------ #
### Un-standardized Q-value difference:

## Formula:
formula <- "firstfix_out_n ~ Qdif + rewLeft_f + (Qdif + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))

## Plot regression line:
png(paste0(plotDir,"regression_firstfixout_nQdif.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif", 
                     xLim = c(-2,2),
                     yLim = c(0.3,0.95), 
                     color = "orchid", 
                     xLab = "Q(Go) - Q(NoGo)", yLab = "p(first fixation on reward)", Main = "First fixation as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()

png(paste0(plotDir,"regression_firstfixout_nQdif_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif", 
                     xLim = c(-2,2),
                     yLim = c(0,1), 
                     color = "orchid", 
                     xLab = "Q(Go) - Q(NoGo)", yLab = "p(first fixation on reward)", Main = "First fixation as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()

# ===================================================================================================================================== #
#### 6A) RESPONSE ~ FIRST FIXATION OUTCOME: #####

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
# (Intercept)      0.24155    0.03772   6.404 0.000000000151 ***
# firstfix_out_f1 -0.05936    0.02179  -2.724        0.00644 ** XXX
# rewLeft_f1      -0.02318    0.02049  -1.131        0.25789   

plot(effect("firstfix_out_f",mod)) # right direction: more Go after first fixation on reward
print_effect(mod,"firstfix_out_f")

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                Df  Chisq Chi Df Pr(>Chisq)   
# firstfix_out_f  8 7.1644      1   0.007436 ** XXX
# rewLeft_f       8 1.2671      1   0.260315   

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
formula <- "RT_cleaned_z ~ firstfix_out_f + reqAction_f + rewLeft_f + (firstfix_out_f + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#                   Estimate Std. Error         df t value     Pr(>|t|)    
# (Intercept)       0.043082   0.046793  63.335823   0.921       0.3607    
# firstfix_out_f1   0.006198   0.011280 314.145434   0.550       0.5831 XXX    
# reqAction_f1     -0.092123   0.014619  58.663962  -6.302 0.0000000414 ***
# rewLeft_f1       -0.018775   0.010867  94.351040  -1.728       0.0873 .  

plot(effect("firstfix_out_f",mod))
print_effect(mod,"firstfix_out_f") # b = 0.006, se = 0.011, t(314.145) = 0.550, p = 0.5831

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                Df   Chisq Chi Df    Pr(>Chisq)    
# firstfix_out_f 14  0.5080      1       0.47600 XXX    
# reqAction_f    14 31.5026      1 0.00000001992 ***
# rewLeft_f      14  3.3072      1       0.06898 .

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
png(paste0(plotDir,"raincloud_bar_RT_cleaned_firstfixout_full.png"),width = 480, height = 480)
plot_raincloud(plotdata, x = "firstfix_out_f", y = "RT_cleaned", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
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
mean(modData$dwell_out_dif_z, na.rm = T)
sd(modData$dwell_out_dif_z, na.rm = T)
modData$dwell_rew_rel_z <- as.numeric(scale(modData$dwell_rew_rel))

## Formula:
formula <- "dwell_out_dif_z ~ firstfix_out_f + rewLeft_f + (firstfix_out_f + rewLeft_f|subject)"
formula <- "dwell_rew_rel_z ~ firstfix_out_f + rewLeft_f + (firstfix_out_f + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
## Difference:
#                 Estimate Std. Error       df t value    Pr(>|t|)    
# (Intercept)     -0.03624    0.03766 62.71993  -0.962    0.339601    
# firstfix_out_f1 -0.16146    0.04229 64.01226  -3.818    0.000306 *** XXX
# rewLeft_f1      -0.18011    0.03188 63.44372  -5.650 0.000000408 ***
## --> GOES INTO PAPER!!!
## Ratio:
#                 Estimate Std. Error       df t value  Pr(>|t|)    
# (Intercept)     -0.06273    0.03166 62.68033  -1.981    0.0519 .  
# firstfix_out_f1 -0.26650    0.05688 63.32121  -4.685 0.0000153 *** XXX
# rewLeft_f1      -0.14554    0.03184 61.66011  -4.572 0.0000238 ***

plot(effect("firstfix_out_f",mod)) # 
print_effect(mod,"firstfix_out_f") # 

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT)
## Difference:
#                Df  Chisq Chi Df   Pr(>Chisq)    
# firstfix_out_f  9 13.232      1    0.0002753 *** XXX
# rewLeft_f       9 26.280      1 0.0000002953 ***
## --> GOES INTO PAPER!!!
## Ratio:
#                Df  Chisq Chi Df Pr(>Chisq)    
# firstfix_out_f  9 18.987      1 0.00001316 *** XXX
# rewLeft_f       9 18.470      1 0.00001726 ***

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
## Diffrence:
#              Estimate Std. Error        df t value       Pr(>|t|)    
# (Intercept)  0.006806   0.046217 62.695639   0.147          0.883    
# difMag_z     0.122341   0.016104 63.370881   7.597 0.000000000175 *** XXX
# rewLeft_f1  -0.157057   0.037319 62.932385  -4.208 0.000083169156 ***
## --> GOES INTO PAPER!!!
## Ratio:
#              Estimate Std. Error        df t value       Pr(>|t|)    
# (Intercept)  0.008508   0.044941 62.525947   0.189        0.85046    
# difMag_z     0.095252   0.012658 62.496390   7.525 0.000000000253 *** XXX
# rewLeft_f1  -0.132380   0.045341 62.826931  -2.920        0.00486 ** 

plot(effect("difMag_z",mod)) # --> look longer at rewards if rewards higher than punishments
print_effect(mod,"difMag_z") # b = 0.095, se = 0.013, t(62.496) = 7.525, p < .001

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT)
## Difference:
#           Df  Chisq Chi Df      Pr(>Chisq)    
# difMag_z   9 41.592      1 0.0000000001125 *** XXX
# rewLeft_f  9 15.861      1 0.0000681701129 ***
## --> GOES INTO PAPER!!!
## Ratio:
#           Df  Chisq Chi Df      Pr(>Chisq)    
# difMag_z   9 40.745      1 0.0000000001735 *** XXX
# rewLeft_f  9  8.124      1        0.004368 ** 

# ===================================================================================================================================== #
#### 8A) DWELL TIME ~ REQUIRED ACTION: ####

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
## Difference:
#               Estimate Std. Error        df t value  Pr(>|t|)    
# (Intercept)   0.006871   0.046216 62.688885   0.149    0.8823    
# reqAction_f1  0.037177   0.008789 61.952634   4.230 0.0000785 *** XXX
# rewLeft_f1   -0.154672   0.037232 62.931433  -4.154    0.0001 ***
#                  Estimate Std. Error        df t value    Pr(>|t|)    
# (Intercept)     -0.035599   0.037688 62.721995  -0.945    0.348502    
# reqAction_f1     0.033193   0.008311 62.050758   3.994    0.000175 *** XXX
# rewLeft_f1      -0.177936   0.031835 63.521295  -5.589 0.000000513 ***
# firstfix_out_f1 -0.158371   0.042309 63.902559  -3.743    0.000392 ***
#                  Estimate Std. Error        df t value       Pr(>|t|)    
# (Intercept)     -0.035868   0.037652 62.735568  -0.953       0.344433    
# reqAction_f1     0.032398   0.008306 62.160002   3.901       0.000238 *** XXX
# rewLeft_f1      -0.179586   0.032131 63.382999  -5.589 0.000000515599 ***
# firstfix_out_f1 -0.158241   0.042506 63.792127  -3.723       0.000419 ***
# difMag_z         0.121052   0.016362 63.338295   7.398 0.000000000391 ***
## RATIO:
#               Estimate Std. Error        df t value  Pr(>|t|)    
# (Intercept)   0.008649   0.044922 62.519323   0.193    0.8479    
# reqAction_f1  0.039081   0.008613 60.175254   4.538 0.0000278 *** XXX 
# rewLeft_f1   -0.130429   0.045145 62.822465  -2.889    0.0053 ** 
#                  Estimate Std. Error        df t value  Pr(>|t|)    
# (Intercept)     -0.062165   0.031696 62.675636  -1.961    0.0543 .  
# reqAction_f1     0.030533   0.007143 61.205669   4.275 0.0000681 *** XXX
# rewLeft_f1      -0.144638   0.031774 61.724681  -4.552 0.0000255 ***
# firstfix_out_f1 -0.264309   0.056887 63.278780  -4.646 0.0000176 ***
#                  Estimate Std. Error        df t value       Pr(>|t|)    
# (Intercept)     -0.062583   0.031679 62.688339  -1.976         0.0526 .  
# reqAction_f1     0.029747   0.007134 61.261540   4.170 0.000097591764 *** XXX
# rewLeft_f1      -0.144698   0.031984 61.679978  -4.524 0.000028197797 ***
# firstfix_out_f1 -0.263546   0.057089 63.223305  -4.616 0.000019637759 ***
# difMag_z         0.093433   0.012973 62.554032   7.202 0.000000000919 ***
## --> GOES INTO PAPER!!!

plot(effect("reqAction_f",mod)) # required Go: more time spend looking at reward, right direction...
print_effect(mod,"reqAction_f") # b = 0.030, se = 0.007, t(61.262) = 4.170, p < .001

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
## DIFFERENCE:
#             Df  Chisq Chi Df Pr(>Chisq)    
# reqAction_f  9 15.964      1 0.00006457 *** XXX
# rewLeft_f    9 15.501      1 0.00008247 ***
#                Df  Chisq Chi Df   Pr(>Chisq)    
# reqAction_f    14 14.389      1    0.0001487 *** XXX
# rewLeft_f      14 25.688      1 0.0000004014 ***
# firstfix_out_f 14 12.740      1    0.0003580 ***
#                Df  Chisq Chi Df      Pr(>Chisq)    
# reqAction_f    20 13.791      1       0.0002044 *** XXX
# rewLeft_f      20 25.648      1 0.0000004096166 ***
# firstfix_out_f 20 12.608      1       0.0003840 ***
# difMag_z       20 39.994      1 0.0000000002547 ***
## RATIO:
#             Df   Chisq Chi Df Pr(>Chisq)    
# reqAction_f  9 17.8978      1 0.00002331 *** XXXX
# rewLeft_f    9  7.9649      1   0.004769 ** 
#                Df  Chisq Chi Df Pr(>Chisq)    
# reqAction_f    14 16.047      1 0.00006178 *** XXX
# rewLeft_f      14 18.286      1 0.00001901 ***
# firstfix_out_f 14 18.705      1 0.00001526 ***
#                Df  Chisq Chi Df      Pr(>Chisq)    
# reqAction_f    20 15.364      1 0.0000886409876 *** XXX
# rewLeft_f      20 18.010      1 0.0000219699621 ***
# firstfix_out_f 20 18.512      1 0.0000168829490 ***
# difMag_z       20 38.190      1 0.0000000006417 ***
## --> GOES INTO PAPER!!!

# ----------------------------------------- #
### Automatized raincloud plot:

## Difference:
png(paste0(plotDir,"raincloud_bar_dwelloutdif_reqAction.png"),width = 480, height = 480)
plot_raincloud(modData, xVar = "reqAction_f", yVar = "dwell_out_dif", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = c(-85, 750),
               color = c("blue","red"), Labels = c("Go","NoGo"),
               xLab = "Required action", yLab = "Dwell time difference", Main = "Dwell time difference as a function of \nrequired action")
dev.off()
png(paste0(plotDir,"raincloud_bar_dwelloutdif_reqAction_full.png"),width = 480, height = 480)
plot_raincloud(modData, xVar = "reqAction_f", yVar = "dwell_out_dif", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(-1500, 1500),
               color = c("blue","red"), Labels = c("Go","NoGo"),
               xLab = "Required action", yLab = "Dwell time difference", Main = "Dwell time difference as a function of \nrequired action")
dev.off()

## Ratio:
png(paste0(plotDir,"raincloud_bar_dwellrewrel_reqAction.png"),width = 480, height = 480)
plot_raincloud(modData, xVar = "reqAction_f", yVar = "dwell_rew_rel", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = c(0.46, 0.89),
               color = c("blue","red"), Labels = c("Go","NoGo"),
               xLab = "Required action", yLab = "Relative dwell time on rewards", Main = "Relative dwell time on rewards as a function of \nrequired action")
dev.off()
png(paste0(plotDir,"raincloud_bar_dwellrewrel_reqAction_full.png"),width = 480, height = 480)
plot_raincloud(modData, xVar = "reqAction_f", yVar = "dwell_rew_rel", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
               color = c("blue","red"), Labels = c("Go","NoGo"),
               xLab = "Required action", yLab = "Relative dwell time on rewards", Main = "Relative dwell time on rewards as a function of \nrequired action")
dev.off()

# ----------------------------------------- #
### Regression bar plot:
# modData <- eyeData

# -------------------- # 
## Difference:

## Formula:
formula <- "dwell_out_dif ~ reqAction_f + (reqAction_f|subject)" # unstandardized

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
plot(effect("reqAction_f",mod)) # raw scale 

png(paste0(plotDir,"regression_dwelloutdif_reqAction.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "reqAction_f", # 
                      # yLim = c(0,1), 
                      yLim = c(-85, 750),
                      color = c("blue","red"), Labels = c("Go","NoGo"), z = 1.96,
                      xLab = "Required Action", yLab = "Dwell time difference", Main = "Dwell time difference as a function of \nrequired action")
dev.off()

png(paste0(plotDir,"regression_dwelloutdif_reqAction_full.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "reqAction_f", # 
                      yLim = c(-1500, 1500),
                      color = c("blue","red"), Labels = c("Go","NoGo"), z = 1.96,
                      xLab = "Required Action", yLab = "Dwell time difference", Main = "Dwell time difference as a function of \nrequired action")
dev.off()

# -------------------- # 
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
modData$dwell_out_dif_z <- as.numeric(scale(modData$dwell_out_dif))
modData$dwell_rew_rel_z <- as.numeric(scale(modData$dwell_rew_rel))

## Formulas:
formula <- "dwell_out_dif_z ~ Qdif_z + rewLeft_f + firstfix_out_f + difMag_z + (Qdif_z + rewLeft_f + firstfix_out_f + difMag_z|subject)"
formula <- "dwell_rew_rel_z ~ Qdif_z + rewLeft_f + firstfix_out_f + difMag_z + (Qdif_z + rewLeft_f + firstfix_out_f + difMag_z|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
## Difference:
#                  Estimate Std. Error        df t value       Pr(>|t|)    
# (Intercept)     -0.034973   0.037819 62.744766  -0.925        0.35864    
# Qdif_z           0.039154   0.008236 51.473977   4.754 0.000016464882 *** XXX
# rewLeft_f1      -0.180598   0.032354 62.965274  -5.582 0.000000538677 ***
# firstfix_out_f1 -0.159552   0.042335 63.918072  -3.769        0.00036 ***
# difMag_z         0.120955   0.016294 63.337972   7.423 0.000000000354 ***
## Ratio:
#                  Estimate Std. Error        df t value       Pr(>|t|)    
# (Intercept)     -0.062158   0.031788 62.706884  -1.955          0.055 .  
# Qdif_z           0.035822   0.007287 67.279404   4.916 0.000005974092 ***
# rewLeft_f1      -0.145430   0.032042 61.635717  -4.539 0.000026800244 ***
# firstfix_out_f1 -0.263926   0.056982 63.262237  -4.632 0.000018562859 ***
# difMag_z         0.093295   0.012931 62.539335   7.215 0.000000000873 ***

plot(effect("Qdif_z",mod)) # higher QGo than QNoGo: higher attention to rewards
print_effect(mod,"Qdif_z") #

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
## Difference:
#                Df  Chisq Chi Df      Pr(>Chisq)    
# Qdif_z         20 24.823      1 0.0000006285760 *** XXX
# rewLeft_f      20 25.676      1 0.0000004038257 ***
# firstfix_out_f 20 14.248      1       0.0001602 ***
# difMag_z       20 41.646      1 0.0000000001094 ***
## Ratio:
#                Df  Chisq Chi Df     Pr(>Chisq)    
# Qdif_z         20 13.231      1      0.0002754 *** XXX
# rewLeft_f      20 17.891      1 0.000023397348 ***
# firstfix_out_f 20 18.711      1 0.000015208858 ***
# difMag_z       20 38.302      1 0.000000000606 ***

# ------------------------------------ #
### Automatized raincloud plot:

# ------------- #
## Difference:

# Prepare data:
nPerc <- 3; yLim <- c(0.30, 1);
nPerc <- 4; yLim <- c(0, 1);
nPerc <- 5; yLim <- c(0, 1);

selVar <- "Qdif"; percVar <- paste0(selVar,"_",nPerc,"Perc");
modData <- create_percentiles(modData,nPerc=nPerc,selVar,perSub=F)
tapply(modData$firstfix_out_n,modData[,percVar],mean,na.rm=T)

## Bar plot:
png(paste0(plotDir,"raincloud_bar_dwelloutdif_",percVar,".png"),width = 480, height = 480)
plot_raincloud(modData, x = percVar, y = "dwell_out_dif", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F,
               yLim = c(-220, 860),
               # yLim = yLim,
               color = "orchid", Labels = "1",
               xLab = " Percentiles(Q(Go) - Q(NoGo))", yLab = "Dwell time difference", Main = "Dwell time difference as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()
png(paste0(plotDir,"raincloud_bar_dwelloutdif_",percVar,"_full.png"),width = 480, height = 480)
plot_raincloud(modData, x = percVar, y = "dwell_out_dif", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F,
               yLim = c(-1500, 1500),
               color = "orchid", Labels = "1",
               xLab = " Percentiles(Q(Go) - Q(NoGo))", yLab = "Dwell time difference", Main = "Dwell time difference as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()

## Line pLot:
png(paste0(plotDir,"raincloud_line_dwelloutdif_",percVar,".png"),width = 480, height = 480)
plot_raincloud(modData, x = percVar, y = "dwell_out_dif", 
               isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T, 
               yLim = c(-220, 860),
               color = "orchid", Labels = "1",
               xLab = " Percentiles(Q(Go) - Q(NoGo))", yLab = "Dwell time difference", Main = "Dwell time difference as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()
png(paste0(plotDir,"raincloud_line_dwelloutdif_",percVar,"_full.png"),width = 480, height = 480)
plot_raincloud(modData, x = percVar, y = "dwell_out_dif", 
               isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T, 
               yLim = c(-1500, 1500),
               color = "orchid", Labels = "1",
               xLab = " Percentiles(Q(Go) - Q(NoGo))", yLab = "Dwell time difference", Main = "Dwell time difference as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()

# ------------- #
## Ratio:

## Prepare data:
nPerc <- 3; yLim <- c(0.30, 1);
nPerc <- 4; yLim <- c(0, 1);
nPerc <- 5; yLim <- c(0, 1);

selVar <- "Qdif"; percVar <- paste0(selVar,"_",nPerc,"Perc");
modData <- create_percentiles(modData,nPerc=nPerc,selVar,perSub=F)
tapply(modData$firstfix_out_n,modData[,percVar],mean,na.rm=T)

## Bar plot:
png(paste0(plotDir,"raincloud_bar_dwellrewrel_",percVar,".png"),width = 480, height = 480)
plot_raincloud(modData, x = percVar, y = "firstfix_out_n", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F,
               yLim = c(0.39, 0.95),
               # yLim = yLim,
               color = "orchid", Labels = "1",
               xLab = " Percentiles(Q(Go) - Q(NoGo))", yLab = "Relative dwell time on rewards", Main = "Relative dwell time on rewards as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()
png(paste0(plotDir,"raincloud_bar_dwellrewrel_",percVar,"_full.png"),width = 480, height = 480)
plot_raincloud(modData, x = percVar, y = "firstfix_out_n", 
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F,
               yLim = c(0, 1),
               color = "orchid", Labels = "1",
               xLab = " Percentiles(Q(Go) - Q(NoGo))", yLab = "Relative dwell time on rewards", Main = "Relative dwell time on rewards as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()

## Line pLot:
png(paste0(plotDir,"raincloud_line_dwellrewrel_",percVar,".png"),width = 480, height = 480)
plot_raincloud(modData, x = percVar, y = "firstfix_out_n", 
               isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T, 
               # yLim = c(0,1),
               yLim = yLim,
               color = "orchid", Labels = "1",
               xLab = " Percentiles(Q(Go) - Q(NoGo))", yLab = "Relative dwell time on rewards", Main = "Relative dwell time on rewards as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()
png(paste0(plotDir,"raincloud_line_dwellrewrel_",percVar,"_full.png"),width = 480, height = 480)
plot_raincloud(modData, x = percVar, y = "firstfix_out_n", 
               isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T, 
               yLim = c(0,1),
               color = "orchid", Labels = "1",
               xLab = " Percentiles(Q(Go) - Q(NoGo))", yLab = "Relative dwell time on rewards", Main = "Relative dwell time on rewards as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()

# ------------------------------------ #
### Plot based on regression:
# modData <- eyeData

# ------------- #
## Difference:

## Formula:
formula <- "dwell_out_dif ~ Qdif_z + rewLeft_f + (Qdif_z + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("Qdif_z",mod)) # raw scale suggested
plot(effect("Qdif_z",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_dwelloutdif_Qdifz.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif_z", 
                       xLim = c(-2,2),
                       yLim = c(-220, 860),
                       color = "orchid", 
                       xLab = "Q(Go) - Q(NoGo) (z)", yLab = "Dwell time difference", Main = "Dwell time difference as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()
png(paste0(plotDir,"regression_dwelloutdif_Qdifz_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif_z", 
                       yLim = c(-1500, 1500),
                       color = "orchid", 
                       xLab = "Q(Go) - Q(NoGo) (z)", yLab = "Dwell time difference", Main = "Dwell time difference as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()

# ------------- #
## Un-standardized Q-value difference:

## Formula:
formula <- "dwell_out_dif ~ Qdif + rewLeft_f + (Qdif + rewLeft_f|subject)" # unstandardized

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)

## Plot regression line:
png(paste0(plotDir,"regression_dwelloutdif_Qdif.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif", 
                       xLim = c(-2,2),
                       yLim = c(-220, 860),
                       color = "orchid", 
                       xLab = "Q(Go) - Q(NoGo)", yLab = "Dwell time difference", Main = "Dwell time difference as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()
png(paste0(plotDir,"regression_dwelloutdif_Qdif_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif", 
                       yLim = c(-1500, 1500),
                       color = "orchid", 
                       xLab = "Q(Go) - Q(NoGo)", yLab = "Dwell time difference", Main = "Dwell time difference as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()

# ------------- #
## Ratio: 

## Formula:
formula <- "dwell_rew_rel ~ Qdif_z + rewLeft_f + (Qdif_z + rewLeft_f|subject)" # unstandardized

## Fit model:
mod <- lmer(formula = formula, data = modData,
             control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("Qdif_z",mod)) # raw scale suggested
plot(effect("Qdif_z",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_dwellrewrel_Qdifz.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif_z", 
                     xLim = c(-2,2),
                     yLim = c(0.25,0.95), 
                     color = "orchid", 
                     xLab = "Q(Go) - Q(NoGo) (z)", yLab = "Relative dwell time on rewards", Main = "Relative dwell time on rewards as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()

png(paste0(plotDir,"regression_dwellrewrel_Qdifz_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif_z", 
                     # xLim = c(-2,2),
                     yLim = c(0, 1), 
                     # yLim = c(0,1), 
                     color = "orchid", 
                     xLab = "Q(Go) - Q(NoGo) (z)", yLab = "Relative dwell time on rewards", Main = "Relative dwell time on rewards as a function of \n Q(Go) - Q(NoGo) (percentiles)")
dev.off()

# ------------- #
## Un-standardized Q-value difference:
# modData <- eyeData
modData$dwell_rew_rel100 <- modData$dwell_rew_rel * 100

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
# (Intercept)      0.40299    0.05067   7.953  0.00000000000000182 ***
# dwell_out_dif_z  0.19152    0.03204   5.977  0.00000000227046699 *** XXX
# reqAction_f1     1.27259    0.09258  13.746 < 0.0000000000000002 ***
# rewLeft_f1       0.01926    0.02296   0.839                0.402   

plot(effect("dwell_out_dif_z",mod)) # more time, more Go
print_effect(mod,"dwell_out_dif_z") # b = 0.192, se = 0.032, z = 5.977, p < .001

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                 Df   Chisq Chi Df            Pr(>Chisq)    
# dwell_out_dif_z 13 28.4425      1         0.00000009652 *** XXX
# reqAction_f     13 88.3336      1 < 0.00000000000000022 *** 
# rewLeft_f       13  0.6591      1                0.4169     

# ---------------------------------------------------- #
### Automatized raincloud plot:

nPerc <- 3; selVar <- "dwell_out_dif"; percVar <- paste0(selVar,"_",nPerc,"Perc");
nPerc <- 4; selVar <- "dwell_out_dif"; percVar <- paste0(selVar,"_",nPerc,"Perc");
nPerc <- 5; selVar <- "dwell_out_dif"; percVar <- paste0(selVar,"_",nPerc,"Perc");

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
               xLab = " Dwell time difference", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n dwell time difference (in seconds)")
dev.off()

# -------------------------------------------------- # 
### Plot based on regression:
# modData <- subset(eyeData, isCatch == 0)
modData$dwell_out_dif <- modData$dwell_rew_abs - modData$dwell_pun_abs

## Formula:
formula <- "response_cleaned ~ dwell_out_dif + (dwell_out_dif|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("dwell_out_dif",mod)) # raw scale suggested
plot(effect("dwell_out_dif",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

# Plot regression line:
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
#### 9B) Relative dwell time: Reward / (reward + punishment) ####

## Formula:
formula <- "response_cleaned ~ dwell_rew_rel_z + reqAction_f + rewLeft_f + (dwell_rew_rel_z + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#                 Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)      0.40390    0.04954   8.153 0.000000000000000355 ***
# dwell_rew_rel_z  0.22142    0.03850   5.751 0.000000008863730183 *** XXX
# reqAction_f1     1.27549    0.09262  13.771 < 0.0000000000000002 ***
# rewLeft_f1       0.01853    0.02391   0.775                0.438 
## --> GOES INTO PAPER!!!

plot(effect("dwell_rew_rel_z",mod)) # more time, more Go
print_effect(mod,"dwell_rew_rel_z") # b = 0.221, se = 0.039, z = 5.751, p < .001

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                 Df  Chisq Chi Df            Pr(>Chisq)    
# dwell_rew_rel_z 13 27.528      1          0.0000001548 *** XXX
# reqAction_f     13 88.482      1 < 0.00000000000000022 ***
# rewLeft_f       13  0.564      1                0.4526  
## --> GOES INTO PAPER!!!

# ---------------------------------------------------- #
### Automatized raincloud plot:
nPerc <- 3; selVar <- "dwell_rew_rel"; percVar <- paste0(selVar,"_",nPerc,"Perc");
nPerc <- 4; selVar <- "dwell_rew_rel"; percVar <- paste0(selVar,"_",nPerc,"Perc");
nPerc <- 5; selVar <- "dwell_rew_rel"; percVar <- paste0(selVar,"_",nPerc,"Perc");

modData <- create_percentiles(modData,nPerc=nPerc,selVar)
round(tapply(modData$response,modData[,percVar],mean,na.rm=T), 3) #
plotData <- modData[!(is.na(modData[,percVar])),] # remove NAs in percVar

png(paste0(plotDir,"raincloud_bar_response_cleaned_",percVar,".png"),width = 480, height = 480)
plot_raincloud(plotData, xVar = percVar, yVar = "response_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
               # yLim = c(0.31, 0.91),
               color = "orchid", 
               xLab = " Percentiles\n(Relative dwell time on rewards)", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n relative dwell time on rewards (percentiles)")
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
png(paste0(plotDir,"MGNGFreeView_regression_response_cleaned_dwell_rew_rel.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_rel", 
                     xLim = c(-0,1),
                     yLim = c(0.07, 0.90),
                     # yLim = c(0, 1),
                     color = "#7B30A0", margin = c(0,1,0,0),
                     xLab = "Dwell time ratio (%)", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n dwell time ratio")
dev.off()
png(paste0(plotDir,"MGNGFreeView_regression_response_cleaned_dwell_rew_rel_full.png"),width = 480, height = 480)
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
png(paste0(plotDir,"MGNGFreeView_regression_response_cleaned_dwell_rew_rel_z.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_rel_z", 
                     xLim = c(-0,1),
                     yLim = c(0.07, 0.90),
                     # yLim = c(0, 1),
                     color = "#7B30A0", margin = c(0,1,0,0),
                     xLab = "Dwell time ratio (z)", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n dwell time ratio")
dev.off()
png(paste0(plotDir,"MGNGFreeView_regression_response_cleaned_dwell_rew_rel_z_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_rel_z", 
                     xLim = c(-0,1),
                     yLim = c(0, 1),
                     color = "#7B30A0", margin = c(0,1,0,0),
                     xLab = "Dwell time ratio (z)", yLab = "p(Go)", Main = "Proportion Go responses as a function of \n dwell time ratio")
dev.off()

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 9C) Reward dwell times only: ####

## Formulas:
formula <- "response_cleaned ~ dwell_rew_abs_z + reqAction_f + rewLeft_f + (dwell_rew_abs_z + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#                 Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)     0.402344   0.052258   7.699   0.0000000000000137 ***
# dwell_rew_abs_z 0.068841   0.030666   2.245               0.0248 * XXX   
# reqAction_f1    1.272018   0.092351  13.774 < 0.0000000000000002 ***
# rewLeft_f1      0.004081   0.022207   0.184               0.8542 

plot(effect("dwell_rew_abs_z",mod)) # more time, more Go: correct direction, but too weak
print_effect(mod,"dwell_rew_abs_z") # b = 0.069, se = 0.031, z = 2.245, p = 0.0248

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                 Df   Chisq Chi Df           Pr(>Chisq)    
# dwell_rew_abs_z 13  4.6165      1              0.03167 *  XXX
# reqAction_f     13 88.5201      1 < 0.0000000000000002 ***
# rewLeft_f       13  0.0317      1              0.85872  

# ---------------------------------------------- # 
### Automatized raincloud plot:

nPerc <- 3; yLim = c(0.15, 1); 
nPerc <- 4; yLim = c(0.15, 0.83);
nPerc <- 5; yLim = c(0.15, 0.90);

## Prepare data:
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

## Formula:
formula <- "response_cleaned ~ dwell_pun_abs_z + reqAction_f + rewLeft_f + (dwell_pun_abs_z + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#                 Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)      0.41993    0.04784   8.777 < 0.0000000000000002 ***
# dwell_pun_abs_z -0.27829    0.04128  -6.741      0.0000000000158 *** XXX
# reqAction_f1     1.27196    0.09276  13.712 < 0.0000000000000002 ***
# rewLeft_f1       0.02138    0.02344   0.912                0.362

plot(effect("dwell_pun_abs_z",mod)) # --> more time, less Go: correct direction
print_effect(mod,"dwell_pun_abs_z") # b = -0.278, se = 0.041, z = -6.741, p < .001

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                 Df  Chisq Chi Df            Pr(>Chisq)    
# dwell_pun_abs_z 13 35.080      1        0.000000003165 *** XXX
# reqAction_f     13 88.059      1 < 0.00000000000000022 ***
# rewLeft_f       13  0.786      1                0.3753  

# ----------------------------------------------- #
### Automatized raincloud plot:
nPerc <- 3; yLim = c(0.15, 1); 
nPerc <- 4; yLim = c(0.15, 0.83);
nPerc <- 5; yLim = c(0.15, 0.90);

## Prepare data:
selVar <- "dwell_pun_abs"; percVar <- paste0(selVar,"_",nPerc,"Perc");
modData <- create_percentiles(modData,nPerc=nPerc,selVar)
tapply(modData$response,modData[,percVar],mean,na.rm=T)
plotData <- modData[!(is.na(modData[,percVar])),]

# Bar plot:
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

# Line plot:
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

## Fit model:
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
#                   Estimate Std. Error         df t value     Pr(>|t|)    
# (Intercept)        0.04428    0.04688   63.15258   0.944       0.3485    
# dwell_out_dif_z   -0.03033    0.01179 3846.05887  -2.574       0.0101 *  XXX
# reqAction_f1      -0.09301    0.01453   56.07651  -6.402 0.0000000333 ***
# rewLeft_f1        -0.02502    0.01065   63.84209  -2.350       0.0219 *  

plot(effect("dwell_out_dif_z",mod)) # -->  more looking at reward: faster
print_effect(mod,"dwell_out_dif_z") # 

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                 Df   Chisq Chi Df    Pr(>Chisq)    
# dwell_out_dif_z 14  4.5329      1       0.03325 *   XXX
# reqAction_f     14 32.0674      1 0.00000001489 ***
# rewLeft_f       14  4.5891      1       0.03218 *    

## ---------------------------------------------- #
### Automatized raincloud plot:

nPerc <- 3; selVar <- "dwell_out_dif"; percVar <- paste0(selVar,"_",nPerc,"Perc");
nPerc <- 4; selVar <- "dwell_out_dif"; percVar <- paste0(selVar,"_",nPerc,"Perc");
nPerc <- 5; selVar <- "dwell_out_dif"; percVar <- paste0(selVar,"_",nPerc,"Perc");

## Prepare data:
modData <- create_percentiles(modData,nPerc=nPerc,selVar)
tapply(modData$RT,modData[,percVar],mean,na.rm=T)
plotData <- modData[!(is.na(modData[,percVar])),]

## Bar plot:
png(paste0(plotDir,"raincloud_bar_RT_cleaned_",percVar,".png"),width = 480, height = 480)
plot_raincloud(plotData, xVar = percVar, yVar = "RT_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = c(0.35, 0.74),
               color = "orchid", Labels = "1",
               xLab = "Dwell time difference", yLab = "RT (in sec.)", Main = "Reaction times as a function of \n dwell time difference")
dev.off()

# ----------------------------------------- #
### Plot based on regression:
# modData <- subset(eyeData, isCatch == 0)
modData$dwell_out_dif <- modData$dwell_rew_abs - modData$dwell_pun_abs # unstandardized

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
#                 Estimate Std. Error       df t value     Pr(>|t|)    
# (Intercept)      0.04747    0.04705 63.07780   1.009       0.3169    
# dwell_rew_rel_z -0.02977    0.01401 61.82957  -2.125       0.0376 * XX 
# rewLeft_f1      -0.02336    0.01123 74.31138  -2.080       0.0410 *  
# reqAction_f1    -0.09291    0.01449 55.68968  -6.413 0.0000000328 ***
## --> GOES INTO PAPER!!!

plot(effect("dwell_rew_rel_z",mod)) # more time on reward: faster
print_effect(mod,"dwell_rew_rel_z") #

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT)
#                 Df   Chisq Chi Df      Pr(>Chisq)    
# dwell_rew_rel_z 14  4.4294      1         0.03532 * XXX 
# rewLeft_f       14  4.2381      1         0.03953 *  
# reqAction_f     14 39.7387      1 0.0000000002903 ***
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

## Bar plot:
png(paste0(plotDir,"raincloud_bar_RT_cleaned_",percVar,".png"),width = 480, height = 480)
plot_raincloud(plotData, xVar = percVar, yVar = "RT_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               # yLim = c(0, 1),
               yLim = c(0.35, 0.74),
               color = "orchid", Labels = "1",
               xLab = " Percentiles\n(Relative dwell time on rewards)", yLab = "RT (in sec.)", Main = "Reaction times as a function of \n relative dwell time on rewards (percentiles)")
dev.off()

# ------------------------------------------------------------------------------------------------------------------------------------- #
#### 10C) Reward dwell time only: ####

## Formula:
formula <- "RT_cleaned_z ~ dwell_rew_abs_z + reqAction_f + rewLeft_f + (dwell_rew_abs_z + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
#                  Estimate Std. Error        df t value     Pr(>|t|)    
# (Intercept)       0.04534    0.04698  63.08120   0.965       0.3382    
# dwell_rew_abs_z  -0.01306    0.01507  63.87063  -0.867       0.3892    XXX
# reqAction_f1     -0.09257    0.01457  58.49632  -6.352 0.0000000345 ***
# rewLeft_f1       -0.02129    0.01131 103.89839  -1.883       0.0626 .

plot(effect("dwell_rew_abs_z",mod)) # more time on reward: faster
print_effect(mod,"dwell_rew_abs_z") # b = -0.013, se = 0.015, t(63.871) = -0.867, p = 0.3892

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()

## RT_cleaned:
#                 Df   Chisq Chi Df   Pr(>Chisq)    
# dwell_rew_abs_z 14  0.7569      1      0.38432 XXX    
# reqAction_f     14 31.9653      1 0.0000000157 ***
# rewLeft_f       14  3.4975      1      0.06146 .  

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

png(paste0(plotDir,"raincloud_bar_RT_",percVar,".png"),width = 480, height = 480)
plot_raincloud(modData, xVar = percVar, yVar = "RT",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0.35, 0.80),
               color = "#009933", Labels = "1",
               xLab = " Percentiles \n(Absolute dwell time on rewards)", yLab = "RT (in sec.)", Main = "Reaction times as a function of \n absolute dwell time on rewards (percentiles)")
dev.off()
png(paste0(plotDir,"raincloud_bar_RT_",percVar,"_full.png"),width = 480, height = 480)
plot_raincloud(modData, xVar = percVar, yVar = "RT",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
               color = "#009933", Labels = "1",
               xLab = " Percentiles \n(Absolute dwell time on rewards)", yLab = "RT (in sec.)", Main = "Reaction times as a function of \n absolute dwell time on rewards (percentiles)")
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
# Plot regression line:
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

## RT_cleaned:
#                   Estimate Std. Error         df t value     Pr(>|t|)    
# (Intercept)        0.04601    0.04665   63.21436   0.986      0.32775    
# dwell_pun_abs_z    0.03923    0.01278 4339.96629   3.069      0.00216 ** XXX
# reqAction_f1      -0.09241    0.01450   54.89145  -6.375 0.0000000399 ***
# rewLeft_f1        -0.02492    0.01064   68.13778  -2.343      0.02205 *  

plot(effect("dwell_pun_abs_z",mod)) # more time on punishment: faster
print_effect(mod,"dwell_pun_abs_z") # b = 0.039, se = 0.013, t(4339.966) = 3.069, p = 0.0022

## p-values With LRT:
mod_LRT <- mixed(mod, modData, method = "LRT", type = "III",
                 control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                 Df   Chisq Chi Df    Pr(>Chisq)    
# dwell_pun_abs_z 14  7.6678      1      0.005621 **  XXX
# reqAction_f     14 31.6698      1 0.00000001827 ***
# rewLeft_f       14  7.1059      1      0.007683 ** 

# ------------------------------------------------- #
## Automatized raincloud plot:
nPerc <- 3; selVar <- "dwell_pun_abs"; percVar <- paste0(selVar,"_",nPerc,"Perc");
nPerc <- 4; selVar <- "dwell_pun_abs"; percVar <- paste0(selVar,"_",nPerc,"Perc");
nPerc <- 5; selVar <- "dwell_pun_abs"; percVar <- paste0(selVar,"_",nPerc,"Perc");
# yLim = c(0.35, 0.80)

## Prepare data:
modData <- create_percentiles(modData,nPerc=nPerc,selVar)
tapply(modData$RT,modData[,percVar],mean,na.rm=T)
plotData <- modData[!(is.na(modData[,percVar])),]

png(paste0(plotDir,"raincloud_bar_RT_cleaned_",percVar,".png"),width = 480, height = 480)
plot_raincloud(modData, xVar = percVar, yVar = "RT_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0.35, 0.80),
               color = "#CC0000", Labels = "1",
               xLab = " Percentiles \n(Absolute dwell time on punishments)", yLab = "RT (in sec.)", Main = "Reaction times as a function of \n absolute dwell time on punishments (percentiles)")
dev.off()
png(paste0(plotDir,"raincloud_bar_RT_cleaned_",percVar,"_full.png"),width = 480, height = 480)
plot_raincloud(modData, xVar = percVar, yVar = "RT_cleaned",
               isBar = T, isPoint = T, isViolin = F, isBoxplot = F, isMean = F, 
               yLim = c(0, 1),
               color = "#CC0000", Labels = "1",
               xLab = " Percentiles \n(Absolute dwell time on punishments)", yLab = "RT (in sec.)", Main = "Reaction times as a function of \n absolute dwell time on punishments (percentiles)")
dev.off()

# ------------------------------------------------- #
## Plot based on regression:
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
## Plot regression line:
png(paste0(plotDir,"regression_RTcleaned_dwellpunabs_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_pun_abs", 
                     xLim = c(0,1500),
                     yLim = c(0, 1),
                     color = "#CC0000", margin = c(0,1,0,0),
                     xLab = "Punishment dwell time", yLab = "Reaction time", Main = "Reaction time as a function of \n punishment dwell time")
dev.off()

# ===================================================================================================================================== #
#### 11) RESPONSE ~ INTERACTION STAKES AND DWELL TIMES: ####

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
# (Intercept)               0.41505    0.05051   8.217 < 0.0000000000000002 ***
# difMag_z                  0.07494    0.03370   2.223               0.0262 *  
# dwell_out_dif_z           0.18266    0.03179   5.746        0.00000000915 ***
# reqAction_f1              1.28070    0.09284  13.795 < 0.0000000000000002 ***
# rewLeft_f1                0.01550    0.02429   0.638               0.5235    
# difMag_z:dwell_out_dif_z -0.02998    0.02301  -1.303               0.1925 XXX
# --> if anything negative effect: antagonistic: either strong stakes effect or strong dwell time effect, but not both?

plot(effect("difMag_z:dwell_out_dif_z",mod)) # positive stakes effect for attention to punishments; negative stakes effect for attention to rewards??? 
print_effect(mod,"difMag_z:dwell_out_dif_z") # b = -0.030, se = 0.023, z = -1.303, p = 0.1925

plot(effect("dwell_out_dif_z:difMag_z",mod)) # positive dwell time effect for low stakes, much weaker under high stakes? 

## p-values With LRT:
mod_LRT <- mixed(mod, modData, family = binomial(), method = "LRT", type = "III",
                 control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
anova(mod_LRT); beep()
#                          Df   Chisq Chi Df            Pr(>Chisq)    
# difMag_z                 26  4.6275      1               0.03146 *  
# dwell_out_dif_z          26 26.6384      1          0.0000002453 ***
# reqAction_f              26 88.6466      1 < 0.00000000000000022 ***
# rewLeft_f                26  0.3842      1               0.53535    
# difMag_z:dwell_out_dif_z 26  1.4484      1               0.22879 XXX

# END