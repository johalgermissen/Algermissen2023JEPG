#### 03_plots_paper.R

#' Execute to 
#' - create all plots presented in the main text and the supplementary material, 
#'   either for sample 1 or sample 2 (choose at the top). 
#' Requires that eyeData_processed.csv has been created previously by either executing 02_eyegaze_analyze_mixedmodels.R of the respective sample or using the version provided in this data sharing collection.
#' Adjust root directory to your own folder structure before running script.

# ================================================================================================================= #
#### Determine sample ID: ####

## Pick the sample ID (1 or 2) for which plots should be generated
sampleID <- 1
sampleName <- paste0("Sample ", sampleID) # name used as title for all plots

# ================================================================================================================= #
#### Set directories: ####

rootDir       <- "/project/2420093.01/" # adjust to your own folder structure before running script

codeDir       <- paste0(rootDir,"analyses/functions/")

dataDir       <- paste0(rootDir,"data/sample",sampleID,"/") 
processedDir  <- paste0(dataDir,"processedData/")

plotDir       <- paste0(rootDir,"data/plots/")
if(!dir.exists(plotDir)){dir.create(plotDir)}

# ================================================================================================================= #
#### Load packages and custom functions: ####

source(paste0(codeDir,"package_manager.R")) # Load functions
source(paste0(codeDir,"00_functions_analyze.R")) # Load functions

# ================================================================================================================= #
#### Read data (eyeData_processed saved previously): ####

## Read aggregated data from earlier:
eyeData <- read.csv(paste0(processedDir,"eyeData_processed.csv"))

## Pre-process again:
eyeData <- wrapper_preprocessing(eyeData) # refresh factors

# ================================================================================================================= #
#### 1a) Go/NoGo accuracy: ####

## Select data:
modData <- subset(eyeData, isCatch == 0)

GNG_aggr <- ddply(modData, .(subject), function(x){
  require(plyr)
  subject <- x$subject[1]
  ACC <- mean(x$ACC, na.rm = T)
  RT <- mean(x$RT, na.rm = T)
  return(data.frame(subject,ACC,RT))
  dev.off()})
round(stat.desc(GNG_aggr[,c("ACC","RT")]),2)

png(paste0(plotDir,"bar_GNGACC.png"),width = 480, height = 480)
custom_singlebar(GNG_aggr, var="ACC", yLim = c(0,1.0), color = "cornflowerblue",
                 xLab = "", yLab = "Accuracy", Main = sampleName)
dev.off()

# ================================================================================================================= #
#### 1b) Catch trial accuracy: ####

## Select data:
catchData <- subset(eyeData, isCatch == 1)

catchData_aggr <- ddply(catchData, .(subject), function(x){
  require(plyr)
  subject <- x$subject[1]
  ACC <- mean(x$ACC, na.rm = T)
  RT <- mean(x$RT, na.rm = T)
  return(data.frame(subject,ACC,RT))
  dev.off()})
round(stat.desc(catchData_aggr[,c("ACC","RT")]),2)

png(paste0(plotDir,"bar_catchACC.png"),width = 480, height = 480)
custom_singlebar(catchData_aggr, var="ACC", yLim = c(0,1.0), xLab = "", yLab = "Accuracy", Main = sampleName)
dev.off()

# ======================================================================================================================== #
#### 1c) Learning curves: ####

modData <- subset(eyeData, isCatch == 0)
modData <- as.data.frame(group_by(modData,subject,cue) %>%  mutate(counter = row_number())) # add cue repetition number
table(modData$counter) # check

## Plot as line plot with ggplot:
png(paste0(plotDir,"lineplot_gg_response_cleaned_counter_reqAction.png"),width = 480, height = 480)
custom_lineplot_gg(modData, xVar="counter", yVar="response_cleaned", zVar="reqAction_f",
                   subVar = "subject",
                   xLab = "Trial number", yLab = "p(Go)", main = sampleName,
                   selCol = c("red","blue"), selLineType = c(1,1),
                   SEweight = 1, yLim = c(0,1), savePNG = F, saveEPS = F)
dev.off()

# ================================================================================================================= #
#### 2a) Motivational bias: Rew > pun or Pun > rew effect on responses ####

## Create valence variable:
eyeData$valence <- as.numeric(eyeData$rewMag > eyeData$punMag)
eyeData$valence_f <- factor(eyeData$valence, levels = c(1,0), labels = c("reward","punishment")) # convert to factor

## Select MGNG task data:
modData <- subset(eyeData, isCatch == 0)
length(table(modData$subject))
table(modData$subject)

plotData <- eyeData[!(is.na(eyeData$valence))  & eyeData$isCatch == 0,] # discard NAs

## Bar plot:
png(paste0(plotDir,"bar_responsecleaned_reqAction_valence.png"),width = 480, height = 480)
custom_barplot2(plotData, xVar="reqAction_f", yVar="response_cleaned", zVar="valence_f", subVar="subject",
                xLab = "Required action", yLab = "p(Go)", zLab = "Valence", selCol = c("#009933","#CC0000"),
                isPoint = F, isBeeswarm = T, yLim = c(0,1), main = sampleName)
dev.off()

# ================================================================================================================= #
#### 2b) Continuous stake effect on responses: ####

## Select data:
modData <- subset(eyeData, isCatch == 0)

# ---------------------------------- #
#### Stake difference: ####

formula <- "response_cleaned ~ difMag + (difMag|subject)"
## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("difMag",mod)) # raw scale suggested

## Plot regression line:
png(paste0(plotDir,"regression_response_cleaned_difMag.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "difMag", 
                     xLim = c(-4,4),
                     yLim = c(0.25, 0.90),
                     color = "#FFC000", # Labels = c(1:5),
                     xLab = "Stake difference", yLab = "p(Go)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_response_cleaned_difMag_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "difMag", 
                     xLim = c(-4,4),
                     yLim = c(0, 1),
                     color = "#FFC000", # Labels = c(1:5),
                     xLab = "Stake difference", yLab = "p(Go)", Main = sampleName)
dev.off()

# ---------------------------------- #
#### Reward stake: ####

## Formula:
formula <- "response_cleaned ~ rewMag + (rewMag|subject)"
## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("rewMag",mod)) # raw scale suggested

# Plot regression line:
png(paste0(plotDir,"regression_response_cleaned_rewMag.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "rewMag", 
                     xLim = c(1,5),
                     yLim = c(0.25, 0.90),
                     color = "#009933", # Labels = c(1:5),
                     xLab = "Reward magnitude", yLab = "p(Go)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_response_cleaned_rewMag_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "rewMag", 
                     xLim = c(1,5),
                     yLim = c(0, 1),
                     color = "#009933", # Labels = c(1:5),
                     xLab = "Reward magnitude", yLab = "p(Go)", Main = sampleName)
dev.off()

# ---------------------------------- #
#### Punishment stake: ####

## Formula:
formula <- "response_cleaned ~ punMag + (punMag|subject)"
## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("punMag",mod)) # raw scale suggested

## Plot regression line:
png(paste0(plotDir,"regression_response_cleaned_punMag.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "punMag", 
                     xLim = c(1,5),
                     yLim = c(0.25, 0.90),
                     color = "#CC0000", 
                     xLab = "Punishment magnitude", yLab = "p(Go)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_response_cleaned_punMag_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "punMag", 
                     xLim = c(1,5),
                     yLim = c(0, 1),
                     color = "#CC0000", 
                     xLab = "Punishment magnitude", yLab = "p(Go)", Main = sampleName)
dev.off()

# ================================================================================================================= #
#### 2c) Continuous stake effect on RTs: ####

modData <- subset(eyeData, isCatch == 0)
modData$RT_cleaned_ms <- modData$RT_cleaned*1000

# ---------------------------------- #
#### Stake difference: ####

## Formula:
formula <- "RT_cleaned_ms ~ difMag + (difMag|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("difMag",mod)) # raw scale suggested

# Plot regression line:
png(paste0(plotDir,"regression_RT_cleaned_difMag.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "difMag", 
                     xLim = c(-4,4),
                     yLim = c(300, 600),
                     color = "#FFC000", # Labels = c(1:5),
                     xLab = "Stake difference", yLab = "Reaction time (ms)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_RT_cleaned_difMag_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "difMag", 
                     xLim = c(-4,4),
                     yLim = c(0, 1000),
                     color = "#FFC000", # Labels = c(1:5),
                     xLab = "Stake difference", yLab = "Reaction time (ms)", Main = sampleName)
dev.off()

# ---------------------------------- #
#### Reward stake: ####

## Formula:
formula <- "RT_cleaned_ms ~ rewMag + (rewMag|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa"))); summary(mod)
plot(effect("rewMag",mod)) # raw scale suggested

# Plot regression line:
png(paste0(plotDir,"regression_RT_cleaned_rewMag.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "rewMag", 
                     xLim = c(1,5),
                     yLim = c(300, 600),
                     color = "#009933", margin = c(0,1,0,0),
                     xLab = "Reward magnitude", yLab = "Reaction time (ms)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_RT_cleaned_rewMag_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "rewMag", 
                     xLim = c(1,5),
                     yLim = c(0, 1000),
                     color = "#009933", margin = c(0,1,0,0),
                     xLab = "Reward magnitude", yLab = "Reaction time (ms)", Main = sampleName)
dev.off()

# ---------------------------------- #
#### Punishment stake: ####

## Formula:
formula <- "RT_cleaned_ms ~ punMag + (punMag|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa"))); summary(mod)
plot(effect("punMag",mod)) # raw scale suggested

## Plot regression line:
png(paste0(plotDir,"regression_RT_cleaned_punMag.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "punMag", 
                     xLim = c(1,5),
                     yLim = c(300, 600),
                     color = "#CC0000", margin = c(0,1,0,0),
                     xLab = "Punishment magnitude", yLab = "Reaction time (ms)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_RT_cleaned_punMag_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "punMag", 
                     xLim = c(1,5),
                     yLim = c(0, 1000),
                     color = "#CC0000", margin = c(0,1,0,0),
                     xLab = "Punishment magnitude", yLab = "Reaction time (ms)", Main = sampleName)
dev.off()

# ================================================================================================================= #
#### 3a) Attention effect on responses: ####

## Select data:
modData <- subset(eyeData, isCatch == 0)

# ---------------------------------- #
#### Dwell time difference: ####

modData$dwell_out_dif <- modData$dwell_rew_abs - modData$dwell_pun_abs

## Formula:
formula <- "response_cleaned ~ dwell_out_dif + (dwell_out_dif|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("dwell_out_dif",mod)) # raw scale suggested

## Plot regression line:
png(paste0(plotDir,"regression_response_cleaned_dwelloutdif.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_out_dif", # not it's understandardized
                     xLim = c(-1500,1500),
                     c(0.07, 0.90),
                     color = "#7B30A0", margin = c(0,1,0,0),
                     xLab = "Dwell time difference (ms)", yLab = "p(Go)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_response_cleaned_dwelloutdif_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_out_dif", 
                     xLim = c(-1500,1500),
                     yLim = c(0, 1),
                     color = "#7B30A0", margin = c(0,1,0,0),
                     xLab = "Dwell time difference (ms)", yLab = "p(Go)", Main = sampleName)
dev.off()

# ---------------------------------- #
#### Dwell time ratio: ####

modData$dwell_rew_rel100 <- modData$dwell_rew_rel * 100

## Formula:
formula <- "response_cleaned ~ dwell_rew_rel100 + (dwell_rew_rel100|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("dwell_rew_rel100",mod)) # raw scale suggested

## Plot regression line:
png(paste0(plotDir,"regression_response_cleaned_dwell_rew_rel.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_rel100", 
                       xLim = c(0,100),
                       yLim = c(0.07, 0.90),
                       color = "#7B30A0", margin = c(0,1,0,0),
                       xLab = "Relative dwell time on rewards (%)", yLab = "p(Go)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_response_cleaned_dwell_rew_rel_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_rel100", 
                       xLim = c(0,100),
                       yLim = c(0, 1),
                       color = "#7B30A0", margin = c(0,1,0,0),
                       xLab = "Relative dwell time on rewards (%)", yLab = "p(Go)", Main = sampleName)
dev.off()

# ---------------------------------- #
#### Reward dwell time: ####

## Formula:
formula <- "response_cleaned ~ dwell_rew_abs + (dwell_rew_abs|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("dwell_rew_abs",mod)) # raw scale suggested

# Plot regression line:
png(paste0(plotDir,"regression_response_cleaned_dwellrewabs.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_abs", 
                     xLim = c(0,1500),
                     yLim = c(0.07, 0.90), # yLim = c(0.25, 0.90),
                     color = "#009933", margin = c(0,1,0,0),
                     xLab = "Reward dwell time (ms)", yLab = "p(Go)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_response_cleaned_dwellrewabs_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_abs", 
                     xLim = c(0,1500),
                     yLim = c(0, 1),
                     color = "#009933", margin = c(0,1,0,0),
                     xLab = "Reward dwell time (ms)", yLab = "p(Go)", Main = sampleName)
dev.off()

# ---------------------------------- #
#### Punishment dwell time: ####

## Formula:
formula <- "response_cleaned ~ dwell_pun_abs + (dwell_pun_abs|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("dwell_pun_abs",mod)) # raw scale suggested

## Plot regression line:
png(paste0(plotDir,"regression_response_cleaned_dwellpunabs.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_pun_abs", # useEffect = F, xVec = c(0, 5000),
                     xLim = c(0,1500),
                     yLim = c(0.07, 0.90),
                     color = "#CC0000", margin = c(0,1,0,0),
                     xLab = " Punishment dwell time (ms)", yLab = "p(Go)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_response_cleaned_dwellpunabs_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_pun_abs", # useEffect = F, xVec = c(0, 5000),
                     xLim = c(0,1500),
                     yLim = c(0, 1),
                     color = "#CC0000", margin = c(0,1,0,0),
                     xLab = " Punishment dwell time (ms)", yLab = "p(Go)", Main = sampleName)
dev.off()

# ================================================================================================================= #
#### 3b) Attention effect of RTs: ####

modData <- subset(eyeData, isCatch == 0)
modData$RT_cleaned_ms <- modData$RT_cleaned*1000

# ---------------------------------- #
#### Dwell time difference: ####

modData$dwell_out_dif <- modData$dwell_rew_abs - modData$dwell_pun_abs # unstandardized

## Formula:
formula <- "RT_cleaned_ms ~ dwell_out_dif + (dwell_out_dif|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("dwell_out_dif",mod)) # raw scale suggested

# Plot regression line:
png(paste0(plotDir,"regression_RT_cleaned_dwelloutdif.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_out_dif", 
                     xLim = c(-1500,1500),
                     yLim = c(300, 600),
                     color = "#7B30A0", margin = c(0,1,0,0),
                     xLab = "Dwell time difference (ms)", yLab = "Reaction time (ms)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_RT_cleaned_dwelloutdif_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_out_dif", 
                     xLim = c(-1500,1500),
                     yLim = c(0, 1000),
                     color = "#7B30A0", margin = c(0,1,0,0),
                     xLab = "Dwell time difference (ms)", yLab = "Reaction time (sec.)", Main = sampleName)
dev.off()

# ---------------------------------- #
#### Dwell time ratio: ####

modData$dwell_rew_rel100 <- modData$dwell_rew_rel * 100

## Formula:
formula <- "RT_cleaned_ms ~ dwell_rew_rel100 + (dwell_rew_rel100|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("dwell_rew_rel100",mod)) # raw scale suggested

# Plot regression line:
png(paste0(plotDir,"regression_RT_cleaned_dwellrewrel.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_rel100", 
                       xLim = c(0,100),
                       yLim = c(300, 600),
                       color = "#7B30A0", margin = c(0,1,0,0),
                       xLab = "Relative dwell time on rewards (%)", yLab = "Reaction time (ms)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_RT_cleaned_dwellrewrel_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_rel100", 
                       xLim = c(0,100),
                       yLim = c(0, 1000),
                       color = "#7B30A0", margin = c(0,1,0,0),
                       xLab = "Relative dwell time on rewards (%)", yLab = "Reaction time (sec.)", Main = sampleName)
dev.off()

# ---------------------------------- #
#### Reward dwell time: ####

formula <- "RT_cleaned_ms ~ dwell_rew_abs + (dwell_rew_abs|subject)"

# Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa"))); summary(mod)
plot(effect("dwell_rew_abs",mod)) # raw scale suggested

# Plot regression line:
png(paste0(plotDir,"regression_RTcleaned_dwellrewabs.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_abs", 
                     xLim = c(0,1500),
                     yLim = c(300, 600),
                     # yLim = c(0, 1),
                     color = "#009933", margin = c(0,1,0,0),
                     xLab = "Reward dwell time (ms)", yLab = "Reaction time (ms)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_RTcleaned_dwellrewabs_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_rew_abs", 
                     xLim = c(0,1500),
                     yLim = c(0, 1000),
                     color = "#009933", margin = c(0,1,0,0),
                     xLab = "Reward dwell time (ms)", yLab = "Reaction time (ms)", Main = sampleName)
dev.off()

# ---------------------------------- #
#### Punishment dwell time: ####

formula <- "RT_cleaned_ms ~ dwell_pun_abs + (dwell_pun_abs|subject)"
## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa"))); summary(mod)
plot(effect("dwell_pun_abs",mod)) # raw scale suggested

## Plot regression line:
png(paste0(plotDir,"regression_RTcleaned_dwellpunabs.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_pun_abs", 
                     xLim = c(0,1500),
                     yLim = c(300, 600),
                     color = "#CC0000", margin = c(0,1,0,0),
                     xLab = "Punishment dwell time (ms)", yLab = "Reaction time (ms)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_RTcleaned_dwellpunabs_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "dwell_pun_abs", 
                     xLim = c(0,1500),
                     yLim = c(0, 1000),
                     color = "#CC0000", margin = c(0,1,0,0),
                     xLab = "Punishment dwell time (ms)", yLab = "Reaction time (ms)", Main = sampleName)
dev.off()

# ================================================================================================================= #
#### 4a) Required action on first fixation: ####

## Select data:
modData <- eyeData

## Formula:
formula <- "firstfix_out_f ~ reqAction_f + (reqAction_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
plot(effect("reqAction_f",mod)) # raw scale suggestedplot(effect("reqAction_f",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_firstfixout_reqAction.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "reqAction_f", # 
                    # yLim = c(0,1), 
                    yLim = c(0.25,0.9), 
                    color = c("red","blue"), Labels = c("Go","NoGo"), z = 1.96,
                    xLab = "Required Action", yLab = "p(first fixation on reward)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_firstfixout_reqAction_full.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "reqAction_f", # 
                    yLim = c(0,1),
                    color = c("red","blue"), Labels = c("Go","NoGo"), z = 1.96,
                    xLab = "Required Action", yLab = "p(first fixation on reward)", Main = sampleName)
dev.off()

# ================================================================================================================= #
#### 4b) Q-value on first fixation: ####

## Select data:
modData <- eyeData

## Formula:
formula <- "firstfix_out_n ~ Qdif + rewLeft_f + (Qdif + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("Qdif",mod)) # raw scale suggested

# Plot regression line:
png(paste0(plotDir,"regression_firstfixout_nQdif.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif", 
                     xLim = c(-2,2),
                     yLim = c(0.3,0.95), 
                     color = "orchid", 
                     xLab = "Q(Go) - Q(NoGo)", yLab = "p(first fixation on reward)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_firstfixout_nQdif_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif", 
                     xLim = c(-2,2),
                     yLim = c(0,1), 
                     color = "orchid", 
                     xLab = "Q(Go) - Q(NoGo)", yLab = "p(first fixation on reward)", Main = sampleName)
dev.off()

# ================================================================================================================= #
#### 4c) Required action on dwell time difference: ####

## Select data:
modData <- eyeData

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
                      yLim = c(-40,1000),
                      color = c("red","blue"), Labels = c("Go","NoGo"), z = 1.96,
                      xLab = "Required Action", yLab = "Dwell time difference (ms)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_dwelloutdif_reqAction_full.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "reqAction_f", # 
                      yLim = c(-1500,1500),
                      color = c("red","blue"), Labels = c("Go","NoGo"), z = 1.96,
                      xLab = "Required Action", yLab = "Dwell time difference (ms)", Main = sampleName)
dev.off()

# ================================================================================================================= #
#### 4d) Required action on dwell time ratio: ####

## Select data:
modData <- eyeData
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
                    color = c("red","blue"), Labels = c("Go","NoGo"), z = 1.96,
                    xLab = "Required Action", yLab = "Relative reward dwell time (%)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_dwellrewrel_reqAction_full.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "reqAction_f", # 
                    yLim = c(0,100),
                    color = c("red","blue"), Labels = c("Go","NoGo"), z = 1.96,
                    xLab = "Required Action", yLab = "Relative reward dwell time (%)", Main = sampleName)
dev.off()

a# ================================================================================================================= #
#### 4e) Q-value on dwell time difference: ####

## Select data:
modData <- eyeData

## Formula:
formula <- "dwell_out_dif ~ Qdif + rewLeft_f + (Qdif + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
plot(effect("Qdif",mod)) # raw scale suggested

# Plot regression line:
png(paste0(plotDir,"regression_dwelloutdif_Qdif.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif", 
                       xLim = c(-2,2),
                       yLim = c(-40,1000),
                       color = "orchid", 
                       xLab = "Q(Go) - Q(NoGo)", yLab = "Dwell time difference (ms)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_dwelloutdif_Qdif_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif", 
                       xLim = c(-2,2),
                       yLim = c(-1500,1500), 
                       color = "orchid", 
                       xLab = "Q(Go) - Q(NoGo)", yLab = "Dwell time difference (ms)", Main = sampleName)
dev.off()

# ================================================================================================================= #
#### 4f) Q-value on dwell time ratio: ####

## Select data:
modData <- eyeData
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
                     xLab = "Q(Go) - Q(NoGo)", yLab = "Relative reward dwell time (%)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_dwellrewrel_Qdif_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "Qdif", 
                     xLim = c(-2,2),
                     yLim = c(0,100), 
                     color = "orchid", 
                     xLab = "Q(Go) - Q(NoGo)", yLab = "Relative reward dwell time (%)", Main = sampleName)
dev.off()

# ================================================================================================================= #
#### 5a) First fixation on dwell time difference: ####

## Select data:
modData <- eyeData
modData$firstfix_out_rev_f <- factor(ifelse(modData$firstfix_out_f=="reward","A",
                                            ifelse(modData$firstfix_out_f=="punishment","B",NA)))

## Formula:
formula <- "dwell_out_dif ~ firstfix_out_rev_f + rewLeft_f + (firstfix_out_rev_f + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
             control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
plot(effect("firstfix_out_rev_f",mod)) # raw scale suggestedplot(effect("reqAction_f",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_dwelloutdif_firstfixoutf.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "firstfix_out_rev_f", # 
                      yLim = c(-300,800),
                      color = c("#009933","#CC0000"), Labels = c("Reward","Punishment"), z = 1.96,
                      xLab = "First fixation", yLab = "Dwell time difference (ms)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_dwelloutdif_firstfixoutf_full.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "firstfix_out_rev_f", # 
                      yLim = c(-1500,1500),
                      color = c("#009933","#CC0000"), Labels = c("Reward","Punishment"), z = 1.96,
                      xLab = "First fixation", yLab = "Dwell time difference (ms)", Main = sampleName)
dev.off()

# ================================================================================================================= #
#### 5b) First fixation on dwell time ratio: ####

## Select data:
modData <- eyeData
modData$dwell_rew_rel100 <- modData$dwell_rew_rel * 100
modData$firstfix_out_rev_f <- factor(ifelse(modData$firstfix_out_f=="reward","A",
                                            ifelse(modData$firstfix_out_f=="punishment","B",NA)))

## Formula:
formula <- "dwell_rew_rel100 ~ firstfix_out_rev_f + rewLeft_f + (firstfix_out_rev_f + rewLeft_f|subject)"

## Fit model:
mod <- lmer(formula = formula, data = modData,
            control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod)
plot(effect("firstfix_out_rev_f",mod)) # raw scale suggestedplot(effect("reqAction_f",mod), rescale.axis=F, ylim = c(0,1)) # 0-1 enforced

## Plot regression line:
png(paste0(plotDir,"regression_dwellrewrel_firstfixoutf.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "firstfix_out_rev_f", # 
                      yLim = c(24,100),
                      color = c("#009933","#CC0000"), Labels = c("Reward","Punishment"), z = 1.96,
                      xLab = "First fixation", yLab = "Relative reward dwell time (%)", Main = sampleName)
dev.off()

png(paste0(plotDir,"regression_dwellrewrel_firstfixoutf_full.png"),width = 480, height = 480)
custom_regressionbar1(mod, selEff = "firstfix_out_rev_f", # 
                      yLim = c(0,100),
                      color = c("#009933","#CC0000"), Labels = c("Reward","Punishment"), z = 1.96,
                      xLab = "First fixation", yLab = "Relative reward dwell time (%)", Main = sampleName)
dev.off()

library(beepr)
beep()
cat("Finished :-) \n")

# END