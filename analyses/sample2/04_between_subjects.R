#### 04_between_subjects.R

#' Execute to 
#' - create fit stakes and attentional models to all subjects of both samples, 
#' - correlate per-subject fixed + random effect to task accuracy, 
#' - create scatter plots,
#' - perform control analyses with other fixation measures.
#' Requires that eyeData_processed.csv has been created previously by either executing 02_eyegaze_analyze_mixedmodels.R of the respective sample or using the version provided in this data sharing collection.
#' Adjust root directory to your own folder structure before running script.

# ================================================================================================================= #
#### Set directories: ####

rootDir       <- "/project/2420093.01/" # adjust to your own folder structure before running script

codeDir       <- paste0(rootDir,"analyses/functions/")

dataDir       <- paste0(rootDir,"data/") 
sample1Dir    <- paste0(dataDir,"sample1/processedData/")
sample2Dir    <- paste0(dataDir,"sample2/processedData/")

plotDir       <- paste0(rootDir,"data/plots/")
if(!dir.exists(plotDir)){dir.create(plotDir)}

# ================================================================================================================= #
#### Load packages and custom functions: ####

source(paste0(codeDir,"package_manager.R")) # Load functions
source(paste0(codeDir,"00_functions_analyze.R")) # Load functions

# ================================================================================================================= #
#### Read in data: ####

selVars <- c("subject","trialnr","rewLeft","rewLeft_f","reqAction","reqAction_f",
             "rewMag","punMag","difMag",
             "response_cleaned","ACC","RT_cleaned","outcome","isCatch",
             "QGo","QNoGo","Qdif",
             "dwell_rew_abs","dwell_pun_abs","dwell_rew_rel","firstfix_out_n","firstfix_out_f")

## Sample 1:
eyeData1 <- read.csv(paste0(sample1Dir,"eyeData_processed.csv"))
eyeData1 <- wrapper_preprocessing(eyeData1) # refresh factors
eyeData1 <- eyeData1[,selVars] # select variables

## Sample 2:
eyeData2 <- read.csv(paste0(sample2Dir,"eyeData_processed.csv"))
eyeData2 <- wrapper_preprocessing(eyeData2) # refresh factors
eyeData2$subject <- eyeData2$subject + 35 # update subject number
eyeData2 <- eyeData2[,selVars] # select variables

## Concatenate data from both samples:
eyeData <- rbind(eyeData1,eyeData2)
table(eyeData$subject)
names(eyeData)

## Create vector identifying sample (1 or 2) per subject for plots:
sampleID <- c(rep(1,length(unique(eyeData1$subject))), rep(2,length(unique(eyeData2$subject))))

# ================================================================================================================= #
#### 1) Go/NoGo task accuracy: ####

modData <- subset(eyeData, isCatch == 0)

ACCvec <- tapply(modData$ACC,modData$subject,mean)
  
# ================================================================================================================= #
#### 2) Stakes effect: ####

# ==================================================== #
#### 2a) Stakes difference: ####

## Select data:
modData <- subset(eyeData, isCatch == 0)
modData$difMag <- modData$rewMag - modData$punMag
modData$difMag_z <- as.numeric(scale(modData$difMag))

# Formula:
formula <- "response_cleaned ~ difMag_z + reqAction_f + rewLeft_f + (difMag_z + reqAction_f + rewLeft_f|subject)"

# Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
beep(); summary(mod)
plot(effect("difMag_z",mod)) # raw scale suggested

# ---------------------------- #
## Extract subject-specific regression coefficient:
stakesEff <- coef(mod)$subject[,2]

# Correlation:
cor(ACCvec,stakesEff) # -0.2398591
rcor.test(cbind(ACCvec,stakesEff))
# ACCvec     ***** -0.240   
# stakesEff  0.017  ***** 
## --> negative: stakes effect hurts performance

# ==================================================== #
#### 2b) Delete outliers:

sort(stakesEff)
which(stakesEff==min(stakesEff)) # 39, i.e. 4 of Study 2
which(stakesEff==max(stakesEff)) # 36, i.e. 1 of Study 2

stakesEff2 <- stakesEff # copy
stakesEff2[which(stakesEff==min(stakesEff))] <- NA # delete minimum
stakesEff2[which(stakesEff==max(stakesEff))] <- NA # delete maximum
valIdx <- !(is.na(stakesEff2)) # indices of remaining subjects

# Correlation:
cor(ACCvec[valIdx],stakesEff2[valIdx], use = "complete.obs") # -0.2606651
rcor.test(cbind(ACCvec[valIdx],stakesEff2[valIdx]))
# [1,]  ***** -0.261
# [2,]  0.010  *****

# ==================================================== #
#### 2c) Reward stakes: ####

## Select data:
modData <- subset(eyeData, isCatch == 0)
modData$rewMag_z <- as.numeric(scale(modData$rewMag))

## Formula:
formula <- "response_cleaned ~ rewMag_z + reqAction_f + rewLeft_f + (rewMag_z + reqAction_f + rewLeft_f|subject)"

# Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
beep(); summary(mod)
plot(effect("rewMag_z",mod)) # raw scale suggested

# ---------------------------- #
## Extract subject-specific regression coefficient:
stakesRewEff <- coef(mod)$subject[,2]

# Correlation:
cor(ACCvec,stakesRewEff) # -0.3817321
rcor.test(cbind(ACCvec,stakesRewEff))
#               ACCvec stakesRewEff
# ACCvec        ***** -0.382      
# stakesRewEff <0.001  *****      
## --> negative: stakes effect hurts performance

# ==================================================== #
#### 2d) Punishment stakes: ####

## Select data:
modData <- subset(eyeData, isCatch == 0)
modData$punMag_z <- as.numeric(scale(modData$punMag))

## Formula:
formula <- "response_cleaned ~ punMag_z + reqAction_f + rewLeft_f + (punMag_z + reqAction_f + rewLeft_f|subject)"

# Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
beep(); summary(mod)
plot(effect("punMag_z",mod)) # raw scale suggested

# ---------------------------- #
## Extract subject-specific regression coefficient:
stakesPunEff <- coef(mod)$subject[,2]

# Correlation:
cor(ACCvec,stakesPunEff) # 0.1653217
rcor.test(cbind(ACCvec,stakesPunEff))
#               ACCvec stakesPunEff
# ACCvec        *****  0.165      
# stakesPunEff  0.102  *****
## --> negative: positive effect helps performance?

# ================================================================================================================= #
#### 3) Attentional effects: ####

# ==================================================== #
#### 3a) Dwell time difference: ####

## Select data:
modData <- subset(eyeData, isCatch == 0)

## Compute dwell tiem difference variable:
modData$dwell_out_dif <- modData$dwell_rew_abs - modData$dwell_pun_abs
modData$dwell_out_dif_z <- as.numeric(scale(modData$dwell_out_dif))

## Formula:
formula <- "response_cleaned ~ dwell_out_dif_z + reqAction_f + rewLeft_f + (dwell_out_dif_z + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
beep(); summary(mod)
plot(effect("dwell_out_dif_z",mod)) # raw scale suggested

# ---------------------------- #
## Extract subject-specific regression coefficient:
dwellDifEff <- coef(mod)$subject[,2]

# Correlation:
cor(ACCvec,dwellDifEff) # 0.453 
rcor.test(cbind(ACCvec,dwellDifEff))
#             ACCvec dwellDifEff
# ACCvec       *****  0.453     
# dwellDifEff <0.001  *****     
  ## --> positive: dwell time effect helps performance

# ==================================================== #
#### 3b) Dwell time ratio: ####

## Select data:
modData <- subset(eyeData, isCatch == 0)
modData$dwell_rew_rel_z <- as.numeric(scale(modData$dwell_rew_rel))

## Formula:
formula <- "response_cleaned ~ dwell_rew_rel_z + reqAction_f + rewLeft_f + (dwell_rew_rel_z + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
beep(); summary(mod)
plot(effect("dwell_rew_rel_z",mod)) # raw scale suggested

# ---------------------------- #
## Extract subject-specific regression coefficient:
dwellRatEff <- coef(mod)$subject[,2]

# Correlation:
cor(ACCvec,dwellRatEff) # 0.235  
rcor.test(cbind(ACCvec,dwellRatEff))
# ACCvec    *****  0.235  
# dwellEff  0.019  *****  
## --> positive: dwell time effect helps performance

# ==================================================== #
#### 3c) Reward dwell time: ####

## Select data:
modData <- subset(eyeData, isCatch == 0)
modData$dwell_rew_abs_z <- as.numeric(scale(modData$dwell_rew_abs))

## Formula:
formula <- "response_cleaned ~ dwell_rew_abs_z + reqAction_f + rewLeft_f + (dwell_rew_abs_z + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
beep(); summary(mod)
plot(effect("dwell_rew_abs_z",mod)) # raw scale suggested

# ---------------------------- #
## Extract subject-specific regression coefficient:
dwellRewEff <- coef(mod)$subject[,2]

# Correlation:
cor(ACCvec,dwellRewEff) # 0.2349428  
rcor.test(cbind(ACCvec,dwellRewEff))
#              ACCvec dwellRatEff
# ACCvec       *****  0.253     
# dwellRatEff  0.011  *****  
## --> positive: dwell time effect helps performance

# ==================================================== #
#### 3d) Punishment dwell time: ####

## Select data:
modData <- subset(eyeData, isCatch == 0)
modData$dwell_pun_abs_z <- as.numeric(scale(modData$dwell_pun_abs))

## Formula:
formula <- "response_cleaned ~ dwell_pun_abs_z + reqAction_f + rewLeft_f + (dwell_pun_abs_z + reqAction_f + rewLeft_f|subject)"

## Fit model:
mod <- glmer(formula = formula, data = modData, family = binomial(),
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
beep(); summary(mod)
plot(effect("dwell_pun_abs_z",mod)) # raw scale suggested

# ---------------------------- #
## Extract subject-specific regression coefficient:
dwellPunEff <- coef(mod)$subject[,2]

# Correlation:
cor(ACCvec,dwellPunEff) # 0.235  
rcor.test(cbind(ACCvec,dwellPunEff))
#              ACCvec dwellPunEff
# ACCvec       ***** -0.332     
# dwellPunEff  0.001  *****  
## --> negative: dwell time effect hurts performance

# ================================================================================================================= #
#### 4) Plot scatterplots with regression line using ggplot: ####

# http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
library(ggplot2)

# Sample ID to determine shape of individual data points:
sampleID <- c(rep(1,length(unique(eyeData1$subject))), rep(2,length(unique(eyeData2$subject))))

## Plot for 2 plots next to each other:
plot_correlation_2 <- function(eff, xVar, xLab){
  fontSize <- 10 # for 4 plots next to each other
  # png(paste0(plotDir,"ggplot_correlation_ACC_",xVar,"_choice_",fontSize,".png"),
  #     width = 480, height = 320) # fontSize <- 17
  tiff(paste0(plotDir,"ggplot_correlation_ACC_",xVar,"_choice_",fontSize,".tiff"),
       width = 4, height = 3, units = 'in', res = 300) # fontSize <- 10
  corData <- data.frame((cbind(ACCvec,eff, sampleID))) # extract data
  p <- ggplot(corData, aes(x=eff, y=ACCvec)) +
    geom_point(shape = sampleID, size = 1, stroke = 0.8, fill = "white") + 
    geom_smooth(method=lm, se=T, col = "red", size = 2) + 
    coord_cartesian(ylim=c(0.3, 1)) +
    geom_vline(xintercept = 0, linetype = 2, color = "black", size = 1) + 
    xlab(xLab) + ylab("Accuracy") +
    theme_classic() + 
    theme(plot.margin = unit(c(0.5,1.0,0.5,0.5), "cm"), # top, right, bottom, left
          axis.text = element_text(size=fontSize, color = "black"),
          axis.title = element_text(size=fontSize, color = "black"), 
          plot.title = element_text(size=fontSize, color = "black", hjust = 0.5), # center title 
          legend.text = element_text(size=fontSize, color = "black"),
          axis.line = element_line(colour = 'black', size = 1))
  print(p)
  dev.off()
}

# Plots for main paper (bigger):
eff <- stakesEff2; xVar <- "difMag2"; xLab <- "Effect of stake difference on responses (b weight)"; plot_correlation_2(eff, xVar, xLab)
eff <- dwellDifEff; xVar <- "dwelloutdif"; xLab <- "Effect of dwell time difference on responses (b weight)"; plot_correlation_2(eff, xVar, xLab)

## Plot for 4 plots next to each other:
plot_correlation_4 <- function(eff, xVar, xLab){
  fontSize <- 15 # for 4 plots next to each other
  # png(paste0(plotDir,"ggplot_correlation_ACC_",xVar,"_choice_",fontSize,".png"),
  #     width = 480, height = 480)
  tiff(paste0(plotDir,"ggplot_correlation_ACC_",xVar,"_choice_",fontSize,".tiff"),
       width = 4, height = 4, units = 'in', res = 300) # fontSize <- 10
  corData <- data.frame((cbind(ACCvec,eff,sampleID))) # extract data
  p <- ggplot(corData, aes(x=eff, y=ACCvec)) +
    geom_point(shape = sampleID, size = 2, stroke = 1, fill = "white") + 
    geom_smooth(method=lm, se=T, col = "red", size = 2) + 
    coord_cartesian(ylim=c(0.3, 1)) +
    geom_vline(xintercept = 0, linetype = 2, color = "black", size = 1) + 
    xlab(xLab) + ylab("Accuracy") +
    theme_classic() + 
    theme(plot.margin = unit(c(0.5,1.0,0.5,0.5), "cm"), # top, right, bottom, left
          axis.text = element_text(size=fontSize, color = "black"),
          axis.title = element_text(size=fontSize, color = "black"), 
          plot.title = element_text(size=fontSize, color = "black", hjust = 0.5), # center title 
          legend.text = element_text(size=fontSize, color = "black"),
          axis.line = element_line(colour = 'black', size = 1))
  print(p)
  dev.off()
}

# Select data & plot for smaller plots (supplementary material):
eff <- stakesEff; xVar <- "difMag"; xLab <- "Effect of stake difference \non responses (b weight)"; plot_correlation_4(eff, xVar, xLab)
eff <- stakesEff2; xVar <- "difMag2"; xLab <- "Effect of stake difference \non responses (b weight)"; plot_correlation_4(eff, xVar, xLab)
eff <- stakesRewEff; xVar <- "rewMag"; xLab <- "Effect of reward magnitude \non responses (b weight)"; plot_correlation_4(eff, xVar, xLab)
eff <- stakesPunEff; xVar <- "punMag"; xLab <- "Effect of punishment magnitude \non responses (b weight)"; plot_correlation_4(eff, xVar, xLab)

eff <- dwellDifEff; xVar <- "dwelloutdif"; xLab <- "Effect of dwell time difference \non responses (b weight)"; plot_correlation_4(eff, xVar, xLab)
eff <- dwellRatEff; xVar <- "dwellrewrel"; xLab <- "Effect of dwell time ratio \non responses (b weight)"; plot_correlation_4(eff, xVar, xLab)
eff <- dwellRewEff; xVar <- "dwellrewabs"; xLab <- "Effect of reward dwell time \non responses (b weight)"; plot_correlation_4(eff, xVar, xLab)
eff <- dwellPunEff; xVar <- "dwellpunabs"; xLab <- "Effect of punishment dwell time \non responses (b weight)"; plot_correlation_4(eff, xVar, xLab)

# ================================================================================================================= #
#### 5) Link attention effects to data quality/ task engagement: ####

cor(dwellDifEff, ACCvec) # 0.4533918 # as above

# ------------------------------------------------------------ #
#### 5a) Go/NoGo accuracy ~ number trials with any fixation: ####

modData <- eyeData # select all trials
modData$any_fix <- ifelse(is.na(modData$firstfix_out_f),0,1) # any fixation per trial or not
anyFix <- tapply(modData$any_fix, modData$subject, sum, na.rm = T) # sum up all dwell times across trials
stat.desc(anyFix)
densityplot(anyFix)

## Accuracy and any fixation:
cor(ACCvec, anyFix) # 0.2251721 --> positive correlation
rcor.test(cbind(ACCvec, anyFix))
#         ACCvec anyFix
# ACCvec  *****  0.225
# anyFix  0.025  *****
  
## Attentional effect and any fixation:
cor(dwellDifEff, anyFix) # 0.1275929 --> no correlation
rcor.test(cbind(dwellDifEff, anyFix))
#             dwellDifEff anyFix
# dwellDifEff  *****       0.128
# anyFix       0.208       *****
  
## Linear regression with both:
summary(lm(dwellDifEff ~ ACCvec + anyFix))
#               Estimate Std. Error t value   Pr(>|t|)    
# (Intercept) -0.1542819  0.1243849  -1.240      0.218    
# ACCvec       0.3918972  0.0817593   4.793 0.00000598 ***
# anyFix       0.0001397  0.0004852   0.288      0.774 

stat.desc(anyFix)
eff <- anyFix; xVar <- "anyFix"; xLab <- "Number trials with any fixation"; 

fontSize <- 20 # for 4 plots next to each other
corData <- data.frame((cbind(ACCvec, eff, sampleID))) # extract data
p <- ggplot(corData, aes(x=eff, y=ACCvec)) +
  geom_point(shape = sampleID, size = 1, stroke = 0.8, fill = "white") + 
  geom_smooth(method=lm, se=T, col = "red", size = 2) + 
  coord_cartesian(xlim=c(125, 264), ylim=c(0.3, 1)) +
  xlab(xLab) + ylab("Accuracy") +
  theme_classic() + 
  theme(plot.margin = unit(c(0.5,1.0,0.5,0.5), "cm"), # top, right, bottom, left
        axis.text = element_text(size=fontSize, color = "black"),
        axis.title = element_text(size=fontSize, color = "black"), 
        plot.title = element_text(size=fontSize, color = "black", hjust = 0.5), # center title 
        legend.text = element_text(size=fontSize, color = "black"),
        axis.line = element_line(colour = 'black', size = 1))
print(p)

# ------------------------------------------------------------ #
#### 5b) Go/NoGo accuracy ~ total dwell time (sum of both stakes): ####

modData <- eyeData # select all data
modData$dwell_sum <- modData$dwell_rew_abs + modData$dwell_pun_abs # sum up both dwell times
dwellSum <- tapply(modData$dwell_sum, modData$subject, sum, na.rm = T) # sum up all dwell times across trials
stat.desc(dwellSum)
densityplot(dwellSum)

## Accuracy and total fixation time:
cor(ACCvec, dwellSum) # 0.08745385 --> no correlation
rcor.test(cbind(ACCvec, dwellSum))
#           ACCvec dwellSum
# ACCvec    *****  0.087  
# dwellSum  0.389  ***** 
  
## Attentional effect and total fixation time:
cor(dwellDifEff, dwellSum) # 0.1348124 --> no correlation
rcor.test(cbind(dwellDifEff, dwellSum))
#             dwellDifEff dwellSum
# dwellDifEff  *****       0.135  
# dwellSum     0.183       ***** 
  
## Linear regression with both:
summary(lm(dwellDifEff ~ ACCvec + dwellSum))
#                  Estimate    Std. Error t value   Pr(>|t|)    
# (Intercept) -0.1607130698  0.0683026125  -2.353     0.0207 *  
# ACCvec       0.3898494874  0.0795396794   4.901 0.00000386 ***
# dwellSum     0.0000001697  0.0000001607   1.056     0.2935    

## Plot:
eff <- dwellSum; xVar <- "dwellSum"; xLab <- "Total dwell time any stakes (ms)"; 

fontSize <- 20 # for 4 plots next to each other
corData <- data.frame((cbind(ACCvec, eff, sampleID))) # extract data
p <- ggplot(corData, aes(x=eff, y=ACCvec)) +
  geom_point(shape = sampleID, size = 1, stroke = 0.8, fill = "white") + 
  geom_smooth(method=lm, se=T, col = "red", size = 2) + 
  coord_cartesian(xlim=c(0, 600000), ylim=c(0.3, 1)) +
  xlab(xLab) + ylab("Accuracy") +
  theme_classic() + 
  theme(plot.margin = unit(c(0.5,1.0,0.5,0.5), "cm"), # top, right, bottom, left
        axis.text = element_text(size=fontSize, color = "black"),
        axis.title = element_text(size=fontSize, color = "black"), 
        plot.title = element_text(size=fontSize, color = "black", hjust = 0.5), # center title 
        legend.text = element_text(size=fontSize, color = "black"),
        axis.line = element_line(colour = 'black', size = 1))
print(p)

# ================================================================================================================= #
#### 6) Link overall reward/punishment fixation bias linked to performance? ####

# --------------------------------------------------------- #
#### 6a) Go/NoGo accuracy ~ number first fixations on rewards/ punishments: ####

modData <- eyeData # select all data

## Aggregate per subject:
firstfix <- tapply(modData$firstfix_out_n, modData$subject, mean, na.rm = T)
stat.desc(firstfix)
densityplot(firstfix)

## Accuracy and overall first fixation pattern:
cor(ACCvec,firstfix) # -0.1056725
rcor.test(cbind(ACCvec,firstfix))
#             ACCvec firstfix
# ACCvec      ***** -0.106    
# firstfix    0.298  *****    
  
## Linear regression with both:
summary(lm(dwellDifEff ~ ACCvec + firstfix))
#             Estimate Std. Error t value   Pr(>|t|)    
# (Intercept) -0.13182    0.08246  -1.599      0.113    
# ACCvec       0.39853    0.08013   4.973 0.00000288 ***
# firstfix     0.01390    0.08840   0.157      0.875      

## Plot:
eff <- firstfix; xVar <- "firstfix"; xLab <- "Overall fixation bias to rewards"; 

fontSize <- 20 # for 4 plots next to each other
corData <- data.frame((cbind(ACCvec,eff, sampleID))) # extract data
p <- ggplot(corData, aes(x=eff, y=ACCvec)) +
  geom_point(shape = sampleID, size = 1, stroke = 0.8, fill = "white") + 
  geom_smooth(method=lm, se=T, col = "red", size = 2) + 
  coord_cartesian(xlim=c(0.4, 1), ylim=c(0.3, 1)) +
  xlab(xLab) + ylab("Accuracy") +
  theme_classic() + 
  theme(plot.margin = unit(c(0.5,1.0,0.5,0.5), "cm"), # top, right, bottom, left
        axis.text = element_text(size=fontSize, color = "black"),
        axis.title = element_text(size=fontSize, color = "black"), 
        plot.title = element_text(size=fontSize, color = "black", hjust = 0.5), # center title 
        legend.text = element_text(size=fontSize, color = "black"),
        axis.line = element_line(colour = 'black', size = 1))
print(p)

# --------------------------------------------------------- #
#### 6b) Go/NoGo accuracy ~ longer dwell time on rewards/ punishments: ####

modData <- eyeData # select all data
modData$dwell_out_dif <- modData$dwell_rew_abs - modData$dwell_pun_abs
  
## Aggregate per subject:
dwellOutDif <- tapply(modData$dwell_out_dif, modData$subject, mean, na.rm = T)
stat.desc(dwellOutDif)
densityplot(dwellOutDif)

cor(ACCvec,dwellOutDif) # -0.269
rcor.test(cbind(ACCvec,dwellOutDif))
#              ACCvec dwellOutDif
# ACCvec       ***** -0.269     
# dwellOutDif  0.007  ****
  
cor(firstfix,dwellOutDif) # 0.5483792

## Linear regression with both:
summary(lm(dwellDifEff ~ ACCvec + dwellOutDif))
#                 Estimate  Std. Error t value   Pr(>|t|)    
# (Intercept) -0.16069976  0.06447217  -2.493     0.0144 *  
# ACCvec       0.42661556  0.08198370   5.204 0.00000111 ***
# dwellOutDif  0.00010178  0.00007626   1.335     0.1852 

## Plot:
eff <- dwellOutDif; xVar <- "dwellOutDif"; xLab <- "Overall dwell time difference"; 

fontSize <- 20 # for 4 plots next to each other
corData <- data.frame((cbind(ACCvec,eff, sampleID))) # extract data
p <- ggplot(corData, aes(x=eff, y=ACCvec)) +
  geom_point(shape = sampleID, size = 1, stroke = 0.8, fill = "white") + 
  geom_smooth(method=lm, se=T, col = "red", size = 2) + 
  coord_cartesian(xlim=c(-100, 750), ylim=c(0.3, 1)) +
  xlab(xLab) + ylab("Accuracy") +
  theme_classic() + 
  theme(plot.margin = unit(c(0.5,1.0,0.5,0.5), "cm"), # top, right, bottom, left
        axis.text = element_text(size=fontSize, color = "black"),
        axis.title = element_text(size=fontSize, color = "black"), 
        plot.title = element_text(size=fontSize, color = "black", hjust = 0.5), # center title 
        legend.text = element_text(size=fontSize, color = "black"),
        axis.line = element_line(colour = 'black', size = 1))
print(p)

# END