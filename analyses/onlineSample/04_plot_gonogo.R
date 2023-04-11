#### 04_plot_gonogo.R ####

#' Load and pre-process raw data,
#' create all plots presented in the supplementary materials.

# ================================================================================================================================================ #
#### Set directories: ####

rootDir       <- "/project/2420133.01/" # adjust to your own folder structure before running script

codeDir       <- paste0(rootDir,"analyses/onlineSample/")

dataDir       <- paste0(rootDir,"data/onlineSample/") 
rawDir        <- paste0(dataDir,"rawData/")
processedDir  <- paste0(dataDir,"processedData/")

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

## Apply function for easy pre-processing:
GNGData <- preprocess_task_data(rawData, variables)

# ================================================================================================================================================ #
#### Plot 1: Responses as function of required action and cue position: ####

plotData <- GNGData # reassign

## Bar plot:
png(paste0(plotDir,"bar_response_reqAction_cuePosition.png"),width = 480, height = 480)
custom_barplot2(plotData, xVar="ReqAction_f", yVar="Response_n", zVar="CuePosition_f", subVar="subject",
                xLab = "Required action", yLab = "p(Go)", zLab = "Cue Position", selCol = c("#009933","#CC0000"),
                isPoint = F, isBeeswarm = T, yLim = c(0,1), main = "")
dev.off()

# ================================================================================================================================================ #
#### Plot 2: RTs as function of required action and cue position: #####
 
plotData <- GNGData
plotData$Reaction.Time_cleaned_ms <- plotData$Reaction.Time_cleaned*1000

## Bar plot:
png(paste0(plotDir,"bar_RTcleaned_reqAction_cuePosition.png"),width = 480, height = 480)
custom_barplot2(plotData, xVar="ReqAction_f", yVar="Reaction.Time_cleaned_ms", zVar="CuePosition_f", subVar="subject",
                xLab = "Required action", yLab = "Reaction time (ms)", zLab = "Cue Position", selCol = c("#009933","#CC0000"),
                isPoint = F, isBeeswarm = T, yLim = c(0,1200), main = "")
dev.off()

# ================================================================================================================================================ #
#### Plot 3: Responses as function of point difference: ####

modData <- GNGData

formula = "Response_n ~ PointDif + (PointDif|subject)"

mod <- glmer(formula = formula, 
             modData, family = binomial(), 
             control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod); beep()
plot(effect("PointDif",mod))

## Plot regression line:
png(paste0(plotDir,"regression_response_pointDif.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "PointDif", 
                       xLim = c(-80,80),
                       yLim = c(0.45, 0.65),
                       color = "#FFC000", 
                       xLab = "Stake difference", yLab = "p(Go)", Main = "")
dev.off()

png(paste0(plotDir,"regression_response_pointDif_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "PointDif", 
                       xLim = c(-80,80),
                       yLim = c(0, 1),
                       color = "#FFC000", 
                       xLab = "Stake difference", yLab = "p(Go)", Main = "")
dev.off()

# ================================================================================================================================================ #
#### Plot 4: RTs as function of point difference: ####

modData <- GNGData
modData$Reaction.Time_cleaned_ms <- modData$Reaction.Time_cleaned*1000

formula = "Reaction.Time_cleaned_ms ~ PointDif + (PointDif|subject)"

mod <- lmer(formula = formula, 
             modData, 
             control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
summary(mod); beep()

# Plot regression line:
png(paste0(plotDir,"regression_RT_cleaned_pointDif.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "PointDif", 
                       xLim = c(-80,80),
                       yLim = c(500, 1100),
                       color = "#FFC000", # Labels = c(1:5),
                       xLab = "Stake difference", yLab = "Reaction time (ms)", Main = "")
dev.off()

png(paste0(plotDir,"regression_RT_cleaned_pointDif_full.png"),width = 480, height = 480)
custom_regressionline1(mod = mod, selEff = "PointDif", 
                       xLim = c(-80,80),
                       yLim = c(0, 1500),
                       color = "#FFC000", # Labels = c(1:5),
                       xLab = "Stake difference", yLab = "Reaction time (ms)", Main = "")
dev.off()

# END
