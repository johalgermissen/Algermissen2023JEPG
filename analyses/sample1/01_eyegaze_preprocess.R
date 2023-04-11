#### 01_eyegaze_preprocess.R ####

#' Sample 1:
#' - Read in raw eye-tracking data (.edf),
#' - epoch data into trials, 
#' - compare trial-by-trial behavioral data/ task settings with behavioral data file,
#' - interpolate short sequences of NAs (blinks)
#' - check whether gaze was within one of the two attentional ROIs at any time point, 
#' - aggregate first fixation/ total dwell time per ROI per trial,
#' - save.
#' Note that behavioral data stored in the eye-tracking file is compared to (and overwritten by) the behavioral data file in case both mismatch.
#' Note that headers of the behavioral data of subjects 23-35 are incorrect have have to be overwritten by the header of subject 22.
#' Adjust root directory to your own folder structure before running script.

# ================================================================================================================= #
#### Set directories: ####

rootDir       <- "/project/2420133.01/" # adjust to your own folder structure before running script

## Code:
codeDir       <- paste0(rootDir,"analyses/functions/")

## Data:
dataDir       <- paste0(rootDir,"data/sample1/") 
rawDir        <- paste0(dataDir,"rawData/")
processedDir  <- paste0(dataDir,"processedData/")

## Raw data:
behavDataDir  <- paste0(rawDir,"behavData/")
eyeDataDir    <- paste0(rawDir,"eyeData")

## Pre-processed data:
eyeDataAggrDir <- paste0(processedDir,"eyeDataAggr/")
dir.create(eyeDataAggrDir, showWarnings = FALSE)
eyeDataFullDir <- paste0(processedDir,"eyeDataFull/")
dir.create(eyeDataFullDir, showWarnings = FALSE)

# ================================================================================================================= #
#### Interpolation settings: ####

maxGap <- 75 # maximal gap to interpolate: 75 ms
maxKeepTrial <- 0.50 # maximal number of NAs to still keep data of trial: 50%

# ================================================================================================================= #
#### ROI Settings: ####

scrx <- 1920 # screen size x dimension
scry <- 1080 # screen size y dimension
eccentricity <- 0.30 # eccentricty of potential outcome presentation
tolFix <- 150 # originally 200 tolerance for whether gaze is in ROI
plotFactor <- 3 # how many tolFix to deviate from midline in plots

# ================================================================================================================= #
#### Options for saving outputs: ####

saveFull = TRUE
saveAggr = TRUE

# ================================================================================================================= #
#### Load packages: ####

library(devtools) # version '2.4.2'
library(eyelinker) # version '0.2.1'
library(plyr) # version '1.8.6'
library(stringr) # version '1.4.0' # for str_sub

# ================================================================================================================= #
#### Load custom functions: ####

source(paste0(codeDir,"00_functions_preprocess.R")) # Load functions

# ================================================================================================================= #
#### Behavioral data: ####

allData <- lapply(list.files(behavDataDir,pattern=".csv",full=TRUE), read.csv, header=T)
for (i in 23:length(allData)){
  names(allData[[i]]) <- names(allData[[22]])
}
behavData <- do.call("rbind",allData)

behavData$trialnr <- behavData$trialNr # rename (inconsistency behavioral and eye-tracking data)

# ================================================================================================================= #
#### Detect eye gaze raw data files: ####

subList <- list.files(path = eyeDataDir, pattern = ".asc", full=TRUE) # files in directory
nSub <- length(subList)
cat("Found data from",nSub,"subjects\n")

# ================================================================================================================= #
#### Loop over subjects, save aggregated data: ####

for (iSub in 1:nSub){ # iSub <- 1
  
  subID <- as.numeric(str_sub(subList[iSub],-9,-8))
  cat(paste0("Start subject ",subID,"\n"))
  
  # ---------------------------------------------------------------- #
  #### Extract behavioral data for this subject: ####
  
  subBehavData <- behavData[which(behavData$subject==subID),
                            c("trialnr","cue","reqAction","reqSide","rewLeft","angle",
                              "rewMag","punMag","response","respSide","ACC","RT","outcome")]
  
  # ---------------------------------------------------------------- #
  #### Load eye tracking data: ####
  cat(paste0("Load ",subList[iSub]),"\n")
  dat <- read.asc(subList[iSub])
  
  ## Read behavioral variables:
  selVariables <-  c("cue","reqAction","reqSide","rewLeft","angle","rewMag","punMag",
                     "response","respSide","ACC","RT","outcome")

  trialData <- readTrials(dat,segmentMessage="StartStakes",beforeEvent=-1,afterEvent=1500,
                          variablesMessages = selVariables,
                          # recode2Num = c(T,T,T,T),
                          recode2Num = rep(F,length(selVariables)),
                          variablesNames = selVariables)
  
  ## Convert to numeric:
  trialData <- recode_behavioral_variables(trialData)
  
  ## Select behavioral data put into eye-tracking data:
  subEyeData <- trialData[which(trialData$trialTime==1),c(names(subBehavData))]
  
  # ---------------------------------------------------------------- #
  #### Check whether behavioral variables match with those from behavioral data set: ####
  ## (behavioral variables first (use to select non-NA rows), eye data second)
  isSame <- compare_data_sets(subBehavData,subEyeData)

  ## If not: overwrite
  if (isSame == F){
    cat("Mismatch in behavioral data, overwrite eyetracking data with behavioral variables from behavioral data")
    
    trialData <- trialData[,c("trialnr","absTime","trialTime","xp","yp","pupil")]
    
    ## Merge with behavioral data:
    trialData <- merge(trialData,subBehavData,by="trialnr")
    # head(trialData)
  }
  
  # ---------------------------------------------------------------- #
  #### Interpolate: ####
  # Interpolate NA sequences shorter than 75 ms, discard trials with > 25% NAs, do not pad edges
  
  trialData <- applyInterpolation(trialData, variable="xp", maxGap = maxGap, maxKeepTrial = maxKeepTrial, padEdges=F, interactive=FALSE) # x
  trialData <- applyInterpolation(trialData, variable="yp", maxGap = maxGap, maxKeepTrial = maxKeepTrial, padEdges=F, interactive=FALSE) # y
  
  # ---------------------------------------------------------------- #
  #### Mark when fixation is in ROIs: ####
  
  trialData$leftAngle <- trialData$angle # difference downwards from 9 o'clock (0 + x)
  trialData$rightAngle <- 180 - trialData$angle # difference downwards from 3 o'clock (180 - x)
  
  trialData <- compute_in_ROI(trialData, xVar = "xp", yVar = "yp", suffix = "left", 
                              centerX = scrx/2, centerY = scry/2, angleVar = "leftAngle", radius = eccentricity, tolerance = tolFix)
  trialData <- compute_in_ROI(trialData, xVar = "xp", yVar = "yp", suffix = "right", 
                              centerX = scrx/2, centerY = scry/2, angleVar = "rightAngle", radius = eccentricity, tolerance = tolFix)
  
  # ---------------------------------------------------------------- #
  #### Save complete data: ####
  
  if(saveFull){
    cat("Save complete data\n")
    
    ## Recode fixation for valences before saving full data:
    trialData$fix_rew <- trialData$rewLeft*trialData$fix_left + (1-trialData$rewLeft)*trialData$fix_right
    trialData$fix_pun <- trialData$rewLeft*trialData$fix_right + (1-trialData$rewLeft)*trialData$fix_left

    ## Select variables for saving:
    selData <- trialData[,c("trialnr","trialTime","fix_rew","fix_pun")]
    write.csv(selData, paste0(eyeDataFulldir,"eye_sub",str_pad(subID,2,"left",0),"_full.csv"), row.names = F)
  }

  # ---------------------------------------------------------------- #
  #### Aggregate data per trial:  ####
  
  if(saveAggr){
    aggrData <- aggregate_within_trials(trialData)
    aggrData$subject <- subID
    
    ## Save: 
    cat(paste0("Save subject ",subID))
    write.csv(aggrData, paste0(eyeDataAggrDir,"eye_sub",str_pad(subID,2,"left",0),"_aggr.csv"), row.names = F)
  }
  cat("-----------------------------------------------\n")
}
cat("Finished preprocessing :-) \n")

# ------------------------------------------------------------------------------ # 
warnings()

# To-be expected warnings:
# 1: In as.numeric.factor(data$respSide) : NAs introduced by coercion
# 2: In as.numeric.factor(data$RT) : NAs introduced by coercion
# 3: In as.numeric.factor(data$outcome) : NAs introduced by coercion

# END