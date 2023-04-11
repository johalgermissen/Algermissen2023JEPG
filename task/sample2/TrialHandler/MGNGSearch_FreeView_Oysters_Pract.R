#### MGNGFreeView_Oysters_Pract.R

rm(list = ls())
set.seed(19913010)
# setwd("D:/AA_MGNGFreeView_Task/Study1/TrialHandler")

# Directory for saving:
# currentDir <- getwd()
currentDir <- "D:/AA_MGNGFreeView_Task/Study1/TrialHandler"
# setwd(currentDir)

makePractSequence <- function(nTrial){
  
  sessionNr <- rep(1,nTrial)
  trialNr <- 1:nTrial
  
  isActionCue <- rep(1,nTrial)
  isStakes <- rep(1,nTrial)
  isReleaseCue <- rep(1,nTrial)
  isOutcome <- rep(1,nTrial)
  
  ### Cue:
  allAct <- c(1,0,1,0,1,1,0,0) # standard sequence required actions
  reqAction <- allAct[1:nTrial]
  cue <- ifelse(reqAction==1,"Pract_Go","Pract_NoGo")

  ### Action side:
  reqSide <- sample(c(rep(1,floor(nTrial/2)),rep(0,ceiling(nTrial/2))),nTrial,replace = F)
  
  ### Stakes position:
  minAngle <- -45
  maxAngle <- 45
  angle <- round(runif(nTrial, minAngle, maxAngle),0)
  
  ### Stakes position:
  rewLeft <- sample(c(rep(1,floor(nTrial/2)),rep(0,ceiling(nTrial/2))),nTrial,replace = F)

  ### Stakes magnitude:
  sampleMin <- 1
  sampleMax <- 5
  
  rewMag <- round(runif(nTrial,min = sampleMin, max = sampleMax))
  punMag <- round(runif(nTrial,min = sampleMin, max = sampleMax))
  
  # Trials with identical rewMag and punMag:
  while (sum(rewMag == punMag) > 0){ # disambiguate if both are the same number
    idx <- which(rewMag == punMag) # indices of trials where both are the same number
    nIdx <- length(idx) # number of such trials
    rewMag[idx] <- round(runif(nIdx, min = sampleMin, max = sampleMax))
    punMag[idx] <- round(runif(nIdx, min = sampleMin, max = sampleMax))
  }

  if(sum(rewMag < sampleMin | rewMag > sampleMax) > 0){stop("Trials with rewMag out of range")}
  if(sum(punMag < sampleMin | punMag > sampleMax) > 0){stop("Trials with punMag out of range")}
  if(sum(rewMag == punMag) > 0){stop("Trials with identical rewMag and punMag")}
  
  ### Validity:
  goValidity <- rep(1,nTrial)
  nogoValidity <- rep(1,nTrial)
  
  ### Catch trials:
  isCatch <- rep(0,nTrial)

  ### Trial timings:
  ITIvector <- c(0,0.1,-0.1,0.2,-0.2)
  ISI <-  0.3 + sample(ITIvector, nTrial, replace = T)
  ITI <-  1.5 + sample(ITIvector, nTrial, replace = T)
  
  # Concatenate:
  taskData <- data.frame(sessionNr,trialNr,
                         isActionCue,isStakes,isReleaseCue,isOutcome,
                         cue,reqAction,reqSide,rewLeft,angle,
                         rewMag,punMag,
                         goValidity,nogoValidity,isCatch,
                         ISI,ITI)
  return(taskData)
}

# Block 1: only releaase
taskData <- makePractSequence(nTrial = 8) 
taskData$isActionCue <- 0
taskData$isStakes <- 0
taskData$cue <- "Pract_Go"
taskData$reqAction <- 1
taskData$rewMag <- 1
taskData$punMag <- 1
write.csv(taskData,paste0(currentDir,"/stimuluslist_pract_block1.csv"), quote = F, row.names = F)

# Block 2: add action cue, add invalidity
taskData <- makePractSequence(nTrial = 8) 
taskData$isStakes <- 0
taskData$goValidity[c(6,8)] <- 0 # add invalidity
taskData$nogoValidity[c(6,8)] <- 0 # add invalidity
write.csv(taskData,paste0(currentDir,"/stimuluslist_pract_block2.csv"), quote = F, row.names = F)

# Block 3: add stakes
taskData <- makePractSequence(nTrial = 4) 
taskData$angle <- c(40,-40,-20, 20)
write.csv(taskData,paste0(currentDir,"/stimuluslist_pract_block3.csv"), quote = F, row.names = F)

# Block 4: add catch trials
taskData <- makePractSequence(nTrial = 8)
catchIdx <- c(3,4,6,8)
taskData$isCatch[catchIdx] <- 1
taskData$reqSide[catchIdx] <- ifelse(taskData$rewLeft[catchIdx]==1 & taskData$rewMag[catchIdx] > taskData$punMag[catchIdx] | 
                                     taskData$rewLeft[catchIdx]==0 & taskData$rewMag[catchIdx] < taskData$punMag[catchIdx],
                          1,0)
write.csv(taskData,paste0(currentDir,"/stimuluslist_pract_block4.csv"), quote = F, row.names = F)

# Block 5: add gaze contingency
taskData <- makePractSequence(nTrial = 4) 
taskData$angle <- c(0,25,5,-25)
catchIdx <- c(2,4)
taskData$isCatch[catchIdx] <- 1
taskData$reqSide[catchIdx] <- ifelse(taskData$rewLeft[catchIdx]==1 & taskData$rewMag[catchIdx] > taskData$punMag[catchIdx] | 
                                       taskData$rewLeft[catchIdx]==0 & taskData$rewMag[catchIdx] < taskData$punMag[catchIdx],
                                     1,0)
write.csv(taskData,paste0(currentDir,"/stimuluslist_pract_block5.csv"), quote = F, row.names = F)

# END