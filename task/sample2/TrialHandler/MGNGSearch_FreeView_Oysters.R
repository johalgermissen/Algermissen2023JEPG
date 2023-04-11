#### MGNGFreeView Create Trial Sequence ####

# Oyster Task Study 1

# - only balance rewLeft
# - reqSide random
# - random angle, uniform -45 - +45
# - rewMag and punMag between 1 - 5, random
# - trials with different cue sets spread into "sessions"
# - split into sessions based on break points, use same cues afterwards

rm(list = ls())
set.seed(19913010)
setwd("D:/AA_MGNGFreeView_Task/Study1/TrialHandler")

# ===================================================================================================== #
#### 1) Make cue sequence ####

# nSes <- 1; nCond <- 8; nRepCond <- 2; nTrialSes <- 16; reduce = T
# makeCondSequence(1,8,2,16,reduce=T)

makeCondSequence <- function(nSes, nCond, nRepCond, isElim = F, nElim = 1){
  #' Create condition indices, check that no condition repeated > 1 time on consecutive times
  #' @param nSes number of sessions per subject
  #' @param nCond number of maximal conditions to be distinguished for creating other objects
  #' @param nRepCond how often each of these conditions is repeated per session
  #' @param nTrialSes total number of trials per session
  #' @param isElim whether to reduce conditions by factor nElim or not for checking (still original conditions outputted)
  #' @param nElim if isElim = T: number of factors to be eliminated
  #' @return Cues array with dimensions sessions, trials, containing condition indices

  nTrialSes <- nCond * nRepCond # might mismatch with nRepStim if nRepCond not integer number
  
  # Initialize empty outcome object:
  Cues <- array(NA, dim = c(nSes,nTrialSes)) # sessions in rows, trial position in columns
  
  # Loop through sessions:
  for (iSes in 1:nSes){ # iSes <- 1
    
    # ------------------------------------------------------------------------------ #
    # a) Create (ordered) condition sequence:
    condSeq <- rep(1:nCond,each=nRepCond) # repeat each condition as often as it should be repeated per session

    # ------------------------------------------------------------------------------ #
    # b) Check whether any condition repeats:
    condRep <- 1 # just initialize condRep to > 0 to enter while loop
    while (length(condRep)>0){
      
      # ------------------------------------------------------------------------------ #
      # i) Randomize condition sequence: Continue shuffling until there are no more than 2 stimulus repetitions.
      conditions <- sample(condSeq) # shuffle vector
      
      # ------------------------------------------------------------------------------ #
      # ii) Reduce conditions for checking for repetitions or not:
      conditionsCheck <- conditions # initialize
      
      if (isElim){ # Reduce from conditions to stimuli:
        for (iElim in 1:nElim){
          nCheck <- length(unique(conditionsCheck))
          conditionsCheck <- (conditionsCheck-1) %% (conditionsCheck/2) + 1 # eliminate last factor
          conditionsCheck <- (conditionsCheck-1) %% (nCheck/2) + 1 # eliminate last factor
        }
      } 
      ## Check:
      # cbind(conditions,conditionsCheck)
      # table(conditions,conditionsCheck)
      
      # ------------------------------------------------------------------------------ #
      # iii) Check whether condition repeated on consecutive trials > nCheck times
      repetitionIdx <- conditionsCheck
      nCheck <- 1 # only one cycle of checks
      for (iCheck in 1:nCheck){
        # iv) Compute difference between adjacent numbers, check if is 0 or not:
        repetitionIdx <- which(diff(repetitionIdx,lag=1)==0)
        # v) Check if indices of adjacent numbers are itself adjacent (1):
        condRep <- which(diff(repetitionIdx) == 1) # empty if no double repetitions
      }
    
    } # end while loop
    
    # ------------------------------------------------------------------------------ #
    # c) Save per session:
    Cues[iSes,] <- conditions # still original conditions outputed per session
    rm(conditions,repetitionIdx,condRep) # remove unnecessary objects
  }
  return(Cues)
} # end makeCondSequence

# Cues <- makeCondSequence(nSes=3,nCond=8,nRepCond=4,nTrialSes=64,reduce=FALSE)

# ===================================================================================================== #
#### 2) Make stakes sequence ####

# nCond <- nStim; nRepCond <- nTrial/nStim
# nCond <- 4; nRepCond <- 24
# minStakes = 1; maxStakes = 5; stepStakes = 1; nCatchCond = 2

makeStakesSequence <-function(nSes, nCond, nRepCond, minStakes = 1, maxStakes = 5, stepStakes = 1, nCatchCond){
  
  #' Take inputs from other functions, create spreadsheet, save
  #' @param nSes number of sessions per subject
  #' @param nCond integer, number of conditions
  #' @param nRepCond integer, number of repetitions per condition
  #' @param minStakes numeric, minimal stakes possible
  #' @param maxStakes numeric, maximal stakes possible
  #' @param stepStakes numeric, steps between stakes
  #' @param nCatchCond numeric, number of catc trials for each condition per session
  #' @return Stakes array with dimensions sessions, conditions, stimulus repetitions
  
  # ---------------------------------------------------------------------------- # 
  ### a) Initialize empty output object:
  
  Stakes <- array(NA, dim = c(nSes, nCond, nRepCond)) # ITI
  
  # ---------------------------------------------------------------------------- # 
  ### b) Initialize possible stakes:
  
  ## Possible stakes: univariate
  allStakes <- seq(minStakes,maxStakes,stepStakes)
  
  ## Possible stakes: bivariate: 10 digit is rew, 1 digit is pun 
  opsStakes <- c()
  for (iRew in allStakes){
    for (iPun in allStakes){
      opsStakes <- c(opsStakes,iRew*10+iPun)
    }
  }
  
  ## Eliminate doublings (rew = pun):
  idx <- which(opsStakes%/%10 == opsStakes%%10) # indices where rew and pun are the same
  opsStakes <- opsStakes[-idx] # eliminate those
  nOpsStakes <- length(opsStakes) # number of stakes
  opsStakes <- sample(opsStakes) # randomize for first time, so selection of possible stakes random
  
  # ---------------------------------------------------------------------------- # 
  ### d) Permute: Loop through sessions and stimuli, add permuted vecStakes vector
  for (iSes in 1:nSes){ # sample for each session
    
    # cat("Start session",iSes,"\n")

    for (iCond in 1:nCond){ # sample for each condition
      
      # cat("Start condition",iCond,"\n")
      
      ## Create vector of stakes:
      
      if (nRepCond < nOpsStakes){ # a) if more possible stakes options than repetitions: crop 
        
        cat("Stakes: Less condition repetitions (",nRepCond,") than possible stakes(",nOpsStakes,"), crop possible stakes\n")
        
        vecStakes <- opsStakes[1:nRepCond] # crop and only store necessary number of trials
        
        catchIdx <- sample(1:length(vecStakes),nCatchCond) # randomly select catch trials
        vecStakes[catchIdx] <- vecStakes[catchIdx] + 100 # mark as catch trials by adding 100
        
      } else { # b) if equal or more repetitions than possible stakes options: repeat possible stakes options
        
        nRepStakes <- nRepCond %/% nOpsStakes # how often to repeat possible stakes vector (integer division)
        vecStakes <- rep(opsStakes,nRepStakes) # repeat possible stakes vector
        
        nDif <- nRepCond - nOpsStakes*nRepStakes # difference in trials left to be filled?
        cat("Stakes: More condition repetitions (",nRepCond,") than possible stakes(",nOpsStakes,"), repeat stakes", nRepStakes, "times, identified ", nDif, "remaining trials\n")
        
        if (nDif == 0 | nDif < nCatchCond){ # if nothing left or less remaining trials than catch trials: fill and randomly select catch trials
          
          cat("Stakes: Remaining trials (",nDif,") less than catch trials (",nCatchCond,"), so sample catch trials randomly\n")
          
          vecStakes <- c(vecStakes,sample(vecStakes,nDif)) # fill difference
          
          catchIdx <- sample(1:length(vecStakes),nCatchCond) # randomly select catch trials
          vecStakes[catchIdx] <- vecStakes[catchIdx] + 100 # mark as catch trials by adding 100
          
        } else { # if remaining trials to be filled and more remaining trials than catch trials:
        
          cat("Stakes: Remaining trials (",nDif,") less than catch trials (",nCatchCond,"), so select catch trials among remaining trials\n")
          
          vecStakes <- c(vecStakes, 
                         sample(vecStakes,nCatchCond) + 100, # randomly sample for catch trials
                         sample(vecStakes,nDif-nCatchCond)) # randomly sample other trials
        }
      }      
      ## Save:
      Stakes[iSes,iCond,] <- sample(vecStakes) # permute and save
      
    } # end iCond
  } # end iSes
  
  return(Stakes)
} # end makeStakesSequence

# Stakes <- makeStakesSequence(nSes=1, nCond=4, nRepCond=24, minStakes = 1, maxStakes = 5, stepStakes = 1, nCatchCond <- 2)
# Stakes <- makeStakesSequence(nSes=1, nCond=6, nRepCond=40, minStakes = 1, maxStakes = 5, stepStakes = 1, nCatchCond <- 2)

# ===================================================================================================== #
#### 3) Make Validity sequence ####

makeValiditySequence <-function(nSes,nCond,nRepCond,nResp,prob){

  #' Take inputs from other functions, create spreadsheet, save
  #' @param nSes number of sessions per subject
  #' @param nCond number of conditions (different identifiers)
  #' @param nRepCond number of condition repetitions
  #' @param nResp number of responses
  #' @param prob feedback validity
  #' @return Validity array with dimensions sessions, conditions, responses, stimulus repetitions
  
  # ---------------------------------------------------------------------------- # 
  ## a) Check how often valid and invalid feedback per session:

  ## Compute separately for Go and NoGo cues so validity is unconfounded with trial settings
  
  nSesValid <- ceiling(nRepCond * prob) # number valid feedbacks for certain cue in one session, round up
  
  if (!((nRepCond * prob)%%1==0)){ # output warning if rounding happens
    cat(paste0("No integer number of valid responses; rounded to ",nSesValid/nRepCond*100,"% instead of ",prob,"% validity!\n"))
  }
  
  nSesInvalid <- nRepCond - nSesValid; # number invalid feedbacks per session
  
  # ---------------------------------------------------------------------------- # 
  ## b) Loop through sessions, sample valid/ invalid trials per stimulus per response
  
  Validity <- array(NA, dim = c(nSes,nStim,nResp,nRepCond)) # initialize output object

  for (iSes in 1:nSes){
    
    # Randomize feedback sequence per cue and per response option: 
    # valid or invalid feedback separately sampled for Go and NoGo
    
    feedSeq <- c(rep(1,nSesValid),rep(0,nSesInvalid)) #  initialize feedback sequence for each stimulus
    
    for (iStim in 1:nStim){ # sample for each condition
      for (iResp in 1:nResp){ # sample for each response
        Validity[iSes,iStim,iResp,] <- sample(feedSeq) # randomize sequence and store
      } # end iResp 
    } # end iStim
  } # end iSes
  
  return(Validity)
} # end makeFeedbackSequence

# makeValiditySequence(nSes,nStim,nRepCond,nResp,prob)
# Validity <- makeValiditySequence(nSes=3,nStim=8,nRepCond=8,nResp=2,prob=0.75)
# b <- makeValiditySequence(1,8,2,8,.8)

# ===================================================================================================== #
#### 4) Make ITI sequence ####

makeITISequence <-function(nSes, nCond, nRepCond, ITIintercept, maxSteps = 4){
  
  #' Take inputs from other functions, create spreadsheet, save
  #' @param nSes number of sessions per subject
  #' @param nCond integer, number of conditions
  #' @param nRepCond integer, number of repetitions per condition
  #' @param ITIintercept numeric, intercept to add to any ITI
  #' @param maxSteps integer <= 4, how much 
  #' @return ITI array with dimensions sessions, conditions, stimulus repetitions
  
  # ---------------------------------------------------------------------------- # 
  ### a) Initialize empty output object:
  ITI <- array(NA, dim = c(nSes, nCond, nRepCond)) # ITI
  # For each session: distinguish conditions
  
  # ---------------------------------------------------------------------------- # 
  ### b) Initialize ITI offset options:
  ITIops <- c(0) # vector containing zero
  iCount <- 0 # initialize count
  for (iStep in 1:maxSteps){
    iCount <- iCount + 1 # increment count
    ITIops <- c(ITIops,0.1*iCount,-0.1*iCount) # add positive and negative offset based on count
  } 
  # ITIops <- c(0.1,-0.1,0.2,-0.2,0.3,-0.3,0.4,-0.4) # potential ITIs hard-coded
  
  # ---------------------------------------------------------------------------- # 
  ### c) Create ordered vector (to be permuted):
  # Repeated selected  ITI options as long as necessary:
  ITIvector <- rep(ITIops,length.out=nRepCond)

  # ---------------------------------------------------------------------------- # 
  ### d) Permute: Loop through sessions and stimuli, add ITI intercept plus permuted ITIvector
  for (iSes in 1:nSes){
    for (iCond in 1:nCond){ # sample for each condition
      # 3) Randomize ITI: gap between feedback and response.
      ITI[iSes,iCond,] <- ITIintercept + sample(ITIvector)
    } # end iStim
  } # end iSes
  
  return(ITI)
} # end makeFeedbackSequence

# makeITISequence(nSes, nCond, nRepCond, ITIintercept, maxSteps)
# ITIall <- makeITISequence(nSes = 3, nCond = 8, nRepCond = 8, ITIintercept = 1.5, maxSteps = 5)
# ISIall <- makeITISequence(nSes = 3, nCond = 8, nRepCond = 8, ITIintercept = 1.5, maxSteps = 5)

# ===================================================================================================== #
#### 5) Create complete task sequence ####

# 8 cues, 1 session: nTrialEff <- 320; nSes <- 1; nCond <- 16; nTrialSes <- 320; nTrialAll <- nSes * nTrialSes; isElim <- T; nElim <- 1

combineSequence <- function(nTrialEff, nSes, nCond, Cues, Stakes, Validity, ISIAll, ITIAll, iSub, isElim = F, nElim = 1){

  #' Take inputs from other functions, create spreadsheet, save
  #' @param nTrialEff number of trials altogether per subject (oversampled if necessary)
  #' @param nSes number of sessions per subject
  #' @param nCond number of conditions (maximal number of distinction in task)
  #' @param Cues array, for each session, each condition, each repetition: valid (1) or not (0)
  #' @param Stakes array, for each session, each condition, each repetition: stakes (3 digit)
  #' @param Validity array, for each session, each condition, each repetition: valid (1) or not (0)
  #' @param ISIAll ISI in array for each condition for each condition repetition
  #' @param ITIAll ISI in array for each condition for each condition repetition
  #' @param iSub subject number just for naming
  #' @param isElim whether to eliminate factor in conditions when mapping conditions on stimuli (default: F)
  #' @param nElim number of dimensions to eliminate (default: 1)
  #' @return nothing returned, directly writes file
  
  # ------------------------------------------------------------------------------ #
  # A) GENERAL SETTINGS:
  
  nTrialSesEff <- nTrialEff/nSes
  
  # ------------------------------------------------------------------------------ #
  ### Increase effective trial number to cover all conditions if necessary (crop sessions later):

  # Now determine effective number of cue repetitions:
  nRepCond <- nTrialSesEff/nCond

  sessionNr <- rep(1:nSes, each = nTrialSesEff)
  trialNr <- 1:nTrialEff # complete trial numbers
  # Overwrite trial number later when cropping
  
  # State that respective part included in trial:
  isActionCue <- rep(1,nTrialEff)
  isStakes <- rep(1,nTrialEff)
  isReleaseCue <- rep(1,nTrialEff)
  isOutcome <- rep(1,nTrialEff)

  # ------------------------------------------------------------------------------ #
  ### B) Stakes magnitude: Sample from uniform distribution between min and max
  
  rewMag <- NA
  punMag <- NA
  isCatch <- rep(0,nTrialEff)
  
  minAngle <- -45
  maxAngle <- 45
  angle <- round(runif(nTrialEff, minAngle, maxAngle),0)
  # summary(lm(angle ~ c(1:length(angle))))
  # library(lattice)
  # densityplot(angle)
  
  # ------------------------------------------------------------------------------ #
  ### C) Map task conditions onto STIMULI AND ACTIONS:
  
  ### Transpose and concatenate all sessions: cues on all trials in entire task:
  # Cues <- makeCondSequence(nSes,nCond,nRepCond,isElim = FALSE,nElim = 3) # make sequence as array of sessions and trials within sessions
  
  conditions = c(t(Cues[1:nSes,])) # turn into one long vector
  
  ### Extract how conditions map onto stimuli:
  
  nElim <- 1 # eliminate only 1 condition: rewLeft
  stimuli <- conditions # initialize stimuli to conditions
  if (isElim){
    for (iElim in 1:nElim){
      nStim <- length(unique(stimuli))
      stimuli <- (stimuli-1) %% (nStim/2) + 1 # eliminate last factor
    }
  }
  nStim <- length(unique(stimuli))
  ## Check:
  # table(conditions,stimuli) # sweeps across 4 conditions
  # cbind(conditions,stimuli) # check conditions against stimuli

  # ------------------------------------------------------------------------------ #
  ### D) Retrieve factors based on conditions:

  ## Action instructed by symbol: Go or NoGo
  reqAction <- ifelse(stimuli %% 2 == 1, 1, 0) # 16th: odd stimuli are Go cues, even stimuli are NoGo cues
  
  ## Side of Go action: left or right   
  reqSide <- c(rep(1,floor(nTrial/2)),rep(0,ceiling(nTrial/2))) # ordered vector
  reqSide <- sample(reqSide) # permute
  
  ## Position of reward stakes: left or right
  rewLeft <- (conditions-1) %/% (nCond/2) + 1 # integer division: divide into 2 halves
  rewLeft <- (rewLeft %% 2) # classify odd/ even halves
  
  ### Check:
  
  ## reqAction:
  # table(conditions,reqAction) # odd/ even condition is Go (1) / NoGo(0)
  # cbind(conditions,reqAction) # odd/ even condition is Go (1) / NoGo(0)
  # table(stimuli,reqAction) # odd/ even stimulus is Go (1) / NoGo(0)
  #
  ## rewLeft:
  # table(conditions,rewLeft) # first and third quarter 1, second and fourth quarter 0
  # cbind(conditions,rewLeft) # first half 1, second half 0
  # table(stimuli,rewLeft) # should be completely orthogonal
  
  # ------------------------------------------------------------------------------ #
  ### D) Initialize empty vectors for cue identity and validity and timing: 
  
  cue <- rep(NA,nTrialEff) # actual stimulus name, e.g. "A1"
  
  goValidity <- rep(NA,nTrialEff)
  nogoValidity <- rep(NA,nTrialEff)
  
  ISI <- rep(NA,nTrialEff) # initialize ISI
  ITI <- rep(NA,nTrialEff) # initialize ITI

  # ------------------------------------------------------------------------------ #
  ### E) Loop over trials to retrieve relevant settings: 

  iSes <- 0 # initialize session count

  for (iTrial in 1:nTrialEff){ # loop through all trials in entire task: iTrial <- 1
    
    # ---------------------------------------------------------------------------- # 
    ### a) For every start of new session:
    
    if(iTrial %in% c(seq(1,nTrialEff,nTrialSesEff))){ 
      
      countStim <- rep(0,nStim) # initialize stimcount to zero
      iSes <- iSes + 1 # increment iSes
      cueMapping <- sample(unique(stimuli)) # random mapping of stimuli on cues
      
    } # end iTrial
    
    # ---------------------------------------------------------------------------- # 
    ### b) Cue:
    
    if (nStim > 4){
      cueSet <- LETTERS[4+iSes] # stimulus set to draw from (start at E)
    } else {
      cueSet <- LETTERS[iSes] # stimulus set to draw from (capital letters: A for 1, B for 2, C for 3, D for 4)
    }
    cue[iTrial] <- paste0(cueSet,cueMapping[stimuli[iTrial]]) # concatenate set letter and stimulus number
    iCue <- stimuli[iTrial] # retrieve stimulus
    countStim[iCue] <- countStim[iCue] + 1 # increase stimulus count
    
    # ---------------------------------------------------------------------------- # 
    ### c) Stakes and catch trials:
    # isCatch[iTrial] <- ifelse(iTrial %in% catchTrials, 1, 0)
    
    trialStakes <- Stakes[iSes,iCue,countStim[iCue]] # retrieve stakes on this trial
    
    if(trialStakes > 100){ # if > 100: mark as catch trial, remove 100
      isCatch[iTrial] <- 1
      trialStakes <- trialStakes %% 100
    }
    
    rewMag[iTrial] <- trialStakes %/% 10 # 10s position
    punMag[iTrial] <- trialStakes %% 10 # 1s position
    
    # Overwrite reqSide to reflect correct catch trial response:
    if(isCatch[iTrial]==1){ 
      # left if (reward left and rew > pun) or if (pun left and pun > rew)
      reqSide[iTrial] <- ifelse(rewLeft[iTrial]==1 & rewMag[iTrial] > punMag[iTrial] | 
                                  rewLeft[iTrial]==0 & rewMag[iTrial] < punMag[iTrial],
                                1,0)
    } 
    
    # ---------------------------------------------------------------------------- # 
    ### d) Outcome validity based on stimuli (not conditions):
    
    goValidity[iTrial] <- Validity[iSes,iCue,1,countStim[iCue]]
    nogoValidity[iTrial] <- Validity[iSes,iCue,2,countStim[iCue]]
    
    # ---------------------------------------------------------------------------- # 
    ### e) Timings based on stimuli (not conditions):
    ISI[iTrial] <- ISIAll[iSes,iCue,countStim[iCue]]
    ITI[iTrial] <- ITIAll[iSes,iCue,countStim[iCue]]
    
  } # end of iTrial
  
  # ---------------------------------------------------------------------------- # 
  ### Checks:
  
  if(!is.numeric(sessionNr)){stop("sessionNr not numeric")}
  if(sum(sessionNr < 1|sessionNr > nSes)>0){stop("sessionNr out of range")}
  if(!is.numeric(trialNr)){stop("trialNr not numeric")}
  if(sum(trialNr < 1|trialNr > nTrialEff)>0){stop("trialNr out of range")}
  
  if(!is.numeric(isActionCue)){stop("isActionCue not numeric")}
  if(sum(isActionCue < 0|isActionCue > 1)>0){stop("isActionCue out of range")}
  if(!is.numeric(isStakes)){stop("isStakes not numeric")}
  if(sum(isStakes < 0|isStakes > 1)>0){stop("isStakes out of range")}
  if(!is.numeric(isReleaseCue)){stop("isReleaseCue not numeric")}
  if(sum(isReleaseCue < 0|isReleaseCue > 1)>0){stop("isReleaseCue out of range")}
  if(!is.numeric(isOutcome)){stop("isOutcome not numeric")}
  if(sum(isOutcome < 0|isOutcome > 1)>0){stop("isOutcome out of range")}
  
  if(!is.character(cue)){stop("cue not character")}
  
  # ---------------------------------------------------------------------------- # 
  
  ### Concatenate all task settings into data frame:
  taskData <- data.frame(sessionNr,trialNr,
                         isActionCue,isStakes,isReleaseCue,isOutcome,
                         cue,reqAction,reqSide,rewLeft,angle,
                         rewMag,punMag,
                         goValidity,nogoValidity,isCatch,
                         ISI,ITI)
  
  return(taskData)

}

# ===================================================================================================== #
#### 6) Upsample to effective trial number ####

# nTrial <- 288; nSes <- 3; nCond <- 8

computeEffectiveTrialnr <- function(nTrial,nSes,nCond){
  
  #' Take requested number trials, check whether compatible with 
  #' - subdivision into nSes sessions
  #' - integer number of condition repetitions per session
  #' @param nTrial integer, any number of trials requested
  #' @param nSes integer, number of sessions into which trials should be split
  #' @param nCond integer, number of conditions; only integer number of conditions per session can occur
  #' @return nTrialEff integer, effective number of trials upsampled if necessary to achieve integer number of condition repetitions per session

  # Determine desired trial number per session:
  nTrialSes <- nTrial / nSes
  
  # Effective number trials per session must be at least each condition once:
  iMult <- 1 # initialize multiplier to 1
  nTrialSesEff <- nCond*iMult # session length must be at least conditions once per session 
  
  # If still not enough:
  while(nTrialSesEff < nTrialSes){
    iMult <- iMult + 1 # increment multiplier
    nTrialSesEff <- nCond*iMult # compute again:
  }
  
  # Compute effective trial number across entire task:
  nTrialEff <- nTrialSesEff*nSes
  
  cat(paste0("Use ",nTrialEff," trials for setup, later crop to ",nTrial," trials \n"))
  
  
  return(nTrialEff)
}

# a <- effective_trialnr(100,32)

# ===================================================================================================== #
#### 7) Execute all functions ####

# nTrial <- 288; nStim <- 4; nSes <- 3; nBlock <- 1; allSubs <- 1

createExecute <- function(nTrial,nStim,nSes,nBlock,allSubs=1:nSubs){
  
  #' Take inputs from other functions, create spreadsheet, save
  #' @param nTrial integer, total number of trials requested 
  #' (will be increased to higher number of conditions if necessary)
  #' @param nStim integer, number of stimuli used per session 
  #' @param nSes integer, number of sessions with different stimuli
  #' @param nBlock integer, number of blocks into which each session should be split into
  #' @param allSubs vector of subject numbers to create
  #' @return nothing returned, directly calls combineSequence to write .csv files

  # ---------------------------------------------------------------------------- #
  # a) sessions and blocks:
  # nSes <- 1 # sessions, i.e. how often repeat task with separate cue sets
  nTrialSes <- nTrial / nSes
  
  # nBlock <- 3 # blocks, i.e. break one session into blocks with separate input files and breaks in-between
  # nTrialBlock <- nTrialSes/ nBlock # only need nTrialBlockEff later
  
  # ---------------------------------------------------------------------------- #
  # b) Eliminate conditions to create stimuli:
  isElim = T
  nElim  = 1 # only eliminate one condition to get rewLeft
  
  # ---------------------------------------------------------------------------- #
  # c) Responses:
  nResp <- 2 # number possible responses
  prob <- 18/24 # .75 # feedback validity: 
  # 24 repetitions: 18 for 75%, 20 for 83%
  # 28 repetitions: 21 for 75%, 23 for 82%
  # Use .80 for 5, 10, 15, 20
  # Use 0.8571429 for 4, 8, 12
  
  # ---------------------------------------------------------------------------- #
  # d) Conditions:
  
  # Determine condition number:
  nCond <- nStim # initialize condition number, update based on planned eliminations below
  # nRepCond <- nRepStim # initialize number for condition repetitions
  
  # Increase if higher number of conditions required:
  if (isElim){
    nCond <- nCond*(2^nElim)
    # nRepCond <- nRepCond/(2^nElim)
  } 
  
  # ---------------------------------------------------------------------------- #
  # e) Trials per stimulus, condition, session:
  nTrialEff <- computeEffectiveTrialnr(nTrial,nSes,nCond)
  nTrialSesEff <- nTrialEff/nSes
  nTrialBlockEff <- nTrialSesEff/nBlock
  nRepCond <- nTrialSesEff/nCond
  nRepStim <- nTrialSesEff/nStim
  
  # ---------------------------------------------------------------------------- #
  # f) Catch trials:
  
  # percCatch <- 4/nRepStim # 4 out of 40 repetitions
  # percCatch <- 2/nRepStim # 2 out of 24 repetitions
  # nCatch <- ceiling(percCatch*nTrialSesEff/nStim)
  nCatchBlock <- 2
  cat(paste0("Use ", nCatchBlock, " catch trials for each cue for each block\n"))

  # ---------------------------------------------------------------------------- #
  # g) Timing intercepts: 
  
  ISIintercept <- 0.3
  ITIintercept <- 1.5
  
  # ---------------------------------------------------------------------------- #
  # h) Loop over above-defined functions: 
  
  for (iSub in allSubs){ # iSub <- 1
    
    # ---------------------------------------------------------------------------- #
    ### i) Cue sequence:
    cat(paste0("Start subject ",iSub),"\n")
    Cues <- makeCondSequence(nSes,nCond,nRepCond,isElim = isElim, nElim = nElim) # condition order based on conditions
    
    # ---------------------------------------------------------------------------- #
    ### ii) Stakes:
    sampleMin <- 1
    sampleMax <- 5
    sampleStep <- 1
    nCatchCond <- nCatchBlock*nBlock # if blocks: catch trials in each block
    Stakes <- makeStakesSequence(nSes = nSes, nCond = nStim, nRepCond = nRepStim, 
                                 minStakes = sampleMin, maxStakes = sampleMax, stepStakes = sampleStep, 
                                 nCatchCond = nCatchCond)
    
    # ---------------------------------------------------------------------------- #
    ### iii) Feedback validity:
    ## Separate for each condition:
    # Validity <- makeValiditySequence(nSes,nCond,nRepCond,nResp,prob) # feedback validity only based on conditions
    ## Separate for each stimulus:
    Validity <- makeValiditySequence(nSes,nStim,nRepStim,nResp,prob) # feedback validity only based on stimuli

    # ---------------------------------------------------------------------------- #
    ### iv) ISI and ITI:
    ## Separate for each condition:
    # ISIAll <- makeITISequence(nSes,nCond,nRepCond,ITIintercept)
    # ITIAll <- makeITISequence(nSes,nCond,nRepCond,ITIintercept)
    ## Separate for each stimulus:
    ISIAll <- makeITISequence(nSes,nStim,nRepStim,ISIintercept,maxSteps = 2) # from 0.1 to 0.5
    ITIAll <- makeITISequence(nSes,nStim,nRepStim,ITIintercept,maxSteps = 3) # from 1.2 to 1.8
    
    # ---------------------------------------------------------------------------- #
    ## v) Combine all:
    entireData <- combineSequence(nTrialEff, nSes, nCond,
                                Cues, Stakes, Validity, ISIAll, ITIAll, iSub,
                                isElim = isElim, nElim = nElim)
    
    # tmp <- entireData$rewMag*10 + entireData$punMag
    # table(entireData$cue,entireData$isCatch) # 2 catch trials per cue
    # table(tmp)
    
    # ---------------------------------------------------------------------------- #
    # vi )Save settings for each block separately:
    
    require(stringr)
    for (iSes in 1:nSes){ # iSes <- 1

      ## Identify session, crop:
      sesData <- entireData[((iSes-1)*nTrialSes+1):(iSes*nTrialSes),] # identify data of entire session
      sesData <- sesData[1:nTrialSesEff,] # crop to desired length
      sesData$trialNr <- c(((iSes-1)*nTrialSesEff+1):((iSes)*nTrialSesEff)) # update trial number after cropping
      
      for (iBlock in 1:nBlock){ # iSes <- 2
         
        ## Identify block:
        blockData <- sesData[((iBlock-1)*nTrialBlockEff+1):(iBlock*nTrialBlockEff),] # identify data of entire block

        ## Create effective block number based on iSes and iBlock:
        blockIdx <- (iSes - 1)*nBlock + iBlock # identifier for writing output file across sessions
        
        ## Save:
        write.csv(blockData,
                  paste0("stimuluslist_test_stimNum_",nTrial,"_sub_",str_pad(iSub, 3, pad = "0"),"_block",blockIdx,".csv"), 
                  quote = F, row.names = F)
        cat(paste0("Saved subject ", str_pad(iSub, 3, pad = "0"), " block ",blockIdx,"\n"))
        
      } # end iBlock
    } # end iSes
  } # end iSub
} # end function

# ----------------------------------------------------------------------------------------------------- #
# -------------------------------------------------------------------- #
#### Test versions 4 cues: ####

nSub <- c(1:2)
# nSub <- c(1:2,27:40)
nStim <- 4
nSes <- 3
nRep <- 22
nTrial <- nSes*nStim*nRep # 240 trials
nBlock <- 1
createExecute(nTrial = nTrial, nStim = nStim, nSes = nSes, nBlock = nBlock, allSubs=nSub) # 1:nSub)

# -------------------------------------------------------------------- #
#### Read files back in to check: ####

# setwd("D:/AA_MGNGFreeView_Task/Task3_Oysters/TrialHandler")
library(stringi)

## 4 cues:
setwd("D:/AA_MGNGFreeView_Task/Study1/TrialHandler")
iSub <- 1
nTrial <- 3* 4 * 22 # session x cues x repetitions
data1 <- read.csv(paste0("stimuluslist_test_stimNum_",nTrial,"_sub_",str_pad(iSub, 3, pad = "0"),"_block1.csv")) # 4 cues
data2 <- read.csv(paste0("stimuluslist_test_stimNum_",nTrial,"_sub_",str_pad(iSub, 3, pad = "0"),"_block2.csv")) # 4 cues
data3 <- read.csv(paste0("stimuluslist_test_stimNum_",nTrial,"_sub_",str_pad(iSub, 3, pad = "0"),"_block3.csv")) # 4 cues
data <- rbind(data1,data2,data3)

## Task conditions:
table(data$cue,data$reqAction) # separated
table(data$cue,data$rewLeft) # orthogonal

## Catch trials:
table(data$cue,data$isCatch)
table(data$cue,data$rewMag) # 4-6 reps
table(data$cue,data$punMag) # 4-6 reps

## Timings:
# mean(data$ISI)
# table(data$ISI)
# mean(data$ITI)
# table(data$ITI)

# -------------------------------------------------------------------- #
#### Demo versions: 4 cues ####

# Only 2 rep:
nSub <- 1:2
nStim <- 4
nSes <- 3
nTrial <- nSes*nStim*2 # 2 repetitions = 24 trials
nBlock <- 1
createExecute(nTrial = nTrial, nStim = nStim, nSes = nSes, nBlock = nBlock, allSubs=nSub) # 1:nSub)

# Only 4 rep:
nSub <- 1:2
nStim <- 4
nSes <- 3
nTrial <- nSes*nStim*4 # 10 repetitions = 80 trials
nBlock <- 1
createExecute(nTrial = nTrial, nStim = nStim, nSes = nSes, nBlock = nBlock, allSubs=nSub) # 1:nSub)

# Only 8 rep:
nSub <- 1:2
nStim <- 4
nSes <- 3
nTrial <- nSes*nStim*8 # 10 repetitions = 80 trials
nBlock <- 1
createExecute(nTrial = nTrial, nStim = nStim, nSes = nSes, nBlock = nBlock, allSubs=nSub) # 1:nSub)

# ----------------------------------------------------------------------------------------------------- #
#### Duration of total experiment: #### 

### 1) Duration of 1 trial:
# ITI: 1.5 sec
# Action cue: 0.7
# Stakes: 1.5
# ISI: 0.3
# Release cue: 0.6
# ISI: 0.7
# Outcome: 1.0 
# disregard catch trials
trialDur <- 1.5+0.7+1.5+0.3+0.6+0.7+1.0 # 1 trial: 6.3

### 2) Number trials:
nSes <- 3
nStim <- 4
nRep <- 24
nTrial <- nSes * nStim * nRep

### 3) Duration of entire experiment:
totalDur <- trialDur*nTrial/60 

cat(nSes,"sessions,",nStim,"stimuli,",nRep,"repetitions:",totalDur,"minutes\n")

# 3 sessions, 4 stimuli, 20 repetitions: 25.20 minutes
# 3 sessions, 4 stimuli, 22 repetitions: 27.72 minutes
# 3 sessions, 4 stimuli, 28 repetitions: 30.24 minutes
# ----------------------------------------------------------------------------------------------------- # 
#### Plot sequence ####

# Cues <- makeCondSequence(nSes,nStim,nRepStim,nTrialSes)
# Validity <- makeValiditySequence(nSes,nStim,nResp,nRepStim,prob)
# ITI <- makeITISequence(nSes,nStim,nRepStim)
# 
# # Stimulus sequence per block:
# for (iSes in 1:nSes){
#   plot(Cues[iSes,],type = "b",main = paste0("Stimulus sequence block ",iSes))
#   readline(prompt="Press [enter] to continue")
# }
# 
# # Feedback validity sequency per stimulus per block:
# for (iSes in 1:nSes){
#   par(mar=c(1,1,1,1))
#   par(mfrow=c(nStim,nResp))
#   for (iStim in 1:nStim){
#     for (iResp in 1:nResp){
#       plot(Validity[iSes,iStim,iResp,], type = "l", 
#            main = paste0("Feedback sequence block ", iSes, " Stimulus ", iStim, " Response", iResp))
#     }
#   }
#   readline(prompt="Press [enter] to continue")
#   par(mar=c(5.1,4.1,4.1,2.1))
#   par(mfrow=c(1,1))
# }

# Validity[1,2,1,]

# -----------------------------------------------------------------------------------------------------
# Recombine RGB colors into Python units:
# x <- 0.2
# x*2-1

# END