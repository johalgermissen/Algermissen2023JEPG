#### 00_functions_analyze.R ####

#' Collection of functions to analyze and plot eye-tracking and behavioral data for both Sample 1 and Sample 2.

# =============================================================================================== #
#### Preprocess behavioral data (create extra variables, convert to factor, etc.): ####

wrapper_preprocessing <- function(data){
  #' Just a wrapper around various pre-processing functions specified below
  #' @param data    data frame with trial-level data
  #' @return data   same data frame with additional pre-processed variables

  # ------------------------------------------ #
  ### Recover isCatch:
  
  cat("Recover catch trials via outcome == NA \n")
  data$isCatch <- ifelse(is.na(data$outcome),1,0)
  # table(data$isCatch)
  # table(data$isCatch,data$subject)
  
  # ------------------------------------------ #
  ### Create new variables:
  
  cat("Create new variables \n")
  # names(data)
  data$trialNr <- data$trialnr # simple rename
  data <- create_extra_variables(data)
  # names(data)
  
  # table(data$response_exact_f)
  # table(data$sesNr,data$subject)
  # table(data$sesNr,data$trialnr)
  # table(data$blockNr,data$subject)
  # table(data$blockNr,data$trialnr)
  # table(data$trialNrSes,data$subject)
  # table(data$trialnrBlock,data$subject)
  
  # ------------------------------------------ #
  ### Recode to proper factors:
  data <- recode_char2fac(data)
  # str(data) # no more "char" variables, all factors
  
  # ------------------------------------------ #
  ### Recode selected factors (firstfix_side_f and firstfix_out_f) to numerical variables:
  data <- recode_fac2num(data)
  # str(data)
  # data[,c("firstfix_side_f","firstfix_side_n","firstfix_out_f","firstfix_out_n")]
  
  # ------------------------------------------ #
  ### Z-standardize numeric variables:
  data <- z_standardize_variables(data)
  
  # ------------------------------------------ #
  ### Half of data:
  data$trialnrHalf_f <- NA
  
  for(iSub in unique(data$subject)){ # iSub <- 24
    
    subIdx <- which(data$subject == iSub) # row indices for subject
    maxTrialSes <- max(data[subIdx,"trialNrSes"])
    cat("Subject",iSub,":",maxTrialSes,"trials per session\n")
    
    idx <- which(data$subject == iSub & data$trialNrSes <= maxTrialSes/2)
    data$trialnrHalf_f[idx] <- "first"
    idx <- which(data$subject == iSub & data$trialNrSes > maxTrialSes/2)
    data$trialnrHalf_f[idx] <- "last"
  }
  
  data$trialnrHalf_f <- factor(data$trialnrHalf_f)
  table(data$trialnrHalf_f)  
  
  return (data)
}

# =============================================================================================== #
#### Load all data files in given directory: ####

load_data <- function(datadir){
  #' Load all data files in given directory.
  #' @param datadir string, directory from where to load all data
  #' @return data   data frame with all variables pre-processed
  
  cat("Load and concatenate all .csv files from directory\n",datadir,"\n")
  allFiles <- list.files(datadir, pattern=".csv",full=TRUE)
  data <- do.call("rbind", lapply(allFiles, read.csv, header=T))
  
  nSub <- length(unique(data$subject))
  nTrial <- max(data$trialNr)
  cat("Found data of",nSub,"subjects with max.",nTrial,"trials\n")
  
  return(data)
}

# =============================================================================================== #
#### Create extra variables (data-set/ task-specific, just outsourced to here): ####

create_extra_variables <- function(data){
  #' Create new variables (factors) 
  #' @param data    data frame just loaded into R
  #' @return data   same data frame with all variables pre-processed

  # Global settings:
  nSub <- length(unique(data$subject))
  nTrial <- max(data$trialNr)
  
  # --------------------------------------------------------------------- #
  ### Create extra response variables:

  cat("Create response variable for any button press\n")
  data$response_all <- ifelse(is.na(data$RT),0,1) # 1 for any trial with RT
  data$response_all_f <- factor(ifelse(data$response_all==1,"Go","NoGo"))
  
  cat("Create exact response (left/right/NoGo) variables\n")
  data$response_exact_n <- ifelse(data$response == 1 & data$respSide == 1, 1,
                             ifelse(data$response == 1 & data$respSide == 0, 2,
                                    ifelse(data$response == 0, 3,NA)))
  data$response_exact_f <- factor(ifelse(data$response == 1 & data$respSide == 1, "Left",
                             ifelse(data$response == 1 & data$respSide == 0, "Right",
                                    ifelse(data$response == 0, "NoGo",NA))))

  cat("Create response variable with wrong side responses as NA\n")
  data$response_valid <- data$response # copy
  data$response_valid[data$response == 1 & data$reqSide != data$respSide] <- NA # if Go and wrong side: NA
  
  # --------------------------------------------------------------------- #
  ### Create extra RT variables:
  
  cat("Create RT variable for before deadline only\n")
  # matches data$response
  data$RT_inTime <- ifelse(data$response==1,data$RT,NA) # RT only for valid Go trials (in time)

  cat("Create RT variable cropped at 200 - 800 ms\n")
  data$response_cleaned <- data$response_all
  data$response_cleaned[which(data$response_all==1 & (data$RT < 0.200 | data$RT > 0.800))] <- NA
  # data$response_cleaned <- ifelse(data$RT > 0.200 & data$RT < 0.800, 1,
  #                                   ifelse(data$response == 0, 0,
  #                                          NA)) # clean too early and too late RTs
  data$RT_cleaned <- ifelse(data$RT > 0.200 & data$RT < 0.800, data$RT, NA) # clean too early and too late RTs
  # clean too early RTs?
  
  # Z-standardize to detect outliers
  criterion <- 2
  cat(paste0("Create RT variable cropped at -",criterion,"*SD and +",criterion,"*SD\n"))
  cat(paste0("Create RT variable cropped 3rd quartile + 1.5 x interquartile range\n"))
  
  varName_z <- paste0("RT_z",criterion,"cleaned")
  varName_IQR <- paste0("RT_IQRcleaned")
  data[,varName_z] <- data$RT
  data[,varName_IQR] <- data$RT
  for (iSub in 1:nSub){ # iSub <- 1
    subIdx <- which(data$subject == iSub & data$isCatch == 0)
    subData <- data[subIdx,]
    
    ## Standardize RTs:
    subData$RT_z <- as.numeric(scale(subData$RT))
    # Too fast RTs:
    tooLowIdx <- which(subData$RT_z < -1*criterion)
    data[subIdx[tooLowIdx],varName_z] <- NA
    # data[subIdx[tooLowIdx],"RT"] # check
    # Too slow RTs:
    tooHighIdx <- which(subData$RT_z > 1*criterion)
    data[subIdx[tooHighIdx],varName_z] <- NA
    # data[subIdx[tooHighIdx],"RT"] # check
    
    ## Interquartile range:
    # https://en.wikipedia.org/wiki/Outlier#Tukey's_fences
    lowQuart <- as.numeric(quantile(subData$RT,0.25, na.rm = T))
    highQuart <- as.numeric(quantile(subData$RT,0.75, na.rm = T))
    IQR <- highQuart - lowQuart
    tooHighIdx <- which(subData$RT_z > highQuart + 1.5*IQR)
    data[subIdx[tooHighIdx],varName_IQR] <- NA
    
  }
  # data[,c("RT",varName_z,varName_IQR)] # check

  # --------------------------------------------------------------------- #
  ### Create extra accuracy variables:
  
  cat("Create accuracy variables for action and side\n")
  
  data$GoACC <- ifelse(data$reqAction==data$response,1,
                       ifelse(data$reqAction!=data$response,0,NA))
  
  data$SideACC <- ifelse(data$reqSide==data$respSide,1,
                         ifelse(data$reqSide!=data$respSide,0,NA))
  
  # --------------------------------------------------------------------- #
  ### Create dwell time difference:
  
  if ("dwell_rew_abs" %in% names(data)){ # check if eye-tracking data available
    cat("Create extra dwell time difference variable\n")
    data$dwell_out_dif <- data$dwell_rew_abs - data$dwell_pun_abs
  }

  # --------------------------------------------------------------------- #
  ### Turn variables to factors/ numerical variables:
  cat("Turn variables into factors/ numerical variables\n")
  
  ## Subject number:
  data$subject_f <- factor(data$subject)
  
  ## Task factors:
  data$reqAction_f <- factor(ifelse(data$reqAction==1,"Go","NoGo"))
  data$reqSide_f <- factor(ifelse(data$reqSide==1,"Left","Right"))
  data$rewLeft_f <- factor(ifelse(data$rewLeft==1,"Left","Right"))
  
  ## Stakes:
  data$difMag <- data$rewMag - data$punMag
  
  ## Responses:
  data$response_f <- factor(ifelse(data$response==1,"Go","NoGo"))
  # data[,c("response","response_f")]
  
  data$respSide_f <- factor(ifelse(data$respSide==1,"Left",
                                   ifelse(data$respSide==0,"Right",NA)))
  # data[,c("respSide","respSide_f")]
  
  ## Accuracy:
  data$ACC_f <- factor(ifelse(data$ACC==1,"Correct",
                              ifelse(data$ACC==0,"Incorrect",NA)))
  # data[,c("ACC","ACC_f")]
  data$GoACC_f <- factor(ifelse(data$GoACC==1,"Correct",
                                ifelse(data$GoACC==0,"Incorrect",NA)))
  data$SideACC_f <- factor(ifelse(data$SideACC==1,"Correct",
                                  ifelse(data$SideACC==0,"Incorrect",NA)))
  
  # --------------------------------------------------------------------- #
  ### Recode outcome stakes into categorical variables (numeric and factor):
  cat("Recode outcome stakes into categorical variables (numeric and factor)\n")
  
  data$outcome_f <- factor(ifelse(data$outcome>0, "reward",
                                  ifelse(data$outcome<0, "punishment",
                                         NA)))
  data$outcome_n <- as.numeric(ifelse(data$outcome>0, 1,
                                      ifelse(data$outcome<0, -1,
                                             NA)))
  data$outcome_magnitude_n <- data$outcome # copy over 
  # data[,c("outcome","outcome_f","outcome_n")]
  
  # --------------------------------------------------------------------- #
  ### Create extra outcome variables:
  
  cat("Create outcome variable with wrong side responses as NA\n")
  data$outcome_valid_n <- data$outcome_n # copy
  data$outcome_valid_n[data$response == 1 & (data$reqSide != data$respSide)] <- NA # if Go and wrong side: NA
  # tapply(is.na(data$outcome_n),data$subject,sum)
  # tapply(is.na(data$outcome_valid_n),data$subject,sum)
  
  data$outcome_magnitude_valid_n <- data$outcome_magnitude_n # copy
  data$outcome_magnitude_valid_n[data$response == 1 & (data$reqSide != data$respSide)] <- NA # if Go and wrong side: NA
  
  # -------------------------------------------------------------------------- #
  ### Create extra validity variables:
  
  cat("Create variables recovery outcome validity\n")
  data$validity_n <- ifelse((data$ACC * 2 - 1) == data$outcome_n, 1, 0)
  data$validity_f <- factor(data$validity_n, levels = c(1, 0), labels = c("valid", "invalid"))
  
  # --------------------------------------------------------------------- #
  ### Add trial number per session and per block:
  
  data$trialNrSes <- NA
  data$trialNrBlock <- NA
  
  for (iSub in unique(data$subject)){
    
    # Select subject data:
    cat("Start subject",iSub,"\n")
    subIdx <- which(data$subject==iSub)
    subData <- data[subIdx,]
    
    # Determine task settings:
    cat("Determine task settings\n")
    nTrial <- length(unique(subData$trialNr))
    if (any(colnames(subData)=="sesNr" & sum(is.na(subData$sesNr))==0)){
      nSes <- length(unique(subData$sesNr))
    } else{
      nSes <- 3
      data[subIdx,"sesNr"] <- rep(1:nSes,each=nTrial/nSes)
    }
    if (any(colnames(subData)=="blockNr" & sum(is.na(subData$blockNr))==0)){
      nBlock <- length(unique(subData$blockNr))
    } else{
      nBlock <- 3
      data[subIdx,"blockNr"] <- rep(1:nBlock,each=nTrial/nBlock)
    }
    cat("Found",nTrial,"trials,",nSes,"sessions,",nBlock,"blocks\n")
    
    # Sessions:
    cat("Create trial number per session\n")
    data$trialNrSes[subIdx] <- (subData$trialNr - 1) %% (nTrial/nSes) + 1
    
    # Blocks:
    cat("Create trial number per block\n")
    data$trialNrBlock[subIdx] <- (subData$trialNr - 1) %% (nTrial/nBlock) + 1
    
  }
  # table(data$subject,data$trialNrSes)
  # table(data$subject,data$trialNrBlock)
  # cbind(data$trialNr,data$trialNrSes,data$trialNrBlock)
  
  cat("Finished :-)\n")
  return(data)
}

# ===============================================================================================#
#### Recode character variables to proper factors: ####

recode_char2fac <- function(data){
  #' Turn string variables into proper factors again
  #' @param data    data frame with trial-level data
  #' @return data   same data frame with all character variables turned into proper factors

  cat("Recode any character variable to factor\n")
    
  for (iCol in 1:ncol(data)){
    if (is.character(data[,iCol])){ # if character
      cat(paste0("Recode variable ",names(data)[iCol],"\n"))
      data[,iCol] <- factor(data[,iCol]) # recode to proper factor
    }
  }
  
  cat("Finished :-)\n")
  return(data)
}

# ===============================================================================================#
#### Recode all numerical variables to factors: ####

recode_num2fac <- function(data,variables=NULL){
  #' Recode selected numerical variable to factor
  #' @param data    data frame with trial-level data
  #' @variables     vector of strings, numerical variables to turn into factors (if not provided: all numerical variables in data frame)
  #' @return data   same data frame with all numerical variables also available as factor ("_f")

  cat("Recode selected numerical variables to factors\n")
  
  ### Determine if input variables given, otherwise take all numerical variables: 
  if(is.null(variables)){
    cat("No variables given, take all numerical variables\n")
    variables <- c()
    for (iCol in 1:ncol(data)){
      if (is.numeric(data[,iCol])){ # if numeric
        variables <- c(variables,names(data)[iCol]) # add variable name
      }
    }
  }

  ### Number variables:
  nVar <- length(variables)
  
  ### Loop through variables:  
  for (iVar in 1:nVar){ # loop through variables
    varName <- variables[iVar]
    cat(paste0("Recode variable ",varName,"\n"))
    newVarName <- paste0(varName,"_f") # new variable name
    data[,newVarName] <- factor(data[,varName]) # recode to proper factor
  }
  cat("Finished :-)\n")
  return(data)
}

# ===============================================================================================#
#### Recode selected factors into numerical variables: ####

recode_fac2num <- function(data){
  #' Recode selected factor to numerical
  #' @param data data frame with trial-level data
  #' @return data with selected factors now also available as numerical variables
  
  cat("Recode selected variables to numeric\n")
  
  cat("Recode firstfix_side_n to numeric\n")
  data$firstfix_side_n <- ifelse(data$firstfix_side_f=="left",1,
                                 ifelse(data$firstfix_side_f=="right",0,
                                        0.5)) # NA as 0.5 for plotting
  cat("Recode firstfix_out_n to numeric\n")
  data$firstfix_out_n <- ifelse(data$firstfix_out_f=="reward",1,
                                 ifelse(data$firstfix_out_f=="punishment",0,
                                        0.5)) # NA as 0.5 for plotting
  
  # data[,c("firstfix_side_f","firstfix_side_n","firstfix_out_f","firstfix_out_n")]
  
  cat("Finished :-)\n")
  return(data)
}

# ===============================================================================================#
#### Z-standardize variables for mixed models: ####

z_standardize_variables <- function(data,varVec=NULL){
  #' Standardize predictors (hard-coded selection)
  #' @param data data frame with trial-level data
  #' @param varVec vector of strings, variables to standardize
  #' @return data with hard-coded selection of variables z-standardize (ending "_z")

  cat("Z-standardize selected variables\n")
  
  ## Specify default variables to standardize:
  if (is.null(varVec)){
    varVec <- c("rewMag","punMag", # stakes
                "RT", # RT
                "dwell_left_abs","dwell_right_abs","dwell_left_rel","dwell_right_rel", # dwell left/ right
                "dwell_rew_abs","dwell_pun_abs","dwell_rew_rel","dwell_pun_rel","dwell_out_dif")
  }
  
  ## Loop through vector, standardize:
  for (iVar in varVec){
    cat("Standarize variable",iVar,"\n")
    newVar <- paste0(iVar,"_z")
    data[,newVar] <- as.numeric(scale(data[,iVar]))
  }

  cat("Finished :-)\n")
  return(data)
}
#### Aggregate 
# ===============================================================================================#
#### Add variables one trial back: ####

add_lag_trial <- function(data,variables,n = 1){
  #' Add variables for n-trials-back per subject per block
  #' @param data        data frame with trial-level data
  #' @param variables   vector of strings, variables to recode 
  #' @param n           integer, lag to be used for creating variables (default: 1)
  #' @return data       same data frame with added version of selected variables one trial back

  cat("Add for selected variables values of last trial\n")
  
  # names(data)
  # variables <- c("rewLeft_f","firstfix_side_f","firstfix_out_f","response")
  
  # Defaults:
  subVar <- "subject"
  blockVar <- "blockNr"
  nSub <- length(unique(data[,subVar]))
  nBlock <- length(unique(data[,blockVar]))
  nVar <- length(variables)
  
  # Initialize new variables to NA:
  for (iVar in 1:nVar){
    varName <- variables[iVar]
    newVarName <- paste0(varName,"_pastTrial") 
    data[,newVarName] <- NA
  } # iVar   
  
  # Loop through subjects:
  for (iSub in 1:nSub){ # iSub <- 1
    cat(paste0("Start subject ",iSub,"\n"))
    
    # Loop through blocks:
    for (iBlock in 1:nBlock){ # iBlock <- 1
      cat(paste0("Start block ",iBlock,"\n"))
      
      rowIdx <- which(data[,subVar]==iSub & data[,blockVar]==iBlock)
      
      # Loop through variables:
      for (iVar in 1:nVar){ # iVar <- 1
        
        varName <- variables[iVar]
        cat(paste0("Start variable ",varName,"\n"))
        newVarName <- paste0(varName,"_pastTrial") 
        data[rowIdx,newVarName] <- lag(data[rowIdx,varName], k = n)
        
      } # iVar   
    } # iBlock
  } # end iSub
  
  # Convert to factor:
  for (iVar in 1:nVar){
    varName <- variables[iVar]
    if (is.factor(data[,varName])){
      factorLevels <- levels(data[,varName])
      nLevels <- length(factorLevels)
      newVarName <- paste0(varName,"_pastTrial") 
      data[,newVarName] <- factor(data[,newVarName],levels = 1:nLevels, labels = factorLevels) 
    }
  } # iVar   

  cat("Finished :-)\n")
  return(data)
    
} # end function

# ===============================================================================================#
#### Add variables one stimulus presentation back: ####

add_lag_stim <- function(data,variables,n=1){
  #' Add variables for n-stimuli-back per subject
  #' @param data        data frame with trial-level data
  #' @param variables   vector of strings, variables to recode 
  #' @param n           integer, lag to be used for creating variables (default: 1)
  #' @return data       same data frame with added version of selected variables one stimulus back
  
  # names(data)
  # data <- data
  # variables <- c("rewLeft_f","firstfix_side_f","firstfix_out_f","response")
  cat("Add for selected variables values of last stimulus\n")
  
  # Defaults:
  subVar <- "subject"
  trialVar <- "trialnr"
  stimVar <- "cue"
  stimCountVar <- "cueCount"
  nSub <- length(unique(data[,subVar]))
  subVec <- unique(data[,subVar])
  nTrial <- length(unique(data[,trialVar]))
  nStim <- length(unique(data[,stimVar]))
  nVar <- length(variables)
  
  # Create numerical variable of cue:
  newStimVar <- paste0(stimVar,"_n")
  cat("Create new numeric version of cue variable called ",newStimVar,"\n")
  data[,newStimVar] <- as.numeric(data[,stimVar])
  # data[,c("cue","cue_n")]
  
  # Create cue count variable:
  data[,stimCountVar] <- NA
  
  # Initialize new variables to NA:
  for (iVar in 1:nVar){
    varName <- variables[iVar]
    newVarName <- paste0(varName,"_pastStim") 
    data[,newVarName] <- NA
  } # iVar   
  
  # Loop through subjects:
  for (iSub in subVec){ # iSub <- 1
    
    cat("# ============================================= # \n")
    cat(paste0("Start subject ",iSub,"\n"))
    
    stimCount <- rep(0,nStim) # initialize stimulus count
    
    # Loop through trials:
    for (iTrial in 1:nTrial){ # iTrial <- 1
      
      cat(paste0("Start trial ",iTrial,"\n"))
      
      rowIdx <- which(data[,subVar]==iSub & data[,trialVar]==iTrial) # row of trial
      stimIdx <- data[rowIdx,newStimVar] # cue of trial
      
      # Increment trial count:
      stimCount[stimIdx] <- stimCount[stimIdx] + 1
      
      # Update cue count variable:
      data[rowIdx,stimCountVar] <- stimCount[stimIdx]
      
      # Check if stimulus presented before:
      if(stimCount[stimIdx] > 1){
        
        lastStimCount <- stimCount[stimIdx] - 1 
        lastRowIdx <- which(data[,subVar]==iSub & data[,newStimVar]==stimIdx & data[,stimCountVar] == lastStimCount) # row of last trial with same stimulus
        
        # Loop through variables:
        for (iVar in 1:nVar){ # iVar <- 1
          
          varName <- variables[iVar]
          newVarName <- paste0(varName,"_pastStim") 
          data[rowIdx,newVarName] <- data[lastRowIdx,varName]
          # data[rowIdx,c(varName,newVarName)]
          
        } # iVar   
      } # if stimCount > 1
      
    } # end iTrial
  } # end iSub
  
  # Convert to factor:
  for (iVar in 1:nVar){
    varName <- variables[iVar]
    if (is.factor(data[,varName])){
      cat(paste0("Recode variable ",varName," to factor\n"))
      factorLevels <- levels(data[,varName])
      newVarName <- paste0(varName,"_pastStim") 
      data[,newVarName] <- factor(data[,newVarName],levels = 1:length(factorLevels), labels = factorLevels) 
    }    
  } # iVar   
  
  cat("Finished :-)\n")
  return(data)
  
} # end function

# =============================================================================================== #
####  Create smoothing function (convolution filter) that works also at the edges: ####

myFilter <- function(myvariable, window_width = 5){
  #' Smooth vector of numeric data, adaptively shorten smoothing kernel at the edges as appropriate
  #' @param myvariable    vector with numeric data, data to smooth
  #' @param window_width  numeric scalar, length of kernel (default: 5)
  #' @return myvariable   input vector, but smoothed     
  
  cat(paste0("Start with window width = ",window_width,"\n"))
  endvariable <- stats::filter(myvariable, 
                               filter = rep(1/window_width, window_width), 
                               "convolution", sides = 2, circular = F)
  
  while(sum(is.na(endvariable)) > 0) {
    window_width <- max(1,window_width - 2)
    cat(paste0("Reduce to window width = ",window_width,"\n"))
    endvariable2 <- stats::filter(myvariable, 
                                  filter = rep(1/window_width, window_width),
                                  "convolution", sides = 2, circular = F)
    for (i in 1:length(endvariable)){
      endvariable[i] <- ifelse(is.na(endvariable[i]),
                               endvariable2[i], endvariable[i])
    } 
  }
  return(endvariable)
}

# =============================================================================================== #
####  Create percentiles: ####

create_percentiles <- function(data,varName,nPerc=5,perSub=F){
  #' Turn string variables into proper factors again
  #' @param data      data frame with trial-level data
  #' @param varName   string, variable for which to create percentiles
  #' @param nPerc     scalar integer, number of percentiles to create
  #' @param perSub    Boolean, create percentiles for each subject (T) or across all subjects (F), default F
  #' @return data     same data frame with all character variables turned into proper factors
  
  subVar <- "subject"  
  nSub <- length(unique(data[,subVar]))
  
  data$selVar <- data[,varName]

  if (perSub == F){
    # a) Across entire data set:
    newVarName <- paste0(varName,"_",nPerc,"Perc") # new variable name
    data[,newVarName] <- NA # initialize
    
    data[,newVarName] <- with(data, cut(selVar, 
                                       breaks=quantile(selVar, probs=seq(0, 1, by=1/nPerc), na.rm=TRUE), 
                                       include.lowest=TRUE))
  } else {
    ## b) For each subject separately:
    newVarName <- paste0(varName,"_",nPerc,"PercSub") # new variable name
    data[,newVarName] <- NA # initialize
    
    # Loop over subjects:
    for (iSub in 1:nSub){ # iSub <- 1
      subIdx <- which(data[,subVar] == iSub)
      subData <- data[subIdx,]
      subQuantiles <- quantile(data$selVar, probs=seq(0, 1, by=1/nPerc), na.rm=TRUE)
      data[subIdx,newVarName] <- with(subData, cut(selVar, breaks=subQuantiles, 
                                                       include.lowest=TRUE))
    }
  }

  # Turn into numeric:
  data[,newVarName] <- as.numeric(data[,newVarName]) # into numeric
  cat(paste0("Take variable ",varName,", add variable ",newVarName,"\n"))
  
  # Give bin sizes:
  cat("Bin sizes:\n")
  print(table(data[,newVarName])) # bin sizes
  
  # Output:
  cat("Finished :-)\n")
  return(data)
}

# =============================================================================================== #
#### Create outer tertiles: ####

create_outer_tertiles <- function(data,varName){
  
  nPerc <- 3
  newVarName <- paste0(varName,"_",nPerc,"Perc") # new variable name
  newVarName_f <- paste0(newVarName,"_f")

  ## Delete variables if they already exist:  
  if(newVarName %in% names(data)){
    cat("Delete old variables\n")
    data[,newVarName] <- NULL
    data[,newVarName_f] <- NULL
  }
  
  ## Create 3 percentiles:
  data <- create_percentiles(data, varName, nPerc=nPerc, perSub=F)
  
  ## Turn into factor:
  data[,newVarName_f] <- factor(ifelse(data[,newVarName]==1,"low",
                                          ifelse(data[,newVarName]==2,"middle",
                                                 ifelse(data[,newVarName]==3,"high",
                                                        NA))))
  
  ## Delete middle tertile:
  data[data[,newVarName]==2,newVarName] <- NA # delete middle tertile
  data[data[,newVarName_f]=="middle",newVarName_f] <- NA # delete middle tertile

  ## Give bin sizes:
  cat("New bin sizes of outer tertiles:\n")
  print(table(data[,newVarName])) # bin sizes
  
  cat(paste0("Missingness: ",sum(is.na(data[,newVarName])),"\n")) # bin sizes
 
  ## Output:
  cat("Finished :-)\n")
  return(data)
  
}

# =============================================================================================== #
#### Count cumulative number of how often each cue has appeared: ####

add_cueRep <- function(data){
  #' Count cumulative number of how often each cue has appeared
  #' @return data data set with new variable "cueRep" indicating cumulative number of cue appearances for each subject

  ## Variable names:
  subVar <- "subject"
  cueVar <- "cue"
  trialVar <- "trialNr"
  
  ## Convert cues to numeric:
  data$cue_n <- as.numeric(as.factor(data[,cueVar]))
  data$cue_n <- data$cue_n - min(data$cue_n) + 1 # correct so first cue index becomes 1
  # table(data[,cueVar],data$cue_n) # check
  
  ## Initialize empty variable:
  data$cueRep <- NA # initialize variable
  
  ## Loop over subjects:
  for (iSub in unique(data[,subVar])){ # iSub <- 34
    
    ## Select subject data:
    subIdx <- which(data[,subVar] == iSub)
    
    ## Descriptives:
    nStim <- length(unique(data$cue_n[subIdx]))
    nTrial <- length(subIdx)
    cat("Start subject",iSub,"with",nStim,"stimuli and",nTrial,"trials\n")

    data[subIdx,trialVar] <- 1:nTrial # overwrite trial number
    cat("Create new consecutive trial number\n")

    ## Initialize count:
    countStim <- rep(0,nStim)
    
    ## Loop over trials:
    for (iTrial in 1:nTrial){
      
      trialIdx <- which(data[,subVar] == iSub & data[,trialVar] == iTrial) # trial index
      trialCue <- data[trialIdx,"cue_n"] # cue on this trial
      countStim[trialCue] <- countStim[trialCue] + 1 # increment
      data[trialIdx,"cueRep"] <- countStim[trialCue] # store current index
    } 
  }
  
  ## Check overall data:
  # table(data$cueRep)
  if (sum(is.na(data$cueRep)) > 0){stop("NAs produced")}

  return(data)
}

# =============================================================================================== #
#### Fit normal distribution and compute RMSE ####

fit_normal_deviation <- function(x,nRep=1000){
  #' Fit normal distribution to data, sample random numbers, sort both vectors, subtract from each, return RMSE
  #' @param x     numerical vector, variable
  #' @param nRep  integer, how often to return procedure
  #' @return RMSE root-mean-squared-error (RMSE) between observed an
  require(MASS)

  ## Exclude NANs:
  x <- x[!(is.na(x))] # exclude NANs
  x <- sort(x) # sort
  
  ## Fit distribution:
  fit <- fitdistr(x, "normal") # fit
  para <- fit$estimate # extract fitted distribution parameters
  
  ## Sample new data, sort:
  xhat <- rnorm(length(x), para[1], para[2])
  xhat <- sort(xhat)

  ## Compute RMSE:
  RMSE <- sqrt(mean((x-xhat)^2))
  
  ## Plot:
  plot(density(x), type = "l", col = "blue", lwd = 2)
  lines(density(xhat), col = "red", lwd = 2, lty = 2)
  legend("topright", legend=c("Observed", "Predicted"),
         col=c("blue", "red"), lty=1:2, cex=0.8)
  
  return(RMSE)
}

# =============================================================================================== #
#### Aggregate per subject (behavior only): ####

aggregate_subject_behav <- function(data){
  #' Aggregate trial-by-trial data per subject.
  #' @param data data frame with trial-level data
  #' @return data aggregated per subject
  
  require(plyr)
  
  aggrData <- ddply(data, .(subject), function(x){
    
  # -------------------------------------------------------------- #
  ## Task and behavioral variables:
  
  subject <- x$subject[1]
  cat("Aggregate subject ",subject,"\n")
  response <- mean(x$response, na.rm = T)
  respSide <- mean(x$respSide, na.rm = T)
  respLate <- mean(x$respLate, na.rm = T)
  
  ACC <- mean(x$ACC, na.rm = T)
  GoACC <- mean(x$GoACC, na.rm = T)
  SideACC <- mean(x$SideACC, na.rm = T)
  
  xRT <- subset(x, is.na(respLate) | respLate==0) # exclude late responses
  RT <- mean(xRT$RT, na.rm = T)
  
  return(data.frame(subject,response,respSide,respLate,ACC,GoACC,SideACC,RT))
  dev.off()})
  
return(aggrData)

}

# =============================================================================================== #
#### Aggregate per subject (also first fixation and dwell time per condition): ####

aggregate_subject_eye <- function(data){
  #' Aggregate trial-by-trial data per subject
  #' @param data data frame with trial-level data
  #' @return data aggregated per subject

  require(plyr)
  
  aggrData <- ddply(data, .(subject), function(x){
    
    # -------------------------------------------------------------- #
    ## Task and behavioral variables:
    
    subject <- x$subject[1]
    cat("Aggregate subject ",subject,"\n")
    response <- mean(x$response)
    
    ACC <- mean(x$ACC)
    RT <- mean(x$RT, na.rm = T)
    
    # -------------------------------------------------------------- #
    ## First fixation:
    
    # Overall:
    fix_left <- sum(x$firstfix_side_f=="left", na.rm = T)
    fix_right <- sum(x$firstfix_side_f=="right", na.rm = T)
    fix_rew <- sum(x$firstfix_out_f=="reward", na.rm = T)
    fix_pun <- sum(x$firstfix_out_f=="punishment", na.rm = T)
    
    # First fixation left/right given required action:
    fix_left_req_go <- sum(x$firstfix_side_f=="left" & x$reqAction_f == "Go", na.rm = T)
    fix_left_req_nogo <- sum(x$firstfix_side_f=="left" & x$reqAction_f == "NoGo", na.rm = T)
    fix_right_req_go <- sum(x$firstfix_side_f=="right" & x$reqAction_f == "Go", na.rm = T)
    fix_right_req_nogo <- sum(x$firstfix_side_f=="right" & x$reqAction_f == "NoGo", na.rm = T)
    # First fixation reward/punishment given required action:
    fix_rew_req_go <- sum(x$firstfix_out_f=="reward" & x$reqAction_f == "Go", na.rm = T)
    fix_rew_req_nogo <- sum(x$firstfix_out_f=="reward" & x$reqAction_f == "NoGo", na.rm = T)
    fix_pun_req_go <- sum(x$firstfix_out_f=="punishment" & x$reqAction_f == "Go", na.rm = T)
    fix_pun_req_nogo <- sum(x$firstfix_out_f=="punishment" & x$reqAction_f == "NoGo", na.rm = T)
    
    # Response given first fixation left/right:
    fix_left_act_go <- sum(x$firstfix_side_f=="left" & x$response_f == "Go", na.rm = T)
    fix_left_act_nogo <- sum(x$firstfix_side_f=="left" & x$response_f == "NoGo", na.rm = T)
    fix_right_act_go <- sum(x$firstfix_side_f=="right" & x$response_f == "Go", na.rm = T)
    fix_right_act_nogo <- sum(x$firstfix_side_f=="right" & x$response_f == "NoGo", na.rm = T)
    # Response given first fixation reward/punishment:
    fix_rew_act_go <- sum(x$firstfix_out_f=="reward" & x$response_f == "Go", na.rm = T)
    fix_rew_act_nogo <- sum(x$firstfix_out_f=="reward" & x$response_f == "NoGo", na.rm = T)
    fix_pun_act_go <- sum(x$firstfix_out_f=="punishment" & x$response_f == "Go", na.rm = T)
    fix_pun_act_nogo <- sum(x$firstfix_out_f=="punishment" & x$response_f == "NoGo", na.rm = T)
    
    # RT given first fixation left/right:
    fix_left_RT <- mean(subset(x,firstfix_side_f=="left")$RT, na.rm = T)
    fix_right_RT <- mean(subset(x,firstfix_side_f=="right")$RT, na.rm = T)
    
    # RT given first fixation left/right:
    fix_rew_RT <- mean(subset(x,firstfix_out_f=="reward")$RT, na.rm = T)
    fix_pun_RT <- mean(subset(x,firstfix_out_f=="punishment")$RT, na.rm = T)
    
    # -------------------------------------------------------------- #
    ## Absolute dwell time:
    
    # Overall:
    dwell_left_abs <- mean(x$dwell_left_abs,na.rm = T)
    dwell_right_abs <- mean(x$dwell_right_abs,na.rm = T)
    dwell_rew_abs <- mean(x$dwell_rew_abs,na.rm = T)
    dwell_pun_abs <- mean(x$dwell_pun_abs,na.rm = T)
    ## Dwell time given required action:
    dwell_left_abs_req_go <- mean(subset(x,reqAction_f == "Go")$dwell_left_abs,na.rm = T)
    dwell_left_abs_req_nogo <- mean(subset(x,reqAction_f == "NoGo")$dwell_left_abs,na.rm = T)
    dwell_right_abs_req_go <- mean(subset(x,reqAction_f == "Go")$dwell_right_abs,na.rm = T)
    dwell_right_abs_req_nogo <- mean(subset(x,reqAction_f == "NoGo")$dwell_right_abs,na.rm = T)
    dwell_rew_abs_req_go <- mean(subset(x,reqAction_f == "Go")$dwell_rew_abs,na.rm = T)
    dwell_rew_abs_req_nogo <- mean(subset(x,reqAction_f == "NoGo")$dwell_rew_abs,na.rm = T)
    dwell_pun_abs_req_go <- mean(subset(x,reqAction_f == "Go")$dwell_pun_abs,na.rm = T)
    dwell_pun_abs_req_nogo <- mean(subset(x,reqAction_f == "NoGo")$dwell_pun_abs,na.rm = T)
    ## Dwell time given chosen action:
    dwell_left_abs_act_go <- mean(subset(x,response_f == "Go")$dwell_left_abs,na.rm = T)
    dwell_left_abs_act_nogo <- mean(subset(x,response_f == "NoGo")$dwell_left_abs,na.rm = T)
    dwell_right_abs_act_go <- mean(subset(x,response_f == "Go")$dwell_right_abs,na.rm = T)
    dwell_right_abs_act_nogo <- mean(subset(x,response_f == "NoGo")$dwell_right_abs,na.rm = T)
    dwell_rew_abs_act_go <- mean(subset(x,response_f == "Go")$dwell_rew_abs,na.rm = T)
    dwell_rew_abs_act_nogo <- mean(subset(x,response_f == "NoGo")$dwell_rew_abs,na.rm = T)
    dwell_pun_abs_act_go <- mean(subset(x,response_f == "Go")$dwell_pun_abs,na.rm = T)
    dwell_pun_abs_act_nogo <- mean(subset(x,response_f == "NoGo")$dwell_pun_abs,na.rm = T)
    
    # -------------------------------------------------------------- #
    ## Relate dwell time:
    
    # Overall:
    dwell_left_rel <- mean(x$dwell_left_rel,na.rm = T)
    dwell_right_rel <- mean(x$dwell_right_rel,na.rm = T)
    dwell_rew_rel <- mean(x$dwell_rew_rel,na.rm = T)
    dwell_pun_rel <- mean(x$dwell_pun_rel,na.rm = T)
    ## Dwell time given required action:
    dwell_left_rel_req_go <- mean(subset(x,reqAction_f == "Go")$dwell_left_rel,na.rm = T)
    dwell_left_rel_req_nogo <- mean(subset(x,reqAction_f == "NoGo")$dwell_left_rel,na.rm = T)
    dwell_right_rel_req_go <- mean(subset(x,reqAction_f == "Go")$dwell_right_rel,na.rm = T)
    dwell_right_rel_req_nogo <- mean(subset(x,reqAction_f == "NoGo")$dwell_right_rel,na.rm = T)
    dwell_rew_rel_req_go <- mean(subset(x,reqAction_f == "Go")$dwell_rew_rel,na.rm = T)
    dwell_rew_rel_req_nogo <- mean(subset(x,reqAction_f == "NoGo")$dwell_rew_rel,na.rm = T)
    dwell_pun_rel_req_go <- mean(subset(x,reqAction_f == "Go")$dwell_pun_rel,na.rm = T)
    dwell_pun_rel_req_nogo <- mean(subset(x,reqAction_f == "NoGo")$dwell_pun_rel,na.rm = T)
    ## Dwell time given chosen action:
    dwell_left_rel_act_go <- mean(subset(x,response_f == "Go")$dwell_left_rel,na.rm = T)
    dwell_left_rel_act_nogo <- mean(subset(x,response_f == "NoGo")$dwell_left_rel,na.rm = T)
    dwell_right_rel_act_go <- mean(subset(x,response_f == "Go")$dwell_right_rel,na.rm = T)
    dwell_right_rel_act_nogo <- mean(subset(x,response_f == "NoGo")$dwell_right_rel,na.rm = T)
    dwell_rew_rel_act_go <- mean(subset(x,response_f == "Go")$dwell_rew_rel,na.rm = T)
    dwell_rew_rel_act_nogo <- mean(subset(x,response_f == "NoGo")$dwell_rew_rel,na.rm = T)
    dwell_pun_rel_act_go <- mean(subset(x,response_f == "Go")$dwell_pun_rel,na.rm = T)
    dwell_pun_rel_act_nogo <- mean(subset(x,response_f == "NoGo")$dwell_pun_rel,na.rm = T)
    
    return(data.frame(subject,response,ACC,RT,
                      fix_left,fix_right,fix_rew,fix_pun,
                      fix_left_req_go,fix_left_req_nogo,fix_right_req_go,fix_right_req_nogo,
                      fix_rew_req_go,fix_rew_req_nogo,fix_pun_req_go,fix_pun_req_nogo,
                      fix_left_act_go,fix_left_act_nogo,fix_right_act_go,fix_right_act_nogo,
                      fix_rew_act_go,fix_rew_act_nogo,fix_pun_act_go,fix_pun_act_nogo,
                      fix_left_RT, fix_right_RT, fix_rew_RT, fix_pun_RT,
                      dwell_left_abs,dwell_right_abs,dwell_rew_abs,dwell_pun_abs,
                      dwell_left_abs_req_go,dwell_left_abs_req_nogo,dwell_right_abs_req_go,dwell_right_abs_req_nogo,dwell_rew_abs_req_go,dwell_rew_abs_req_nogo,dwell_pun_abs_req_go,dwell_pun_abs_req_nogo,
                      dwell_left_abs_act_go,dwell_left_abs_act_nogo,dwell_right_abs_act_go,dwell_right_abs_act_nogo,dwell_rew_abs_act_go,dwell_rew_abs_act_nogo,dwell_pun_abs_act_go,dwell_pun_abs_act_nogo,
                      dwell_left_rel,dwell_right_rel,dwell_rew_rel,dwell_pun_rel,
                      dwell_left_rel_req_go,dwell_left_rel_req_nogo,dwell_right_rel_req_go,dwell_right_rel_req_nogo,dwell_rew_rel_req_go,dwell_rew_rel_req_nogo,dwell_pun_rel_req_go,dwell_pun_rel_req_nogo,
                      dwell_left_rel_act_go,dwell_left_rel_act_nogo,dwell_right_rel_act_go,dwell_right_rel_act_nogo,dwell_rew_rel_act_go,dwell_rew_rel_act_nogo,dwell_pun_rel_act_go,dwell_pun_rel_act_nogo))
    dev.off()})
  return(aggrData)
}

# ==================================================================================================== #
#### Process variables (demean/ standardize/ conditional probabilities) for plotting in wide format: ####

prepare_data_plot_wide <- function(data, variables, centerSub = F, scaleSub = F, condProb = F){
  #' Extract variables, reshape, prepare such that they can easily be fed into raincloud plot
  #' @param data data frame, aggregated per subject, with variables \code{variables}
  #' @param variables vector of variable names which to plot
  #' @param centerSub whether to subtract overall mean per subject
  #' @param centerSub whether to z-standaridize data row per subject
  #' @param condProb whether to divide adjacent rows (1&2, 3&4, ...) by their sum to create conditional probabilities
  #' @return preprocessed data frame containing $x, $y, $xj, $subject, $subject_f
  
  ## Select variables:
  plotdata_wide <- data[,c("subject",variables)]
  nVar <- length(variables)
  nRow <- nrow(plotdata_wide)
  nCol <- ncol(plotdata_wide)

  cat("Mean per variable:\n")
  print(colMeans(plotdata_wide[,2:nCol], na.rm = T))
  
  if (centerSub == T){
    cat("Demean per subject\n")
    for (iRow in 1:nRow){ # iRow <- 1
      plotdata_wide[iRow,2:nCol] <- plotdata_wide[iRow,2:nCol] - mean(as.numeric(plotdata_wide[iRow,2:nCol]),na.rm = T)
    } 
  }

  if (scaleSub == T){
    cat("Z-standardize per subject\n")
    for (iRow in 1:nRow){ # iRow <- 1
      plotdata_wide[iRow,2:nCol] <- (plotdata_wide[iRow,2:nCol] - mean(as.numeric(plotdata_wide[iRow,2:nCol]),na.rm = T)) / std(as.numeric(plotdata_wide[iRow,2:nCol]),na.rm = T)
    } 
  }
  
  ## Conditional probabilities per condition:
  if (condProb == T){
    cat("Compute conditional probabilities for pairs of adjacent input variables\n")
    
    nPair <- nVar/2 # number adjacent pairs:
    for (iPair in 1:nPair){
      countVec <- plotdata_wide[,1+2*iPair-1] + plotdata_wide[,1+2*iPair] # add adjacent rows together
      plotdata_wide[,1+2*iPair-1] <- plotdata_wide[,1+2*iPair-1] / countVec # divide by total count
      plotdata_wide[,1+2*iPair] <- plotdata_wide[,1+2*iPair] / countVec # divide by total count
    }
    
    cat("New mean per variable:\n")
    print(colMeans(plotdata_wide[,2:nCol], na.rm = T))
  }
  
  ## Reshape to long format:
  plotdata_long <- reshape(plotdata_wide, idvar = "subject", varying = variables, 
                           timevar = "condition", v.names = "fixation", direction = "long")

  d <- plotdata_long
  
  ## Make subject factor (e.g. when split per subject in plotting):
  d$subject_f <- factor(d$subject)
  
  ## Assign to simple labels x and y:
  d$y <- d$fixation
  d$x <- d$condition
  
  ## Add jitter to x (position):
  set.seed(321)
  d$j <- jitter(rep(0,nrow(d)), amount=.09)
  d$xj <- d$x + d$j
  
  # Return:
  return(d)
}

# ==================================================================================================== #
#### Process variables (demean/ standardize/ conditional probabilities) for plotting in long format: ####

prepare_data_plot_long <- function(data, x, y, subVar = "subject", jitterNum = 0.09){
  require(plyr)
  
  if (!(x %in% names(data))){stop("Variable x not contained in data set")}
  if (!(y %in% names(data))){stop("Variable y not contained in data set")}
  
  data$x <- data[,x]
  data$y <- data[,y]
  data$subject <- data[,subVar]
  
  d <- ddply(data, .(x, subject), function(iData){
    
    y <- mean(iData$y, na.rm = T)
    
    return(data.frame(y))
    dev.off()})

  ## Format x:
  d$condition <- d$x # save condition labels 
  d$x <- as.numeric(d$x) # turn numeric variable
  d$subject_f <- factor(d$subject)   
  
  ## Add jitter to x (position):
  set.seed(321)
  if (jitterNum==0){ # if no jitter
    d$j <- 0
  } else { # if jitter
    d$j <- jitter(rep(0,nrow(d)), amount = jitterNum) # default: 0.9
  }
  d$xj <- d$x + d$j
  
  cat(paste0("Min = ",round(min(d$y,na.rm = T),3),"; max = ",round(max(d$y,na.rm = T),3),"\n"))

  return(d)
}

# ===============================================================================================#
#### Determine y-axis limits: ####

determine_ylim <- function(data){
  #' Determine optimal y-axis limits based on some input heuristics
  #' @param data data frame, aggregated per subject, long format, with variable \code{y}
  #' @return yLim vector with to elements: minimal and maximal y-axis limit
  
  require(plyr)
  
  # Determine minimum and maximum:
  yMin <- min(data$y,na.rm = T)
  yMax <- max(data$y,na.rm = T)
  
  if(yMin >= 0 & yMax <= 1){ # if likely probability
    yLim <- c(0,1)
  } else if (yMax > 100) { # if vary big number: likely plot from 0 onwards
    yMin <- 0 # from zero onwards
    yMax <- round_any(yMax, 100, f = ceiling) # round up to next hundred
    yLim <- c(yMin,yMax)
    
  } else { # take the numerically bigger one, symmetric
    yMaxAbs <- ceiling(c(abs(yMin),yMax)) # round to next integer
    yLim <- c(-1*yMaxAbs,1*yMaxAbs)
  }
  
  return(yLim)
} 

# ===============================================================================================#
#### Plot single bar plot with individual data points: ####

custom_singlebar <- function(d, var, yLim = c(0,1), color = "grey80",
                           xLab = "x", yLab = "y", Main = "Plot"){
  
  require(ggplot2)
  require(ggbeeswarm)
  
  ## Aggregate again with Rmisc:
  # library(Rmisc)
  # summary_d <- summarySEwithin(d,measurevar="ACC", idvar = "subject", na.rm = T)
  
  lineWidth <- 1.3
  fontSize <- 30
  colAlpha <- .70
  lineWidth <- 1.5

  ## Add grouping variable (containing all subjects):
  nSub <- nrow(d)
  d$x <- rep(1,nSub)
  d$j <- jitter(rep(0,nrow(d)), amount=.06)
  d$xj <- d$x + d$j
  
  ## Repeat selected variable:
  d$y <- d[,var]

  p <- ggplot(data = d, aes(x = x, y = y)) # initialize
  
  ## Bars:
  p <- p + stat_summary(fun = mean, geom = "bar", fill = color,
                        color = 'black', width = 0.3, lwd = lineWidth)
  
  ## Confidence intervals: 
  p <- p + stat_summary(fun.data = mean_cl_normal, geom =
                          "errorbar", width = 0.05, lwd = lineWidth)
  
  ## Individual data points:
  p <- p + geom_beeswarm(color = "black", fill = color, alpha = colAlpha)
  # p <- p + geom_point(data = d, aes(x = xj), color = "black", fill = "grey40", 
  #                     shape = 21, size = 4,
  #                     alpha = colAlpha)
  
  ## Other settings:
  
  if (mean(yLim) == 0.5){ # if conditional probabilities:
    p <- p + geom_hline(yintercept=0.5, linetype=2, color="black")
  }
  
  ## X-axis:
  p <- p + scale_x_continuous(limits = c(0.5,1.5), breaks = c(0,1,2), labels = c("","",""))
  
  # Y-axis:
  p <- p + scale_y_continuous(limits = yLim, breaks = seq(yLim[1],yLim[-1],0.25)) 
  
  # Axis labels:
  p <- p + xlab(xLab) + ylab(yLab) +
    
    ## Add title:
    ggtitle(Main) +
    
    ## Add theme:
    theme_classic()
    
    ## Font sizes:
  p <- p + theme(axis.text = element_text(size=fontSize),
                 axis.title = element_text(size=fontSize), 
                 plot.title = element_text(size=fontSize, hjust = 0.5), # center title 
                 legend.text = element_text(size=fontSize))

  # Print plot in the end:
  print(p)
  return(p)
}
                           
# =============================================================================================== #
#### Plot flexible raincloud plot: #####

plot_raincloud <- function(data, xVar, yVar, subVar = "subject",
                           isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T, 
                           isMirror = F, useCond = NULL, jitterNum = 0.09,
                           yLim = NULL, color, Labels = NULL, xLab = "x", yLab = "y", Main = NULL, fontSize = NULL){
  #' Make raincloud plot
  #' @param data data frame, with variables \code{variables}
  #' @param xVar string, name of variable that goes on x-axis. Factor or numeric.
  #' @param yVar string, name of variable that goes on y-axis. Variable needs to be numeric.
  #' @param subVar string, name of variable containing subject identifier (default: subject).
  #' @param isBar Boolean, plot means per condition as bar (default: FALSE)
  #' @param isPoint Boolean, plot individual data points per condition as small points (default: TRUE)
  #' @param isViolin Boolean, plot distribution per condition as violin plot (default: TRUE)
  #' @param isBoxplot Boolean, plot median and interquartile per condition with boxplot (default: FALSE)
  #' @param isMean Boolean, plot means per condition as thick connected points with error bars (default: TRUE) 
  #' @param isMirror Determine position of boxplots and violin plots: always on the left (FALSE, default) or mirred to the middle (TRUE)
  #' @param useCond vector of numbers, use only subset of input conditions (default: all conditions)
  #' @param jitterNum numeric, amount of jitter to use for points (default: 0.9).
  #' @param isMirror Determine position of boxplots and violin plots: always on the left (FALSE, default) or mirred to the middle (TRUE)
  #' @param yLim vector of two numbers for y-axis limits (default: determine based on min and max of input data)
  #' @param color vector of strings (HEX colors), colors for input conditions (d$x)
  #' @param Labels vector of strings, labels for input conditions (d$x) on the x-axis
  #' @param xLab string, label for x-axis (default: "x")
  #' @param yLab string, label for y-axis (default: "y")
  #' @param Main string, title of plot (default: "NULL")
  #' @param fontSize integer, font size for axes ticks and labels and title.
  #' @return makes raincloud plot
  #'  See: https://github.com/jorvlan/open-visualizations/blob/master/R/repmes_tutorial_R.Rmd
  #'  See: https://github.com/jorvlan/open-visualizations/blob/master/R/repmes_tutorial_R.pdf
  
  require(ggplot2)
  require(ggstatsplot) # for geom_flat_violin
  require(gghalves) # for half plots
  
  ## General settings:
  colAlpha <- .50
  lineWidth <- 1.5
  
  # ---------------------------------------------------------------------------- #
  ### Check for NAs in xVar:
  
  if (sum(is.na(data[,xVar]))>0){
    cat("xVar contains NAs, remove\n")
    data <- data[!(is.na(data[,xVar])),]
  }
  
  # ---------------------------------------------------------------------------- #
  ### Aggregate data per condition per subject into long format:
  
  d <- prepare_data_plot_long(data, x = xVar, y = yVar, subVar = subVar, jitterNum = jitterNum)
  
  # ---------------------------------------------------------------------------- #
  ### Determine defaults:
  
  cat("Set defaults\n")
  
  ## Determine y limits if not given:
  if(is.null(yLim)){
    cat("Automatically determine y-axis\n")
    yLim <- determine_ylim(d)
  }
  
  ## Determine number of conditions to use:
  if(is.null(useCond)){ # if not defined: use all conditions
    nCond <- length(unique(d$x))
    useCond <- 1:nCond #  sort(unique(d$x))
    d$condition <- as.numeric(as.factor(d$x)) # from 1 to nCond
  } else {
    cat(paste0("Use only conditions ",toString(useCond)),"\n")
    nCond <- length(useCond)
    # Loop through selected conditions, recompute x so it is consecutive:
    ix <- 0 # initialize
    for (iCond in useCond){
      ix <- ix + 1 # increment
      d[which(d$condition==iCond),"x"] <- ix
    } 
    d$xj <- d$x + d$j # recompute xj
  }
  
  if (is.null(Labels)){
    cat("Automatically extract x-axis labels\n")
    Labels <- sort(unique(d$x))
  }
  
  if(length(color)!=nCond){
    cat("Less colors provided than conditions, repeat first color for all conditions\n")
    color <- rep(color[1],nCond)
    }
  if(length(Labels)!=nCond){
    cat("Less labels provided than conditions, use just numbers from 1 to number of conditions\n")
    Labels <- 1:nCond
    }
  
  if (is.null(fontSize)){
    ## Font sizes for ordinary viewing: 15
    # fontSize <- 15
    ## Font sizes for saving: 30
    fontSize <- 30
    cat(paste0("No font size provided, use font size ",fontSize),"\n")
  }
  
  # --------------------------------------------------------------------- #
  ## Compute summary per condition:
  
  require(Rmisc)
  dsel = subset(d, condition %in% useCond) # select used conditions:
  summary_d <- summarySEwithin(dsel, measurevar="y", withinvar = "x", idvar = "subject", na.rm = T)
  # automatically rescales x to 1:nCond
  # summary_d$x <- as.numeric(summary_d$x)
  if (is.numeric(d$x)){summary_d$x <- sort(unique(d$x))} # back to original numerical values
  summary_d$condition <- as.numeric(as.factor(summary_d$x)) # consecutive count (for labels)

  # --------------------------------------------------------------------- #
  ## Determine position of boxplots and violin plots: always on the left (FALSE, default) or mirred to the middle (TRUE)

  if(isMirror == T){ # if mirror images for box plots and violin plots
    sideVec <- c(rep("l",floor(nCond/2)),rep("r",ceil(nCond/2)))
  } else { # otherwise default
    sideVec <- rep("l",nCond)
  }
  
  # --------------------------------------------------------------------- #
  ## Start ggplot: 
  p <- ggplot(data = d, aes(y = y)) # initialize

  # --------------------------------------------------------------------- #
  ## Bars:
  if (isBar == T){
    cat("Make bar plot\n")
    
    for(iCond in useCond){
      p <- p + geom_bar(data = subset(summary_d, condition == iCond), aes(x = x, y = y), stat = "identity",
                        fill = color[iCond], col = "black", width = 0.4, lwd = lineWidth, alpha = 0.3)
      p <- p + geom_errorbar(data = subset(summary_d, condition==iCond), 
                             aes(x = x, y = y, ymin = y-ci, ymax = y+ci),
                             color = "black", width = 0.10, lwd = lineWidth, alpha = 0.6) 
    }
  }
  
  # --------------------------------------------------------------------- #
  ## Individual data points:
  
  if(isPoint == T){
    cat("Make individual data points\n")

    for(iCond in useCond){
      p <- p + geom_point(data = subset(d, condition == iCond), aes(x = xj), color = color[iCond], size = 1.5, 
                          alpha = .35) # alpha = .50
    }
    # Add lines to combine points:
    p <- p + geom_line(data = subset(d, condition %in% useCond), aes(x = xj, group = subject), 
                      size = 1.0, color = 'grey40', alpha = 0.35) # lightgray
  }
  # Till 2021-11-22: color = grey70, size = 1
  
  # --------------------------------------------------------------------- #
  ## Box plots:
  
  if (isBoxplot == T){
    
    cat("Make box plot\n")

    for(iCond in useCond){
      p <- p + geom_half_boxplot(
        data = subset(d, condition == iCond), aes(x=x, y = y), position = position_nudge(x = .15), 
        side = sideVec[iCond], outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1, 
        fill = color[iCond], alpha = colAlpha)
    }
  }
  
  # Violin plots:
  if (isViolin == T){
    
    cat("Make violin plot\n")
    
    for(iCond in useCond){
      p <- p + geom_half_violin(
        data = subset(d, condition == iCond),aes(x = x, y = y), position = position_nudge(x = -.12),
        side = sideVec[iCond], fill = color[iCond], alpha = colAlpha, trim = FALSE)
    }
  }
  
  # --------------------------------------------------------------------- #
  ## Mean and CI per condition:
  
  posNudge = 0 # .13
  if (isMean == T){
    
    cat("Make point for mean\n")
    
    ## Thick line connecting means (plot line first and points on top):
    p <- p + geom_line(data = summary_d, aes(x = x, y = y), color = color[iCond],
                       position = position_nudge(x = 1*posNudge), size = 1.5)
    
    ## Error shades:
    p <- p + geom_ribbon(data = summary_d, aes(x = x, y = y, ymin = y-ci, ymax = y+ci),
                         color = color[iCond], alpha=0.15, linetype = 0)

    ## Points for means and error bars:
    for(iCond in useCond){
      p <- p + geom_point(data = subset(summary_d, condition==iCond), aes(x = x, y = y), # point
               position = position_nudge(x = -1*posNudge), color = color[useCond[iCond]], alpha = .6, size = 3) # size = 2
      
      p <- p + geom_errorbar(data = subset(summary_d, condition==iCond), aes(x = x, y = y, # error bar
                                                 ymin = y-ci, ymax = y+ci),
                position = position_nudge(-1*posNudge), color = color[useCond[iCond]], width = 0.05, size = 1.5, alpha = .6)
    }
  }
  
  if (mean(yLim) == 0.5){ # if conditional probabilities:
    p <- p + geom_hline(yintercept=0.5, linetype=2, color="black", size = 1) # dotted line in the middle
  }
  if (yLim[1] == 0 & yLim[2] == 1){
    # p <- p + scale_y_break(c(0, 0.5, 1))
    p <- p + scale_y_continuous(breaks = seq(0, 1, by = 0.5)) # only 0, 0.5, 1 as axis labels
  }

  # --------------------------------------------------------------------- #
  cat("Adjust axes, labels\n")
  
  ## Y-axis:
  # p <- p + scale_y_continuous(limits = yLim) 
  p <- p + coord_cartesian(ylim=yLim) 
  
  ## Labels:
  p <- p + scale_x_continuous(breaks=sort(unique(d$x)), labels=Labels[useCond]) +
  xlab(xLab) + ylab(yLab)
  
  ## Other settings:
  if (!(is.null(Main))){
    cat("Add title\n")
    p <- p + ggtitle(Main) # title off for printing for poster
  }  
    
  ## Theme:
  p + theme_classic()
  
  ## Font sizes:
  p <- p + theme(axis.text = element_text(size=fontSize),
                 axis.title = element_text(size=fontSize), 
                 plot.title = element_text(size=fontSize, hjust = 0.5), # center title 
                 legend.text = element_text(size=fontSize))
  
  # Print plot in the end:
  print(p)
  cat("Finished :-)\n")
  return(p)
}

# =============================================================================================== #
#### Plot beeswarm plot: #####

plot_beeswarm <- function(data, xVar, yVar, subVar = "subject",
                          nRound = NULL, yLim = NULL, color, Labels, 
                          xLab = "x", yLab = "y", Main = "Plot", fontSize = NULL){
  #' Make raincloud plot
  #' @param data data frame, with variables \code{variables}
  #' @param xVar string, name of variable that goes on x-axis. Factor or numeric.
  #' @param yVar string, name of variable that goes on y-axis. Variable needs to be numeric.
  #' @param subVar string, name of variable containing subject identifier (default: subject).
  #' @param nRound numeric, digits used for rounding dependent variable y.
  #' @param yLim vector of two numbers for y-axis limits (default: determine based on min and max of input data)
  #' @param color vector of strings (HEX colors), colors for input conditions (d$x)
  #' @param Labels vector of strings, labels for input conditions (d$x) on the x-axis
  #' @param xLab string, label for x-axis (default: "x")
  #' @param yLab string, label for y-axis (default: "y")
  #' @param Main string, title of plot (default: "Plot")
  #' @param fontSize integer, font size for axes ticks and labels and title.
  #' @return makes beeswarm and box plot for each condition.

  require(Rmisc)
  require(ggplot2)
  require(ggbeeswarm)

  ## General settings:
  colAlpha <- .20
  lineWidth <- 1.5
  
  ## Font sizes for ordinary viewing and saving:
  if (is.null(fontSize)){
    ## Font sizes for ordinary viewing: 15
    # fontSize <- 15
    ## Font sizes for saving: 30
    fontSize <- 30
    cat(paste0("No font size provided, use font size ",fontSize),"\n")
  }
  
  # ---------------------------------------------------------------------------- #
  ### Aggregate data per condition per subject into long format:
  
  d <- prepare_data_plot_long(data, x = xVar, y = yVar, subVar = subVar)
  
  # ---------------------------------------------------------------------------- #
  ### Determine defaults:
  
  cat("Set defaults\n")
  
  # Determine y limits if not given:
  if(is.null(yLim)){
    yLim <- determine_ylim(d)
  }
  
  nCond <- length(unique(d$condition))
  condNames <- sort(unique(d$condition))
  
  # --------------------------------------------------------------------- #
  ## Compute summary per condition:

  summary_d <- summarySEwithin(d, measurevar = "y", withinvar = "x", idvar = "subject", na.rm = T)
  # automatically rescales x to 1:nCond
  # summary_d$x <- as.numeric(summary_d$x)
  if (is.numeric(d$x)){summary_d$x <- sort(unique(d$x))} # back to original numerical values
  summary_d$condition <- as.numeric(as.factor(summary_d$x)) # consecutive count (for labels)
  
  if (!is.null(nRound)){
    cat(paste0("Round DV on ",nRound," digits"))
    d$y <- round(d$y, nRound)
    # summary_d$y <- round(summary_d$y, nRound)
  }
  
  # --------------------------------------------------------------------- #
  ## Start ggplot: 
  
  p <- ggplot(data = d, aes(x = x, y = y)) # initialize
  
  # --------------------------------------------------------------------- #
  ## Make beeswarm:
  
  cat("Make beeswarm plot\n")

  p <- p + geom_beeswarm()

  # --------------------------------------------------------------------- #
  ## Box plots:

  cat("Make box plot\n")
  
  for(iCond in 1:nCond){
    p <- p + geom_boxplot(
      data = subset(d, condition == condNames[iCond]), aes(x = x, y = y), 
      width = 0.3,
      color = color[iCond], fill = color[iCond], alpha = colAlpha)
      
      # position = position_nudge(x = .15), 
      # side = sideVec[iCond], outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1, 
      # fill = color[iCond], alpha = colAlpha)
  }

  # --------------------------------------------------------------------- #
  cat("Adjust axes, labels\n")
  
  # Y-axis:
  # p <- p + scale_y_continuous(limits = yLim) 
  p <- p + coord_cartesian(ylim=yLim) 
  
  # Labels:
  p <- p + scale_x_continuous(breaks=sort(unique(d$x)), labels=Labels) +
    xlab(xLab) + ylab(yLab) +
    
    # Other settings:
    ggtitle(Main) + # title off for printing for poster
    theme_classic()
    
  ## Font sizes:
  p <- p + theme(axis.text=element_text(size=fontSize),
                 axis.title=element_text(size=fontSize), 
                 title = element_text(size=fontSize), 
                 legend.text = element_text(size=fontSize))
  
  # Print plot in the end:
  print(p)
  cat("Finished :-)\n")
}

# =============================================================================================== #
#### REGRESSION LINES 1 IV: Plot regression line per group and per subject based on model output: #####

custom_regressionline1 <- function(mod, selEff, xLim = NULL, yLim = NULL, subVar = "subject", useEffect = TRUE, xVec = NULL,
                                 color = "red", margin = NULL, xLab = "x", yLab = "y", Main = NULL, fontSize = NULL){
  #' Plot group-level regression line and subject-level regression lines based on 1 continuous predictor.
  #' @param mod model fitted with lme4
  #' @param selEff string, name of predictor to plot
  #' @param xLim vector of two numbers for y-axis limits
  #' @param yLim vector of two numbers for y-axis limits (default: determine based on min and max of input data)
  #' @param subVar string, name of grouping variable (default: subject)
  #' @param useEffect boolean, extract upper and lower bound of confidence interval from effects(mod) (TRUE) or compute yourself based on vcov(mod) (FALSE)
  #' @param xLim vector of two numbers for x-axis ticks, to be used only if useEffect = FALSE
  #' @param color strings (HEX colors), colors for line and error shade (default: red)
  #' @param margin vector of 4 numbers, margin of plot (default: NULL)
  #' @param xLab string, label for x-axis (default: "x")
  #' @param yLab string, label for y-axis (default: "y")
  #' @param Main string, title of plot (default: "Plot")
  #' @param fontSize integer, font size for axes ticks and labels and title.
  #' @return makes regression line plot
  #'  See: https://github.com/jorvlan/open-visualizations/blob/master/R/repmes_tutorial_R.Rmd
  #'  See: https://github.com/jorvlan/open-visualizations/blob/master/R/repmes_tutorial_R.pdf
  
  require(ggplot2)
  require(lme4)
  
  # General settings:
  colAlpha <- .95
  lineWidth <- 1.5
  
  if (is.null(fontSize)){
    ## Font sizes for ordinary viewing: 15
    # fontSize <- 15
    ## Font sizes for saving: 30
    fontSize <- 30
    cat(paste0("No font size provided, use font size ",fontSize),"\n")
  }
  
  ## Check if selEff is in model:
  if (!(selEff %in% names(coef(mod)$subject))){stop("selEff not a predictor in mod")}
  
  ## X-axis for which to generate plots:
  if (useEffect){ # if x samples automatically generated by effects() to be used
    tmp <- effect(selEff,mod) # retrieve objects from effects
    xVecEval <- as.numeric(tmp$variables[[1]]$levels)

  } else { # if use by hand
    if (is.null(xVec)){
      cat("No x-axis samples provided, extract from effects(mod)")
      xVecEval <- as.numeric(tmp$variables[[1]]$levels)
    } else {
      xVecEval <- seq(xVec[1],xVec[2],(xVec[2]-xVec[1])/100) # 100 samples between min and max
    }
  }
  xLen <- length(xVecEval) # number of x-axis samples
  
  if (is.null(xLim)){ # if no x-axis limits provided
    xLim <- c(xVecEval[1],xVecEval[xLen])
  }

  # --------------------------------------------------------------------- #
  ## Start ggplot: 
  d <- data.frame(x = xVecEval, y = xVecEval) #  just to initialize ggplot
  p <- ggplot(data = d) # initialize
  
  # --------------------------------------------------------------------- #
  ## Loop over subjects, create single subject lines + ribbons:
  cat("Draw random-effects lines\n")

  ## Extract coefficients per subject:
  subCoefs <- coef(mod)
  iCol <- which(names(subCoefs$subject)==selEff) # localize where in subCoefs effect of interest is
  nSub <- nrow(subCoefs$subject)

  for (iSub in 1:nSub){ # iSub <- 1
    iInter <- subCoefs$subject[iSub,1] # extract intercept
    iSlope <- subCoefs$subject[iSub,iCol] # extract slope
    yVecEval <- iInter + xVecEval * iSlope
    # if (summary(mod)$objClass=="glmerMod"){yVecEval <- inv.logit(yVecEval)} # bring to response scale
    if (isGLMM(mod)){yVecEval <-mod@resp$family$linkinv(yVecEval)} # bring to response scale
    # Create single data points (2 should be enough, i.e. xmin and xmax)
    d <- data.frame(x = xVecEval, y = yVecEval)
    
    ## Thick line connecting means (plot line first and points on top):
    p <- p + geom_path(data = d, aes(x = x, y= y), color = 'grey40', # color = 'grey70'
                       alpha=0.35, size = 1.0)
  }

  # --------------------------------------------------------------------- #
  ## Overall group-level line:
  cat("Draw fixed-effects line\n")
  
  groupCoefs <- fixef(mod)
  # groupSE <- VarCorr(mod)
  # iCol <- which(colnames(groupCoefs)==selEff) # localize where in groupCoefs effect of interest is
  iInter <- as.numeric(groupCoefs[1]) # extract intercept
  iSlope <- as.numeric(groupCoefs[iCol]) # extract slope
  yVecEval <- iInter + xVecEval * iSlope
  # if (summary(mod)$objClass=="glmerMod"){yVecEval <- inv.logit(yVecEval)}
  if (isGLMM(mod)){yVecEval <-mod@resp$family$linkinv(yVecEval)} # bring to response scale
  # Create single data points (2 should be enough, i.e. xmin and xmax)
  d <- data.frame(x = xVecEval, y = yVecEval)
  
  ## Thick line connecting means (plot line first and points on top):
  p <- p + geom_path(data = d, aes(x = x, y= y), color = color, size = 1.5)
  # position = position_nudge(x = 1*posNudge), 
  
  # ----------------------------------------------------- #
  ## Error shades:
  # https://github.com/cran/effects/blob/master/R/Effect.R
  if (useEffect){
    # ------------------------------- #
    ## Option A: Extract from effect() object:
    tmp <- effect(selEff,mod)
    
    ## Mean/ line itself:
    yVecEval <- as.numeric(tmp$fit) # y-axis coordinates (untransformed)
    if (isGLMM(mod)){yVecEval <-mod@resp$family$linkinv(yVecEval)} # transform to response scale
    
    ## Lower and upper limit of CI interval:
    ymin <- t(tmp$lower) # lower bound of CI interval
    ymax <- t(tmp$upper) # upper bound of CI interval
    if (isGLMM(mod)){ymin <-mod@resp$family$linkinv(ymin)} # transform to response scale
    if (isGLMM(mod)){ymax <-mod@resp$family$linkinv(ymax)} # transform to response scale

  } else {
    # ------------------------------- #
    
    ## Option B: Compute SE yourself:
    mmat <- matrix(0,xLen,ncol(coef(mod)$subject)) # initialize design matrix: by default all regressors 0
    mmat[,1] <- rep(1,xLen) # add intercept
    mmat[,iCol] <- xVecEval # add regressor of interest
    V <- vcov(mod, complete=FALSE) # covariance matrix from model
    vcov <- mmat %*% V %*% t(mmat) # multiply with design matrix
    var <- diag(vcov) # variance
    se <- sqrt(var) # se # see tmp$se
    conf <- 1.96
    yVecEval <- iInter + xVecEval * iSlope # recompute before transform
    ymin <- yVecEval - se*conf # lower bound of CI interval
    ymax <- yVecEval + se*conf # upper bound of CI interval
    if (isGLMM(mod)){ymin <-mod@resp$family$linkinv(ymin)} # bring to response scale
    if (isGLMM(mod)){ymax <-mod@resp$family$linkinv(ymax)} # bring to response scale
    
  }
  # ------------------------------- #
  ## Plot error bars/ shades:
  d <- data.frame(x = xVecEval, y = yVecEval, ymin = ymin, ymax = ymax)
  p <- p + geom_ribbon(data = d, aes(x = x, y = y, ymin = ymin, ymax = ymax),
                       fill = color, alpha=0.20, linetype = 0)

  # if (mean(yLim) == 0.5){ # if conditional probabilities:
  #   p <- p + geom_hline(yintercept=0.5, linetype=2, color="black", size = 1)
  # }
  
  # --------------------------------------------------------------------- #
  cat("Adjust axes, labels\n")
  
  ## Y-axis:
  p <- p + coord_cartesian(xlim=xLim,ylim=yLim) 
  
  ## X-axis:
  xTickVec <- round(seq(xLim[1],xLim[2],(xLim[2]-xLim[1])/2),2)
  p <- p + scale_x_continuous(breaks=xTickVec, labels=xTickVec)
  
  ## Labels:
  p <- p + xlab(xLab) + ylab(yLab)
    
  ## Other settings:
  if (!is.null(Main)){
    cat("Add title\n")
    p <- p + ggtitle(Main)
  }
  
  p <- p + theme_classic()
  if (!is.null(margin)){
    cat("Adjust margin\n")
    p <- p + theme(plot.margin = unit(margin, "cm"))
  }

  ## Font sizes:
  p <- p + theme(axis.text=element_text(size=fontSize),
                 axis.title=element_text(size=fontSize), 
                 plot.title = element_text(size=fontSize, hjust = 0.5), 
                 legend.text = element_text(size=fontSize))
  
  ## Print plot in the end:
  print(p)
  cat("Finished :-)\n")
  return(p)
}  

# =============================================================================================== #
#### REGRESSION LINES 2 IV: Plot regression line per condition per group and per subject based on model output: #####

custom_regressionline2 <- function(mod, xVar, zVar, subVar = "subject", 
                                   xLim = NULL, yLim = NULL, xVec = NULL,
                                   colVec = c("blue","red"), margin = NULL, 
                                   xLab = "x", yLab = "y", Main = NULL, fontSize = NULL){
  #' Plot group-level regression line and subject-level regression lines based on 1 continuous predictor and 1 binary predictor.
  #' @param mod model fitted with lme4.
  #' @param xVar string, name of continuous predictor to plot on x-axis.
  #' @param zVar string, name of binary predictor to plot with different colors.
  #' @param subVar string, name of grouping variable (default: subject).
  #' @param xLim vector of two numbers for y-axis limits.
  #' @param yLim vector of two numbers for y-axis limits (default: determine based on min and max of input data).
  #' @param xVec vector of two numbers for x-axis ticks.
  #' @param colVec vector of strings (HEX colors), colors for line and error shade (default: blue and red).
  #' @param margin vector of 4 numbers, margin of plot (default: NULL).
  #' @param xLab string, label for x-axis (default: "x").
  #' @param yLab string, label for y-axis (default: "y").
  #' @param Main string, title of plot (default: "Plot").
  #' @param fontSize integer, font size for axes ticks and labels and title.
  #' @return makes regression line plot.
  #'  See: https://github.com/jorvlan/open-visualizations/blob/master/R/repmes_tutorial_R.Rmd
  #'  See: https://github.com/jorvlan/open-visualizations/blob/master/R/repmes_tutorial_R.pdf
  
  require(ggplot2)
  require(lme4)
  
  ## General settings:
  alphaSub <- 0.10
  alphaShade <- 0.20
  sizeGroup <- 1.5
  sizeSub <- 1
  lineWidth <- 1.5
  
  if (is.null(fontSize)){
    ## Font sizes for ordinary viewing: 15
    # fontSize <- 15
    ## Font sizes for saving: 30
    fontSize <- 30
    cat(paste0("No font size provided, use font size ",fontSize),"\n")
  }
  
  ## Binary (factor) predictor:
  zVar1 <- paste0(zVar,"1") 
    
  ## Name of interaction:
  intVar <- paste0(xVar,":",zVar)
  intVar1 <- paste0(intVar,"1")
  
  ## Check if iVar is in model:
  if (!(intVar1 %in% names(coef(mod)$subject))){stop(paste0("Interaction effect",iVar,"not a predictor in mod"))}
  
  ## X-axis for which to generate plots:
  tmp <- effect(intVar,mod) # retrieve objects from effects
  xVecEval <- as.numeric(tmp$variables[[1]]$levels)
    
  if (!(is.null(xVec))){ # if xVec provided as input
    xVecEval <- seq(xVec[1],xVec[2],(xVec[2]-xVec[1])/100) # 100 samples between min and max
  }

  xLen <- length(xVecEval) # number of x-axis samples
  
  if (is.null(xLim)){ # if no x-axis limits provided
    xLim <- c(xVecEval[1],xVecEval[xLen])
  }
  
  # --------------------------------------------------------------------- #
  ### Extract all coefficients:
  
  ## Extract coefficients:
  groupCoefs <- fixef(mod)
  subCoefs <- coef(mod)
  
  ## Indices of effects:
  xCol <- which(names(subCoefs$subject)==xVar) # localize where in subCoefs effect of interest is
  zCol <- which(names(subCoefs$subject)==zVar1) # localize where in subCoefs effect of interest is
  intCol <- which(names(subCoefs$subject)==intVar1) # localize where in subCoefs effect of interest is
  
  ## Number of subjects:
  nSub <- nrow(subCoefs$subject)
  
  # --------------------------------------------------------------------- #
  ### Initialize empty ggplot: 
  
  d <- data.frame(x = xVecEval, y = xVecEval) #  just to initialize ggplot
  p <- ggplot(data = d) # initialize
  
  # --------------------------------------------------------------------- #
  ### Loop over subjects, create single subject lines + ribbons:
  cat("Draw random-effects lines\n")
  
  for (iSub in 1:nSub){ # iSub <- 1
    
    # Note zero-sum coding!!!
    
    ## Intercepts:
    iInter1 <- subCoefs$subject[iSub,1] + subCoefs$subject[iSub,zCol] # extract intercept condition 1
    iInter2 <- subCoefs$subject[iSub,1] - subCoefs$subject[iSub,zCol] # extract intercept condition 2
    ## Slopes:
    iSlope1 <- subCoefs$subject[iSub,xCol] + subCoefs$subject[iSub,intCol] # extract slope condition 1
    iSlope2 <- subCoefs$subject[iSub,xCol] - subCoefs$subject[iSub,intCol] # extract slope condition 2
    
    ## Simulated y-data per subject:
    yVecEval1 <- iInter1 + xVecEval * iSlope1
    yVecEval2 <- iInter2 + xVecEval * iSlope2
    
    if (isGLMM(mod)){yVecEval1 <- mod@resp$family$linkinv(yVecEval1)} # bring to response scale
    if (isGLMM(mod)){yVecEval2 <- mod@resp$family$linkinv(yVecEval2)} # bring to response scale
    
    ## Create single data points (2 should be enough, i.e. xmin and xmax)
    d <- data.frame(x = xVecEval, y1 = yVecEval1, y2 = yVecEval2)
    
    ## Thick line connecting means (plot line first and points on top):
    p <- p + 
      geom_path(data = d, aes(x = x, y = y1), color = colVec[1],
                alpha = alphaSub, size = sizeSub) + 
      geom_path(data = d, aes(x = x, y = y2), color = colVec[2],
                alpha = alphaSub, size = sizeSub)
    
  }
  
  # --------------------------------------------------------------------- #
  ## Overall group-level line:
  
  cat("Draw fixed-effects line\n")

  ## Extract from effect() object:
  tmp <- effect(intVar,mod)
  idx1 <- 1:xLen
  idx2 <- (xLen+1):(2*xLen)
    
  ## Line itself (mean): 
  yVecEval1 <- as.numeric(tmp$fit)[idx1] # y-axis coordinates (untransformed)
  yVecEval2 <- as.numeric(tmp$fit)[idx2] # y-axis coordinates (untransformed)
  if (isGLMM(mod)){yVecEval1 <- mod@resp$family$linkinv(yVecEval1)} # transform to response scale
  if (isGLMM(mod)){yVecEval2 <- mod@resp$family$linkinv(yVecEval2)} # transform to response scale
  
  ## Lower and upper limit of CI interval:
  ymin1 <- t(tmp$lower)[idx1] # lower bound of CI interval condition 1
  ymin2 <- t(tmp$lower)[idx2] # lower bound of CI interval condition 2
  ymax1 <- t(tmp$upper)[idx1] # upper bound of CI interval condition 1
  ymax2 <- t(tmp$upper)[idx2] # upper bound of CI interval condition 2
  if (isGLMM(mod)){ymin1 <- mod@resp$family$linkinv(ymin1)} # transform to response scale
  if (isGLMM(mod)){ymin2 <- mod@resp$family$linkinv(ymin2)} # transform to response scale
  if (isGLMM(mod)){ymax1 <- mod@resp$family$linkinv(ymax1)} # transform to response scale
  if (isGLMM(mod)){ymax2 <- mod@resp$family$linkinv(ymax2)} # transform to response scale

  # --------------------------------------------------------------------- #
  ### Thick line connecting group-level means:
  
  d <- data.frame(x = xVecEval, y1 = yVecEval1, y2 = yVecEval2)
  p <- p + 
    geom_path(data = d, aes(x = x, y = y1), color = colVec[1], size = sizeGroup) + 
    geom_path(data = d, aes(x = x, y = y2), color = colVec[2], size = sizeGroup)
  
  # --------------------------------------------------------------------- #
  ### Error shades:
  # https://github.com/cran/effects/blob/master/R/Effect.R
  ## Plot error bars/ shades:
  d <- data.frame(x = xVecEval, y = yVecEval1, ymin = ymin1, ymax = ymax1)
  p <- p + geom_ribbon(data = d, aes(x = x, y = y, ymin = ymin, ymax = ymax),
                       fill = colVec[1], alpha = alphaShade, linetype = 0)
  d <- data.frame(x = xVecEval, y = yVecEval2, ymin = ymin2, ymax = ymax2)
  p <- p + geom_ribbon(data = d, aes(x = x, y = y, ymin = ymin, ymax = ymax),
                       fill = colVec[2], alpha = alphaShade, linetype = 0)
  
  # --------------------------------------------------------------------- #
  cat("Adjust axes, labels\n")
  
  ## Y-axis:
  p <- p + coord_cartesian(xlim=xLim,ylim=yLim) 
  
  ## X-axis:
  xTickVec <- round(seq(xLim[1],xLim[2],(xLim[2]-xLim[1])/2),2)
  p <- p + scale_x_continuous(breaks=xTickVec, labels=xTickVec)
  
  # Z-axis labels:
  # cat("Add legend\n")
  # p <- p + scale_color_discrete(labels=c('Go', 'NoGo')) + labs()
  # p <- p + scale_color_manual(name="Required action", values = colVec) + #  tmp$variables[[zCol-1]]$levels) + 
  #   labs(color = "Required action")
  
  ## Labels:
  p <- p + xlab(xLab) + ylab(yLab)
  
  ## Other settings:
  if (!is.null(Main)){
    cat("Add title\n")
    p <- p + ggtitle(Main)
  }
  
  p <- p + theme_classic()
  if (!is.null(margin)){
    cat("Adjust margin\n")
    p <- p + theme(plot.margin = unit(margin, "cm"))
  }
  
  ## Font sizes:
  p <- p + theme(axis.text = element_text(size=fontSize),
                 axis.title = element_text(size=fontSize), 
                 plot.title = element_text(size=fontSize, hjust = 0.5), 
                 legend.text = element_text(size=fontSize))
  
  # Print plot in the end:
  print(p)
  cat("Finished :-)\n")
  return(p)
}  

# =============================================================================================== #
#### REGRESSION BARS 1 IV: Plot regression bars per group and per subject based on model output: #####

custom_regressionbar1 <- function(mod, selEff, yLim = NULL, subVar = "subject", Labels = c("1","-1"),
                                 color = "red", margin = NULL, z = 1.96, xLab = "x", yLab = "y", Main = NULL, fontSize = NULL){
  #' Plot group-level regression bar and subject-level regression lines based on 1 binary predictor.
  #' @param mod model fitted with lme4
  #' @param selEff string, name of predictor to plot
  #' @param yLim vector of two numbers for y-axis limits (default: determine based on min and max of input data)
  #' @param subVar string, name of grouping variable (default: subject)
  #' @param color strings (HEX colors), colors for line and error shade (default: red)
  #' @param margin vector of 4 numbers, margin of plot (default: NULL)
  #' @param xLab string, label for x-axis (default: "x")
  #' @param yLab string, label for y-axis (default: "y")
  #' @param Main string, title of plot (default: NULL)
  #' @param fontSize integer, font size for axes ticks and labels and title.
  #' @return makes regression line plot
  #'  See: https://github.com/jorvlan/open-visualizations/blob/master/R/repmes_tutorial_R.Rmd
  #'  See: https://github.com/jorvlan/open-visualizations/blob/master/R/repmes_tutorial_R.pdf
  
  require(ggplot2)
  require(lme4)
  
  ## General settings:
  colAlpha <- .95
  lineWidth <- 1.5
  
  if (is.null(fontSize)){
    ## Font sizes for ordinary viewing: 15
    # fontSize <- 15
    ## Font sizes for saving: 30
    fontSize <- 30
    cat(paste0("No font size provided, use font size ",fontSize),"\n")
  }
  
  ## Overwrite grouping variable:
  # coef(mod)$subject <- 
  #   names(coef(mod)[1])
  
  ## Check if selEff is in model:
  selEff1 <- paste0(selEff,"1")
  if (!(selEff1 %in% names(coef(mod)$subject))){stop("selEff not a predictor in mod")}
  
  ## x-coordinates for which to plot
  xVecEval <- c(-1, 1)
  xLen <- length(xVecEval)
  
  # --------------------------------------------------------------------- #
  ## Start ggplot: 
  d <- data.frame(x = xVecEval, y = xVecEval) #  just to initialize ggplot
  p <- ggplot(data = d) # initialize
  
  # --------------------------------------------------------------------- #
  ## Loop over subjects, create single subject lines + ribbons:
  cat("Draw random-effects lines\n")
  
  ## Extract coefficients per subject:
  subCoefs <- coef(mod)
  iCol <- which(names(subCoefs$subject)==selEff1) # localize where in subCoefs effect of interest is
  nSub <- nrow(subCoefs$subject)
  
  for (iSub in 1:nSub){ # iSub <- 1
    iInter <- subCoefs$subject[iSub,1] # extract intercept
    iSlope <- subCoefs$subject[iSub,iCol] # extract slope
    yVecEval <- iInter + xVecEval * iSlope * -1 # swap both x-axis positions
    # if (summary(mod)$objClass=="glmerMod"){yVecEval <- inv.logit(yVecEval)} # bring to response scale
    if (isGLMM(mod)){yVecEval <-mod@resp$family$linkinv(yVecEval)} # bring to response scale
    # Create single data points (2 should be enough, i.e. xmin and xmax)
    d <- data.frame(x = xVecEval, y = yVecEval)
    
    ## Thick line connecting means (plot line first and points on top):
    p <- p + geom_path(data = d, aes(x = x, y= y), color = 'grey40', # color = 'grey70'
                       alpha=0.35, size = 1)
    # position = position_nudge(x = 1*posNudge), 
    
  }
  
  ## Determine y-axis limits if necessary:
  # iRound <- 2
  # if (is.null(yLim)){
  #   yLim <- c(round(min(d$y),iRound)-0.01,round(max(d$y),iRound)-0.01)
  # }
  
  
  # --------------------------------------------------------------------- #
  ## Overall group-level mean and error-bar:
  cat("Draw fixed-effects line\n")
  
  groupCoefs <- fixef(mod)
  # groupSE <- VarCorr(mod)
  # iCol <- which(colnames(groupCoefs)==selEff) # localize where in groupCoefs effect of interest is
  iInter <- as.numeric(groupCoefs[1]) # extract intercept
  iSlope <- as.numeric(groupCoefs[iCol]) # extract slope
  yVecEval <- iInter + xVecEval * iSlope * -1
  # if (summary(mod)$objClass=="glmerMod"){yVecEval <- inv.logit(yVecEval)}
  if (isGLMM(mod)){yVecEval <-mod@resp$family$linkinv(yVecEval)} # bring to response scale
  # Create single data points (2 should be enough, i.e. xmin and xmax)
  d <- data.frame(x = xVecEval, y = yVecEval)
  
  # ------------------------------- #
  ## Thick line connecting means (plot line first and points on top):
  p <- p + geom_path(data = d, aes(x = x, y= y), color = "black", size = 1.5)
  # position = position_nudge(x = 1*posNudge), 

  # ------------------------------- #
  ## Point for mean:
  p <- p + geom_point(data = d, aes(x = x, y = y), # point
                      color = color, alpha = 1, size = 5) # size = 2
  # position = position_nudge(x = -1*posNudge),
  
  # ------------------------------- #
  ## Error shades:
  # https://github.com/cran/effects/blob/master/R/Effect.R
  # mmat <- matrix(0,xLen,ncol(coef(mod)$subject)) # initialize design matrix: by default all regressors 0
  # mmat[,1] <- rep(1,xLen) # add intercept
  # mmat[,iCol] <- xVecEval # add regressor of interest 
  # V <- vcov(mod, complete=FALSE) # covariance matrix from model
  # vcov <- mmat %*% V %*% t(mmat) # multiply with design matrix
  # var <- diag(vcov) # variance
  # se <- sqrt(var) # se
  # ymin = yVecEval-z*se
  # ymax = yVecEval+z*se
  tmp <- effect(selEff,mod)
  ymin <- tmp$lower 
  ymax <- tmp$upper 
  if (isGLMM(mod)){ymin <-mod@resp$family$linkinv(ymin)} # bring to response scale
  if (isGLMM(mod)){ymax <-mod@resp$family$linkinv(ymax)} # bring to response scale
  d <- data.frame(x = xVecEval, y = yVecEval, ymin = ymin, ymax = ymax)
  p <- p + geom_errorbar(data = d, aes(x = x, y = y, ymin = ymin, ymax = ymax),
                         color = color, width = 0.15, size = 1.5, alpha = .6)
  # position = position_nudge(-1*posNudge), 

  # if (mean(yLim) == 0.5){ # if conditional probabilities:
  #   p <- p + geom_hline(yintercept=0.5, linetype=2, color="black", size = 1)
  # }
  
  # --------------------------------------------------------------------- #
  cat("Adjust axes, labels\n")
  
  ## Y-axis:
  if (yLim[1] == 0 & yLim[2] == 1){
    # p <- p + scale_y_break(c(0, 0.5, 1))
    p <- p + scale_y_continuous(breaks = seq(0, 1, by = 0.5)) # only 0, 0.5, 1 as axis labels
  }
  p <- p + coord_cartesian(xlim = c(-1.5, 1.5), ylim=yLim) 
  
  ## X-axis:
  p <- p + scale_x_continuous(breaks=xVecEval, labels=Labels)
  
  ## Labels:
  p <- p +  xlab(xLab) + ylab(yLab)
    
  ## Other settings:
  if (!is.null(Main)){
    p <- p + ggtitle(Main) # title off for printing for poster
  }
  
  p <- p + theme_classic() # theme
  
  if (!is.null(margin)){
    p <- p + theme(plot.margin = unit(margin, "cm"))
  }
  
  ## Font sizes:
   p <- p + theme(axis.text=element_text(size=fontSize),
                  axis.title=element_text(size=fontSize), 
                  plot.title = element_text(size=fontSize, hjust = 0.5), # center title 
                  legend.text = element_text(size=fontSize))
  
  ## Print plot in the end:
  print(p)
  cat("Finished :-)\n")
  return(p)
}  

# =============================================================================================== #
#### Plot learning curve per cue per subject: #####

plot_stim_subject <- function(data, iSub = NULL, isSmooth = F, kernelLength = 5, jitter = 0){
  #' Plot learning curve per cue per subject using basic plot() function 
  #' @param data data frame with variables 'response', 'cue', 'reqAction', 'subject'
  #' @param iSub integer, subject to select
  #' @param isSmooth boolean, whether to smooth learning curves or not
  #' @param jitter numeric, jitter y-coordinate in steps of \var{jitter} to avoid overlapping curves, default 0
  #' @return nothing returned, just plotting
  
  ## Fixed variables:
  respVar <- "response"
  reqActVar <- "reqAction"
  stimVar <- "cue"
  subVar <- "subject"
  lwd <- 3
  
  ## All subjects:
  if (is.null(iSub)){
    subVec <- sort(unique(data[,subVar]))
  } else {
    subVec <- iSub
  }

  ## Loop through all subjects:
  for (iSub in subVec){ # iSub <- 1
    cat(paste0("Plot subject ",iSub,"\n"))
    
    ## Select subject data:
    subIdx <- which(data[,subVar]==iSub)
    subData <- data[subIdx,]

    ## All stimuli:
    stimVec <- sort(unique(data[,stimVar]))
    nStim <-length(stimVec)
    
    ## Offset:
    if (jitter != 0){cat(paste0("Use offset of ",jitter,"\n"))}
    offset <- as.numeric(scale(seq(jitter,nStim*jitter,jitter), center = T, scale = F))
    
    ## Initialize new plot:
    # plot.new()
    # plot(NULL,type='n',axes=FALSE,ann=FALSE)
    plot(NULL, type="n", xlim=c(0, 20), ylim=c(0 - min(offset), 1 + max(offset)),
         xlab = "Cue repetitions", ylab ="p(Go)", main = paste0("Learning per cue for subject ",iSub))

    for (iStim in 1:nStim){ # iCue <- "A1"
      
      stimName <- stimVec[iStim]
      
      ## Select cue data:
      stimIdx <- which(subData[,stimVar]==stimName)
      stimData <- subData[stimIdx,]
      
      colorName <- ifelse(stimData[1,reqActVar]==1,"blue","red")
      
      xVec <- 1:nrow(stimData)
      yVec <- stimData[,respVar]
      if (isSmooth){
        yVec <- adapt_filter(yVec, kernelLength = kernelLength)
      }
      lines(xVec, offset[iStim] + yVec, col = colorName, lwd = lwd)
    } # end iStim
    
    ## Add legend:
    legend(0.05, 0.95, legend=c("Go", "NoGo"),
           col=c("blue", "red"), lty=1, cex=0.8) # , box.lty=0)
    
    readline(prompt="Press [enter] to continue")
    
  } # end iSub
  
  cat("Finished :-)\n")
}

# =============================================================================================== #
#### Wrapper for fitting Q-values: #####

wrapper_Qvalues <- function(data){
  
  # ------------------------------------------ #
  ## Check necessary variables:
  # sum(is.na(data$cue))
  # sum(is.na(data$response))
  # sum(is.na(data$outcome_n))
  
  ## Subject info:
  subVec <- unique(data$subject)
  nSub <- max(subVec)
  
  # ------------------------------------------ #
  ## Initialize parameter values:
  alphaRange <- seq(0, 1, 0.01)
  betaRange <- seq(1, 40, 0.1) # originally 20
  cat(paste0("Fit alpha between ", min(alphaRange), " and ", max(alphaRange), "\n"))
  cat(paste0("Fit beta between ", min(betaRange), " and ", max(betaRange), "\n"))
  
  ## Initialize outcome vectors:
  alphaBest <- rep(NA, nSub) # initialize empty outcome vector
  betaBest <- rep(NA, nSub) # initialize empty outcome vector
  logLikBest <- rep(NA, nSub) # initialize empty outcome vector
  
  # ------------------------------------------ #
  ## Fit via grid-search:
  for (iSub in subVec){
    out <- do_grid_search(alphaRange, betaRange, data, iSub)
    alphaBest[iSub] <- out[1]
    betaBest[iSub] <- out[2]
    logLikBest[iSub] <- out[3]
  }
  
  # ------------------------------------------ #
  ## Simulate Q-values:
  data$QGo <- NA
  data$QNoGo <- NA
  for (iSub in subVec){ # iSub <- 24
    data <- simulate_RW_model_osap(alphaBest[iSub], betaBest[iSub], data, iSub)
  }
  
  # ------------------------------------------ #
  ## Compute Q-value difference:
  data$Qdif <- data$QGo - data$QNoGo
  data$Qdif_z <- as.numeric(scale(data$Qdif))
  
  # ------------------------------------------ #
  ### Al sorts of checks:
  
  ## Check NAs:
  # sum(is.na(eyeData$QGo))
  # sum(is.na(eyeData$QNoGo))
  
  ## Plot Q-value time course of entire experiment:
  # ggplot(eyeData,aes(x=trialnr,y=QGo,col=reqAction_f))+geom_smooth(lwd=3) # QGo
  # ggplot(eyeData,aes(x=trialnr,y=QNoGo,col=reqAction_f))+geom_smooth(lwd=3) # QNoGo
  
  # Per subject:
  # ggplot(eyeData,aes(x=trialnr,y=QGo,col=reqAction_f))+geom_smooth(lwd=3)+facet_wrap(~subject) # QGo
  # ggplot(eyeData,aes(x=trialnr,y=QNoGo,col=reqAction_f))+geom_smooth(lwd=3)+facet_wrap(~subject) # QNoGo
  
  ## Plot Q-value time course within session:
  # ggplot(eyeData,aes(x=trialNrSes,y=QGo,col=reqAction_f))+geom_smooth(lwd=3) # QGo
  # ggplot(eyeData,aes(x=trialNrSes,y=QNoGo,col=reqAction_f))+geom_smooth(lwd=3) # QNoGo
  
  ## Per subject:
  # ggplot(eyeData,aes(x=trialNrSes,y=QGo,col=reqAction_f))+geom_smooth(lwd=3)+facet_wrap(~subject) # QGo
  # ggplot(eyeData,aes(x=trialNrSes,y=QNoGo,col=reqAction_f))+geom_smooth(lwd=3)+facet_wrap(~subject) # QNoGo
  
  ## Check difference:
  # densityplot(eyeData$Qdif) # platykurtic
  # tapply(eyeData$Qdif,eyeData$reqAction_f,mean,na.rm=T)
  #        Go       NoGo 
  # 0.3896550 -0.3253613 
  
  output <- list(data, alphaBest, betaBest, logLikBest)
  cat("Output is list with objects data, alphaBest, betaBest, logLikBest\n")
  return(output)
}

# ===============================================================================================#
#### Perform grid search on Recorla-Wagner model: #####

do_grid_search <- function(alpha, beta, data, iSub){ # iSub <- 25
  #' Perform grid search over defined alpha and beta vectors on data of subject iSub
  #' @param alpha vector, values of learning rate parameter to loop over
  #' @param beta bector, value of inverse temperature parameter to loop over
  #' @param data data frame, to extract relevant variables from
  #' @param iSub numeric, subject index
  #' @return vector with best fitting alpha, beta, and maximal log-likelihood
  
  ## Fixed variables:
  subVar <- "subject" # needs to be numeric
  stimVar <- "cue" # gets later turned into numeric
  respVar <- "response" # needs to be numeric
  # outVar <- "outcome_valid_n" # needs to be numeric; NA when response, but wrong side: outcome_valid_n, outcome_magnitude_valid_n
  outVar <- "outcome_magnitude_valid_n" # needs to be numeric; NA when response, but wrong side: outcome_valid_n, outcome_magnitude_valid_n
  
  if(!(subVar %in% colnames(data))){"variable subject not in data set"}
  if(!(stimVar %in% colnames(data))){"variable cue not in data set"}
  if(!(respVar %in% colnames(data))){"variable response not in data set"}
  if(!(outVar %in% colnames(data))){"variable outcome_n not in data set"}
  
  # Retrieve data of selected subject:
  subIdx <- which(data[, subVar] == iSub)
  
  # Cues:
  s <- as.numeric(as.factor(data[subIdx, stimVar])) # transform into 1-16
  s <- s - min(s) + 1 # bring first index to 1
  nTrial <- length(s)
  
  # Choices:
  c <- data[subIdx,respVar] # extract choices
  if (sum(c==0) > 0){ # if zeros contained: invert coding to 0 becomes highest, highest index lowest
    c <- 1 + max(c, na.rm = T) - c
  }
  c <- c - min(c, na.rm = T) + 1 # bring first index to 1
  
  # Outcomes:
  r <- data[subIdx, outVar] # leave at 1, 0, NA
  
  # Perform grid search:
  loglik <- matrix(NA, nrow = length(alpha), ncol = length(beta)) # initialize grid
  for (iAlpha in 1:length(alpha)){
    for (iBeta in 1:length(beta)){
      par <- c(alpha[iAlpha], beta[iBeta])
      loglik[iAlpha,iBeta] <- fit_RW_model(par, s, c, r)
    }
  }
  
  # Find maximum:
  logMax <- max(loglik)
  maxIdx <- which(loglik == logMax, arr.ind = TRUE)
  
  # Store in vector:
  alphaBest <- alpha[maxIdx[1]] # alpha
  betaBest <- beta[maxIdx[2]] # beta
  cat(paste0("Subject ", iSub, ": best alpha = ", 
             alphaBest,"; best beta = ", 
             betaBest, "; log-likelihood = ", 
             round(logMax,2)," (p = ", round(exp(logMax / nTrial), 3),
             " per trial)\n"))
  
  # Define color palette for head map:
  rwb <- colorRampPalette(c("blue","lightblue", "green", "yellow", "red")) # initialize color palette
  
  # Plot heatmap:
  filled.contour(alpha, beta, loglik, color = terrain.colors, color.palette = rwb, # zlim = c(-2,2),
                 plot.title = title(paste0("Subject ", iSub, ": Log-likelihood given alpha and beta"), 
                                    xlab = "alpha", ylab = "beta")) # 19 frequency bins 
  
  return(c(alphaBest,betaBest,logMax))
}

# =============================================================================================== #
#### Fit Rescorla Wagner model: #####

fit_RW_model <- function(par, s, c, r){
  #' Compute log-likelihood for Rescorla Wagner model given alpha and beta 
  #' @param par numeric, value of (1) learning rate parameter and (2) inverse temperature parameter
  #' @param s vector, cue indices (numeric or character)
  #' @param c vector, choice (positive integer starting at 1)
  #' @param r vector, outcome (numeric, skip trials with NA)
  #' @return log-likelihood given alpha and beta and data
  
  # Extract parameters:
  alpha <- par[1]
  beta <- par[2]
  
  # Initialize Q-values at 0:
  Q <- matrix(0, nrow = length(unique(s)), ncol = length(unique(c))) # stimuli in rows, responses in columns
  loglik <- 0 # initialize log-likelihood at 0
  
  # Loop over trials:
  nTrial <- length(s) # number trials
  for (iTrial in 1:nTrial){
    # cat("Subject",iSub,", trial",iTrial,": r = ",r[iTrial],"\n")
    
    if (!(is.na(r[iTrial]))){ # if outcome defined: compute log-likelihood and update
      
      # Softmax: p(chose the chosen option)
      pChoice <- exp(beta * Q[s[iTrial], c[iTrial]]) / sum(exp(beta * Q[s[iTrial], ]))
      
      # Log-likelihood: log-transform, add to log-likelihood:
      loglik <- loglik + log(pChoice)
      
      # Update based on outcome:
      Q[s[iTrial],c[iTrial]] <- Q[s[iTrial], c[iTrial]] + alpha * (r[iTrial] - Q[s[iTrial], c[iTrial]])
    }
  }
  return(loglik)
}

# =============================================================================================== #
#### Forward simulate RW model (QGo and QNoGo, one-step-ahead predictions): #####

simulate_RW_model_osap <- function(alpha, beta, data, iSub){ # iSub <- 25
  #' Simulate QGo and QNoGo given empirical choices and outcomes for given subject given input parameters.
  #' @param alpha numeric, value of the learning rate parameter to use for this subject.
  #' @param beta numeric, value of the inverse temperature parameter to use for this subject.
  #' @param data data frame, to extract relevant task variables from.
  #' @param iSub numeric, index of subject to simulate for.
  #' @return data frame with added variables QGo and QNoGo.
  
  # -------------------------------------------------------------------------- #
  ### Fixed variables:
  
  subVar <- "subject" # needs to be numeric
  trialVar <- "trialnr" # needs to be numeric
  stimVar <- "cue" # gets later turned into numeric
  respVar <- "response" # needs to be numeric
  outVar <- "outcome_valid_n" # needs to be numeric; NA when no outcome (catch task) or response, but wrong side: outcome_valid_n, outcome_magnitude_valid_n
  QGo <- "QGo"
  QNoGo <- "QNoGo"
  pGo <- "pGo"
  pNoGo <- "pNoGo"
  
  # -------------------------------------------------------------------------- #
  ### Retrieve data of selected subject:
  
  ## Retrieve row indices for this subject:
  subIdx <- which(data[, subVar] == iSub)
  
  cat("Start subject", iSub, "\n")
  
  ## Cues:
  s <- as.numeric(as.factor(data[subIdx, stimVar])) # transform into 1-16
  s <- s - min(s) + 1 # bring first index to 1
  
  ## Choices:
  c <- data[subIdx, respVar]
  if (sum(c==0) > 0){ # if zeros contained: invert coding to 0 becomes highest, highest index lowest
    c <- 1 + max(c) - c
  }
  c <- c - min(c) + 1 # bring first index to 1
  
  ## Outcomes:
  r <- data[subIdx, outVar] # leave at 1,0
  
  ## Initialize Q-values at 0:
  Q <- matrix(0,nrow = length(unique(s)), ncol = length(unique(c))) # stimuli in rows, responses in columns
  nTrial <- length(s)
  
  # -------------------------------------------------------------------------- #
  ## Loop over trials:
  
  for (iTrial in 1:nTrial){ # iTrial <- 1
    
    rowIdx <- which(data[,subVar] == iSub & data[, trialVar] == iTrial) # trial index
    
    ## Compute choice probabilities:
    qTrial <- Q[s[iTrial], ] # retrieve Q-values for this cue
    pTrial <- exp(beta * qTrial) / sum(exp(beta * qTrial)) # apply softmax to get probabilities
    
    ## Store Q-values and choice probabilities:
    data[rowIdx, QGo] <- Q[s[iTrial], 1]
    data[rowIdx, QNoGo] <- Q[s[iTrial], 2]
    data[rowIdx, pGo] <- pTrial[1]
    data[rowIdx, pNoGo] <- pTrial[2]
    
    ## Prediction error update:
    if (!is.na(c[iTrial]) & !is.na(r[iTrial])){ # if c and r are defined: update
      Q[s[iTrial],c[iTrial]] <- Q[s[iTrial], c[iTrial]] + alpha * (r[iTrial] - Q[s[iTrial], c[iTrial]])
    }
  }
  return(data)
}

# =============================================================================================== #
#### Forward simulate RW model: #####

simulate_RW_model_modSim <- function(alpha, beta, data, iSub){ # iSub <- 25
  #' Generate new choices and outcomes for given subject given input parameters.
  #' @param alpha numeric, value of the learning rate parameter to use for this subject.
  #' @param beta numeric, value of the inverse temperature parameter to use for this subject.
  #' @param data data frame, to extract relevant task variables from.
  #' @param iSub numeric, index of subject to simulate for.
  #' @return data frame with added variables response_sim_n, outcome_sim_n, outcome_magnitude_sim_n.
  
  # alpha <- 0.1; beta <- 20; data <- eyeData; iSub <- 1
  
  # -------------------------------------------------------------------------- #
  ### Fixed variables:
  
  ## Input variables:
  subVar <- "subject" # needs to be numeric
  trialVar <- "trialnr" # needs to be numeric
  stimVar <- "cue" # gets later turned into numeric
  rewStakes <- "rewMag" # numeric
  punStakes <- "punMag" # numeric
  reqAction <- "reqAction" # numeric
  outValid <- "validity_n" # numeric
  QGo <- "QGo"
  QNoGo <- "QNoGo"
  pGo <- "pGo"
  pNoGo <- "pNoGo"
  
  ## Output variables:
  respVar <- "response_sim_n" #
  ACCVar <- "ACC_sim_n" #
  outVar <- "outcome_sim_n" #
  outMagVar <- "outcome_magnitude_sim_n" #
  
  # -------------------------------------------------------------------------- #
  ## Retrieve data of selected subject:
  subIdx <- which(data[, subVar] == iSub)
  
  # cat("Start subject", iSub, "\n")
  
  ## Cues:
  s <- as.numeric(as.factor(data[subIdx, stimVar])) # transform into 1-12
  s <- s - min(s) + 1 # bring first index to 1
  
  ## Initialize Q-values at 0:
  Q <- matrix(0, nrow = length(unique(s)), ncol = 2) # stimuli in rows, responses in columns
  nTrial <- length(s)
  
  for (iTrial in 1:nTrial){ # iTrial <- 1
    
    rowIdx <- which(data[, subVar] == iSub & data[, trialVar] == iTrial) # trial index
    
    ## Simulate choice and outcome:
    if (is.na(data[rowIdx, outValid])){ # if catch trial
      
      pTrial <- c(NA, NA)
      c <- NA
      a <- NA
      r <- NA
      
    } else {
      
      ## Choice:
      qTrial <- Q[s[iTrial], ] # retrieve Q-values for this cue
      pTrial <- exp(beta * qTrial) / sum(exp(beta * qTrial)) # apply softmax to get probabilities
      c <- rbinom(1, 1, pTrial[1])
      
      ## Accuracy:
      a <- ifelse(data[rowIdx, reqAction] == c, 1, 0) 
      
      ## Outcome:
      r <- ifelse(a == data[rowIdx, outValid], 1, -1) 
      # if (a == data[rowIdx, outValid]){r <- 1} # correct & valid | incorrect & invalid
      # if (a != data[rowIdx, outValid]){r <- -1} # incorrect & valid | correct & invalid
      # if (a == 1 & data[rowIdx, outValid] == 1){r <- 1} # correct & valid
      # if (a == 1 & data[rowIdx, outValid] == 0){r <- -1} # correct & invalid
      # if (a == 0 & data[rowIdx, outValid] == 1){r <- -1} # incorrect & valid
      # if (a == 0 & data[rowIdx, outValid] == 0){r <- 1} # incorrect & invalid
      
      ## Prediction error update:
      Q[s[iTrial], 2 - c] <- Q[s[iTrial], 2 - c] + alpha * (r - Q[s[iTrial], 2 - c])
    }
    
    rMag <- ifelse(r == 1, data[rowIdx, rewStakes],
                   ifelse(r == -1, data[rowIdx, punStakes] * -1, 
                          NA))
    
    ## Store variables:
    data[rowIdx, QGo] <- Q[s[iTrial], 1]
    data[rowIdx, QNoGo] <- Q[s[iTrial], 2]
    data[rowIdx, pGo] <- pTrial[1]
    data[rowIdx, pNoGo] <- pTrial[2]
    data[rowIdx, respVar] <- c
    data[rowIdx, ACCVar] <- a
    data[rowIdx, outVar] <- r
    data[rowIdx, outMagVar] <- rMag
    
  } # end iTrial
  
  return(data)
  
}

# =============================================================================================== #
#### Print effect from lme4 model: #####

print_effect <- function(mod, eff, nDigit = 3){
  #' Print selected effect from lme4 model
  #' @param mod fitted model
  #' @param eff string, name of effect for which to print effect
  #' @param nDigit integer, number of digits to round after comma, default 2
  #' @return nothing returned, but printed
  require(stringi)
  
  nPad <- nDigit + 2
  
  if (str_sub(eff,-1) == "f"){eff <- paste0(eff,"1")} # add 1 at the end if eff is factor
  
  ## Extract output of fixed effects:
  coefs <- summary(mod)$coefficients # extract coefficients
  idx <- which(rownames(coefs)==eff) # find effect back
  if (length(idx)==0){stop(paste0("Effect ", eff, " not found"))} 
  
  ## Retrieve coefficients:
  if (summary(mod)$objClass=="glmerMod"){ # glmer

    ## Extract relevant info:
    b <- coefs[idx,1]
    se <- coefs[idx,2]
    zScore <- coefs[idx,3]
    pVal <- coefs[idx,4]
    
  } else if (summary(mod)$objClass=="lmerModLmerTest"){ # lmer
    
    ## Extract relevant info:
    b <- coefs[idx,1]
    se <- coefs[idx,2]
    dfs <- coefs[idx,3]
    zScore <- coefs[idx,4]
    pVal <- coefs[idx,5]
    
  } else {
    stop("Unknown model type")
  }
  
  ## Variable padding of b based on sign:
  bPad <- ifelse(b > 0,nPad,nPad+1) # pad to 5 digits if negative
  zPad <- ifelse(zScore > 0,nPad,nPad+1) # pad to 5 digits if negative
  
  ## Handle b:
  if (round(b,nDigit) == 0){
    bText <- "0"
  } else {
    bText <- str_pad(round(b,nDigit), bPad, side="right", pad="0")
  }

  ## Handle se:
  if (round(se,nDigit) == 0){
    seText <- "0"
  } else {
    seText <- str_pad(round(se,nDigit), nPad, side="right", pad="0")
  }
  
  ## Handle statistic for given object:
  if (summary(mod)$objClass=="glmerMod"){
    zStat <- ", z = "
  } else {
    zStat <- paste0(", t(",round(dfs,nDigit),") = ")
  }
  
  if (round(zScore,nDigit) == 0){
    zText <- "0"
  } else {
    zText <- str_pad(round(zScore,nDigit), zPad, side="right", pad="0")
  }
  
  ## Handle very small p-values:
  if (pVal < 0.001){
    pText <- "p < .001"
  } else {
    pText <- paste0("p = ", str_pad(round(pVal,(nDigit+1)), 5, side="right", pad="0")) # p-value: always 5 digits
  }
  
  # Print to console:
  cat(paste0("b = ", bText,
               ", se = ", seText,
               zStat, zText,
               ", ", pText,"\n"))
}

# =============================================================================================== #
#### Fit lm per subject: #####

loop_lm_subject <- function(data, formula, iCoef = 2, isGLM = F){
  #' Perform lm separately for each subject
  #' @param data data frame with variable subject and DVs and IVs
  #' @param formula string with formula to fit in Wilkinson notation
  #' @param iCoef coefficient to print out, default 2 (first IV after intercept)
  #' @param isGLM boolean, fit generalized lm with binomial link function
  #' @return bVec vector of b-values (for one-sample t-test on group-level), also print per subject
  require(stringr)
  
  ## Fixed variables:
  subVar <- "subject"
  nDigit <- 2
  
  ## All subjects:
  subVec <- sort(unique(data[,subVar]))
  nSub <- length(subVec)
  
  bVec <- rep(NA,nSub) # save b-value per subject
  
  # --------------------------------------------------- #
  ## Loop through all subjects:
  for (iSub in 1:nSub){ # iSub <- 1
    
    subName <- subVec[iSub]
    
    ## Select subject data:
    subIdx <- which(data[,subVar]==subName)
    subData <- data[subIdx,]
    
    ## Fit model:
    if (isGLM) {
      mod <- glm(formula = formula, data = subData, family = binomial())
    } else {
      mod <- lm(formula = formula, data = subData)
      dfs <- df.residual(mod)
    }
    
    ## Extract coefficients:
    coefs <- summary(mod)$coefficients # extract coefficients
    
    ## Extract relevant info:
    b <- coefs[iCoef,1]
    se <- coefs[iCoef,2]
    tStat <- coefs[iCoef,3]
    pVal <- coefs[iCoef,4]
    
    ## Save b-value:
    bVec[iSub] <- b
    
    ## Rounding:
    b <- round(b,nDigit)
    se <- round(se,nDigit)
    tStat <- round(tStat,nDigit)
    pVal <- round(pVal,(nDigit+1))
                    
    ## Variable padding based on sign:
    bPad <- ifelse(b > 0,4,5) # pad to 5 digits if negative
    tPad <- ifelse(tStat > 0,4,5) # pad to 5 digits if negative

    ## Create text objects:
    if (b==0){
      bText <- "0"
    } else {
      bText <- str_pad(b, bPad, side="right", pad="0")
    }
    
    seText <- str_pad(se, nDigit, side="right", pad="0")
    
    statText <- ifelse(isGLM,"z = ",paste0(", t(",round(dfs,nDigit),") = "))
    
    if (tStat==0){
      tText <- "0"
    } else {
      tText <- str_pad(tStat, tPad, side="right", pad="0")
    }
    
    # Handle very small p-values:
    if (pVal < 0.001){
      pText <- "< .001"
    } else if (pVal == 1){
      pText <- "= 1"
    } else {
      pText <- paste0("= ", str_pad(pVal, 5, side="right", pad="0"))
    }
    
    # Print to console:
    cat(paste0("Subject ",subName,":\n"))
    cat(paste0("b = ", bText,
               ", se = ", seText,
               ", ",statText, tText,
               ", p ", pText,"\n"))

  } # end iSub
  return(bVec)
}

# =============================================================================================== #
#### Fit lm per subject: #####

loop_glm_subject <- function(data, formula, iCoef = 2){
  #' Wrapper on loop_lm_subject to perform glm separately for each subject
  #' @param data data frame with variable subject and DVs and IVs
  #' @param formula string with formula to fit in Wilkinson notation
  #' @param iCoef coefficient to print out, default 2 (first IV after intercept)
  #' @return bVec vector of b-values (for one-sample t-test on group-level), also print per subject
  require(stringr)
  
  bVec <- loop_lm_subject(data, formula, iCoef = iCoef, isGLM = T)

  return(bVec)
}

# =============================================================================================== #
#### Permutation test: #####

permutation_test <- function(x, y, n = 10000){
  #' Permutation test of 2-sided paired samples t-test
  #' Inputs:
  #' x,y = two vectors of equal length that will be used for permutation. Hypothesis x != y is tested
  #' n = number of permutations (default: 10000)
  #' Outputs: 
  #' p = p-value (number of samples in permutation distribution more extreme than critical value)
  
  if (length(x) != length(y)){
    stop("Error: x and y of different length")
  } 
  
  ## Preparation:
  nS <-  length(x) # number of samples in vectors 
  diff <- x-y # difference vector of x and y 
  testval <- mean(diff, na.rm = T)/sd(diff, na.rm = T) # empirical test statistics (here: Cohen's d)
  permdist <-  rep(NA, n) # initialize
  
  ## Loop over permutations: 
  for(i in 1:n){
    sign <- sample(c(1,-1), nS, replace = T) # sample vectors of signs with -1 or 1
    testVec <- (diff) * sign # multiply differences between x and y with randomly sampled signs
    permdist[i] <- mean(testVec, na.rm = T)/sd(testVec, na.rm = T) # Take the mean, divide by std (ie Cohen's d), store in permutation distribution
  }
  
  ## Compute p-value: 
  p1 <- sum(permdist > testval) / length(permdist) # number samples in permutation distribution that are larger than empirical test statistic. 
  p2 <- sum(permdist < testval) / length(permdist) # number samples in permutation distribution that are smaller than empirical test statistic
  p <- min(p1,p2)*2 # times 2 because 2-sided test
  return(p)
}

# ================================================================================================================================================ #
#### Barplot 1 IV: Aggregate per condition per subject, plot (1 IV on x-axis): ####

custom_barplot1 <- function(data, xVar=NULL, yVar=NULL, subVar="subject", 
                            xLab = "Condition", yLab = "p(Go)", selCol = "red",
                            isPoint = F, isBeeswarm = F, yLim = NULL, savePNG = F, saveEPS = F){
  #' Make bar plot with error bars and individual-subject data points.
  #' @param data data frame, trial-by-trial data.
  #' @param xVar string, name of variable that goes on x-axis. If numeric, it will be converted to an (ordered) factor.
  #' @param yVar string, name of variable that goes on y-axis. Needs to be numeric.
  #' @param subVar string, name of variable containing subject identifier (default: subject).
  #' @param xLab string, label for x-axis (default: "x").
  #' @param yLab string, label for y-axis (default: "y").
  #' @param selCol vector of strings (HEX colors), colors for bars (default: "red").
  #' @param isPoint Boolean, plot individual data points per condition as small points (default: FALSE).
  #' @param isBeewswarm Boolean, plot individual data points per condition as beeswarm densities (default: FALSE).
  #' @param yLim vector of two numbers, y-axis (default: automatically determined by ggplot).
  #' @param savePNG Boolean, save as .png file.
  #' @param saveEPS Boolean, save as .eps file.
  #' @return creates (and saves) plot.
  
  ## Load required packages:
  require(plyr) # for ddply
  require(Rmisc) # for summarySEwithin
  
  ## Fixed plotting settings:
  lineWidth <- 1.5
  fontSize <- 30
  dodgeVal <- 0.6
  colAlpha <- 1
  
  ## Create variables under standardized names:
  data$x <- data[,xVar]
  data$y <- data[,yVar]
  data$subject <- data[,subVar]
  
  ## Aggregate data:
  aggrData <- ddply(data, .(subject, x), function(x){
    y <- mean(x$y, na.rm = T)
    return(data.frame(y))
    dev.off()})
  cat(paste0("Min = ",round(min(aggrData$y),3),"; Max = ",round(max(aggrData$y),3)),"\n")
  
  ## Add jittered x-axis variable for points:
  aggrData$xpos <- as.numeric(aggrData$x) # to numeric
  aggrData$j <- jitter(rep(0,nrow(aggrData)), amount=.09) # jitter
  aggrData$xj <- aggrData$xpos + aggrData$j # add jitter
  
  ## Determine y limits if not given:
  if(is.null(yLim)){
    yLim <- determine_ylim(aggrData)
  }
  
  ## Aggregate across subjects with Rmisc:
  summary_d <- summarySEwithin(aggrData, measurevar="y", idvar = "subject", na.rm = T,
                               withinvars = c("x"))
  
  ## ggplot:
  # Name:
  plotName <- paste0("custombarplot1_",yVar,"_",xVar)
  if (isPoint){plotName <- paste0(plotName,"_points")} 
  
  ## Saving:
  if (saveEPS){cat("Save as eps\n"); setEPS(); postscript(paste0(plotdir,plotName,".eps"), width = 480, height = 480)}
  if (savePNG){cat("Save as png\n"); png(paste0(plotdir,plotName,".png"), width = 480, height = 480)}
  
  ## Start plot:
  p <- ggplot(summary_d,aes(x, y)) + 
    # Bars of means:
    stat_summary(fun = mean, geom = "bar", position = "dodge", width = 0.6, 
                 lwd = lineWidth, fill = selCol, color = "black") + 
    # Error bars:
    geom_errorbar(data = summary_d, 
                  aes(x = x, y = y, ymin = y-se, ymax = y+se),
                  position = position_dodge(width = dodgeVal), width = 0.1, 
                  lwd = lineWidth, color = "black", alpha = 1)
  ## Individual data points:
  if (isPoint){
    p <- p + geom_point(data = aggrData, aes(x = xj), shape = 1, size = 2, stroke = 1, # size = 0.6, 
                        color = "black", alpha = colAlpha)
  }
  if (isBeeswarm){
    p <- p + geom_beeswarm(data = aggrData, aes(x = xpos), shape = 1, size = 2, stroke = 1, # size = 0.6, 
                        color = "black", alpha = colAlpha)
  }
  
  ## Settings:
  if (yLim[1] == 0 & yLim[2] == 1){
    # p <- p + scale_y_break(c(0, 0.5, 1))
    p <- p + scale_y_continuous(breaks = seq(0, 1, by = 0.5)) # only 0, 0.5, 1 as axis labels
  }
  if(!(is.null(yLim))){p <- p + coord_cartesian(ylim=yLim)}
  require(ggthemes)
  p <- p + labs(x=xLab, y = yLab) +
    # ggtitle(paste0(yVar," as function of \n",xLab)) + # add title
    theme_classic() +
    theme(axis.text=element_text(size=25),axis.title=element_text(size=30), title = element_text(size=30),
          axis.line=element_line(colour = 'black', size = lineWidth)) # fixed font sizes
  # theme(axis.text=element_text(size=fontSize),axis.title=element_text(size=fontSize), title = element_text(size=fontSize),
  #       axis.line=element_line(colour = 'black', size = lineWidth)) # font sizes based on variable
  print(p)
  if(savePNG | saveEPS){dev.off()}
}

# ================================================================================================================================================ #
#### Barplot 2 IVs: Aggregate per condition per subject, plot (2 IVs, 1 on x-axis, on as color/ adjacent bars): ####

custom_barplot2 <- function(data, xVar, yVar, zVar, subVar = "subject", 
                            xLab = "Condition", yLab = "p(Go)", zLab = "Action", main = NULL,
                            selCol = c("blue","red"), isPoint = F, isBeeswarm = F, yLim = NULL, savePNG = F, saveEPS = F){
  #' Make bar plots with 2 IVs: x-axis and color
  #' @param data data frame, trial-by-trial data.
  #' @param xVar string, name of variable that goes on x-axis. If numeric, it will be converted to an (ordered) factor.
  #' @param yVar string, name of variable that goes on y-axis. Needs to be numeric.
  #' @param zVar string, name of variable that determines bar coloring. Needs to be a factor.
  #' @param subVar string, name of variable containing subject identifier (default: subject).
  #' @param xLab string, label for x-axis (default: "x").
  #' @param yLab string, label for y-axis (default: "y").
  #' @param zLab string, label for color legend (default: "z").
  #' @param selCol vector of strings (HEX colors), colors for input levels of zVar (default: c("blue","red")).
  #' @param yLim vector of two numbers, y-axis (default: automatically determined by ggplot).
  #' @param isPoint Boolean, plot individual data points per condition as small points (default: TRUE).
  #' @param isBeeswarm Boolean, plot individual data points per condition as beeswarm density (default: FALSE).
  #' @param savePNG Boolean, save as .png file.
  #' @param saveEPS Boolean, save as .eps file.
  #' @return creates (and saves) plot.
  
  ## Load packages:
  require(plyr) # for ddply
  require(Rmisc) # for summarySEwithin
  require(ggbeeswarm) # for ggbeeswarm
  
  ## Fixed plotting settings:
  lineWidth <- 1.3
  fontSize <- 30
  dodgeVal <- 0.6
  colAlpha <- 1
  condCol <- rep(selCol,2)
  
  ## Create variables under standardized names:
  data$x <- data[,xVar]
  data$y <- data[,yVar]
  data$z <- data[,zVar]
  data$subject <- data[,subVar]
  
  # ------------------------------------------- #
  ## Aggregate data per subject per condition:
  aggrData <- ddply(data, .(subject, x, z), function(x){
    y <- mean(x$y, na.rm = T)
    return(data.frame(y))
    dev.off()})
  ## Wide format: each subject/condition1/condition2 in one line, variables subject, x, y, z
  
  ## Add condition variable:
  aggrData$cond <- as.numeric(aggrData$x)*2 - 2 + as.numeric(aggrData$z)
  nCond <- length(unique(aggrData$cond))
  if (length(condCol) < nCond){condCol <- rep(condCol, length.out = nCond)}
  
  ## Add jittered x-axis for points:
  aggrData$xj <- as.numeric(aggrData$x) # convert to numeric, copy to xj
  aggrData$xpos <- aggrData$xj + (as.numeric(aggrData$z) - 1.5)*2*0.15 # convert to [1 2], to [-0.5,0.5], * 2 so [-1 1], scale
  aggrData$j <- jitter(rep(0,nrow(aggrData)), amount=.05) # pure jitter
  aggrData$xj <- aggrData$xpos + aggrData$j # add jitter to xpos
  
  ## Determine y limits if not given:
  if(is.null(yLim)){
    yLim <- determine_ylim(aggrData)
  }

  ## Aggregate across subjects with Rmisc:
  summary_d <- summarySEwithin(aggrData, measurevar="y", idvar = "subject", na.rm = T,
                               withinvars = c("x","z"))
  ## Aggregated over subjects, one row per condition, variables x, z, N, y, sd, se, ci
  
  ## ggplot:
  ## Name:
  plotName <- paste0("custombarplot2_",yVar,"_",xVar,"_",zVar)
  if (isPoint){plotName <- paste0(plotName,"_points")} 
  
  ## Saving:
  if (saveEPS){cat("Save as eps\n"); setEPS(); postscript(paste0(plotdir,plotName,".eps"), width = 480, height = 480)}
  if (savePNG){cat("Save as png\n"); png(paste0(plotdir,plotName,".png"), width = 480, height = 480)}
  
  ## Start plot:
  p <- ggplot(summary_d,aes(x, y, fill = z)) + 
    
    ## Bars of means:
    stat_summary(fun = mean, geom = "bar", position = "dodge", width = 0.6,
                 lwd = lineWidth, color = "black") + 
    
    ## Error bars:
    geom_errorbar(data = summary_d, 
                  aes(x = x, y = y, ymin = y-se, ymax = y+se),
                  position = position_dodge(width = dodgeVal), width = 0.2, 
                  lwd = lineWidth, color = "black", alpha = 1)
  
  ## Individual data points:
  if (isPoint){
    for(iCond in 1:nCond){ # add separately per condition
    p <- p + geom_point(data = aggrData[aggrData$cond==iCond,],
                        aes(x = xj), # position = "dodge",
                        shape = 21, size = 2, stroke = 1.2, color = "black", fill = condCol[iCond],
                        alpha = 0.5) # colAlpha)
    }
  }
  
  ## Beeswarm style plots:
  if (isBeeswarm){
    for(iCond in 1:nCond){ # add separately per condition
      p <- p + geom_beeswarm(data = aggrData[aggrData$cond==iCond,],
                          aes(x = xpos), # position = "dodge",
                          # priority = "ascending",
                          shape = 21, size = 2, stroke = 1.2, color = "black", fill = condCol[iCond],
                          alpha = 0.5) # colAlpha)
    }
  }
  
  ## Add title:
  if (!(is.null(main))){
    p <- p + ggtitle(main)  
  }

  ## Settings:
  if (yLim[1] == 0 & yLim[2] == 1){
    # p <- p + scale_y_break(c(0, 0.5, 1))
    p <- p + scale_y_continuous(breaks = seq(0, 1, by = 0.5)) # only 0, 0.5, 1 as axis labels
  }
  if(!(is.null(yLim))){p <- p + coord_cartesian(ylim=yLim)}
  
  ## Add theme, fontsizes:
  require(ggthemes)
  p <- p + labs(x=xLab, y = yLab, fill = zLab) +
    # ggtitle(paste0(yVar," as function of \n",xLab," and ",zLab)) + 
    # expand_limits(y=c(0,1)) + 
    scale_fill_manual(values=selCol) + 
    theme_classic() + 
    theme(axis.text=element_text(size=fontSize),
          axis.title=element_text(size=fontSize), 
          plot.title = element_text(size=fontSize, hjust = 0.5), 
          legend.text = element_text(size=fontSize),
          legend.title=element_blank(), legend.position = "none")
  
  print(p)
  if(savePNG | saveEPS){dev.off()}
}

# ================================================================================================================================================ #
#### Lineplot 1 IV: Aggregate per time point per condition per subject, plot (1 IV for any condition, time on x-axis): ####

custom_lineplot <- function(data, xVar="counter", yVar="response_cleaned", zVar="condition_f", subVar="subject", 
                            xLab = "Time (trial number)", yLab = "p(Go)", main = "",
                            selCol = c("#009933","#CC0000","#009933","#CC0000"), selLineType = c(1,1,2,2),
                            SEweight = 1, yLim = NULL, savePNG = F, saveEPS = F){
  #' Make line plot with group-level and individual lines.
  #' @param data data frame, trial-by-trial data.
  #' @param xVar string, name of variable that goes on x-axis. Variable needs to be numeric.
  #' @param yVar string, name of variable that goes on y-axis. Variable needs to be numeric.
  #' @param zVar string, name of variable that determines bar coloring. Variable needs to be a factor.
  #' @param subVar string, name of variable containing subject identifier (default: subject).
  #' @param xLab string, label for x-axis (default: "x").
  #' @param yLab string, label for y-axis (default: "y").
  #' @param main string, label for y-axis (default: "y").
  #' @param selCol vector of strings (HEX colors), colors for input levels of zVar (default: c("#009933","#CC0000","#009933","#CC0000")).
  #' @param selLineType vector of numerics, line types to use (default: c(1,1,2,2))
  #' @param SEweight scalar, weight to use for error shades (how many times SE; default: 1).
  #' @param yLim vector of two numbers, y-axis (default: NULL).
  #' @param savePNG Boolean, save as .png file.
  #' @param saveEPS Boolean, save as .eps file.
  #' @return creates (and saves) plot.
  
  ## Load packages:
  require(plyr) # for ddply
  require(Rmisc) # for summarySEwithin
  
  ## Fixed plotting settings:
  LWD <- 3 # axes of plot
  CEX <- 1.5 # axes ticks and labels
  lineWidth <- 3 # line
  fontSize <- 20
  dodgeVal <- 0.6
  colAlpha <- 1

  ## Create variables under standardized names:
  data$x <- data[,xVar]
  data$y <- data[,yVar]
  data$z <- data[,zVar]
  data$subject <- data[,subVar]
  
  # --------------------------------------------------- #
  ## Aggregate data:
  aggrData <- ddply(data, .(subject, x, z), function(x){
    y <- mean(x$y, na.rm = T)
    return(data.frame(y))
    dev.off()})
  # Wide format: each subject/x condition/z condition in one line, variables subject, x, y, z
  
  ## Determine y limits if not given:
  if(is.null(yLim)){
    yLim <- determine_ylim(aggrData)
  }
  
  # --------------------------------------------------- #
  ## Aggregate across subjects with Rmisc:
  summary_d <- summarySEwithin(aggrData, measurevar="y", idvar = "subject", na.rm = T,
                               withinvars = c("x","z"))
  # Aggregated over subjects, one row per condition, variables x, z, N, y, sd, se, ci
  
  # Data dimensions:
  xVec <- unique(sort(as.numeric(summary_d$x)))
  xMax <- max(xVec)
  condNames <- unique(summary_d$z)
  nCond <- length(unique(summary_d$z))

  # --------------------------------------------------- #
  ## ggplot:
  ## Name:
  plotName <- paste0("lineplot_",yVar,"_",xVar,"_",zVar)

  ## Saving:
  if (saveEPS){cat("Save as eps\n"); setEPS(); postscript(paste0(plotdir,plotName,".eps"), width = 480, height = 480)}
  if (savePNG){cat("Save as png\n"); png(paste0(plotdir,plotName,".png"), width = 480, height = 480)}
  
  ## Start plot:
  par(mar = c(5.1, 5.1, 4.1, 2.1)) # bottom, left, top, right
  for (iCond in 1:nCond){ # iCond <- 1
    condName <- condNames[iCond] # name of condition
    yVec <- summary_d$y[summary_d$z == condName] # y-variable
    seVec <- summary_d$se[summary_d$z == condName] # se variable
    plot(xVec, yVec, type = "l", 
         col = selCol[iCond], lty = selLineType[iCond], axes = F,
         lwd = LWD, cex.lab=CEX, cex.axis=CEX, cex.main=CEX,
         xlab = xLab, ylab = yLab, main = main,
         xlim = c(0, xMax), ylim = yLim)
    axis(side = 1, lwd = LWD, cex.axis = CEX, at = seq(0,xMax,5), line = 0)
    axis(side = 2, lwd = LWD, cex.axis = CEX, at = c(0, 0.5, 1))
    polygon(c(xVec,rev(xVec)),
            c(yVec-SEweight*seVec,rev(yVec+SEweight*seVec)),col = alpha(selCol[iCond],0.2), border = F)
    par(new = TRUE)
    
  }
  
  # Add legend:
  legend("top", legend=condNames,
         col=selCol, lty = selLineType, border = 0, lwd = LWD, cex = CEX, horiz=TRUE, bty = "n")
  
  if(savePNG | saveEPS){dev.off()}
  par(mar = c(5.1, 4.1, 4.1, 2.1)) # bottom, left, top, right
  
}

# ================================================================================================================================================ #
#### Lineplot 1 IV with ggplot: Aggregate per time point per condition per subject, plot (1 IV for any condition, time on x-axis): ####

custom_lineplot_gg <- function(data, xVar="counter", yVar="response_cleaned", zVar="condition_f", subVar="subject", 
                            xLab = "Time (trial number)", yLab = "p(Go)", main = "",
                            selCol = c("#009933","#CC0000","#009933","#CC0000"), selLineType = c(1,1,2,2),
                            SEweight = 1, yLim = NULL, savePNG = F, saveEPS = F){
  #' Make line plot with group-level lines plus shades in ggplot.
  #' @param data data frame, trial-by-trial data.
  #' @param xVar string, name of variable that goes on x-axis. Variable needs to be numeric.
  #' @param yVar string, name of variable that goes on y-axis. Variable needs to be numeric.
  #' @param zVar string, name of variable that determines bar coloring. Variable needs to be a factor.
  #' @param subVar string, name of variable containing subject identifier (default: subject).
  #' @param xLab string, label for x-axis (default: "x").
  #' @param yLab string, label for y-axis (default: "y").
  #' @param Main string, label for y-axis (default: "y").
  #' @param selCol vector of strings (HEX colors), colors for input levels of zVar (default: c("#009933","#CC0000","#009933","#CC0000")).
  #' @param selLineType vector of numerics, line types to use (default: c(1,1,2,2))
  #' @param SEweight scalar, weight to use for error shades (how many times SE; default: 1).
  #' @param yLim vector of two numbers, y-axis (default: NULL).
  #' @param savePNG Boolean, save as .png file.
  #' @param saveEPS Boolean, save as .eps file.
  #' @return creates (and saves) plot.
  
  ## Load packages:
  require(plyr) # for ddply
  require(Rmisc) # for summarySEwithin
  
  ## Fixed plotting settings:
  lineWidth <- 1.3
  fontSize <- 30
  colAlpha <- 1
  
  ## Create variables under standardized names:
  data$x <- data[,xVar]
  data$y <- data[,yVar]
  data$z <- data[,zVar]
  data$subject <- data[,subVar]
  
  # --------------------------------------------------- #
  ## Aggregate data:
  aggrData <- ddply(data, .(subject, x, z), function(x){
    y <- mean(x$y, na.rm = T)
    return(data.frame(y))
    dev.off()})
  # Wide format: each subject/x condition/z condition in one line, variables subject, x, y, z
  
  ## Determine y limits if not given:
  if(is.null(yLim)){
    yLim <- determine_ylim(aggrData)
  }
  
  # --------------------------------------------------- #
  ## Aggregate across subjects with Rmisc:
  summary_d <- summarySEwithin(aggrData, measurevar="y", idvar = "subject", na.rm = T,
                               withinvars = c("x","z"))
  summary_d$x <- as.numeric(summary_d$x) # back to numeric to get continuous x-axis
  # Aggregated over subjects, one row per condition, variables x, z, N, y, sd, se, ci
  
  # Data dimensions:
  xVec <- unique(sort(as.numeric(summary_d$x)))
  xMax <- max(xVec)
  condNames <- unique(summary_d$z)
  nCond <- length(unique(summary_d$z))
  
  # --------------------------------------------------- #
  ## ggplot:
  ## Name:
  plotName <- paste0("lineplot_gg_",yVar,"_",xVar,"_",zVar)
  
  ## Saving:
  if (saveEPS){cat("Save as eps\n"); setEPS(); postscript(paste0(plotdir,plotName,".eps"), width = 480, height = 480)}
  if (savePNG){cat("Save as png\n"); png(paste0(plotdir,plotName,".png"), width = 480, height = 480)}
  
  ## Start plot:
  # par(mar = c(5.1, 5.1, 4.1, 2.1)) # bottom, left, top, right
  p <- ggplot(data = summary_d, aes(x = x, y = y))
  
  for (iCond in 1:nCond){ # iCond <- 1
    condData <- subset(summary_d, z == condNames[iCond]) # select data for this condition
    condData$ymin <- condData$y - SEweight * condData$se # lower edge of shade
    condData$ymax <- condData$y + SEweight * condData$se # upper edge of shade
    ## Shade:
    p <- p + geom_ribbon(data = condData, aes(x = x, y = y, ymin = ymin, ymax = ymax, group = 1),
                         fill = alpha(selCol[iCond],0.2))
    ## Line:
    p <- p + geom_path(data = condData, aes(x = x, y = y, group = 1), 
                         col = selCol[iCond], linetype = selLineType[iCond], size = lineWidth) 
  }
  # Add title:
  if (!(is.null(main))){
    p <- p + ggtitle(main)  
  }
  
  ## X-axis:
  p <- p + scale_x_continuous(limits = c(0, xMax), breaks = seq(0,xMax,5))
  
  ## Y-axis:
  if (yLim[1] == 0 & yLim[2] == 1){
    p <- p + scale_y_continuous(breaks = seq(0, 1, by = 0.5)) # only 0, 0.5, 1 as axis labels
  }
  if(!(is.null(yLim))){p <- p + coord_cartesian(ylim=yLim)}
  
  ## Add theme, fontsizes:
  require(ggthemes)
  p <- p + labs(x=xLab, y = yLab, fill = condNames) +
    scale_fill_manual(values=selCol) + 
    theme_classic() + 
    theme(axis.text=element_text(size=fontSize),
          axis.title=element_text(size=fontSize), 
          plot.title = element_text(size=fontSize, hjust = 0.5), 
          legend.text = element_text(size=fontSize),
          legend.title=element_blank(), legend.position = "none")
  
  print(p)
  if(savePNG | saveEPS){dev.off()}
  # par(mar = c(5.1, 4.1, 4.1, 2.1)) # bottom, left, top, right
  
}

# END