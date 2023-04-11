#### 00_functions_preprocess.R ####

#' Collection of functions to pre-process eye-tracking data for both Sample 1 and Sample 2.

# -------------------------------------------------------------------------------------------------- #
#### Read asc file and return size ####

readEyelink <- function(fileName){
  #' Just a wrapper for function \code{read.asc} from package \code{eyelinker},
  #' so everything is together
  #' @param fileName string argument, file name ending on \code{.asc}
  #' @return object read in with \code{read.asc}, has fields
  #'  \code{raw}, \code{msg}, \code{sacc}, \code{fix}, \code{blinks}, \code{info}
  
  require(eyelinker)
  
  cat(paste0("Read file ",fileName, " of size ",file.size(fileName)/1000000," MB\n"))
  dat <- read.asc(fileName, parse_all = T)
  cat(paste0("Output size: ",object.size(dat)/1000000," MB\n"))
  
  return(dat)
}

# -------------------------------------------------------------------------------------------------- #
#### Segment data into trials, read additional trial-by-trial variables ####

readTrials <- function(dat,segmentMessage,beforeEvent,afterEvent,variablesMessages=NULL,recode2Num=NULL,variablesNames=NULL){
  #' Epoch data into trials based on occurrence of \code{segmentMessage} as time point zero, 
  #' with \code{beforeEvent} as positive integer of samples before \code{segmentMessage} 
  #' and \code{afterEvent} as positive integer of samples after \code{segmentMessage};
  #' can also read other variables if written in eyelink file
  #' returns epoched data with all trials features retrieved
  #' @param dat raw data read-in with read.asc from package \code{Eyelinker}
  #' @param segmentMessage string, message to be used for segmenting data into trials, used to set time point 0
  #' @param beforeEvent positive integer, number of samples to be included before \code{segmentMessage}
  #' @param afterEvent positive integer, number of samples to be included after \code{segmentMessage}
  #' @param variablesMessages vector of strings, contains messages for which values will be retrieved (assumes value one row below message)
  #' @param recode2Num vector of booleans, specify for each variable whether to recode or not
  #' @param variablesNames vector of strings, new variable names for extracted variables in output object; default is NULL, which means \code{variablesMessage} will be used as variable names
  #' @return data frame with epoched data
  #' @examples 
  #' dat <- read.asc("s_01.asc")
  #' data <- readTrials(dat,segmentMessage="StartMask",beforeEvent=1000,afterEvent=3000)
  #' (Consider letting messages for other data to-be-retrieved be specified in input??)

  ## Check whether variablesMessages, recode2Num, and variablesNames are of same length:
  if (!(is.null(variablesMessages))){
    if(length(variablesMessages)!=length(recode2Num)){
      stop("Arguments variablesMesssage and recode2Num are of different lengths")
    }
    if(is.null(variablesNames)){
      variablesNames <- variablesMessages
      cat("Argument variablesNames unspecified, will use variablesMessages as variable names\n")
    }
    if(length(variablesMessages)!=length(variablesNames)){
      stop("Arguments variablesMesssage and variablesNames are of different lengths")
    }
  } # end of if-clause if variablesMessages exist
  
  ## Extract raw data:
  raw <- dat$raw
  
  ## Determine time of start:
  msgIdx <- which(dat$msg$text == segmentMessage) # indices where message for segmentation presented
  msgTime <- dat$msg$time[msgIdx] # timings of all trials at those indices 
  nTrials <- length(msgTime) # number trials
  startTime <- msgTime - beforeEvent # start segmentation xx ms before message
  stopTime <- msgTime + afterEvent # stop segmentation xx ms after message

  ## Determine trial details:
  if (!(is.null(variablesMessages))){
    
    variablesList <- list() # initialize empty list
    nVariables <- length(variablesMessages) # length to loop through
    
    for (iVariable in 1:nVariables){
      
      ## Read variable values (one row below variable names appears):
      thisVariable <- dat$msg$text[which(dat$msg$text == variablesMessages[iVariable])+1]
      
      if (recode2Num[iVariable]==T){ # if input says convert to numeric: recode into numeric
        thisVariable <- as.numeric(levels(thisVariable))[thisVariable]
        # https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-integer-numeric-without-loss-of-information
      
      } else { # else: assume factor, only remove unused levels
        thisVariable <- droplevels(factor(thisVariable))
        
      } # end if-clause for recoding to numeric
      
      variablesList[[iVariable]] <- thisVariable # add to list
      
    } # end for-loop iVariable
  } # end if-clause variablesMessages are not empty
  
  ## Initialize data frame:
  trialList <- list()
  
  ## Loop over trials:
  for (iTrial in 1:nTrials){ # iTrial = 1
    cat(paste0("Read trial ",iTrial,"\n"))
    
    ## Retrieve start and stop time for this trial:
    startRow <- which(raw$time == startTime[iTrial])
    stopRow <- which(raw$time == stopTime[iTrial])
    
    ## Check whether computed rows exist, otherwise warn:
    if (length(startRow)==0){
      cat(paste0("Trial ",iTrial,": startRow undefined, consider shortening beforeEvent\n"))
    }
    if (length(stopRow)==0){
      cat(paste0("Trial ",iTrial,": stopRow undefined, consider shortening afterEvent\n"))
    }
    
    ## Extract eye-tracking measure for this trial:
    xp <- raw$xp[startRow:stopRow]
    yp <- raw$yp[startRow:stopRow]
    pupil <- raw$ps[startRow:stopRow]
    
    ## Trial number and timing:
    trialDuration <- length(pupil) # number of pupil samples determines trial duration
    trialnr <- rep(iTrial,trialDuration)
    absTime <- raw$time[startRow:stopRow] # absolute timing relative to start of eye-tracker
    trialTime <- seq(from=-1*beforeEvent,to=(trialDuration-beforeEvent-1),by=1) # relative time within trial

    ## Extract trial features from messages:
    if (!(is.null(variablesMessages))){
      
      allVariables <- as.data.frame(matrix(NA,trialDuration,nVariables)) # initialize empty data frame
      
      for (iVariable in 1:nVariables){
        variableAllValues <- variablesList[[iVariable]] # extract this variable
        variableValue <- variableAllValues[iTrial] # extract value of variable for this trial 
        allVariables[,iVariable] <- rep(variableValue,trialDuration) # repeat trial feature as often as samples within trial:
      } # end for-loop iVariable
      
      colnames(allVariables) <- variablesNames # add variable names given as inputs
      
      ## Concatenate eye-tracking variables and trial features (allVariables) into data frame:
      trialList[[iTrial]] <- as.data.frame(cbind(trialnr,absTime,trialTime,xp,yp,pupil,allVariables))

      } else {
      
      ## Concatenate into data frame without allVariables:
      trialList[[iTrial]] <- as.data.frame(cbind(trialnr,absTime,trialTime,xp,yp,pupil))
      
    } # end if-clause variablesMessages are not empty
  } # end for-loop iTrial
  
  ## Append data frames of all trials:
  data <- do.call(rbind, trialList)
  
  ## Warn about warning messages that can occur when converting factors to numeric:
  if (!(is.null(recode2Num)) & sum(recode2Num) > 0){ # if recode2Num defined at at least one True
    cat(paste0("Expect warning about 'NAs introduced by coercion' for every variable converted to numeric, i.e. ", sum(recode2Num), " warnings\n"))
  } # end if recode2Num > 0

  cat("Done :-)\n")
  return(data)
} # end function

# -------------------------------------------------------------------------------------------------- #
#### Recode factors to numeric ####

recode_behavioral_variables <- function(data){
  #' Recode factors into numeric
  #' @param data data frame, trial-epoched data with variable \code{variable}
  #' @return data data frame with recoded variables
  
  ## Define simple function:
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  
  ## Factor to numeric:
  data$reqAction <- as.numeric.factor(data$reqAction)
  data$rewLeft <- as.numeric.factor(data$rewLeft)
  data$reqSide <- as.numeric.factor(data$reqSide)
  data$angle <- as.numeric.factor(data$angle) # continuous points
  data$rewMag <- as.numeric.factor(data$rewMag) # continuous points
  data$punMag <- as.numeric.factor(data$punMag) # continuous points
  data$response <- as.numeric.factor(data$response)
  data$respSide <- as.numeric.factor(data$respSide)
  data$ACC <- as.numeric.factor(data$ACC)
  data$RT <- as.numeric.factor(data$RT) # will introduces NAs for NoGos
  data$outcome <- as.numeric.factor(data$outcome) # continuous points
  
  ## Factor with proper labels:
  # not necessary since will be aggregated anyway...?
  data$reqAction_f <- factor(ifelse(data$reqAction==1,"Go","NoGo"))
  data$reqSide_f <- factor(ifelse(data$reqSide==1,"Left","Right"))
  data$rewLeft_f <- factor(ifelse(data$rewLeft==1,"Left","Right"))
  data$response_f <- factor(ifelse(data$response==1,"Go","NoGo"))
  data$respSide_f <- factor(ifelse(data$respSide==1,"Left","Right"))
  data$ACC_f <- factor(ifelse(data$ACC==1,"Correct","Incorrect"))

  return(data)
}

# -------------------------------------------------------------------------------------------------- #
#### Determine amount of missingness ####

compare_data_sets <- function(data1,data2,varVec=NULL){

  #' Determine whether 2 data sets are identical with regard to selected variables
  #' @param data1 data frame, first data set, use only rows where data1 is not NA
  #' @param data2 data frame, second data set
  #' @param varVec vector of strings, variables to compare (default: all of data1)
  #' @return TRUE (all variables the same) or FALSE (at least one variable not the same)
  
  if(is.null(varVec)){
    cat("No selection of variables provided, use all variables of first data set\n")
    varVec <- names(data1)
  }
  
  cat("Variables data set 1:\n",names(data1),"\n")
  cat("Variables data set 2:\n",names(data2),"\n")
  
  allSame <- T
  
  for (iVar in varVec){ # iVar <- 1
    valIdx <- !(is.na(data1[,iVar]))
    mSame <- mean(data1[valIdx,iVar]==data2[valIdx,iVar])
    if (is.na(mSame)){
      cat("Variable",iVar,": NAs in data2 detected\n")
      allSame <- F
    } else if (mSame == 1){
      cat("Variable",iVar,": data sets correspond\n")
    } else {
      misMatchIdx <- which(data1[,iVar]!=data2[,iVar])
      cat("Variable",iVar,": data sets mismatch in rows",misMatchIdx,"\n")
      allSame <- F
    }
  }
  return(allSame)
}
  
  
# -------------------------------------------------------------------------------------------------- #
#### Determine amount of missingness ####

count_missingness <- function(data,variable){
  #' Determine how much (in absolute and relative terms) of variable \code{variable} is NA.
  #' @param data data frame, trial-epoched data with variable \code{variable}
  #' @param variable string argument, variable to be interpolated
  #' just print how much is missing, no object returned

  nNA <- sum(is.na(trialData$xp))
  nRow <- nrow(data)
  cat(paste0("Variable ",variable,": ",nNA," out of ",nRow," samples (",round(nNA/nRow,2),"%) missing\n"))
}

# -------------------------------------------------------------------------------------------------- #
#### Linearly interpolate NAs ####

applyInterpolation <- function(data,variable="xp",maxGap = NULL, maxKeepTrial = NULL, padEdges=FALSE, interactive=FALSE){
  #' Performs linear interpolation using na.approx from package zoo
  #' @param data data frame, trial-epoched data with variable \code{variable}
  #' @param variable string argument, variable to be interpolated
  #' @param maxGap numeric, maximum length of NA sequences to be interpolated (in samples)
  #' @param maxKeepTrial numeric, maximum number of NAs to still interpolate trial, otherwise set entire trial to NA (in samples or as percentage)
  #' @param padEdges boolean, fill leading and trailing NAs of trials with first/ valid non-NA
  #' @param interactive boolean, plot each trial before/ after interpolation (after padding edges) or not
  #' @return data frame, variable \code{variable} manipulated
  require(zoo) # for na.approx

  ## Defaults:
  timeVar <- "trialTime"
  trialVar <- "trialnr"
  nTrials <- length(unique(data[,trialVar]))
  
  NATrials <- 0
  
  ## Loop over trials:
  for (iTrial in 1:nTrials){ # iTrial <- 253
    cat(paste0("Trial ", iTrial,": Interpolate NAs\n"))
    rowIdx <- which(data[,trialVar]==iTrial)

    ## Trial length and number of missing samples (in samples):    
    nSamples <- length(data[rowIdx,variable]) # length of trial in samples
    nMissing <- sum(is.na(data[rowIdx,variable])) # number of missing samples in this trial
    
    ## Set maxKeepTrial and maxGap:
    if (is.null(maxKeepTrial)){ # if maxKeepTrial not set: set to total length of trial
      maxKeepTrial <- nSamples
    }
    if (maxKeepTrial < 1){ # if given as percentage:
      maxKeepTrial <- maxKeepTrial*nSamples
    }
    
    if (is.null(maxGap)){ # if maxGap not set: set to total length of trial
      maxGap <- nSamples
    }
    if (maxGap < 1){ # if given as percentage:
      maxGap <- maxGap*nSamples
    }
    
    ## Check if any data non-NA:
    if(nMissing >= maxKeepTrial){
      
      data[rowIdx,variable] <- NA # set entire trial to NA ()
      cat(paste0("Trial ", iTrial,": ",nMissing," out of ",nSamples," samples are NA, no interpolation\n"))
      NATrials <- NATrials + 1
      
    } else { # if any non-NA data points:
      
      ## Plot before interpolation:
      if (interactive==TRUE){
        plot(data[rowIdx,timeVar],data[rowIdx,variable], type = "l", 
             ylim = c(scrx/2-tolFix*plotFactor,scrx/2+tolFix*plotFactor),
             main = paste0("Trial ", iTrial, " - before interpolation"))
        readline(prompt="Press [enter] to continue")
      }
      
      # isNA <- is.na(data[rowIdx,variable])
      # which(isNA)
      # diff(which(isNA))
      
      ## Apply interpolation:
      data[rowIdx,variable] <- na.approx(data[rowIdx,variable], maxgap = maxGap, na.rm = FALSE) # replace NAs with interpolated value
      
      ## Plot after interpolation:
      if (interactive==TRUE){
        plot(data[rowIdx,timeVar],data[rowIdx,variable], type = "l", col = "green",
             ylim = c(scrx/2-tolFix*plotFactor,scrx/2+tolFix*plotFactor), 
             main = paste0("Trial ", iTrial, " - after interpolation"))
        readline(prompt="Press [enter] to continue")
      }
      
      ## Interpolate also from start and until end of window: Pad edges:
      if (padEdges == TRUE){
        data <- applyPadEdges(data,variable,rowIdx,iTrial,interactive)
      } # end if for padding edges
      
    } # end if loop
    
  } # end for-loop iTrial
  
  cat("Found",NATrials,"trials without any data\n")
  
  return(data)
}

# -------------------------------------------------------------------------------------------------- #
#### Interpolate leading and trailing NAs at beginning & end of trial ####

applyPadEdges <- function(data, variable="xp", rowIdx, iTrial, interactive=F){
  #' Replaces leading NAs by first non-NA value,
  #' Replaces trailing NAs by last non-NA value.
  #' @param data data frame, trial-epoched data with variable \code{variable}
  #' @param variable string argument, variable to be padded
  #' @param rowIdx vector of integers, vector of rows of trial to be padded
  #' @param iTrial integer, trial number (for title in plot)
  #' @param interactive boolean, plot trial time series after padding edges (in blue) or not
  #' @return no return, just plotting
  #' Should only be used if enough empty time before baseline/ after trial 
  require(zoo)
  cat(paste0("Trial ", iTrial,": Pad edges of variable ",variable,"\n"))
  
  ## First interpolate:
  data[rowIdx,variable] <- na.approx(data[rowIdx,variable], na.rm = FALSE) # replace NAs with interpoloated value

  ## Determine first and last sample:
  firstRowIdx <- head(rowIdx,1)
  lastRowIdx <- tail(rowIdx,1)
  
  ## Determine non-NA samples:
  filledIdx <- which(!(is.na(data[rowIdx,variable]))) # non-NAs
  firstfilled <- rowIdx[head(filledIdx,1)] # first non-NA
  lastfilled <- rowIdx[tail(filledIdx,1)] # last non-NA
  
  ## Replace leading NAs:
  if(firstfilled!=firstRowIdx){
    data[firstRowIdx:(firstfilled-1),variable] <- data[firstfilled,variable]
  }
  
  ## Replace trailing NAs:
  if(lastfilled!=lastRowIdx){
    data[(lastfilled+1):lastRowIdx,variable] <- data[lastfilled,variable]
  }
  
  ## Plot after removal of NAs:
  if (interactive==TRUE){
    plot(data[rowIdx,timeVar],data[rowIdx,variable], type = "l", col = "blue", 
         ylim = c(scrx/2-tolFix*plotFactor,scrx/2+tolFix*plotFactor),
         main = paste0("Trial ", iTrial, " - after padding edges"))
    readline(prompt="Press [enter] to continue")
  }
  return(data)
}

# -------------------------------------------------------------------------------------------------- #
#### Plot one coordinate over time with ROIs ####

plot_coordinate_ROI <- function(data, xVar = "trialTime", yVar = "xp",
                                leftROI,rightROI,radius){
  #' Line plot of variable (over time) per trial, highlight ROIs.
  #' @param data data frame, trial-epoched data with variable \code{variable} and 'trialnr'
  #' @param xVar variable on x-axis, should be e.g. time within trial
  #' @param yVar variable on y-axis, should be variable of interest
  #' @param leftROI relevant coordinate of center of left ROI
  #' @param rightROI relevant coordinate of center of right ROI
  #' @param radius radius of both ROIs
  #' No output, just plotting.
  
  ## Determines edges of ROIs:
  leftROImin <- leftROI - radius
  leftROImax <- leftROI + radius
  rightROImin <- rightROI - radius
  rightROImax <- rightROI + radius

  ## Defaults:
  timeVar <- xVar
  trialVar <- "trialnr"
  nTrial <- length(unique(data[,trialVar]))
  RtVar <- "RT"
  
  ## Determine average RT:
  avgRT <- mean(data[which(data[,timeVar] == 1),RtVar], na.rm = T) # retrieve only first sample, mean with NAs

  ## Loop over trials:
  for (iTrial in 1:nTrial){ # iTrial <- 1
    rowIdx <- which(data[,trialVar]==iTrial) # row indices of this trial
    
    ## Time variable:
    timeLine <- data[rowIdx,xVar] # extract time variable for this trial
    timeMin <- timeLine[1] # first entry
    timeMax <- timeLine[length(timeLine)] # last entry
    
    ## Check whether trial is Go trial:
    isGo <- data[rowIdx[1],"response"]

    ## Add RT:
    if (isGo == 1){ # If trial is Go trial: retrieve RT
      RT <- data[rowIdx[1],RtVar]
      RTcol <- "green" # green for Go
      # RTs recorded relevant to StartCueOutcome (when outcomes appear), so same timing as recoded trial time
    } else { # else if NoGo trial: take average RT
      RT <- avgRT
      RTcol <- "red" # red for NoGo
    }# end if
    
    ## Make line plot:
    plot(timeLine,data[rowIdx,yVar], type = "l", lwd = 2,
         cex.main = 2, cex.lab = 1.5, cex.axis = 1.25, 
         ylim = c(leftROImin,rightROImax),
         xlab = "Time (in ms)", ylab = "X coordinate (in pixels)", main = paste0("Trial ", iTrial))

    ## Vertical line for RT;
    abline(v=RT*1000,col=RTcol,lwd=2,lty=2)

    ## Make polygons for ROIs:
    xx <- c(timeLine[1],timeLine[length(timeLine)],timeLine[length(timeLine)],timeLine[1],timeLine[1])
    yy <- c(leftROImin,leftROImin,leftROImax,leftROImax,leftROImin) # left polygon
    polygon(xx,yy, density=0, col="#FF8000") # left ROI
    yy <- c(rightROImin,rightROImin,rightROImax,rightROImax,rightROImin) # right polygon
    polygon(xx,yy, density=0, col="#0000FF") # right ROI
    
    ## Press to continue:
    readline(prompt="Press [enter] to continue")
  }
}

# -------------------------------------------------------------------------------------------------- #
#### Plot x and y coordinates over time with ggplot ####

plot_xycoordinate_ROI <- function(data, xVar = "xp", yVar = "yp", tVar = "trialTime",
                                  centerX = 960, centerY = 540, angleVar = "angle",
                                  radius = 0.30, tolerance = 150){
  #' Line plot of variable (over time) per trial, highlight ROIs.
  #' @param data data frame, trial-epoched data with variable \code{xVar}, \code{yVar}, \code{tVar} and 'trialnr'
  #' @param xVar variable on x-axis, should be e.g. time within trial
  #' @param yVar variable on y-axis, should be variable of interest
  #' @param centerX numeric, x-coordinate of center of screen
  #' @param centerY numeric, y-coordinate of center of screen
  #' @param angleVar numeric, downwards displacement of ROI center from origin at 9 o'clock
  #' @param radius numeric, displacement of ROI center from center of screen
  #' @param tolerance numeric, tolerance around ROI center where image is rendered visible
  #' No output, just plotting.
  
  require(ggplot2)
  require(ggforce)
  require(gridExtra)
  require(viridis)
  
  ## Defaults settings:
  trialVar <- "trialnr"
  nTrial <- length(unique(data[,trialVar]))
  nRounding <- 3

  ## Names of additional added variables:
  leftROIXnorm <- "leftROIXnorm"
  leftROIYnorm <- "leftROIYnorm"
  leftROIXpix <- "leftROIXpix"
  leftROIYpix <- "leftROIYpix"
  rightROIXnorm <- "rightROIXnorm"
  rightROIYnorm <- "rightROIYnorm"
  rightROIXpix <- "rightROIXpix"
  rightROIYpix <- "rightROIYpix"
  radAngle <- "radAngle"
  
  ## Convert angle to radians:
  data[,radAngle] <- data[,angleVar] * pi/180
  
  ## Compute ROIs relative to screen borders (-1 to 1):
  data[,leftROIXnorm] <- -1*round(cos(data[,radAngle])*radius,nRounding) # left side: negative
  data[,leftROIYnorm] <- round(sin(data[,radAngle])*radius,nRounding) 
  data[,rightROIXnorm] <- round(cos(data[,radAngle])*radius,nRounding) # right side: positive
  data[,rightROIYnorm] <- round(sin(data[,radAngle])*radius,nRounding) 
  
  ## Convert to pixels on screen:
  data[,leftROIXpix] <- centerX + centerX*data[,leftROIXnorm] #  
  data[,leftROIYpix] <- centerY - centerY*data[,leftROIYnorm] # downwards
  data[,rightROIXpix] <- centerX + centerX*data[,rightROIXnorm] #  
  data[,rightROIYpix] <- centerY - centerY*data[,rightROIYnorm] # downwards
  
  ## Loop over trials:
  for (iTrial in 1:nTrial){ # iTrial <- 1
    
    ## Extract data for this trial:
    thisdata <- data[which(data[,trialVar]==iTrial),] # data for this trial
    
    ## Rename variables for easy use in ggplot:
    thisdata$xVar <- thisdata[,xVar]
    thisdata$yVar <- thisdata[,yVar]
    thisdata$tVar <- thisdata[,tVar]
    
    ## Data of ROI specifications:
    circles <- data.frame(
      x0 = c(thisdata[1,leftROIXpix],thisdata[1,rightROIXpix]),
      y0 = c(thisdata[1,leftROIYpix],thisdata[1,rightROIYpix]),
      r = c(tolerance,tolerance)
    )
    
    ## Plot with ggplot:
    p1 <- ggplot() + 
            geom_point(data=thisdata,aes(x=xVar,y=yVar,col=tVar)) + # time course
            geom_point(aes(x=centerX, y=centerY), shape = 10, size = 5) + # middle of screen
            geom_circle(aes(x0 = x0, y0 = y0, r = r), data = circles) + # ROIs
            # xlim(0,scrx) + ylim(0,scry) + 
            xlim(0,centerX*2) + ylim(0,centerY*2) + # axes
            ggtitle(paste0("Trial ",iTrial,", angle = ",thisdata[1,angleVar])) +  # title
            scale_colour_gradientn(colors=viridis(10)) +  # color scale for time domain
            coord_fixed() + # make rectangular so circles are round
            theme_classic() # remove background
    print(p1)
    # p2 <- ggplot() +
    #   geom_point(data=thisdata,aes(x=tVar,y=xVar)) + # time course
    #   ylim(xROILeft-radius,xROIRight+radius) + # axes
    #   theme_bw() # remove background
    # p3 <- ggplot() +
    #   geom_point(data=thisdata,aes(x=tVar,y=yVar)) + # time course
    #   ylim(yROILeft-radius,yROIRight+radius) + # axes
    #   theme_bw() # remove background
    # print(grid.arrange(p1, p2, p3, nrow = 3))
    readline(prompt="Press [enter] to continue")
  }
}

# -------------------------------------------------------------------------------------------------- #
#### Compute index of whether gaze is within ROI or not ####

compute_in_ROI <- function(data, xVar = "xp", yVar = "yp", suffix = NULL, 
                           centerX = 1920/2, centerY=1080/2, angleVar = "angle", radius = 0.30, tolerance = 150){
  #' Specify ROI (center point and radius),
  #' indicate whether sample is in ROI (1) or not (0).
  #' @param data data frame, trial-epoched data with variable \code{variable}
  #' @param xVar string argument, x-coordinate to be compared with ROI
  #' @param yVar string argument, y-coordinate to be compared with ROI
  #' @param centerX numeric, x-coordinate of center of screen
  #' @param centerY numeric, y-coordinate of center of screen
  #' @param angleVar numeric, downwards displacement of ROI center from origin at 9 o'clock
  #' @param radius numeric, displacement of ROI center from center of screen
  #' @param tolerance numeric, tolerance around ROI center where image is rendered visible
  #' @return data with newVariable indicate whether variable is in ROI (1) or not (0)

  ## Save names of input variables:
  allVars <- names(data)
  
  ## Options:
  nRounding <- 3
  
  ## If suffix for new variable names not defined:
  if(is.null(suffix)){
    suffix <- "ROI"
    cat(paste0("Argument suffix unspecified, use _ROI\n"))
  }
  
  ## Determine names of new variables:
  newDist <- paste0("dist_",suffix)
  newFix <- paste0("fix_",suffix)
  
  ## Names of additional added variables:
  radAngle <- "radAngle"
  ROIXnorm <- "ROIXnorm"
  ROIYnorm <- "ROIYnorm"
  ROIXpix <- "ROIXpix"
  ROIYpix <- "ROIYpix"

  ## Convert angle to radians:
  data[,radAngle] <- data[,angleVar] * pi/180
  
  ## Compute ROIs relative to screen borders (-1 to 1):
  data[,ROIXnorm] <- round(cos(data[,radAngle])*radius,nRounding) # left side: negative
  data[,ROIYnorm] <- round(sin(data[,radAngle])*radius,nRounding) 
  
  ## Convert to pixels on screen:
  data[,ROIXpix] <- centerX - centerX*data[,ROIXnorm] # leftwards
  data[,ROIYpix] <- centerY - centerY*data[,ROIYnorm] # downwards

  ## Distance from ROI via Pythagoras:
  data[,newDist] <- sqrt((data[,xVar] - data[,ROIXpix])^2 + (data[,yVar] - data[,ROIYpix])^2)

  ## Indicate whether fixation in ROI or not:
  data[,newFix] <- ifelse(data[,newDist] <= tolerance,1,0)
  
  ## Output statistics:
  inROI <- sum(data[,newFix]==1,na.rm=T)
  nSamples <- nrow(data)
  cat(paste0("Found ",inROI, " out of ",nSamples," samples (",round(inROI/nSamples,2)*100,"%) to be in ROI\n"))
  
  cat("Done :-)\n")
  data <- data[,c(allVars,newDist,newFix)]
  return(data)
}

# -------------------------------------------------------------------------------------------------- #
#### Align sign of trial data so first fixation is always upwards ####

rectify_firstfix <- function(data, variable, ROI1, ROI2, varMean=NULL){
  #' Change sign of variable \code{variable} for each trial such that first fixation is always upwards.
  #' @param data data frame, trial-epoched data 
  #' @param variable string, variable for which to flip sign
  #' @param ROI1 string, variable indicating whether \code{variable} in first ROI 
  #' @param ROI2 string, variable indicating whether \code{variable} in second ROI 
  #' @param varMean vector of numerics, mean of each variable in \code{variable} used for demeaning
  #' @return data re-epoched to RT
  
  ## Defaults:
  trialVar <- "trialnr"
  nTrial <- length(unique(data[,trialVar]))

  ## Detect varMean given, otherwise compute mean for each variable:
  if (is.null(varMean)){
    cat("Found no given variable means for demeaning, compute empirically\n")
    varMean <- mean(data[,variable],na.rm = T)
  }
  
  ## First demean variables completely:
  cat(paste0("Demean variable"))
  data[,variable] <- data[,variable] - varMean
  
  ### Loop through trials:
  for (iTrial in 1:nTrial){ # iTrial <- 1

    rowIdx <- which(data[,trialVar]==iTrial) # rows in this trial
    
    ## Check if any fixation in ROI: 
    ROI1idx <- which(data[rowIdx,ROI1]==1) # samples in ROI1
    ROI2idx <- which(data[rowIdx,ROI2]==1) # samples in ROI2
    # (will be NA if no fixations)
    
    ## Determine first sample in each ROI:
    minROI1 <- ROI1idx[1] # first sample in ROI1
    minROI2 <- ROI2idx[1] # first sample in ROI2
    # (will be NA if no fixation)
    
    ## If none of them NA:
    if (!is.na(minROI1) & !is.na(minROI2)){
      ## If left (bottom) before right (up):
      if ((minROI1 < minROI2)){ # if first left, before right
        cat(paste0("For trial ",iTrial,": Rectify data\n"))
        data[rowIdx,variable] <- data[rowIdx,variable] * -1 # flip
      }
    }
    
    ## If left (bottom), but no right (up)
    if (!is.na(minROI1) & is.na(minROI2)){ # if first left, before right
      cat(paste0("For trial ",iTrial,": Rectify data\n"))
      data[rowIdx,variable] <- data[rowIdx,variable] * -1 # flip
    }
  }
  
  ## Add mean back in:
  cat(paste0("Add mean in again\n"))
  data[,variable] <- data[,variable] + varMean
  
  return(data)
}

# -------------------------------------------------------------------------------------------------- #
#### Identify which samples are before response: ####

mark_before_RT <- function(data, tolerance = 0){
  #' Mark for trials with Go responses whether sample occurrred \code{tolerance}
  #' seconds before response (1) or not (0). NA for trials with NoGo responses.
  #' @param data data frame, trial-epoched data 
  #' @param tolerance numeric, how many milliseconds before RT to stop (default: 0)
  #' @return data with new variable "isBeforeRT" indicating whether sample is before RT or not
  
  require(stringr)
  
  ## Defaults:
  timeVar <- "trialTime"
  trialVar <- "trialnr"
  RtVar <- "RT"
  nTrial <- length(unique(data[,trialVar]))
  
  newVarName <- paste0("isBeforeRT_tol",str_pad(tolerance, 3, pad = "0"))
  
  ## Initialize variable:
  data[,newVarName] <- NA
  
  ## Determine average RT:
  avgRT <- mean(data[which(data[,timeVar] == 1),RtVar], na.rm = T) # retrieve only first sample, mean with NAs
  
  ## Loop over trials:
  for (iTrial in 1:nTrial){ # iTrial <- 1
    
    cat(paste0("Trial ", iTrial,": Mark samples before (RT - ",tolerance," ms)\n"))
    
    # Retrieve row indices of trial:
    rowIdx <- which(data[,trialVar]==iTrial) # row indices of this trial
    # length(rowIdx)

    # --------------------------------------------------------------- #
    ### Retrieve RT:
    
    ## Check whether trial is Go trial:
    isGo <- data[rowIdx[1],"response"]

    if (isGo == 1){ # If trial is Go trial: retrieve RT
      RT <- data[rowIdx[1],RtVar]
      # RTs recorded relevant to StartCueOutcome (when outcomes appear), so same timing as recoded trial time
    } else { # else if NoGo trial: take average RT
      RT <- avgRT
    }# end if

    ## Detect samples before RT:
    idx <- which(data[rowIdx,timeVar]*.001 <= RT - tolerance*.001) # indices relative to trial onset
    beforeRTidx <- rowIdx[idx] # retrieve indices absolute to entire data frame
    data[beforeRTidx,newVarName] <- 1
    
    ## Detect samples after RT:
    idx <- which(!(rowIdx %in% beforeRTidx))
    afterRTidx <- rowIdx[idx] # retrieve indices absolute to entire data frame
    # length(afterRTidx)
    data[afterRTidx,newVarName] <- 0

  } # end iTrial
  
  ## Statistics:
  cat("After classifying samples into before/after RT:\n")
  cat(table(data[,newVarName]))
  
  return(data)
}

# -------------------------------------------------------------------------------------------------- #
#### Plot average time course per condition: ####

plot_avg_cond <- function(data1, data2, yVar="xp", timeVar="trialTime", 
                          yLabel = "X coordinate (in pixels)", mainLabel = "condition",
                          colorVec=c("green","red"), legendVec=c("Cond 1","Cond 2"),
                          isSave=FALSE, splitType="condition",suffix=""){
  #' Aggregates time course within a trial separately for two conditions, plots them
  #' @param data1 data frame, trial-epoched data, first condition 
  #' @param data2 data frame, trial-epoched data, second condition 
  #' @param yVar string, dependent variable to plot (default: "xp")
  #' @param timeVar string, variable containing time within trial (default: "trialTime")
  #' @param yLabel string, y-axis label (default: "X coordinate (in pixels)")
  #' @param mainLabel string, input for caption specifying conditions (default: "condition")
  #' @param colorVec vector of 2 strings, containing colors to use (default: green and red)
  #' @param legendVec vector of 2 strings, condition names for legend (default: cond1 and cond2) 
  #' @return data with new variable "isBeforeRT" indicating whether sample is before RT or not
  
  ## Assume that plotDir yLim and and subID are set globally
  
  ## Defaults:
  RtVar <- "RT"
  
  ## Time variable:
  xVar <- unique(data1[,timeVar])
  
  ## Aggregate relevant variable across trials:
  yVar1 <- tapply(data1[,yVar],data1[,timeVar],mean,na.rm=T) # average per sample within trial
  yVar2 <- tapply(data2[,yVar],data2[,timeVar],mean,na.rm=T) # average per sample within trial
  
  ## Start saving:
  if(isSave==T){
    png(paste0(plotDir,"MGNGFreeView_xp_trial_",splitType,suffix,"_sub",str_pad(subID,2,"left","0"),".png"),width = 480, height = 480)
  }
  
  ## Plot average time courses:
  plot(xVar,yVar1,"l",col=colorVec[1],lwd = 2,
       cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.25, 
       xlab = "Time (in ms)", ylab = yLabel, ylim = yLim,
       main = paste0("Sub ",subID,": Average per ",mainLabel))
  lines(xVar,yVar2,"l",col=colorVec[2], lwd = 2)
  
  ## Plot middle of screen:
  abline(h=scrx/2,lwd=1.5,lty=2)

  ## Add legend:
  legend("topleft", legend=legendVec,
         col=colorVec, lty=1, cex=1)
  
  ## End saving:
  if(isSave==T){
    dev.off()
  }
  
}

# -------------------------------------------------------------------------------------------------- #
#### Identify which samples are before response: ####

reepoch_resplocked <- function(data, variables, startTime = -500, stopTime = 500){
  #' Mark for trials with Go responses whether sample occurrred \code{tolerance}
  #' seconds before response (1) or not (0). NA for trials with NoGo responses.
  #' @param data data frame, trial-epoched data 
  #' @param variables vector of string, variables to change (other variables will keep value at timeVar=)
  #' @param startTime numeric, how many ms before RT to begin reepoched trials (fill in NA if necessary) 
  #' @param endTime numeric, how many ms after RT to end reepoched trials  (fill in NA if necessary)
  #' @return newData re-epoched to RT
  
  ## Defaults:
  timeVar <- "trialTime"
  maxDur <- max(data[,timeVar])
  trialVar <- "trialnr"
  nTrial <- length(unique(data[,trialVar]))
  RtVar <- "RT"
  nVar <- length(variables)
  trialDuration <- stopTime - startTime
  
  ## Determine other(variables where to just keep value at first sample):
  allVariables <- names(data)
  otherVariables <- allVariables[!(allVariables %in% variables)]
  
  ## Determine average RT:
  avgRT <- mean(data[which(data[,timeVar] == 1),RtVar], na.rm = T) # retrieve only first sample, mean with NAs
  
  ## New trial time:
  trialTime <- startTime:(stopTime-1) # account for crossing 0
  
  ## Initialize new data frame:
  trialList <- list()
  
  # ----------------------------------------------------------------- #
  ## Loop over trials:
  for (iTrial in 1:nTrial){ # iTrial <- 209
    cat(paste0("Trial ", iTrial,": Relock to RT\n"))
    
    ## Retrieve row indices of trial:
    rowIdx <- which(data[,trialVar]==iTrial) # row indices of this trial

    # --------------------------------------------------------------- #
    ### Retrieve RT:
    
    ## Check whether trial is Go trial:
    isGo <- data[rowIdx[1],"response"]
    
    if (isGo == 1){ # If trial is Go trial: retrieve RT
      RT <- data[rowIdx[1],RtVar]
      ## RTs recorded relevant to StartCueOutcome (when outcomes appear), so same timing as recoded trial time
    } else { # else if NoGo trial: take average RT
      RT <- avgRT
    }# end if
    
    RT <- round(RT,3) # round to ms precision
    
    # --------------------------------------------------------------- #
    ### Determine when trial starts/ stops in old and new data:

    ## Samples to extract from old data:
    startSample <- RT*1000+startTime # where to start this re-epoched trial
    stopSample <- RT*1000+stopTime-1 # where to stop this re-epoched trial
    # make stopSample one sample shorter become 0 included
    
    ## Samples to fill in in new data:
    newStartSample <- 1
    newStopSample <- trialDuration
    
    ## Correct if necessary:
    if (startSample < 1){
      newStartSample <- newStartSample - (startSample-1)  # set new start sample further forward
      startSample <- 1 # reset old start sample to minimally zero
      cat(paste0("Old start sample of ",startSample," too early, reset to 1 \n"))
    }
    if (stopSample > maxDur){
      cat("Old start sample too late, reset \n")
      newStopSample <- newStopSample - (stopSample-maxDur) # set new start sample further back
      stopSample <- maxDur # reset old start sample to minimally zero
      cat(paste0("Old stop sample of ",stopSample," too late, reset to",maxDur,"\n"))
    }
    
    # --------------------------------------------------------------- #
    ## Extract selected variables in selected time frame:
    varFrame <- data.frame(matrix(nrow = trialDuration, ncol = nVar)) # initialize empty data frame
    
    for(iVar in 1:nVar){ # iVar <- 1
      varName <- variables[iVar] # retrieve variable name
      varFrame[newStartSample:newStopSample,iVar]  <- data[rowIdx[startSample:stopSample],varName]
    }
    
    # --------------------------------------------------------------- #
    ## Extract only first sample for other variables:

    otherValues <- data[rowIdx[1],otherVariables]
    
    # --------------------------------------------------------------- #
    ## Put into one data frame per trial in list:
    
    trialList[[iTrial]] <- as.data.frame(cbind(iTrial,trialTime,varFrame,otherValues))
    
  }
  
  ## Append data frames of all trials:
  newData <- do.call(rbind, trialList)
  
  ## Add names back in:
  names(newData) <- c(trialVar,timeVar,variables,otherVariables)
  
  return(newData)
}

# -------------------------------------------------------------------------------------------------- #
#### Identify adjacent fixations within ROI as clusters, give index ####

identify_fixations <- function(data, variables, interactive = F){
  
  #' Loop through trials, identify clusters of adjacent fixations within an ROI, give indices to clusters.
  #' @param data data frame, trial-epoched data with variables \code{variables}
  #' @param variables vector of variable names for which to identify clusters (each variable needs to have values 1 and 0)
  #' @param interactive whether to print specifics on fixation to console or not
  #' @return data with new variable "_fixIdx" for each input variable indicating index of each cluster
  
  ## Defaults:
  nVariables <- length(variables) # length to loop through
  trialVar <- "trialnr"
  nTrials <- length(unique(data[,trialVar]))
  
  for (iVariable in 1:nVariables){ # for each variable
    
    thisVariable <- variables[iVariable] # retrieve variable
    cat(paste0("Start for variable ",thisVariable),"\n")
      
    newVariable <- paste0(thisVariable,"_fixIdx") # create name for fixation index
    data[,newVariable] <- NA # initialize new variable
    
    for (iTrial in 1:nTrials){ # iTrial <- 1
      cat(paste0("Trial ", iTrial,": Identify fixations\n"))
      rowIdx <- which(data[,trialVar]==iTrial)
      nSamples <- length(rowIdx)
      
      ## Initialize:
      sampleCount <- 0
      fixCount <- 0
      memory <- 0

      ## Loop through samples:
      for (iSample in 1:nSamples){
        sampleIdx <- rowIdx[iSample]

        ## Retrieve whether fixation or not:
        sampleIsFix <- data[sampleIdx,thisVariable] # retrieve whether fixation or not
        
        ## Increment fixation count at start of new fixation:
        if(sampleIsFix==1 & memory==0){
          fixCount <- fixCount + 1
        }

        ## Add fixation index to newVariable:
        data[sampleIdx,newVariable] <- ifelse(sampleIsFix==1,fixCount,NA)

        ## Print end of fixation:
        if(sampleIsFix==0 & memory==1 & interactive==TRUE){
          cat(paste0("Trial ",iTrial, ": Finished fixation no. ", fixCount, " with ", length(which(data[rowIdx,newVariable]==fixCount)), " samples\n"))
        }

        ## Update memory:
        memory <- sampleIsFix
        
      } # end iSample 
      if (interactive==TRUE){
        cat(paste0("Trial ",iTrial, ": Detected ", fixCount, " fixations\n"))
      }

    } # end iTrial
    
  } # end iVariable
    
  return(data)
}
  
# -------------------------------------------------------------------------------------------------- #
#### Discard too short fixations: ####

remove_short_fixations <- function(data, isFixVar, numFixVar, threshold){
  #' Loop through trials, remove identified clusters shorter than threshold.
  #' @param data data frame, trial-epoched data with variables \code{isFixVar} and \code{numFixVar}
  #' @param isFixVar contains indicator whether fixation is within ROI (1) or not (0)
  #' @param numFixVar contains indices attached to each cluster of adjacent fixations of ROI
  #' @param threshold threshold duration below which fixations of clusters should be deleted in \code{isFixVar} 
  #' @return data where \code{ixFixVar} is cleaned for too short fixations
  
  ## If name for numFixVar not defined:
  if(is.null(numFixVar)){
    numFixVar <- paste0(numFixVar,"_fixIdx")
    cat(paste0("Argument numFixVar unspecified, now called ",numFixVar,"\n"))
  }
  
  ## Defaults:
  trialVar <- "trialnr"
  nTrials <- length(unique(data[,trialVar])) # number trials
  
  ## Loop through trials: 
  for (iTrial in 1:nTrials){ # iTrial <- 1
    
    cat(paste0("Start trial ",iTrial,"\n"))
    
    rowIdx <- which(data[,trialVar]==iTrial) # row indices of trial 
    nFix <- max(data[rowIdx,numFixVar], na.rm = T) # number fixations of trial
    if(nFix==-Inf){nFix <- 0}

    ## Loop through fixations:
    if (nFix > 0){
      
      for (iFix in 1:nFix){ # loop through fixations
        
        fixIdx <- which(data[,trialVar]==iTrial & data[,numFixVar]==iFix) # row indices of fixation
        durFix <- length(fixIdx) # fixation duration
        # cat(paste0("Fixation ",iFix,": Duration is ",durFix,"\n"))
        
        if (durFix < threshold){ # if shorter than tolerance
          data[fixIdx,isFixVar] <- 0 # set status of fixation back to zero
          data[fixIdx,numFixVar] <- NA # delete cluster index
          cat(paste0("Delete fixation ",iFix,": Only ",durFix," samples\n"))
        }
      }
    } # end iFix
  } # end iTrial
  
  return(data)
}

# ===================================================================================== #
#### Recode fixations from left/right to reward/punishment: ####

recode_fixation_valence <- function(data){
  #' Compute whether fixation is on reward/ punishments based on stakes mapping and fix_left/fix_right
  #' @param data data frame, trial-epoched data
  #' @return same data set plus variables \code{fix_rew} and \code{fix_pun}

  cat("Recode fixations left/right into fixations reward/punishment \n")

  trialVar <- "trialnr"
  
  ## Initialize variables:
  data$fix_rew <- NA
  data$fix_pun <- NA
  
  nTrial <- length(unique(data$trialnr))
  
  for (iTrial in 1:nTrial){ # iTrial <- 1
    
    cat(paste0("Start trial ",iTrial,"\n"))
    rowIdx <- which(data[,trialVar]==iTrial) # row indices of trial 
    rowIdx1 <- rowIdx[1]
    yLeft <- data$fix_left[rowIdx]
    yRight <- data$fix_right[rowIdx]
    data$fix_rew[rowIdx] <- ifelse(data$rewLeft[rowIdx1]==1,yLeft,yRight)
    data$fix_pun[rowIdx] <- ifelse(data$rewLeft[rowIdx1]==0,yLeft,yRight)
    
  } # end iTrial
  
  return(data)
  
}
  
# ===================================================================================== #
#### Aggregate within trials: ####

aggregate_within_trials <- function(data){
  #' Aggregate within each trial, keep behavioral data variables, 
  #' compute first fixation (left/ right and reward/ punishment),
  #' compute absolute and relative dwell time (left/ right and reward/ punishment).
  #' @param data data frame, trial-epoched data
  #' @return aggregated data: one row for each trial 
  
  aggrData <- ddply(data, .(trialnr), function(x){

    # -------------------------------------------------------------- #
    ## Task and behavioral variables:
    
    subject <- subID # needs to be given
    trialnr <- x$trialnr[1]
    cat(paste0("Aggregate trialnr ",trialnr,"\n"))
    cue <- x$cue[1]
    reqAction <- x$reqAction[1]
    reqSide <- x$reqSide[1]
    rewLeft <- x$rewLeft[1]
    rewMag <- x$rewMag[1]
    punMag <- x$punMag[1]
    response <- x$response[1]
    respSide <- x$respSide[1]
    ACC <- x$ACC[1]
    RT <- x$RT[1]
    outcome <- x$outcome[1]
    
    # -------------------------------------------------------------- #
    ## First fixation:
    
    # Retrieve index of first sample with fixation on ROI:
    left_vec <- which(x$fix_left==1)
    right_vec <- which(x$fix_right==1)
    if (length(left_vec)==0){left_vec <- nrow(x)} # if not defined: set to last sample in trial
    if (length(right_vec)==0){right_vec <- nrow(x)} # if not defined: set to last sample in trial
    
    # First fixation left/ right:
    firstfix_side_f <- factor(ifelse(left_vec[1] < right_vec[1],"left", # if left first
                                     ifelse(left_vec[1] > right_vec[1],"right", # if right first
                                            NA))) # incomplete
    # Recode into reward/punishment:
    firstfix_out_f <- factor(ifelse(firstfix_side_f=="left" & rewLeft==1,"reward",
                                    ifelse(firstfix_side_f=="right" & rewLeft==1,"punishment",
                                           ifelse(firstfix_side_f=="left" & rewLeft==0,"punishment",
                                                  ifelse(firstfix_side_f=="right" & rewLeft==0,"reward",
                                                         NA)))))
    
    # -------------------------------------------------------------- #
    ## Dwell time left/ right absolute:
    dwell_left_abs <- sum(x$fix_left,na.rm = T)
    dwell_right_abs <- sum(x$fix_right,na.rm = T)
    
    # Relative to view on any ROI:
    dwell_left_rel <- dwell_left_abs/(dwell_left_abs+dwell_right_abs)
    dwell_right_rel <- dwell_right_abs/(dwell_left_abs+dwell_right_abs)

    ## Recode abs to reward/punishment:
    dwell_rew_abs <- ifelse(rewLeft==1,dwell_left_abs,dwell_right_abs)
    dwell_pun_abs <- ifelse(rewLeft==1,dwell_right_abs,dwell_left_abs)

    # Relative to view on any ROI:
    dwell_rew_rel <- dwell_rew_abs/(dwell_rew_abs+dwell_pun_abs)
    dwell_pun_rel <- dwell_pun_abs/(dwell_rew_abs+dwell_pun_abs)
    
    # -------------------------------------------------------------- #
    return(data.frame(subject,trialnr,
                      cue,reqAction,reqSide,rewLeft,rewMag,punMag,
                      response,respSide,ACC,RT,outcome,
                      firstfix_side_f,firstfix_out_f,
                      dwell_left_abs,dwell_right_abs,dwell_left_rel,dwell_right_rel,
                      dwell_rew_abs,dwell_pun_abs,dwell_rew_rel,dwell_pun_rel))
    dev.off()})
  
  ## Factor with proper labels:
  aggrData$reqAction_f <- factor(ifelse(aggrData$reqAction==1,"Go","NoGo"))
  aggrData$reqSide_f <- factor(ifelse(aggrData$reqSide==1,"Go","NoGo"))
  aggrData$rewLeft_f <- factor(ifelse(aggrData$rewLeft==1,"Left","Right"))
  aggrData$response_f <- factor(ifelse(aggrData$response==1,"Go","NoGo"))
  aggrData$respSide_f <- factor(ifelse(aggrData$respSide==1,"Left","Right"))
  aggrData$ACC_f <- factor(ifelse(aggrData$ACC==1,"Correct","Incorrect"))

  return(aggrData)
}

# END