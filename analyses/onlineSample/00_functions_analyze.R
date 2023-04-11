#### 00_functions_analyze.R ####

#' Collection of functions to analyze and plot behavioral data from the online sample.

#------------------------------------------------------------------------------------------------------------------------
#### Create subject variable: ####

create_sub_ID <- function(data, idvar = "Participant.Public.ID"){
  #' Add numeric subject ID from 1:nSub and identifier for each subject for each trial.reshape_questionnaire
  #' @param data    data frame with columns idvar (default: \code{Participant.Public.ID}) and \code{Trial.Number}.
  #' @return data   same data frame with new variables \code{subject} and \code{Sub_Trialnr} 
  
  ## Identify unique participant identifiers:
  data <- droplevels(data[!is.na(data[,idvar]),]) # delete if any is NA
  idvar_unique <- sort(unique(factor(data[,idvar]))) # unique identities, sort chronologically
  idvar_unique <- idvar_unique[which(idvar_unique!="")] # remove empty
  
  ## Create new variable:
  cat("Add new subject variable \"subject\"\n")
  data$subject <- NA
  for (iSub in 1:length(idvar_unique)){ # recode to numbers from 1-nSub
    data$subject[which(data[,idvar]==idvar_unique[iSub])] <- iSub
  }
  
  ## Create identifier for each trial for each subject:
  cat("Add new variable \"Sub_Trialnr\"\n")
  data$Sub_Trialnr <- paste(data$subject, data$Trial.Number, sep = "_")
  return(data)
}

#------------------------------------------------------------------------------------------------------------------------
#### Determine outcome based on response, rewarded action, and validity: ####

determine_outcome <- function(data){
  #' Determine exact outcome participants received.
  #' @param data    data frame with columns \code{Response}, \code{ReqAction}, \code{Validity}, \code{RewardNum}, \code{PunishmentNum}
  #' @return data   same data frame with new variable \code{Outcome}.
  
  cat("Compute variable \"Outcome\" based on Response, RewAction, Validity\n")
  data$Outcome <- NA
  for (iRow in 1:nrow(data)){
    data$Outcome[iRow] <- ifelse(data$Response[iRow]=="Go" & data$ReqAction[iRow]=="Go" & data$Validity[iRow]=="Valid", data$RewardNum[iRow],data$Outcome[iRow])
    data$Outcome[iRow] <- ifelse(data$Response[iRow]=="Go" & data$ReqAction[iRow]=="Go" & data$Validity[iRow]=="Invalid", data$PunishmentNum[iRow]*-1,data$Outcome[iRow])
    data$Outcome[iRow] <- ifelse(data$Response[iRow]=="Go" & data$ReqAction[iRow]=="NoGo" & data$Validity[iRow]=="Valid", data$PunishmentNum[iRow]*-1,data$Outcome[iRow])
    data$Outcome[iRow] <- ifelse(data$Response[iRow]=="Go" & data$ReqAction[iRow]=="NoGo" & data$Validity[iRow]=="Invalid", data$RewardNum[iRow],data$Outcome[iRow])
    data$Outcome[iRow] <- ifelse(data$Response[iRow]=="NoGo" & data$ReqAction[iRow]=="Go" & data$Validity[iRow]=="Valid", data$PunishmentNum[iRow]*-1,data$Outcome[iRow])
    data$Outcome[iRow] <- ifelse(data$Response[iRow]=="NoGo" & data$ReqAction[iRow]=="Go" & data$Validity[iRow]=="Invalid", data$RewardNum[iRow],data$Outcome[iRow])
    data$Outcome[iRow] <- ifelse(data$Response[iRow]=="NoGo" & data$ReqAction[iRow]=="NoGo" & data$Validity[iRow]=="Valid", data$RewardNum[iRow],data$Outcome[iRow])
    data$Outcome[iRow] <- ifelse(data$Response[iRow]=="NoGo" & data$ReqAction[iRow]=="NoGo" & data$Validity[iRow]=="Invalid", data$PunishmentNum[iRow]*-1,data$Outcome[iRow])
  } 
  return(data)
}

# ================================================================================================================================================ #
#### Extract and prepare Go/NoGo task data: ####

preprocess_task_data <- function(data, variables=c(), idvar = "Participant.Private.ID"){
  #' Preprocess Go/NoGo task data:
  #' - find rows reflecting Go/NoGo trials, 
  #' - identify subjects,
  #' - select specified variables,
  #' - recover where rewards/ punishments were presented,
  #' - recover on which side cue was presented,
  #' - delete RTs for NoGo responses,
  #' - delete too short RTs,
  #' - recover outcome received,
  #' - recover difference between potential outcomes,
  #' - recover instructions, response, accuracy,
  #' - compute compute trial number per block,
  #' - compute action cue identity, 
  #' - recode variables to (ordered) factors, standardize numeric variables.
  #' @param data data frame, data frame with raw data of all subjects concatenated. 
  #' @param variables vector of strings, names of variables to keep (default: empty).
  #' @param idvar string, variable to identify subject IDs (default: "Participant.Private.ID").
  #' @return data data frame with only catch trials selected, recoded variables.
  
  require(dplyr)
  
  # ------------------------------------------- # 
  ### 1) Find columns with display = "trial", type = "test" and response:
  
  cat("Select rows with display \"Trial\", type \"Test\", Response \"Go\" or \"NoGo\"\n")
  # table(data$display)
  # table(data$type)
  # table(data$Response)
  # levels(as.factor(data$Response))
  outputData <- subset(data, display=="Trial" & type == "Test"  & Response %in% c("Go","NoGo"))

  # ------------------------------------------- # 
  ### 2) Identify subjects:
  
  outputData <- create_sub_ID(outputData,idvar = "Participant.Private.ID")

  # ------------------------------------------- # 
  ### 3) Select specified variables:
  
  if (length(variables)!=0){
    cat("Only keep specified variables\n")
    selVariables <- variables[variables %in% colnames(outputData)]
    outputData <-outputData[,c("subject","Sub_Trialnr",selVariables)]
  }
  
  # ------------------------------------------- # 
  ### 4) Outcome presentation (reward left or right):
  
  cat("Add variable \"RewardPosition\" (reward presented on left or right side)\n")
  outputData$RewardPosition <- NA
  for (iRow in 1:nrow(outputData)){
    outputData$RewardPosition[iRow] <- ifelse(grepl("m",outputData$LeftOutcome[iRow]),"Right","Left")
  }
  # outputData[,c("LeftOutcome","RightOutcome","RewardPosition")]

  # ------------------------------------------- # 
  ### 5) Add whether cue was on reward or punishment side:
  
  cat("Add variable \"CuePosition\" (whether cue on reward or punishment side)\n")
  outputData$CuePosition <- ifelse(grepl("m",outputData$CueOutcome),"Punishment","Reward")

  outputData$CuePosition_n <- ifelse(outputData$CuePosition=="Reward",1,-1) # to numeric
  # outputData[,c("CuePosition","LeftOutcome","RightOutcome","CuePosition","CueOutcome")]
  outputData$CueOutcome_n <- outputData$CueOutcomeNum * outputData$CuePosition_n # add exact number on cue side (+1 when on reward side, -1 when on punishment side)
  
  # ------------------------------------------- # 
  ### 6) Delete RTs for NoGo responses:
  
  cat("Convert RTs to numeric\n")
  outputData$Reaction.Time <- as.numeric(outputData$Reaction.Time) # convert RTs to numeric
  
  cat("Convert RTs from ms to s\n")
  outputData$Reaction.Time <- as.numeric(outputData$Reaction.Time)/1000 # convert RTs to seconds
  
  # with(outputData,tapply(Reaction.Time,Response,mean,na.rm = T)) # RTs for NoGo counted as 1500 ms
  # with(outputData,tapply(Reaction.Time,Response,sd,na.rm = T)) # RTs for NoGo all close to 1500 ms
  # with(outputData,tapply(Reaction.Time,Response,min,na.rm = T)) # RTs for NoGo all close to 1500 ms
  # with(outputData,tapply(Reaction.Time,Response,max,na.rm = T)) # RTs for NoGo all close to 1500 ms
  cat("Delete RTs for NoGo responses\n")
  outputData$Reaction.Time[which(outputData$Response=="NoGo")] <- NA # delete RTs for NoGo
  # with(outputData,tapply(is.na(Reaction.Time),Response,sum)) # No NAs for Go, only for NoGo
  
  # ------------------------------------------- #
  ### 7) Delete too short RTs:
  
  minRT <- 0.3
  count <- sum(outputData$Reaction.Time < minRT, na.rm = T)
  cat(paste0("Found ",count," trials with RTs < ",minRT,"\n"))

  cat(paste0("Create new variable \"Reaction.Time_cleaned\" with RTs shorter than ",minRT," seconds deleted\n"))
  outputData$Reaction.Time_cleaned <- outputData$Reaction.Time # initialize new variable
  outputData$Reaction.Time_cleaned[which(outputData$Reaction.Time < minRT)] <- NA # delete RTs if shorter than 300 ms
  
  # ------------------------------------------- #
  ### 8) Determine outcome:
  # based on Response, RewAction, Validity
  cat("Determine trial-by-trial outcome\n")
  outputData <- determine_outcome(outputData)
  # outputData[1:15,c("subject","Trialnr","ReqAction","Response","Validity","Outcome")]
  
  # ------------------------------------------- #
  ### 9) Outcome valence:
  cat("Add variable OutcomeVal\n")
  outputData$OutcomeVal <- ifelse(outputData$Outcome>0,"Positive","Negative")
  outputData$OutcomeVal_n <- ifelse(outputData$Outcome>0,1,-1)
  # table(outputData$OutcomeVal)
  
  # ------------------------------------------- #
  ### 10) Difference between potential outcomes:
  cat("Add variable PointDif\n")
  outputData$PointDif <- outputData$RewardNum - outputData$PunishmentNum
  outputData$CueValence_n <- ifelse(outputData$PointDif > 0, 1, 0)
  outputData$CueValence_f <- factor(outputData$CueValence_n, levels = c(1,0), labels = c("Positive","Negative"))
  
  # ------------------------------------------- # 
  ### 11) Recompute instructions:
  cat("Add variable Instruction\n")
  outputData$Instruction <- ifelse(outputData$Blocknr < 3,"Before","After")
  
  # ------------------------------------------- #
  ### 12) Recompute response variable after deleting too short RTs:
  cat("Recreate response variable after deleting too short RTs\n")
  outputData$Response <- droplevels(as.factor(outputData$Response))
  outputData$Response_n <- ifelse(outputData$Response=="Go",1,0)
  
  # ------------------------------------------- #
  ### 13) Recompute accuracy:
  cat("Recompute variable accuracy\n")
  outputData$Accuracy_n <- ifelse(outputData$ReqAction==outputData$Response,1,0)
  outputData$Accuracy_f <- factor(outputData$Accuracy_n, levels = c(1,0), labels = c("Correct","Incorrect"))
  
  # ------------------------------------------- #
  ### 14) Compute trial number per block:
  cat("Add variable trial number per block\n")
  outputData$Trialnr_Block <- (outputData$Trialnr - 1) %% (length(unique(outputData$Trialnr))/length(unique(outputData$Blocknr))) + 1
  outputData$Trialnr_within_Block <- (outputData$Trialnr_Block - 1) %/% 14 + 1
  
  # ------------------------------------------- #
  ### 15) Action cue identity:
  cat("Retrieve action cue identity\n")
  outputData$ActionCue <- NA
  for (iRow in 1:nrow(outputData)){
    outputData$ActionCue[iRow] <- ifelse(outputData$LeftActionCue[iRow]=="Transparent.png",outputData$RightActionCue[iRow],outputData$LeftActionCue[iRow])
  }
  # outputData[,c("LeftActionCue","RightActionCue","ActionCue")]
  # outputData$ActionCue <- str_remove(outputData$ActionCue,".png")
  
  # ------------------------------------------- #
  ### 16) Recode variables to (ordered) factors, standardize numeric variables:
  
  colnames(outputData)
  cat("Convert to factors and ordered factors\n")
  
  ## Continuous variables:
  outputData$ReqAction_n <- ifelse(outputData$ReqAction=="Go",1,0)
  outputData$Validity_n <- ifelse(outputData$Validity=="Valid",1,0)
  
  ## Standard factors:
  outputData$CuePosition_f <- factor(outputData$CuePosition, labels = c("Left","Right"))
  outputData$RewardPosition_f <- factor(outputData$RewardPosition, labels = c("Left","Right"))
  outputData$ReqAction_f <- factor(outputData$ReqAction, labels = c("Go","NoGo"))
  outputData$RewAction_f <- factor(outputData$RewAction, labels = c("Go","NoGo"))
  outputData$Validity_f <- factor(outputData$Validity, labels = c("Valid","Invalid"))
  outputData$CuePosition_f <- factor(outputData$CuePosition, levels = c("Reward","Punishment"), labels = c("Reward","Punishment"))
  outputData$Instruction_f <- factor(outputData$Instruction, levels = c("Before","After"), labels = c("Before","After"))
  outputData$Condition_f <- factor(paste0(outputData$ReqAction_f,"2",outputData$CuePosition_f))
  
  ## Ordered factors:
  outputData$CuePosition_o <- ordered(outputData$CuePosition_f, levels = c("Left","Right"))
  outputData$RewardPosition_o <- ordered(outputData$RewardPosition_f, levels = c("Left","Right"))
  outputData$ReqAction_o <- ordered(outputData$ReqAction_f, levels = c("Go","NoGo"))
  outputData$RewAction_o <- ordered(outputData$RewAction_f, levels = c("Go","NoGo"))
  outputData$Validity_o <- ordered(outputData$Validity_f, levels = c("Valid","Invalid"))
  outputData$CuePosition_o <- ordered(outputData$CuePosition_f, levels = c("Reward","Punishment"))
  outputData$Instruction_o <- ordered(outputData$Instruction_f, levels = c("Before","After"))
  outputData$Condition_o <- ordered(outputData$Condition_f, levels = c("Go2Reward","Go2Punishment","NoGo2Reward","NoGo2Punishment"))
  outputData$Accuracy_o <- ordered(outputData$Accuracy_f, levels = c("Correct","Incorrect"))
  
  ## Standardize continuous variables:
  outputData$Reaction.Time_z <- as.numeric(scale(outputData$Reaction.Time))
  outputData$Outcome_z <- as.numeric(scale(outputData$Outcome))
  outputData$RewardNum_z <- as.numeric(scale(outputData$RewardNum))
  outputData$PunishmentNum_z <- as.numeric(scale(outputData$PunishmentNum))
  outputData$CueOutcomeNum_z <- as.numeric((outputData$CueOutcomeNum))
  outputData$PointDif_z <- as.numeric(scale(outputData$PointDif))
  # ignore LeftOutcomeNum, RightOutcomeNum, Decoy, CueOutcome
  
  cat("Done :-)\n")
  return(outputData)
}

# ================================================================================================================================================ #
#### Extract and prepare catch trial data: ####

preprocess_catch_data <- function(data, variables = c(), idvar = "Participant.Private.ID"){
  #' Preprocess catch trial data:
  #' - find rows reflecting catch trials, 
  #' - identify subjects,
  #' - select specified variables,
  #' - recover which numbers were presented,
  #' - recover outcome received,
  #' - recover required response, accuracy, and RT.
  #' @param data data frame, data frame with raw data of all subjects concatenated. 
  #' @param variables vector of strings, names of variables to keep (default: empty).
  #' @param idvar string, variable to identify subject IDs (default: "Participant.Private.ID").
  #' @return data data frame with only catch trials selected, recoded variables.
  
  require(dplyr)
  
  # ------------------------------------------- # 
  ### 1) Find rows with Screen.Name = "ScreenCatchTrial" and Spreadsheet.Row above 25:
  
  cat("Select rows with Screen.Name \"ScreenCatchTrial\"\n")
  outputData <- data[data$Screen.Name == "ScreenCatchTrial" & data$Spreadsheet.Row > 25,]
  
  # Save row indices of catch trials:
  idx <- as.numeric(rownames(outputData))
  
  # ------------------------------------------- # 
  ### 2) Identify subjects:
  
  outputData <- create_sub_ID(outputData, idvar = idvar)
  
  # ------------------------------------------- # 
  ### 3) Select specified variables:
  
  if (length(variables)!=0){
    cat("Only keep specified variables\n")
    selVariables <- variables[variables %in% colnames(outputData)]
    outputData <-outputData[,c("subject","Sub_Trialnr",selVariables)]
  }
  
  # ------------------------------------------- # 
  ### 4) Recover which numbers were presented:
  
  # idx[155]
  # data[140:153,30:78]
  # data[33580:33590,30:78]
  
  outputData$Trialnr <- data[idx-1,"Trialnr"] # trial in total task space (1:224)
  
  outputData$ReqAction <- data[idx-1,"ReqAction"] # required response
  outputData$CatchResponse <- as.numeric(outputData$Response) # save catch response in different variable
  outputData$Response <- data[idx-9,"Response"] # Go/NoGo response
  outputData$Validity <- data[idx-1,"Validity"] 
  
  outputData$LeftOutcomeNum <- data[idx-1,"LeftOutcomeNum"]
  outputData$RightOutcomeNum <- data[idx-1,"RightOutcomeNum"]
  outputData$RewardNum <- data[idx-1,"RewardNum"]
  outputData$PunishmentNum <- data[idx-1,"PunishmentNum"]
  
  # ------------------------------------------- #
  ### 5) Determine outcome:
  # based on Response, RewAction, Validity
  cat("Determine trial-by-trial outcome\n")
  
  outputData <- determine_outcome(outputData)
  # outputData[1:15,c("subject","Trialnr","ReqAction","Response","Validity","Outcome")]
  
  # ------------------------------------------- #
  ### 6) Recover required response:
  cat("Recover required catch response\n")
  
  outputData$ReqCatchResponse <- ifelse(abs(outputData$Outcome)==outputData$RewardNum,outputData$PunishmentNum,outputData$RewardNum)
  # outputData[1:15,c("ReqAction","Response","Validity","RewardNum","PunishmentNum","Outcome","ReqCatchResponse")]
  
  # ------------------------------------------- # 
  ### 5) Recover accuracy:
  cat("Recover accuracy\n")
  
  outputData$Accuracy_n <- ifelse(outputData$ReqCatchResponse == outputData$CatchResponse,1,0)
  outputData$Accuracy_f <- factor(outputData$Accuracy_n, levels = c(1,0), labels = c("Correct","Incorrect"))
  
  # ------------------------------------------- # 
  ### 5) Recover RTs:
  cat("Convert RTs to numeric\n")
  outputData$Reaction.Time <- as.numeric(outputData$Reaction.Time) # convert RTs to numeric
  cat("Convert RTs from ms to s\n")
  outputData$Reaction.Time <- as.numeric(outputData$Reaction.Time)/1000 # convert RTs to seconds
  
  cat("Done :-)\n")
  return(outputData)
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
  
  # ------------------------------------------- #
  ### Load required packages:
  require(plyr) # for ddply
  require(Rmisc) # for summarySEwithin
  require(ggbeeswarm) # for ggbeeswarm
  
  # ------------------------------------------- #
  ### Fixed plotting settings:
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
  ### Aggregate data per subject per condition:
  aggrData <- ddply(data, .(subject, x, z), function(x){
    y <- mean(x$y, na.rm = T)
    return(data.frame(y))
    dev.off()})
  # Wide format: each subject/condition1/condition2 in one line, variables subject, x, y, z
  
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
  
  # ------------------------------------------- #
  ### Aggregate across subjects with Rmisc:
  summary_d <- summarySEwithin(aggrData, measurevar="y", idvar = "subject", na.rm = T,
                               withinvars = c("x","z"))
  # Aggregated over subjects, one row per condition, variables x, z, N, y, sd, se, ci
  
  # ------------------------------------------- #
  ### ggplot:
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
  
  # ------------------------------------------- #
  ## Load required packages:
  require(ggplot2)
  require(lme4)
  
  # ------------------------------------------- #
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
    
    # Mean/ line itself:
    yVecEval <- as.numeric(tmp$fit) # y-axis coordinates (untransformed)
    if (isGLMM(mod)){yVecEval <-mod@resp$family$linkinv(yVecEval)} # transform to response scale
    
    # Lower and upper limit of CI interval:
    ymin <- t(tmp$lower) # lower bound of CI interval
    ymax <- t(tmp$upper) # upper bound of CI interval
    if (isGLMM(mod)){ymin <-mod@resp$family$linkinv(ymin)} # transform to response scale
    if (isGLMM(mod)){ymax <-mod@resp$family$linkinv(ymax)} # transform to response scale
    # ymin;ymax
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
    # ymin;ymax
  }
  # ------------------------------- #
  ## Plot error bars/ shades:
  d <- data.frame(x = xVecEval, y = yVecEval, ymin = ymin, ymax = ymax)
  p <- p + geom_ribbon(data = d, aes(x = x, y = y, ymin = ymin, ymax = ymax),
                       fill = color, alpha=0.20, linetype = 0)
  
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
  
  # Print plot in the end:
  print(p)
  cat("Finished :-)\n")
  return(p)
}  

# ================================================================================================================================================ #
#### Aggregate into long format: ####

aggregate_long <- function(data,variables){ # 
  require(plyr) # for ddply
  outputData <- ddply(data, .(subject,ReqAction,CuePosition,Instruction), function(x){
  # outputData <- ddply(data, .(subject,ReqAction,CuePosition), function(x){
    subject <- x$subject[1]
    Response <- mean(x$Response_n,na.rm = T)
    Accuracy <- mean(x$Accuracy_n,na.rm = T)
    Reaction.Time <- mean(x$Reaction.Time,na.rm = T)
    return(data.frame(subject, Response, Accuracy, Reaction.Time))
    dev.off()})
  return(outputData)
}

# ================================================================================================================================================ #
#### Extract flexible questionnaire data: ####

reshape_questionnaire <- function(data, selString = "-quantised", idvar = "Participant.Private.ID", convert2num = T){
  #' Preprocess questionnaire data by 
  #' - deleting rows with NA or empty string in \code{idvar}, 
  #' - selecting variables that contain letter string  \code{selString},
  #' - converting numerical variables to factors.
  #' @param data        data frame with raw data, should contain column \code{idvar}.
  #' @param selString   string, letter string of variables of interest to keep.
  #' @param idvar       string, name of variable containing subject identifier (default: Participant.Private.ID).
  #' @param convert2num boolean, whether to convert factors to numeric variables (default: TRUE).
  #' @return data       same data frame processed. 

  require(reshape2)
  require(stringr)
  
  # ------------------------------------------- # 
  ## 1) Select relevant columns:
  cat("Only select variables \"ID variable\", \"Question.Key\", \"Response\"\n")
  data <- data[,c(idvar,"Question.Key","Response")]
  
  # ------------------------------------------- # 
  ## 2) Select rows without NAs:
  cat("Only select rows without any NAs\n")
  data <- data[which(!is.na(data[,idvar])),]
  
  # ------------------------------------------- # 
  ## 3) Reshape to wide format:
  cat("Cast to wide format\n")
  wideData <- reshape(data, idvar = idvar, timevar = "Question.Key", v.names = "Response", direction = "wide")
  
  # ------------------------------------------- # 
  ## 4) Create subject ID;
  numIDvar <- 1
  # wideData <- create_sub_ID(wideData,idvar = idvar)
  # numIDvar <- 2
  
  # ------------------------------------------- # 
  ## 5) Select rows without NAs and empty string:
  cat(paste0("Delete rows where ",idvar," is not NA or an empy string \n"))
  wideData <- wideData[which(!is.na(wideData[,idvar])),] # not NA on idvar
  wideData <- wideData[which(wideData[,idvar]!=""),] # not empty string on idvar

  # ------------------------------------------- # 
  ## 6) Select columns based on string:
  cat("Extract columns with \"quantised\" (variable name contains \"selString\") answers\n")
  selData <- wideData[,c(which(grepl(idvar,colnames(wideData))),which(grepl(selString, colnames(wideData))))] # select only these variables
  # selData <- wideData[,c(which(grepl(idvar,colnames(wideData))),which(grepl("subject",colnames(wideData))),which(grepl(selString, colnames(wideData))))] # select only these variables
  
  # ------------------------------------------- # 
  ## 7) Convert factor to numeric:
  cat("Convert factors to numeric\n")
  if (convert2num){
    selData <- droplevels(selData)
    for (i in (1+numIDvar):ncol(selData)){
      if (is.factor(selData[,i])){
        selData[,i] <- as.numeric(levels(selData[,i]))[selData[,i]] 
      }
    }
  }
  
  # ------------------------------------------- # 
  ## 8) Rename variables:
  cat(paste0("Delete \"Response.\" and ", selString," from the column names"))
  varNames <- colnames(selData)[(1+numIDvar):ncol(selData)] # extract
  varNames <- str_remove(varNames,"Response.") # remove
  varNames <- str_remove(varNames,selString) # remove
  colnames(selData)[(1+numIDvar):ncol(selData)] <- varNames # add back in
  
  return(selData)
}
  
# END