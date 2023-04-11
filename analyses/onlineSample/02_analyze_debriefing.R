#### 02_analyze_debriefing.R ####

#' Load and pre-process debriefing data, 
#' print comments to console,
#' save pre-processed data.

# ================================================================================================================================================ #
#### Set directories: ####

rootDir       <- "/project/2420133.01/" # adjust to your own folder structure before running script

codeDir       <- paste0(rootDir,"analyses/onlineSample/")

dataDir       <- paste0(rootDir,"data/onlineSample/") 
rawDir        <- paste0(dataDir,"rawData/")
processedDir  <- paste0(dataDir,"processedData/")

# ================================================================================================================================================ #
#### Load packages and custom functions: #

source(paste0(codeDir,"package_manager.R")); # Load packages, set factor coding
source(paste0(codeDir,"00_functions_analyze.R")); # Load functions

# ================================================================================================================================================ 
#### Debriefing: ####

data <- read.csv(paste0(rawDir, "data_exp_16464-v2_questionnaire-d257.csv")) # Debriefing
data <- reshape_questionnaire(data,selString = "Response.",convert2num = F)

# -------------------------------------------------------- #
## Hypothesis comment:
data$debriefingQ01

## Additional strategy radio button:
data$debriefingQ02

## Additional strategies comments:
data$debriefingQ03

## Following strategy radio button:
data$debriefingQ04

## Following strategy comments:
data$debriefingQ05

## Additional comments:
data$debriefingQ06

# -------------------------------------------------------- #
### Save:

data <- data[,c("Participant.Public.ID","SubID","debriefingQ01","debriefingQ02","debriefingQ03","debriefingQ04","debriefingQ05","debriefingQ06")]
write.csv(data,paste0(processedDir, "MGNGSearch_Debriefing.csv"), row.names = F)

## END
