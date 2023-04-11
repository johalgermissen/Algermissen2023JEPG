#### 02_analyze_demographics.R ####

#' Load and pre-process demographics data, 
#' compute descriptive statistics on demographic data, 
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
#### Demographics: ####

data <- read.csv(paste0(rawDir, "data_exp_16464-v2_questionnaire-v8vg.csv")) # demographics
data <- reshape_questionnaire(data, selString = "R", convert2num = F)
data <- data[,c("Participant.Public.ID","age","gender","handedness")] # sub-select relevant variables

# ------------------------------------------- #
### Gender:

table(data$gender) 
# Female   Male 
#     18     16

# ------------------------------------------- #
### Age:

data$age <- as.numeric(data$age)
round(stat.desc(data$age),2) 
# nbr.val     nbr.null       nbr.na          min          max        range          sum       median 
#   34.00         0.00         0.00        19.00        27.00         8.00       763.00        22.00 
#    mean      SE.mean CI.mean.0.95          var      std.dev     coef.var 
#   22.44         0.36         0.72         4.31         2.08         0.09 
min(data$age) # 19 
max(data$age) # 27
mean(data$age) # 22.44
sd(data$age) # 2.08
densityplot(data$age)

# ------------------------------------------- #
### Handedness:

table(data$handedness) # 31 right-handed, 3 left-handed
# Left-handed Right-handed 
#           3           31

# ------------------------------------------- #
### Save:

write.csv(data,paste0(processedDir, "MGNGSearch_Demographics.csv"), row.names = F)

# END
