# README General

## Content ##
This collection contains the following files:
- **analyses**: scripts for all behavioral and eye-tracking data required to reproduce the reported results
- **task**: scripts and files for running the task (for both samples)

**Code** for the entire paper will be **maintained** under https://github.com/johalgermissen/Algermissen2023JEPG, with a permanent copy of the code at the time of publication under https://github.com/denoudenlab/Algermissen2023JEPG.

**Raw data and processed data** for this project are available under https://doi.org/10.34973/05tj-3w64 under the MIT licence.

## Root directory ##
Note that, for many files, the **root directory** of this project needs to be adjusted. 

## Analyses folder ##

### Analysis software used ###
All analyses were performed in _R_. See the file _functions/analyses/package_manager.R_ for all respective package version numbers and a print out of both _sessionInfo()_ and _loadedNamespaces()_.

### functions ###
Contains functions common to preprocessing and analyzing **both** samples (sample 1 and sample 2) reported in the main text.

- *package_manager.R*: Loads packages (contains version numbers and print-outs of _sessionInfo()_ and _loadedNamespaces()_), set _scipen_ and _factor codings_.
- *00_functions_preprocess.R*: Contains several functions needed for pre-processing eye-tracking data (called by *01_eyegaze_preprocess.R*).
- *00_functions_analyze.R*: Contains several functions for pre-processing and analyzing behavioral and eye-tracking data (called by *02_eyegaze_analyze_mixedmodels.R*).

### sample1 and sample 2 ###

Contains scripts to pre-process and analyze eye-tracking data for each respective sample. Run files in numerical order:
- *01_eyegaze_preprocess.R*: Pre-process eye-tracking data: Read eye-tracking data, compare behavioral data stored in eye-tracking file to behavioral dat stored in behavioral file, interpolate short sequences of mising data (blinks), determine whether fixation is in pre-specified ROIs, aggregate fixation and dwell time per trial, save.
- *02_eyegaze_analyze_mixedmodels.R*: Analyze data: Perform mixed-effects linear and logistic regression models to obtain all results reported in the main text and supplementary material for the respective sample.
- *03_plots_paper.R* (only available in sample2; can be run on both data sets): Create plots for the respective sample for all figures in main text and supplementary material.
- *04_between_subjects.R* (only in sample2; loads in data from both samples): Compute between-subjects correlations: Fit models for stakes and attentional effects across both samples, correlate with accuracy, create scatterplots, perform control analyses with other fixation measures.

### onlineSample ###

Contains scripts to pre-process and analyze behavioral data reported in the Supplementary Material S7. Run files in numerical order:
- *00_functions_analyze.R*: Contains several functions for pre-processing and analyzing behavioral data.
- *01_preparing_questionnaires.R*: Load in raw questionnaire data, pre-process, save as processed data.
- *02_analyze_debriefing.R*: Read in and inspect debriefing responses.
- *02_analyze_demographics.R*: Read in and inspect sample demopgraphics.
- *03_analyze_gonogo_mixedmodels.R*: Analyze task data: Perform mixed-effects linear and logistic regression models to obtain all results reported in Supplementary Material S7.
- *04_plot_gonogo.R*: Create plots displayed in Supplementary Material S7.
- *05_analyze_questionnaires.R*: Analyze relationships between questionnaires and task behavior using mixed-effects models. Requires questionnaires to be pre-processed and saved as separate files (by running _01_preparing_questionnaires.R_).
- *package_manager.R*: Loads packages (contains version numbers and print-outs of _sessionInfo()_ and _loadedNamespaces()_), set _scipen_ and _factor codings_.

## Data folder ##
Contains raw and pre-processed eye-tracking and behavioral data for samples 1 and 2 (main text) and the online sample reported in the Supplementary Material S7. Each sample contains the following sub-folders:

### sample1 and sample2 ###

Raw data (and processed data for the questionnaires) from the two eye-tracking studies reported as Sample 1 and Sample 2 in the main text of the manuscript.

#### rawData ####
- *behavData*: behavioral raw data (separate file per subject) with the following variables:
    - *subject*: integer, subject number.
    - *age*: integer, subject age.
    - *gender*: string (male, female, other), subject gender.
    - *handedness*: string (left_hand, right_hand), subject dominant hand.
    - *eye*: string (left_eye, right_eye), subject dominant eye.
    - *sessionNr*: integer between 1 and 3, block number (identical to blockNr).
    - *blockNr*: integer between 1 and 3, block number (identical to sessionNr).
    - *trialNr*: integer between 1 and 264, trial number.
    - *cue*: string, cue identifier within block, letter (A-C) indicates block, number (1-4) cue identity (see task folder).
    - *reqAction*: 1 or 0, required response, either Go (1) or NoGo (0).
    - *reqSide*: 1 or 0, required response side (whether oyster was still open), either left (1) or right (0).
    - *rewLeft*: 1 or 0, stakes mapping, either rewards on the left (1) or on the right (0).
    - *angle*: integer betwen -45 and 45, displacement of stakes from horizontal meridian.
    - *rewMag*: integer between 1 and 5, reward stake.
    - *punMag*: integer between 1 and 5, punishment stake.
    - *validity*: 1 or 0, feedback valdidity, either valid (1) or invalid (0).
    - *response*: 1 or 0, either Go response (1) or NoGo response (0).
    - *respSide*: 1 or 0 or NA, side of response if Go response, either left (1) or right (0).
    - *ACC*: 1 or 0, either correct (1) or incorrect (0) response.
    - *RT*: float, RT of any Go response (in sec.).
    - *outcome*: integer between -5 and 5, outcome obtained.
    - *isCatch*: 1 or 0, either catch trial (1) or Go/NoGo trial (0).
    - For further information, see the *wrapper_preprocessing* function in the *00_functions_analyze.R* script.
- NOTE: in sample2, there were originally two files each with subject IDs 33 and 56, while there were no files with subject IDs 34 and 57. These files have been manually renamed to 34 and 57, but the variable "subject" still contains the old subject ID values. The data with subject ID 33 and age 20 is counted as subject ID 34. The data with subject ID 56 and age 19 is counted as subject ID 57 (both implemented in *01_eyegaze_preprocess.R* for sample2). Note that this confusion does not affect counter-balancing because once an odd and once an even number were mistaken.
- *eyeData* with *.*: eye-tracking raw data (separate file per subject; both as .edf and converted to .asc) collected with an Eyelink 1000. Data can be conveniently read in with the *eyelinker* package in R. Data contains four columns (interrupted by messages); columns are:
    - sample identifier (in ms relative to recording start).
    - x-coordinate of gaze.
    - y-coordinate of gaze.
    - pupil diameter.
    - For more information, see the respective file header and the Eyelink 1000 documentation.

#### processedData ####
- *eyeData_processed.csv*: all behavioral/ eye-tracking data from all files in *eyeDataAggr* concatenated and additionall pre-processed; can be loaded as input for all relevant analyses reported in the main manuscript.
- folder *eyeDataAggr* with files *eye_subXX_aggr.csv*: pre-processed eye-tracking data (separate file per participant) aggregated per trial. For more information, see steps performed in *01_eyegaze_preprocess*.

### onlinesample ###

#### raw data ####
Input data files with task data and questionnaire data; processed with *01_preparing_questionnaires.R* (which calls *reshape_questionnaire()* function from *00_functions_analyze.R*) and *03_analyze_gonogo_mixedmodels.R*. See these two files for explanations of variables and their meaning.

- *data_exp_16464-v2_questionnaire-3bu2.csv*: Data from the Self-Control Scale (SCS). 
- data_exp_16464-v2_questionnaire-23ew.csv: Data from the Behavioral Activation/ Behavioral Inhibition System Scales (BIS/BAS).
- data_exp_16464-v2_questionnaire-59ym.csv: Data from the football scenarios described in Zeelenberg, van den Bos, van Dijk, and Pieter (2022); Loss scenario.
- data_exp_16464-v2_questionnaire-clcu.csv: Data from the football scenarios described in Zeelenberg, van den Bos, van Dijk, and Pieter (2022); Win scenario.
- data_exp_16464-v2_questionnaire-d257.csv: Data with 6 debriefing questions.
- data_exp_16464-v2_questionnaire-s5vw.csv: Data on when certain questionnaires were started and finished.
- data_exp_16464-v2_questionnaire-v8vg.csv: Data with participant demographics.
- Data sets with pattern *-task-*: Go/NoGo task data of different participants split into 10 sub-data sets.
    - data_exp_16464-v2_task-1wew.csv
    - data_exp_16464-v2_task-ahzx.csv
    - data_exp_16464-v2_task-b5ss.csv
    - data_exp_16464-v2_task-f8n6.csv
    - data_exp_16464-v2_task-livv.csv
    - data_exp_16464-v2_task-p1uv.csv
    - data_exp_16464-v2_task-pxjz.csv
    - data_exp_16464-v2_task-q7n3.csv
    - data_exp_16464-v2_task-y4iu.csv
    - data_exp_16464-v2_task-yret.csv

#### processed data ####
Files with questionnaire data after having processed them with *01_preparing_questionnaires.R* (which calls *reshape_questionnaire()* function from *00_functions_analyze.R*).

- *MGNGSearch_BISBAS.csv*: Data from the Behavioral Activation/ Behavioral Inhibition System Scales (BIS/BAS).
- *MGNGSearch_Debriefing.csv*: Responses to six debriefing questions per participant.
- *MGNGSearch_Demographics.csv*: File with demographics per participant.
- *MGNGSearch_Football.csv*: Data from the football scenarios described in Zeelenberg, van den Bos, van Dijk, and Pieter (2022); Win and Loss scenario.
- *MGNGSearch_SCS.csv*: Data from the Self-Control Scale (SCS). 
 
## Task folder ##
Contains the code and files necessary to run the task in PsychoPy for both samples 1 and 2.

- *MGNGSearch_FreeView_Study1_2021_02_25_final.py* and *MGNGSearch_FreeView_Study2_2021_09_15.py*: script to execute task.
- *BehavData*: target directory to store behavioral output data.
- *Instructions*: instructions stored as .png files to be loaded in task; created in file *Instructions_MGNGSearch_FreeView_final.pptx*.
- *TaskStimuli*: contains task stimuli:
    - *ActionCues*: Agathodaimon stimuli used as Go/NoGo cues; arranged in groups of four cues each (letter indicates group, number indicates stimulus within group) as well as practice stimuli *Pract_Go.png* and *Pract_NoGo.png*.
    - *RawStimuli*: Examples of single pearl and tumor stimuli in both orange and blue as well as oyster with orange/ blue semi-circle on the left/ right; created in *RawStimuli.pptx*.
    - *ReleaseCues*: stimuli used for response: black oyster open at the left/ right; can up-right and tilted to the left/ right.
    - *StakesCues*: pearl and tumor stimuli in orange and blue, 1-5 items; fuzzy stimuli for covert stimuli in gaze-contingent design.
    - *Oysters_All.pptx*: File to create stakes stimuli.
- *TrialHandler*: files with input variables per trial:
    - *MGNGSearch_FreeView_Oyster.R*: script used to create individual sequence with constraints per subject ID.
    - *MGNGSearch_FreeView_Oyster_Pract.R*: script used to create sequences for five practice trial blocks (fixed across subject IDs).
    - *stimuluslist_pract_blockXXX.csv*: trial sequences for 5 practice blocks.
    - *stimuluslist_test_stimNum_264_sub_XXX_blockXXX.csv*: individual trial sequences per block per subject ID.

END of file.

