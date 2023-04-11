#### All packages and settings for analysis ####

# =========================================================================================== #
#### Packages: ####
cat("Start loading packages\n")
require(car) # version '3.0.11'
require(effsize) # version '0.8.1' # for Cohen.d
require(ggplot2) # version '3.3.5' 
require(lattice) # version '0.20.45' 
require(ltm) # version '1.1.1' 
require(pastecs) # version '1.3.21' # for stat.desc
require(psych) # version '2.1.9' 
require(Rmisc) # version '1.5' # for summarySEwithin

# ----------------------------------------- #
# Tidyverse:
require(dplyr) # version '1.0.7'
require(magrittr) # version '2.0.1'

# ----------------------------------------- #
# Linear mixed effects models:
require(lme4) # version '1.1.27.1'
require(afex) # version '1.0.1'
require(effects) # version '4.2.0'

# ----------------------------------------- #
# For raincloud plots:
require(readr) # version '2.0.2'
require(tidyr) # version '1.1.4'
require(gghalves) # version '0.1.1' # for half plots
require(ggplot2) # version '3.3.5'
require(ggstatsplot) # version '0.9.0' # for geom_flat_violin
require(Hmisc) # version '4.6.0' 
require(plyr) # version '1.8.6'
require(RColorBrewer) # version '1.1.2'
require(reshape2) # version '1.4.4'

# ----------------------------------------- #
## Facilitate detecting when model finished:
require(beepr) # version '1.3'

# =========================================================================================== #
#### General settings: #####

cat("Set scipen to 20\n")
options(scipen = 20)

cat("Set contrasts to sum-to-zero coding\n")
options(contrasts = c("contr.sum", "contr.poly"))

# print("Delete all objects")
# rm(list=ls())

# =========================================================================================== #
#### Output of sessionInfo(): #####

# R version 4.0.3 (2020-10-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=English_Netherlands.1252  LC_CTYPE=English_Netherlands.1252   
# [3] LC_MONETARY=English_Netherlands.1252 LC_NUMERIC=C                        
# [5] LC_TIME=English_Netherlands.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] beepr_1.3          gghalves_0.1.1     ggstatsplot_0.9.0  reshape2_1.4.4     RColorBrewer_1.1-2
# [6] Hmisc_4.6-0        Formula_1.2-4      survival_3.2-13    tidyr_1.1.4        readr_2.0.2       
# [11] itsadug_2.4        plotfunctions_1.4  mgcv_1.8-38        nlme_3.1-153       effects_4.2-0     
# [16] afex_1.0-1         lme4_1.1-27.1      Matrix_1.3-4       magrittr_2.0.1     dplyr_1.0.7       
# [21] effsize_0.8.1      psych_2.1.9        pastecs_1.3.21     ltm_1.1-1          polycor_0.7-10    
# [26] msm_1.6.9          MASS_7.3-54        ggplot2_3.3.5      Rmisc_1.5          plyr_1.8.6        
# [31] lattice_0.20-45    car_3.0-11         carData_3.0-4     
# 
# loaded via a namespace (and not attached):
#   [1] readxl_1.3.1           backports_1.2.1        splines_4.0.3          gmp_0.6-2             
# [5] kSamples_1.2-9         TH.data_1.1-0          digest_0.6.28          SuppDists_1.1-9.5     
# [9] htmltools_0.5.2        lmerTest_3.1-3         fansi_0.5.0            checkmate_2.0.0       
# [13] memoise_2.0.0          paletteer_1.4.0        cluster_2.1.2          tzdb_0.1.2            
# [17] openxlsx_4.2.4         sandwich_3.0-1         jpeg_0.1-9             colorspace_2.0-2      
# [21] mitools_2.4            haven_2.4.3            xfun_0.27              crayon_1.4.2          
# [25] zeallot_0.1.0          zoo_1.8-9              glue_1.4.2             gtable_0.3.0          
# [29] emmeans_1.7.0          statsExpressions_1.2.0 Rmpfr_0.8-5            abind_1.4-5           
# [33] scales_1.1.1           mvtnorm_1.1-3          DBI_1.1.1              PMCMRplus_1.9.2       
# [37] Rcpp_1.0.7             performance_0.8.0      xtable_1.8-4           htmlTable_2.3.0       
# [41] tmvnsim_1.0-2          foreign_0.8-81         survey_4.1-1           datawizard_0.2.1      
# [45] htmlwidgets_1.5.4      ellipsis_0.3.2         pkgconfig_2.0.3        reshape_0.8.8         
# [49] nnet_7.3-16            multcompView_0.1-8     utf8_1.2.2             tidyselect_1.1.1      
# [53] rlang_0.4.12           munsell_0.5.0          cellranger_1.1.0       tools_4.0.3           
# [57] cachem_1.0.6           audio_0.1-8            generics_0.1.0         stringr_1.4.0         
# [61] fastmap_1.1.0          BWStest_0.2.2          rematch2_2.1.2         knitr_1.36            
# [65] zip_2.2.0              purrr_0.3.4            WRS2_1.1-3             correlation_0.7.1     
# [69] compiler_4.0.3         rstudioapi_0.13        curl_4.3.2             png_0.1-7             
# [73] tibble_3.1.5           stringi_1.7.5          parameters_0.15.0      forcats_0.5.1         
# [77] nloptr_1.2.2.2         vctrs_0.3.8            pillar_1.6.4           lifecycle_1.0.1       
# [81] mc2d_0.1-21            estimability_1.3       data.table_1.14.2      insight_0.14.5        
# [85] patchwork_1.1.1        R6_2.5.1               latticeExtra_0.6-29    gridExtra_2.3         
# [89] rio_0.5.27             codetools_0.2-18       boot_1.3-28            assertthat_0.2.1      
# [93] withr_2.4.3            mnormt_2.0.2           multcomp_1.4-17        bayestestR_0.11.0     
# [97] expm_0.999-6           parallel_4.0.3         hms_1.1.1              grid_4.0.3            
# [101] rpart_4.1-15           coda_0.19-4            minqa_1.2.4            numDeriv_2016.8-1.1   
# [105] base64enc_0.1-3

# =========================================================================================== #
#### Printed out versions from laodedNamespaces() ####

# see https://stackoverflow.com/questions/11103189/how-to-find-out-which-package-version-is-loaded-in-r
# for (package_name in sort(loadedNamespaces())) {
#   print(paste(package_name, packageVersion(package_name)))
# }
# [1] "abind 1.4.5"
# [1] "afex 1.0.1"
# [1] "assertthat 0.2.1"
# [1] "audio 0.1.8"
# [1] "backports 1.2.1"
# [1] "base 4.0.3"
# [1] "base64enc 0.1.3"
# [1] "bayestestR 0.11.0"
# [1] "beepr 1.3"
# [1] "boot 1.3.28"
# [1] "BWStest 0.2.2"
# [1] "cachem 1.0.6"
# [1] "car 3.0.11"
# [1] "carData 3.0.4"
# [1] "cellranger 1.1.0"
# [1] "checkmate 2.0.0"
# [1] "cluster 2.1.2"
# [1] "coda 0.19.4"
# [1] "codetools 0.2.18"
# [1] "colorspace 2.0.2"
# [1] "compiler 4.0.3"
# [1] "correlation 0.7.1"
# [1] "crayon 1.4.2"
# [1] "curl 4.3.2"
# [1] "data.table 1.14.2"
# [1] "datasets 4.0.3"
# [1] "datawizard 0.2.1"
# [1] "DBI 1.1.1"
# [1] "digest 0.6.28"
# [1] "dplyr 1.0.7"
# [1] "effects 4.2.0"
# [1] "effsize 0.8.1"
# [1] "ellipsis 0.3.2"
# [1] "emmeans 1.7.0"
# [1] "estimability 1.3"
# [1] "expm 0.999.6"
# [1] "fansi 0.5.0"
# [1] "fastmap 1.1.0"
# [1] "forcats 0.5.1"
# [1] "foreign 0.8.81"
# [1] "Formula 1.2.4"
# [1] "generics 0.1.0"
# [1] "gghalves 0.1.1"
# [1] "ggplot2 3.3.5"
# [1] "ggstatsplot 0.9.0"
# [1] "glue 1.4.2"
# [1] "gmp 0.6.2"
# [1] "graphics 4.0.3"
# [1] "grDevices 4.0.3"
# [1] "grid 4.0.3"
# [1] "gridExtra 2.3"
# [1] "gtable 0.3.0"
# [1] "haven 2.4.3"
# [1] "Hmisc 4.6.0"
# [1] "hms 1.1.1"
# [1] "htmlTable 2.3.0"
# [1] "htmltools 0.5.2"
# [1] "htmlwidgets 1.5.4"
# [1] "insight 0.14.5"
# [1] "itsadug 2.4"
# [1] "jpeg 0.1.9"
# [1] "knitr 1.36"
# [1] "kSamples 1.2.9"
# [1] "lattice 0.20.45"
# [1] "latticeExtra 0.6.29"
# [1] "lifecycle 1.0.1"
# [1] "lme4 1.1.27.1"
# [1] "lmerTest 3.1.3"
# [1] "ltm 1.1.1"
# [1] "magrittr 2.0.1"
# [1] "MASS 7.3.54"
# [1] "Matrix 1.3.4"
# [1] "mc2d 0.1.21"
# [1] "memoise 2.0.0"
# [1] "methods 4.0.3"
# [1] "mgcv 1.8.38"
# [1] "minqa 1.2.4"
# [1] "mitools 2.4"
# [1] "mnormt 2.0.2"
# [1] "msm 1.6.9"
# [1] "multcomp 1.4.17"
# [1] "multcompView 0.1.8"
# [1] "munsell 0.5.0"
# [1] "mvtnorm 1.1.3"
# [1] "nlme 3.1.153"
# [1] "nloptr 1.2.2.2"
# [1] "nnet 7.3.16"
# [1] "numDeriv 2016.8.1.1"
# [1] "openxlsx 4.2.4"
# [1] "paletteer 1.4.0"
# [1] "parallel 4.0.3"
# [1] "parameters 0.15.0"
# [1] "pastecs 1.3.21"
# [1] "patchwork 1.1.1"
# [1] "performance 0.8.0"
# [1] "pillar 1.6.4"
# [1] "pkgconfig 2.0.3"
# [1] "plotfunctions 1.4"
# [1] "plyr 1.8.6"
# [1] "PMCMRplus 1.9.2"
# [1] "png 0.1.7"
# [1] "polycor 0.7.10"
# [1] "psych 2.1.9"
# [1] "purrr 0.3.4"
# [1] "R6 2.5.1"
# [1] "RColorBrewer 1.1.2"
# [1] "Rcpp 1.0.7"
# [1] "readr 2.0.2"
# [1] "readxl 1.3.1"
# [1] "rematch2 2.1.2"
# [1] "reshape 0.8.8"
# [1] "reshape2 1.4.4"
# [1] "rio 0.5.27"
# [1] "rlang 0.4.12"
# [1] "Rmisc 1.5"
# [1] "Rmpfr 0.8.5"
# [1] "rpart 4.1.15"
# [1] "rstudioapi 0.13"
# [1] "sandwich 3.0.1"
# [1] "scales 1.1.1"
# [1] "splines 4.0.3"
# [1] "stats 4.0.3"
# [1] "statsExpressions 1.2.0"
# [1] "stringi 1.7.5"
# [1] "stringr 1.4.0"
# [1] "SuppDists 1.1.9.5"
# [1] "survey 4.1.1"
# [1] "survival 3.2.13"
# [1] "TH.data 1.1.0"
# [1] "tibble 3.1.5"
# [1] "tidyr 1.1.4"
# [1] "tidyselect 1.1.1"
# [1] "tmvnsim 1.0.2"
# [1] "tools 4.0.3"
# [1] "tzdb 0.1.2"
# [1] "utf8 1.2.2"
# [1] "utils 4.0.3"
# [1] "vctrs 0.3.8"
# [1] "withr 2.4.3"
# [1] "WRS2 1.1.3"
# [1] "xfun 0.27"
# [1] "xtable 1.8.4"
# [1] "zeallot 0.1.0"
# [1] "zip 2.2.0"
# [1] "zoo 1.8.9"
# END