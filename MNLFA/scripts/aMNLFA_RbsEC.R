# Code calls functions from aMNLFA.R 
# Note: Robin Sifre made some changes to the
# functions. Some were substantive changes (e.g. changing how family-wise error
# was adjusted for), and some were fixing some bugs that was causing the package
# to crash
#https://cran.r-project.org/web/packages/aMNLFA/index.html

####################################################
# Load libraries
####################################################
library(aMNLFA)
library(MplusAutomation)
library(multiplex)
library(ggplot2)
library(dplyr)
####################################################
# Setting paths & reading in data 
####################################################
# Set directory to where script will output mplus files.
wd <- '/Users/sifre002/Desktop/Invariance/CalibSample1/REP/'
setwd(wd)
# Read in cleaned RBS-EC data 
df <- read.csv("/Users/sifre002/Desktop/Invariance/2019-09-05_NOBAB_cleanData_MNLFA.csv",
               header=TRUE)
homedir = '/Users/sifre002/Box/sifre002/18-Organized-Code/invariance-repetitive-bx/MNLFA/'

# For reproducibility, read in previously-generated calib_sample
ru = read.csv('/Users/sifre002/Desktop/Invariance/CalibSample3/calib_sample1.csv')
ru = data.frame(ru)

####################################################
# 1.	Define aMNLFA objects (aMNLFA.object) 
# Comment out subscale that is not currently being run. 
####################################################
# Repetitive motor 
 ob <-aMNLFA.object(dir          = wd, # , indicate the location of the data.
                   mrdata        = df,
                   indicators    = c("REP1", "REP2","REP3","REP4","REP5", # list a set of indicators of a single factor
                                     "REP6","REP7","REP8","REP9"),
                   catindicators = c("REP1","REP2","REP3","REP4","REP5", # list a set of indicators that are binary or ordinal
                                     "REP6","REP7","REP8","REP9"),
                   time        = c("AGE18"),
                   meanimpact    = c("AGE18", "MALE",
                                     "TABLET",
                                      "AGEXTAB"), # Contrast coding of nominal variables
                   varimpact     = c("AGE18"), # contrast coding of nominal variables
                   measinvar     = c("AGE18", 
                                     "TABLET", "MALE",
                                     "AGEXTAB"), # List variables to be included in tests for measurement non-invariance.
                   factors       = c("PROJECT"),
                   ID            = "ID2",
                   thresholds    = TRUE) # indicate whether you would like to test measurement invariance of thresholds for ordinal indicators.

# Self-directed
ob <-aMNLFA.object(dir          = wd, # , indicate the location of the data.
                  mrdata        = df,
                  indicators    = c("SELF1","SELF2","SELF3","SELF5", # list a set of indicators of a single factor
                                    "SELF6"),
                  catindicators = c("SELF1","SELF2","SELF3","SELF5", # list a set of indicators that are binary or ordinal
                                    "SELF6"),
                  time        = c("AGE18"),
                  meanimpact    = c("AGE18", 
                                    "TABLET",
                                    "AGEXTAB", "MALE"), # Contrast coding of nominal variables
                  varimpact     = c("AGE18"), # contrast coding of nominal variables
                  measinvar     = c("AGE18", 
                                    "TABLET",
                                    "AGEXTAB", "MALE"), # List variables to be included in tests for measurement non-invariance.
                  factors       = c("PROJECT"),
                  ID            = "ID2",
                  thresholds    = TRUE) # indicate whether you would like to test measurement invariance of thresholds for ordinal indicators.

# Higher-order
ob <-aMNLFA.object(dir          = wd, # , indicate the location of the data.
                   mrdata        = df,
                   indicators    = c( "RIT1", "RIT2", "RIT3","RIT4","RIT5", # list a set of indicators of a single factor
                                     "RIT6","RIT7", "RIT8", "RIT9", "RIT10",
                                     "RES1","RES2","RES3","RES4","RES5", "RES6","RES7", "RES8"),
                   catindicators = c("RIT1", "RIT2","RIT3","RIT4","RIT5", # list a set of indicators that are binary or ordinal
                                     "RIT6","RIT7", "RIT8", "RIT9", "RIT10",
                                     "RES1","RES2","RES3","RES4","RES5", "RES6","RES7", "RES8"),
                   time        = c("AGE18"),
                   meanimpact    = c("AGE18", 
                                     "TABLET",
                                     "AGEXTAB", "MALE"),
                   varimpact     = c("AGE18"), # contrast coding of nominal variables
                   measinvar     = c("AGE18", 
                                     "TABLET",
                                     "AGEXTAB", "MALE"),
                   factors       = c("PROJECT", "SEX"),
                   ID            = "ID2",
                   thresholds    = TRUE) # indicate whether you would like to test measurement invariance of thRITholds for ordinal indicators.

####################################################
# 2.	Plot items over time
####################################################
aMNLFA.itemplots(ob)

#################################################### 
# 3.	Draw a calibration sample.
# Sample one calibration per ID. Outputs a calibration file. 
#################################################### 
source(paste(homedir, 'aMNLFA_sample.R', sep=''))
aMNLFA.sample(ob,ru)
#################################################### 
# Optional code - used for adding manual additions to MPLUS script 
# (e.g. constraining variance of moderators so model would converge)
#################################################### 
#calib.dat <- read.delim("/Users/sifre002/Desktop/Invariance/CalibSample2/REP_FIXED/calibration.dat", header=FALSE)
#vars = read.csv("/Users/sifre002/Desktop/Invariance/CalibSample2/REP_FIXED/vars.csv", header=TRUE)
#vars = vars$x
#colnames(calib.dat) = vars
#var(calib.dat$TABLET)
#var(calib.dat$MALE)

####################################################
# 4.	Create Mplus input files for mean impact, variance impact, 
# and item-by-item measurement non-invariance (aMNLFA.initial)
####################################################
source(paste(homedir, 'aMNLFA_initial.R', sep =''))
aMNLFA.initial(ob) 

##################################
# 4. Run Models 
################################
runModels(replaceOutfile = 'never') # This will run all models in the path you set. This will take some time. 

##################################
# 5. Incorporate all ‘marginally significant’ terms into a simultaneous Mplus input file 
################################
source(paste(homedir, 'aMNLFA_simultaneous.R', sep =''))
aMNLFA.simultaneous(ob)
runModels(replaceOutfile = 'never') 
#Running this code results in a single Mplus script file (round2calibration.inp)
#	All mean and variance impact terms with p<.10 are included 

##################################
# 6. Trim non-invariant terms 
################################
source(paste(homedir, 'aMNLFA_final.R', sep = ''))
aMNLFA.final(ob)
runModels(replaceOutfile = 'never') 
# creates round3calibration.inp

##################################
# 7. (only for longitudinal data) Use parameter values generated from the last calibration model to fix
# parameter values in the scoring model using the full, longitudinal dataset
################################
aMNLFA.scores(ob)
#The resulting Mplus script uses the long (mr.dat) data file and outputs factor
#score estimates for each observation. Run the resulting script manually.
# This script produces a file containing factor score estimates if data are cross-sectional.


##################################
# 8. Describe and visualize factor score estimates and generate empirical item
# characteristic curves
##################################
# NOTE: Step 7 saves "scores.dat" in the wrong directory (was in my root dir). 
# Need to move this file to wdir before running this last line. 
source(paste(homedir,'aMNLFA_scoreplots.R', sep = ''))
aMNLFA.scoreplots(ob)

