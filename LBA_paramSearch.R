#Load packages
require('tidyverse')
require('nloptr')
require('rtdists')
require("R.matlab")

source('LBA_Fit.R', TRUE)
source('readData.R', TRUE)

#Initialise key variables
trialNum <- 48
pStart <- 1
pEnd <- 1

evalNum <- 500
evalSaveLim <- 500

#Opening parameters
startParams <- c(1, 1, 1, 3, 3, 3, .3, .3, .3, .5, .5, .5, .3, .3, .3)
lower <- c(0, 0, 0, 1, 1, 1, 0, 0, 0, .1, .1, .1, .05, .05, .05)
upper <- c(5, 5, 5, 5, 5, 5, 1, 1, 1, 2, 2, 2, 1, 1, 1)

#Create tibbles to store output ----
Threat_True <- tibble('A_True' = 0, 'A_False' = 0, 'A_NotSeen' = 0,
                         'B_True'  = 0, 'B_False' = 0, 'B_NotSeen' = 0,
                         'T0_True' = 0, 'T0_False' = 0, 'T0_NotSeen' = 0,
                         'MEANDF_True' = 0, 'MEANDF_False' = 0, 'MEANDF_NotSeen' = 0,
                         'SD_True' = 0, 'SD_False' = 0, 'SD_NotSeen' = 0,
                         'NLL' = 0)

Safe_True <- tibble('A_True' = 0, 'A_False' = 0, 'A_NotSeen' = 0,
                      'B_True'  = 0, 'B_False' = 0, 'B_NotSeen' = 0,
                      'T0_True' = 0, 'T0_False' = 0, 'T0_NotSeen' = 0,
                      'MEANDF_True' = 0, 'MEANDF_False' = 0, 'MEANDF_NotSeen' = 0,
                      'SD_True' = 0, 'SD_False' = 0, 'SD_NotSeen' = 0,
                      'NLL' = 0)

Threat_False <- tibble('A_True' = 0, 'A_False' = 0, 'A_NotSeen' = 0,
                      'B_True'  = 0, 'B_False' = 0, 'B_NotSeen' = 0,
                      'T0_True' = 0, 'T0_False' = 0, 'T0_NotSeen' = 0,
                      'MEANDF_True' = 0, 'MEANDF_False' = 0, 'MEANDF_NotSeen' = 0,
                      'SD_True' = 0, 'SD_False' = 0, 'SD_NotSeen' = 0,
                      'NLL' = 0)

Safe_False <- tibble('A_True' = 0, 'A_False' = 0, 'A_NotSeen' = 0,
                      'B_True'  = 0, 'B_False' = 0, 'B_NotSeen' = 0,
                      'T0_True' = 0, 'T0_False' = 0, 'T0_NotSeen' = 0,
                      'MEANDF_True' = 0, 'MEANDF_False' = 0, 'MEANDF_NotSeen' = 0,
                      'SD_True' = 0, 'SD_False' = 0, 'SD_NotSeen' = 0,
                      'NLL' = 0)

# Start Eval Loop ----
  
for (p in pStart:pEnd) {
  
  #read in data
  pData <- readData(p)
  
  TTdata <- filter(pData, Veracity == 1, ShockCue == 2)
  STdata <- filter(pData, Veracity == 1, ShockCue == 1)
  TFdata <- filter(pData, Veracity == 2, ShockCue == 2)
  SFdata <- filter(pData, Veracity == 2, ShockCue == 1)
  
  #NLL = LBA_Fit(startParams, pData, trialNum)
  
  #Optimisation options, first rough pass
  opts <- list("algorithm"="NLOPT_GN_DIRECT",
               "maxeval" = evalNum)
  
  param <- nloptr(eval_f = LBA_Fit, 
                  x0 = startParams,
                  lb = lower,
                  ub = upper, 
                  opt = opts, 
                  pData = TFdata, 
                  trialNum = trialNum/4)
  
  #Optimisation options, first rough pass
  opts <- list("algorithm"="NLOPT_GN_DIRECT_L",
               "maxeval" = evalNum)
  
  param <- nloptr(eval_f = LBA_Fit, 
                  x0 = param$solution,
                  lb = lower,
                  ub = upper, 
                  opt = opts, 
                  pData = TFdata, 
                  trialNum = trialNum/4)
  
  Threat_False[p, 1:15] <- param$solution
  Threat_False[p, 16] <- param$objective
}
