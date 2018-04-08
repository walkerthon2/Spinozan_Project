LBA_Fit <- function(params, pData, trialNum) {
  
  #Initialise params ----
  #1st position in array is the "True" accumulator, 2nd is "False", 3rd is "Not Seen"
  
  A <- params[1:3] #Start point bias
  
  B <- params[4:6] #Threshold
  
  if (A[1] > B[1] | A[2] > B[2] | A[3] > B[3]){
    NLL = 10000
    return(NLL)
  }
  
  T0 <- params[7:9] #Motor Response Time
  
  MEAN_V <- params[10:12] #Mean Drift Rates
  
  SD_V <- params[13:15] #SD of the drift rate
  
  #Trials ----
  likelihood <- matrix(nrow = trialNum)
  for (t in 1:trialNum) {
    
    c = pData$Response[t]
    
    #Reorganise so the winning accumulator is first
    hold_A <- A[-c]
    trial_A <- c(A[c], hold_A)
    hold_B <- B[-c]
    trial_B <- c(B[c], hold_B)
    hold_T0 <- T0[-c]
    trial_T0 <- c(T0[c], hold_T0)
    hold_MEAN_V <- MEAN_V[-c]
    trial_MEAN_V <- c(MEAN_V[c], hold_MEAN_V)
    hold_SD_V <- SD_V[-c]
    trial_SD_V <- c(SD_V[c], hold_SD_V)

    
    likelihood[t] <- n1CDF(pData$RT[t], A = trial_A, b = trial_B, t0 = trial_T0, mean_v = trial_MEAN_V, sd_v = trial_SD_V, silent = TRUE)
    
  }
  
  NLL = -sum(log(likelihood), na.rm = TRUE)
  # print(NLL)
  return(NLL)
  
}