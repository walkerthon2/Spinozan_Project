readData <- function(participant) {
  #Read in Matlab files using readMat package
  filename <- paste(c("C1_S",participant, ".mat"), collapse = '')
  TD <- readMat(filename)
  #Get only parts where participant makes a response (test trials)
  part1 <- as.tibble(TD$DATA[[6]]) %>% 
    filter(V2 == 2)
  part2 <- as.tibble(TD$DATA[[7]]) %>% 
    filter(V2 == 2)
  
  #Merge the two sections
  tempData <- merge(part1, part2, all = TRUE)
  
  #Create the data frame of relevant variables
  rawData <- tibble(Veracity = tempData$V4, ShockCue = tempData$V5, Shocked = tempData$V6,
                     Response = tempData$V10, RT = tempData$V13)
  
  #Remove foil trials and trials where the participant was shocked
  rawData <- filter(rawData, Veracity != 3, Shocked != 1)
  
  return(rawData)
}