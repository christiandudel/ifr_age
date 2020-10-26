### Calculate scenarios #############################################


### General #########################################################

  # Packages
  library(tidyverse)

  # For handling of age
  midinterval <- 2.5
  lastinterval <- 2.5

  # Load data
  IFRs <- read_csv("Output/IFRs.csv")
  Counts <- read_csv("Output/Counts.csv")

  # Data frame for results
  Scenarios <- data.frame(IFR=character(),
                          Cases=character(),
                          Result=character())
  
  
### Age #############################################################
  
  # Add midinterval information
  Counts <- Counts %>% mutate(Age=
                              ifelse(Age==max(Age),
                                     Age+lastinterval,
                                     Age+midinterval))
  
  # Only keep corresponding IFRs
  IFRs <- IFRs %>% filter(Age%in%Counts$Age)

  
### Calculate #######################################################
  
  # Get scenarios
  IFR_scenarios <- names(IFRs)[-1]
  n_IFRs <- length(IFR_scenarios)
  Count_scenarios <- names(Counts)[-1]
  
  # Loop over scenarios for cases
  for(i in Count_scenarios) {
    
    # Death counts? If so apply inverse method
    death <- strsplit(i,split="_")
    death <- death[[1]][2]
    death <- death=="Deaths"
    
    # If death counts...
    if(death) {
      
      # Inverse method
      result <- apply(IFRs[,-1],2,
                      function(x) {
                        sum(Counts[,i])/sum(1/x*Counts[,i])
                        })
      
    # If no death counts...
    } else {
      
      # Normal calculation
      result <- apply(IFRs[,-1],2,
                      function(x) {
                        sum(x*Counts[,i]/sum(Counts[,i]))
                        })
      
    }
    
    # Put into data frame
    result <- data.frame(IFRs=names(result),
                         Cases=rep(i,n_IFRs),
                         Result=result)
    
    # Combine with other results
    Scenarios <- rbind(Scenarios,result)
    
  }
  
  # Edit row names
  rownames(Scenarios) <- paste(1:dim(Scenarios)[1])
  
  # Save
  write.csv(Scenarios,"Output/Scenarios.csv",row.names = F)