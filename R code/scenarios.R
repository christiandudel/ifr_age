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
  IFR_always <- c(IFR_scenarios[1:27])
  n_IFRs <- length(IFR_scenarios)
  Count_scenarios <- names(Counts)[-1]
  
  # Loop over scenarios for cases
  for(i in Count_scenarios) {
    
    # Death counts? If so apply inverse method
    tmp     <- strsplit(i,split="_")
    death   <- tmp[[1]][2]
    death   <- death=="Deaths"
    
    # Get country and corresponding IFR scenarios
    country <- tmp[[1]][1]
    scenarios <- strsplit(IFR_scenarios,split="_")
    scenarios <- lapply(scenarios, function(x) any(x==country) )
    scenarios <- unlist(scenarios)
    scenarios <- c(IFR_always,IFR_scenarios[scenarios])
    n_IFRs <- length(scenarios)
    
    # If death counts...
    if(death) {
      
      # Inverse method
      result <- apply(IFRs[,scenarios],2,
                      function(x) {
                        sum(Counts[,i])/sum(1/x*Counts[,i])
                        })
      
    # If no death counts...
    } else {
      
      # Normal calculation
      result <- apply(IFRs[,scenarios],2,
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
  
  # Country variable
  Scenarios$Country <- unlist(lapply(strsplit(Scenarios$Cases,"_"),function(x) x[[1]]))
  Scenarios$Cases <- unlist(lapply(strsplit(Scenarios$Cases,"_"),function(x) x[[2]]))
  
  # Save
  write.csv(Scenarios,"Output/Scenarios.csv",row.names = F)

