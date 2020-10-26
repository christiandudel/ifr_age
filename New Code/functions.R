### ungroupIFR ######################################################

  # Arguments:
  # IFR : numeric, age-specific IFRs
  # interval : numeric, age intervals
  # midinterval : numeric, middle of age intervals
  # age : numeric vector, ages for output
  # resolution : numeric, Age steps for output if automatically generated
  # maxage : numeric, Maximum age if automatically generated

  # Function
  ungroupIFR <- function(IFR,interval,midinterval=NULL,
                         age=NULL,resolution=1,maxage=NULL,minage=0) {
  
    # Transform
    lIFR <- log(IFR)
    
    # Mid-interval
    if(!is.null(midinterval)) interval <- interval+midinterval
    
    # Check if intervals match
    nmid <- length(midinterval)
    nint <- length(interval)
    if(nmid!=nint & nmid>1) warning("interval and midinterval do not match")
    
    # Fit splines
    fit <- smooth.spline(y=lIFR,x=interval)
  
    # Ages for prediction
    if(is.null(maxage)&is.null(age)) maxage <- max(interval)+last(diff(interval))
    if(is.null(age)) age <- seq(minage,maxage,by=resolution)
  
    # Predict
    predicted <- predict(fit,x=age)$y
    
    # Exp
    ungrouped <- exp(predicted)
    
    # Result
    return(ungrouped)
   
  }
  

### Match e_x #######################################################  
  
  match_e_x <- function(e_1,e_2,interval,midinterval=NULL,maxage=NULL,minage=0,split=T,
                        resolution=0.1,outputresolution=1) {

    # Check if intervals match
    nmid <- length(midinterval)
    nint <- length(interval)
    if(nmid!=nint & nmid>1) warning("interval and midinterval do not match")
    
    # Set maxage
    if(is.null(maxage)) maxage <- max(interval)+last(diff(interval))
    
    # Mid-interval
    if(!is.null(midinterval)) interval <- interval+midinterval 

    # Splitting
    if(split) {
  
      # Fitting    
      fit1 <- smooth.spline(x=interval,y=e_1)
      fit2 <- smooth.spline(x=interval,y=e_2)
      
      # Ages for prediction
      age <- seq(minage,maxage,by=resolution)
      
      # Predict
      e_1 <- predict(fit1,x=age)$y
      e_2 <- predict(fit2,x=age)$y
      interval <- age
      
    }
    
    # Get output ages
    intage <- seq(minage,maxage,by=outputresolution)
    
    # Reference values
    ref_e <- e_1[which(interval%in%intage)]
    
    # Find matching ages
    diff <- sapply(ref_e,"-",e_2)
    diff <- abs(diff)
    matching <- apply(diff,2,function(x) interval[which.min(x)])
    
    # Return output
    return(matching)
    
  }