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
    
    # If IFR=0
    IFR[IFR<0.00001] <- 0.00001
  
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
  
  
### ungroup_lx ######################################################
  
  # Ungroup l_x
  ungroup_lx <- function(lx){
    
    # Ages (hard-coded five-year age groups)
    ages <- c(0,1,seq(5,100,5))
    
    # Smoothing spline
    smooth_lx <- smooth.spline(x=ages,y=lx)
    
    # New ages: single years
    new_age <- c(seq(0,1,1),seq(2,5,1),seq(6,100,1))
    
    # Predict from spline
    predict_lx <- predict(smooth_lx,new_age,len=new_age)
    
    # Return
    return(predict_lx$y)
    
  }
  
  
### Calculate life expectancy at age x ##############################
  
  lexp_age_specific <- function(lx,ax,mx){  
    
    # d_x: Deaths aged x
    dx <- c(-diff(lx), lx[length(lx)])  
    
    # L_x: Person years
    Lx1 <- lx[-1]+ax[1:(length(ax)-1)]*dx[1:(length(dx)-1)]  
    Lx2 <- if(mx[length(mx)] == 0) 0 else dx[length(dx)]/mx[length(mx)]  
    Lx <- c(Lx1, Lx2)  
    
    # Cumulative person years  
    Tx <- rev(cumsum(rev(Lx)))  
    
    # Remaining life expectancy at age x
    ex <- Tx/lx  
    
    # If dividion by zero
    ex[is.nan(ex)] <- 0  
    
    # Return result
    return(ex)    
    
  }  
  
  
### Function for weighted row means #################################
  
  rowMeansW <- function(data,weight) {
    
    # Calculate contributions
    tmp <- apply(data,1,function(x) x*weights)
    
    # Sum
    colSums(tmp)
    
  }
  