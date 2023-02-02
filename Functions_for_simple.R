library(mgcv)


# returns data in correct format
simple_model_dat <- function(ed, date, weekday) {
  a <- julian(as.Date(date),origin=as.Date("2019-12-31")) ## start of data
  dat <- data.frame(tote = ed,tag = (a-1) + 1:length(ed))
  n <- nrow(dat)
  b <- weekday
  days <- c(b, b+1, b+2, b+3, b+4, b+5, b+6) %% 7
  for (i in 1:7) {
    if (days[i] == 0) {
      days[i] = 7
    }
  }
  dat$wt <- rep(days,length.out=n) ## day of week - day 1 monday
  dat
}

# fits simple model
simple_model_b <- function(ed, date, weekday) {
  a <- julian(as.Date(date),origin=as.Date("2019-12-31")) ## start of data
  dat <- data.frame(tote = ed,tag = (a-1) + 1:length(ed))
  n <- nrow(dat)
  b <- weekday
  days <- c(b, b+1, b+2, b+3, b+4, b+5, b+6) %% 7
  for (i in 1:7) {
    if (days[i] == 0) {
      days[i] = 7
    }
  }
  dat$wt <- rep(days,length.out=n) ## day of week - day 1 monday
  n <- nrow(dat)
  k <- round(n*.9) ## parameters for spline smooth
  
  ## params of ISARIC based infection to death distribution 
  ## lognorm -  mean = exp(mu + sig2/2); var = (exp(sig2)-1)mean^2
  ##            sig2 = log(v/m^2+1); mu = log(m) - sig2/2
  sig <- log(12^2/27.7^2+1)^.5; mu <- log(27.7) - sig^2/2 
  
  ## first estimate Poisson model to get weights for linear scale regression
  ## death_i = sum_j s(day_i-lag_j) dln(lag_j)
  b0 <- gam(tote~s(tag,k=k),family=poisson,data=dat,method="REML")
  
  ## Now set up to fit model as in ?linear.functional.terms 
  Dp <- 80 ## how many lags 
  lag <- 1:Dp-1 ## the daily lags
  dln <- dlnorm(lag,mu,sig) ## evaluate inf to death dist
  #plot(lag,dln) ## visualize it
  Tag <- matrix(rep(dat$tag,Dp)-rep(lag,each=n),n,Dp) ## The lagged day matrix
  W <- matrix(dln,n,Dp,byrow=TRUE)  ## The weighting matrix probability of this duration
  ## We know that early cases will almost all be short duration, so it is not
  ## sensible to infer to Dp days before outbreak...
  tag0 <- min(dat$tag) - 21 ## first day of infection to try to infer 
  ii <- Tag < tag0  ## days before day0 in 'Tag' matrix
  Tag[ii] <- tag0-1 ## set them all to the day before first
  W[ii] <- 0 ## and down-weight them to make no contribution
  ## Similarly don't try to infer to end of death data run - not enough info.
  tag1 <- max(dat$tag) - 10 ## last day to try to infer
  ii <- Tag > tag1 ## days beyond that
  Tag[ii] <- tag1 + 1 ## set them to day after day1
  W[ii] <- 0 ## and downweight to zero
  d2 <- list(Tag=Tag,W=W,tote=dat$tote,wt=dat$w)
  w <- 1/fitted(b0)
  
  ## Fit model for inferring daily infections....
  b <- gam(tote~s(Tag,by=W,k=k,bs="ad",m=8,pc=list(Tag=40))+s(wt,k=7,bs="cc")-1,weights=w,
           method="REML",knots=list(wt=c(0,7)),data=d2) ## weekly term allows for slight weekly ripple in rates
  b
}




  
