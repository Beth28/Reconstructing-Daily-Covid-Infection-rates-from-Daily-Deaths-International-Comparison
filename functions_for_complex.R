## Empirical Bayes version of fatal infection profile inference from death series.
## (c) Simon N. Wood, 2020
## This version is with restructuring to put fitting into properly defined functions.
## Includes analysis with renewal model...

## functions for post fit MCMC...

lpi <- function(beta,y,Xf,Xw,B,St,theta,Dev=Dev0) {
  ## log likelihood + prior (excluding fixed constants)
  d <- Dev(beta,y,Xf,Xw,B,theta=theta,deriv=FALSE)
  -drop(d$D +  beta %*% St %*% beta)/2
}

rmvt <- function(n,mu,V,df) {
  ## simulate multivariate t variates  
  y <- rmvn(n,mu*0,V)
  v <- rchisq(n,df=df)
  t(mu + t(sqrt(df/v)*y))
}

dmvt <- function(x,mu,V,df,R=NULL) {
  ## multivariate t log density...
  p <- length(mu);
  if (is.null(R)) R <- chol(V)
  z <- forwardsolve(t(R),x-mu)
  k <- - sum(log(diag(R))) - p*log(df*pi)/2 + lgamma((df+p)/2) - lgamma(df/2)
  k - if (is.matrix(z)) (df+p)*log1p(colSums(z^2)/df)/2 else (df+p)*log1p(sum(z^2)/df)/2
}

dmvn <- function(x,mu,V,R=NULL) {
  ## multivariate normal density mgcv:::rmvn can be used for generation 
  if (is.null(R)) R <- chol(V)
  z <- forwardsolve(t(R),x-mu)
  -colSums(z^2)/2-sum(log(diag(R))) - log(2*pi)*length(mu)/2
}

## Compute R using simple SEIR model...

getR <- function(i,gamma=1/3,delta=.2) {
  ## Compute R implied by incidence/fatal infection profile from simple
  ## SEIR model. dE/dt = i - gamma*E, dI/dt = gamma E - delta*I
  ## R = i/(I*delta). Use 1 day discretized version here. Note that
  ## if all infections are k*i, then k cancels in definition of R.
  n <- length(i)
  E <- 0;I <- i
  for (j in 2:n) {
    E <- i[j-1] + (1-gamma)*E
    I[j] <- gamma*E + (1-delta)*I[j-1]
  }
  return(list(R=i/(I*delta),I=I))
} ## getR

sampleR <- function(res,thin=100,gamma=1/3,delta=1/5) {
  ## Given samples in res$bs and infection profile model matrix in
  ## res$Xf compute sample of R profiles from simple SEIR model. 
  m <- floor(nrow(res$bs)/thin)
  R <- matrix(0,nrow(res$Xf),m)
  for (i in 1:m) {
    f <- exp(res$Xf %*% res$bs[1+(i-1)*thin,1:ncol(res$Xf)])
    R[,i] <- getR(f,gamma,delta)$R
  }
  R
} ## sampleR

sensitivityR <- function(f,g=1:5,d=2:10,gamma=3,delta=5,
                         ylim=NULL,last.day=NULL) {
  ## Sensitivity plots w.r.t. parameters f SEIR model used for R
  R0 <- getR(f,1/gamma,1/delta)$R
  R0[1:delta] <- NA; R <- R0
  day <- 1:length(R0)-21
  xlim <- if (is.null(last.day)) range(day) else c(min(day),last.day)
  if (is.null(ylim)) ylim <- range(Rq)
  c1 <- 1.3
  plot(day,log(R),type="l",lwd=3,ylim=ylim,xlim=xlim,cex.lab=c1)
  
  for (i in 1:length(d)) {
    R <- getR(f,1/gamma,1/d[i])$R
    R[1:(1*d[i])] <- NA
    lines(day,log(R),col="grey")
  }
  
  for (i in 1:length(g)) {
    R <- getR(f,1/g[i],1/delta)$R
    R[1:delta] <- NA
    lines(day,log(R),col="blue",lty=2)
  }
  lines(day,log(R0),lwd=2);abline(v=11,col=2);abline(0,0,col=4)
} ## sensitivityR

plotR <- function(res,last.day=NULL,ylim=NULL,ylab="log(R)",xlab = "Days since 31st December",times=c(-9,0,3,7),cex=1.3) {
  ## plot CI for R computed using infection profile and SEIR model
  R <- sampleR(res)
  n <- nrow(R); day <- 1:n+34
  Rq <- log(apply(R,1,quantile,prob=c(.025,.16,.5,.84,.975)))
  Rq[,1:5] <- NA
  xlim <- if (is.null(last.day)) range(day) else c(min(day),last.day)
  if (is.null(ylim)) ylim <- range(Rq)
  plot(day,Rq[3,],type="l",ylim=ylim,xlim=xlim,ylab=ylab,xlab = xlab, cex.lab=cex)
  polygon(c(day,day[n:1]),c(Rq[1,],Rq[5,n:1]),col="lightgrey",border=NA)
  polygon(c(day,day[n:1]),c(Rq[2,],Rq[4,n:1]),col="grey",border=NA)
  lines(day,Rq[3,]);#abline(v=11,col=2);
  # if (!is.null(times)) for (i in 1:length(times)) abline(v=times[i],lty=i+1)
  abline(0,0,col=4) 
} ## plotR



## Renewal model (see dynamic-model/code.r for derivative checking code) Flaxman model-can remove

renew <- function(X,beta,N=6.7e7,ifr=.006,deriv=TRUE) {
  ## simple renewal model with sensitivities...
  ## Gamma dist used has mean 6.5 and CV 0.62. i.e. shape = 6.5 * .62^2, scale = 1/.62^2.
  ## beta[1] = log(z[1]) - the innoculum
  ## R = exp(X%*%beta[-1])
  ## z is predicted daily infections
  nt <- nrow(X)
  R <- exp(X %*% beta)
  i0 <- exp(beta[1]) ## initial infection
  z <- rep(i0,nt)    ## infections
  p <- length(beta)
  d1z <- matrix(0,p,nt)      ## first deriv infections w.r.t. coefs
  d2z <- array(0,c(p,p,nt))  ## second deriv infections w.r.t. coefs
  d1z[1,1] <- d2z[1,1,1] <- i0 ## deriv z[1] w.r.t. beta[1]
  ## set up renewal model...
  t <- 0:nt+.5; t[1] <- 0
  g <- pgamma(t[-1],shape=6.5*.62^2,scale=1/.62^2) - pgamma(t[-nt],shape=6.5*.62^2,scale=1/.62^2)
  for (t in 2:nt) { ## main iteration
    damp <- (1-sum(z[1:(t-1)])/N)      ## damping as susceptible fraction declines
    rnew <- sum(z[1:(t-1)]*g[(t-1):1]) ## the renewal process itself 
    z[t] <- damp*R[t]*rnew             ## new infections
    ## what follows is just the chain rule applied to the preceding terms...
    if (deriv) {
      d1damp <- -rowSums(d1z[,1:(t-1),drop=FALSE])/N
      d1R <- R[t]*X[t,]
      d1rnew <- colSums(t(d1z[,1:(t-1),drop=FALSE])*g[(t-1):1]) 
      d1z[,t] <- d1damp*R[t]*rnew + damp*d1R*rnew + damp*R[t]*d1rnew
      d2damp <- -apply(d2z[,,1:(t-1),drop=FALSE],c(1,2),sum)/N
      d2R <- R[t] * outer(X[t,],X[t,])
      d2rnew <- apply(d2z[,,1:(t-1),drop=FALSE],c(1,2),function(x) sum(x*g[(t-1):1]))
      d2z[,,t] <- d2damp*R[t]*rnew + outer(d1damp,d1R)*rnew + outer(d1damp,d1rnew)*R[t] +
        outer(d1R,d1damp)*rnew + damp*d2R*rnew + damp*outer(d1R,d1rnew) +
        outer(d1rnew,d1damp)*R[t] + damp * outer(d1rnew,d1R) + damp*R[t]*d2rnew
    } ## if deriv
  }
  ## multiply by ifr to get fatal infection profile...
  list(z=z*ifr,d1z=d1z*ifr,d2z=d2z*ifr)
} ## renew

Devr <- function(beta,y,Xf,Xw,B,theta=30,deriv=TRUE) {
  ## negative binomial deviance, grad and Hessian
  ## for the renewal model in which Rt is the smooth function
  ## controlling the generation of infections
  ## beta - model cofficients
  ## y - response (deaths)
  ## Xf model matrix for log Rt. First column should be zero
  ## to allow first parameter of this model to be used as
  ## log innoculum.
  ## Xw model matrix for weekly cycle in deaths/reported deaths
  ## B the forward matrix mapping infections to death rate
  ## theta - negative binomial theta
  ind <- 1:ncol(Xf)
  betaf <- beta[ind]  ## controls incidence
  betaw <- beta[-ind] ## weekly cycle
  r0 <- renew(Xf,betaf,deriv=deriv) ## run the renewal model and its sensitivities
  delta <- drop(B %*% r0$z) ## expected deaths
  
  etaw <- drop(Xw %*% betaw)
  mu <- exp(log(delta)+etaw)
  
  muth <- mu + theta
  yth <- y + theta
  dev <-  2* sum(y * log(pmax(1, y)/mu) - yth * log(yth/muth))
  if (!deriv) return(list(D=dev))
  
  delta1 <- B %*% t(r0$d1z)
  mu1 <- cbind(mu/delta*delta1,mu*Xw) ## dmu/dbeta
  D1 <- colSums(2*(yth/muth-y/mu)*mu1) ## dD/dbeta
  ## Hessian...
  p <- length(beta)
  pf <- ncol(Xf)
  D2 <- matrix(0,p,p)
  for (i in 1:p) {
    if (i<=pf) {
      mu2 <-  cbind((mu/delta)*(B %*% t(r0$d2z[i,,])), drop(B %*% r0$d1z[i,])*mu/delta*Xw)
    } else {
      ii <- i - pf
      mu2 <- cbind((B%*%t(r0$d1z))*mu/delta*Xw[,ii],(mu*Xw[,ii])*Xw)
    }
    for (j in 1:i) {
      D2[i,j] <- D2[j,i] <- sum(-2 * (yth/muth^2 - y/mu^2)*mu1[,i]*mu1[,j] +
                                  2*(yth/muth-y/mu)*mu2[,j])
    }	    
  }
  list(D=dev,D1=D1,D2=D2,mu=mu,f=r0$z,fw=etaw)
} ## Devr

plotRt <- function(res,last.day=NULL,ylim=NULL,ylab="log(R)",times=c(-9,0,3,7)) {
  ## plot CI for R computed using renewal model
  ii <- 1:ncol(res$Xf)
  lR <- drop(res$Xf %*% res$beta[ii])
  seR <- rowSums(res$Xf*(res$Xf %*% res$Vb[ii,ii]))^.5
  n <- length(lR); day <- 1:n-21
  Rq <- rbind(lR+2*seR,lR+seR,lR,lR-seR,lR-2*seR) 
  #Rq <- log(apply(R,1,quantile,prob=c(.025,.16,.5,.84,.975)))
  #Rq[,1:5] <- NA
  xlim <- if (is.null(last.day)) range(day) else c(min(day),last.day)
  if (is.null(ylim)) ylim <- range(Rq)
  c1 <- 1.3
  plot(day,Rq[3,],type="l",ylim=ylim,xlim=xlim,ylab=ylab,cex.lab=c1)
  polygon(c(day,day[n:1]),c(Rq[1,],Rq[5,n:1]),col="lightgrey",border=NA)
  polygon(c(day,day[n:1]),c(Rq[2,],Rq[4,n:1]),col="grey",border=NA)
  lines(day,Rq[3,]);abline(v=11,col=2);
  if (!is.null(times)) for (i in 1:length(times)) abline(v=times[i],lty=i+1)
  abline(0,0,col=4) 
} ## plotRt


## Main model functions...

Dev0 <- function(beta,y,Xf,Xw,B,theta=30,deriv=TRUE) {
  ## negative binomial deviance, grad and Hessian
  ## beta - model cofficients
  ## y - response (deaths)
  ## Xf model matrix for smooth underlying infection rate
  ## Xw model matrix for weekly cycle in deaths/reported deaths
  ## B the forward matrix mapping infections to death rate
  ## theta - negative binomial theta
  ind <- 1:ncol(Xf)
  betaf <- beta[ind]
  betaw <- beta[-ind]
  eta <- drop(Xf %*% betaf)
  f <- exp(eta)
  muf <- drop(B %*% f)
  muf1 <- B %*% (f*Xf)
  etaf1 <- muf1/muf ## detaf/dbeta
  etaw <- drop(Xw %*% betaw)
  mu <- exp(log(muf)+etaw)
  mu1 <- cbind(mu*etaf1,mu*Xw) ## dmu/dbeta
  muth <- mu + theta
  yth <- y + theta
  dev <-  2* sum(y * log(pmax(1, y)/mu) - yth * log(yth/muth))
  if (!deriv) return(list(D=dev))
  D1 <- colSums(2*(yth/muth-y/mu)*mu1) ## dD/dbeta
  ## Hessian...
  p <- length(beta)
  pf <- ncol(Xf)
  D2 <- matrix(0,p,p)
  for (i in 1:p) {
    if (i<=pf) {
      mu2 <-  cbind((mu/muf)*(B %*% (f*Xf*Xf[,i])), etaf1[,i]*mu*Xw)
    } else {
      ii <- i - pf
      mu2 <- cbind(etaf1*mu*Xw[,ii],(mu*Xw[,ii])*Xw)
    }
    for (j in 1:i) {
      D2[i,j] <- D2[j,i] <- sum(-2 * (yth/muth^2 - y/mu^2)*mu1[,i]*mu1[,j] +
                                  2*(yth/muth-y/mu)*mu2[,j])
    }	    
  }
  list(D=dev,D1=D1,D2=D2,mu=mu,f=f,fw=etaw)
} ## Dev0

fit1 <- function(beta,sp,y,Xf,Xw,B,S,theta=30,tol=1e-7,Dev=Dev0) {
  ## fit smooth model by Newton iteration, given smoothing parameters...
  d <- Dev(beta,y,Xf,Xw,B,theta=theta)
  p <-  length(beta)
  St <- matrix(0,p,p)
  ii <- 1:ncol(Xf);
  k <- length(S)-1
  for (j in 1:k) St[ii,ii] <- St[ii,ii] + sp[j]*S[[j]]
  ii <- ncol(Xf)+1:ncol(Xw);St[ii,ii] <- sp[k+1]*S[[k+1]]
  pdev <- drop(d$D +  beta %*% St %*% beta)
  nok <- TRUE
  while (nok) {
    eh <- eigen(d$D2 + 2* St) ## eigen decomp of penalized Hessian
    iv <- eh$values;thresh <- if (min(iv)<0) max(iv)*1e-5 else  max(iv)*1e-10 
    iv[iv<thresh] <- thresh 
    iv <- 1/iv   ## perturb to +ve def (guarantee descent direction)
    gr <- drop(d$D1 + St%*%beta*2) ## add smoothing penalty/prior grad to dev grad
    if (all(abs(gr)<pdev*tol)) break ## converged
    step <- - drop(eh$vectors %*% (iv*(t(eh$vectors) %*% gr))) ## Newton step
    pdev1 <- 2*pdev
    while (!is.finite(pdev1)||pdev1>pdev+1e-12) { ## Newton step control loop
      beta1 <- beta + step
      d1 <- Dev(beta1,y,Xf,Xw,B,theta=theta)
      pdev1 <- drop(d1$D + beta1 %*% St %*% beta1)
      if (!is.finite(pdev1)||pdev1>pdev) step <- step/ if (is.finite(pdev1)) 2 else 100
      step.fail <- all.equal(beta+step,beta,tolerance=.Machine$double.eps^.75)==TRUE
      if (step.fail) break
    }
    if (step.fail) break
    d <- d1;pdev <- pdev1;beta <- beta1
  }
  #Vb=solve(d$D2/2 + St)
  Vb=chol2inv(chol(d$D2/2 + St))
  list(beta=beta,Vb=Vb,step.fail=step.fail)
} ## fit1

dconv <- function(p0,p1) {
  ## convolve discrete distributions p0 and p1 on [0,1,2,...]
  n0 <- length(p0)
  n1 <- length(p1)
  p <- p0*0 ## output array
  ind <- ii <- 1:n1
  for (i in 1:n0) { ## convolution loop
    p[ii] <- p[ii] + p0[i]*p1[ind]
    ii <- ii+1 ## move p index on 1 place
    x <- ii<=n0 ## don't go beyond array end
    ii <- ii[x] 
    ind <- ind[x]
  }
  p[p==0] <- .Machine$double.eps
  p/sum(p) ## return standardized
} ## dconv



full.fit <- function(deaths,day,dow,theta,dilation=0,mcmc=TRUE,ei2d=3.19,si2d=.44,
                     renew=FALSE,full.mcmc = FALSE,ks=20,bs="tp",lambda=NULL) {
  ## fit model - dilation 0 for none, 4 for as paper.
  ## log(i2d) ~ N(ei2d,si2d^2)
  ## NB theta must be supplied here - usually from simple smooth additive fit to deaths
  day1 <- day;
  dday <- 10
  ii <- day>dday-1;day1[ii] <- day1[ii]+dilation/2
  ii <- day>dday;day1[ii] <- day1[ii]+dilation
  ii <- day>dday+1;day1[ii] <- day1[ii]+dilation/2
  
  sm <- smoothCon(s(day,k=ks,bs=bs),data=data.frame(day=day1))[[1]]
  eps <- 1e-4
  Xg <- PredictMat(sm,data.frame(day=day1+eps))
  smw <- smoothCon(s(dow,k=7,bs="cc"),data=data.frame(dow=dow),
                   absorb.cons=TRUE,knots=list(dow=c(0,7)))[[1]]
  
  Xf <- sm$X;Xw <- smw$X;
  Xg <- (Xg-sm$X)/eps ## the infection smooth grad...
  S <- sm$S;S[[length(S)+1]] <- smw$S[[1]]
  #S <- list(sm$S[[1]],smw$S[[1]])
  if (renew) { ## then use simple epidemic model
    if (FALSE) { ## force step change
      Xf <- cbind(0,Xf,as.numeric(day>dday)) ## expand to allow initial condition estimation
      for (j in 1:(length(S)-1)) S[[j]] <- rbind(0,cbind(0,S[[j]],0),0) ## pad S[[1]] accordingly
    } else {
      Xf <- cbind(0,Xf) ## expand to allow initial condition estimation
      for (j in 1:(length(S)-1)) S[[j]] <- rbind(0,cbind(0,S[[j]])) ## pad S[[1]] accordingly
    }
    Dev <- Devr ## use alternative Dev
    yy <- c(rep(1,30),rep(-.3,nrow(Xf)-30))
    beta <- rep(0,ncol(Xf)+ncol(Xw));
    bb <- coef(lm(yy~Xf[,-1]-1))
    beta[1+1:length(bb)] <- bb
    beta[1] <- 8.5
  } else {
    beta <- rep(0,ncol(Xf)+ncol(Xw))
    Dev=Dev0
  }  
  nc <- length(deaths)
  ## Set up matrix mapping infections to future deaths based on published
  ## incubation and disease duration distributions...
  if (is.null(ei2d)) {
    ## convolve ISARIC onset to death with McAloon infection to onset dist...
    ## pd[1] is prob of duration 0, pd[2] of diration 1, etc 
    d <- dconv(dlnorm(0:nc,2.840799,0.5719524),dlnorm(0:40,1.63,.5))
  } else {
    d <- dlnorm(0:nc,ei2d,si2d)
    d[d==0] <- .Machine$double.eps
  }  
  B <- matrix(0,nc,nc)
  for (i in 1:nc) { ## map case rate day i-1 to death rate day i ...
    #B[,i] <- c(rep(0,i-1),d[2:(nc-i+1)])
    B[,i] <- if (i) c(rep(0,i-1),d[1:(nc-i+1)])
  }
  pd <- d[-1]
  ## Empirical Bayes model estimation. NB theta as supplied, smoothing parameters to
  ## maximize approximate Laplace approximate Marginal Likelihood by Extended
  ## Fellner Schall algorithm (Wood and Fasiolo (2017) Biometrics)... 
  
  
  if (is.null(lambda)) {
    lambda <- rep(1,length(S));
    fixed.lambda <- FALSE
  } else fixed.lambda <- TRUE
  
  rank <- c(sm$rank,smw$rank)
  iif <- 1:ncol(Xf);iiw <- ncol(Xf) + 1:ncol(Xw)
  for (i in 1:20) {
    ## run Newton for fit given smoothing params, lambda...
    f <- fit1(beta,lambda,deaths,Xf,Xw,B,S,theta=theta,tol=1e-7,Dev=Dev)
    beta <- f$beta;Vb <- f$Vb;
    d <- Dev(beta,deaths,Xf,Xw,B,theta=theta) ## extract best fit deviance + derivs
    if (i==1) D0 <- d$D else {
      if (abs(D0-d$D)<1e-3*d$D) break ## stop when fit change small
      D0 <- d$D
    }
    if (fixed.lambda) break
    ## Commented out forces Hessian of Deviance to be +ve def, not just
    ## Hessian of penalized deviance. Only needed if lambda iteration
    ## diverges to negative
    #ed <- eigen(d$D2/2)
    #ed$values[ed$values<0] <- 0
    #D2 <- ed$vectors%*%(ed$values*t(ed$vectors))
    #Vb <- solve(D2+lambda*sm$S[[1]])
    
    ## update smoothing parameters, lambda...
    k <- length(S) - 1
    for (j in 1:k)  {
      mult <- drop((rank[j]/lambda[j] - sum(Vb[iif,iif]*S[[j]]))/
                     (beta[iif]%*%S[[j]]%*%beta[iif]))
      if (mult>100) mult <- 100;if (mult<0.01) mult <- 0.01
      lambda[j] <- lambda[j]*mult 
    }		  
    k <- k + 1		  
    mult <- drop((rank[k]/lambda[k] - sum(Vb[iiw,iiw]*S[[k]]))/
                   (beta[iiw]%*%S[[k]]%*%beta[iiw]))
    if (mult>100) mult <- 100;if (mult<0.01) mult <- 0.01
    lambda[k] <- lambda[k]*mult 
    
  } ## update loop
  
  ret <- list(Xf=Xf,Xw=Xw,Xg=Xg,S=S,beta=beta,Vb=Vb,B=B,deaths=deaths,theta=theta,
              ei2d=ei2d,si2d=si2d,lambda=lambda)
  
  if (mcmc) { ## Metropolis Hastings simulation to improve posterior inference
    ## total penalty matrix
    p <- length(beta);St <- matrix(0,p,p)
    ii <- 1:ncol(Xf);k <- length(S)-1
    for (j in 1:k) St[ii,ii] <- St[ii,ii] + lambda[j]*S[[j]]
    ii <- ncol(Xf)+1:ncol(Xw);St[ii,ii] <- lambda[k+1]*S[[k+1]]
    
    ns <- if (is.numeric(mcmc)) mcmc else 100000;
    t.df <- 4
    bp <- rmvt(ns,beta,Vb,df=t.df) ## beta proposals
    lfp <- dmvt(t(bp),beta,Vb,df=t.df) ## log proposal density
    
    R <- chol(Vb) 
    step <- rmvn(ns,beta*0,Vb/4) ## random walk steps (mgcv::rmvn)
    
    u <- runif(ns);us <- runif(ns) ## for acceptance check
    
    bs <- bp;j <- 1;accept <- 0
    lpi0 <- lpi(bs[1,],deaths,Xf,Xw,B,St,theta,Dev=Dev)
    for (i in 2:ns) { ## MH loop
      ## first a static proposal...
      lpi1 <- lpi(bs[i,],deaths,Xf,Xw,B,St,theta,Dev=Dev)
      if (u[i] < exp(lfp[j]-lfp[i]+lpi1-lpi0)) {
        lpi0 <- lpi1;accept <- accept + 1
        j <- i ## row of bs containing last accepted beta
      } else bs[i,] <- bs[i-1,]
      ## now a random walk proposal...
      lpi1 <- lpi(bs[i,]+step[i,],deaths,Xf,Xw,B,St,theta,Dev=Dev)
      if (us[i] < exp(lpi1-lpi0)) { ## accept random walk step
        lpi0 <- lpi1;j <- i
        bs[i,] <- bs[i,] + step[i,]
        lfp[i] <- dmvt(bs[i,],beta,Vb,df=4,R=R) ## have to update static proposal density
      } 
      if (i%%2000==0) cat(".")
    }
    accept <- accept/ns
    ii <- 1:ncol(Xf); fi <- Xf%*%t(bs[,ii])
    fsim <- exp(apply(fi,1,quantile,probs=c(.025,.16,.5,.84,.975)))
    ret$accept <- accept;ret$fsim <- fsim;ret$bs <- bs
    if (full.mcmc) ret$fi <- fi
  } ## if (mcmc)
  ret
} ## full.fit


## Now for plotting the results...

plot.ip <- function(res,mcmc=TRUE,approx=TRUE,last.day=NULL,lock.down=11,
                    ylab="fatal infections", xlab = "Days since 31st December",c1=1,renew=FALSE,plot.peak=TRUE) {
  ## infection profile plotting
  if (is.null(res$fsim)) mcmc <- FALSE
  iif <- 1:ncol(res$Xf)
  se <- rowSums((res$Xf %*% res$Vb[iif,iif])*res$Xf)^.5
  Dev <- if (renew) Devr else Dev0
  d <- Dev(res$beta,res$deaths,res$Xf,res$Xw,res$B,theta=res$theta)
  lag <- 34 ## get day zero timing right at 13th March  
  day <- 1:length(res$deaths)+lag
  xlim <- if (is.null(last.day)) range(day) else c(min(day),last.day)
  ll2 <- exp(log(d$f)-2*se);ul2 <- exp(log(d$f)+2*se)
  ul3 <- exp(log(d$f)+3*se)
  n <- length(ll2)
  ll <- exp(log(d$f)-se);ul <- exp(log(d$f)+se)
  ii <- 1:(length(d$f)-23);yl<- max(ul3[ii])
  plot(day,d$f,type="l",ylim=c(0,yl),ylab=ylab, xlab = xlab,xlim=xlim,cex.lab=c1)
  ll <- exp(log(d$f)-se);ul <- exp(log(d$f)+se)
  if (mcmc) {
    polygon(c(day,day[n:1]),c(res$fsim[1,],res$fsim[5,n:1]),col="lightgrey",border=NA)
    polygon(c(day,day[n:1]),c(res$fsim[2,],res$fsim[4,n:1]),col="grey",border=NA)
    lines(day,res$fsim[3,])
    if (approx) {
      lines(day,ll2,lty=3,col=4);lines(day,ul2,lty=3,col=4)
      lines(day,ll,lty=2,col=4);lines(day,ul,lty=2,col=4)
      lines(day,d$f,col=4)
    }
  } else {
    polygon(c(day,day[n:1]),c(ul2,ll2[n:1]),col="lightgrey",border=NA)
    polygon(c(day,day[n:1]),c(ul,ll[n:1]),col="grey",border=NA)
    lines(day,d$f)
  }
  ## add the posterior for peak location
  if (plot.peak) {
    nb <- if (renew) 5000 else 10000
    bb <- if (mcmc) res$bs[,iif] else rmvn(nb,res$beta[iif],res$Vb[iif,iif])
    if (renew) { ## have to run the model to get infection profiles and their peaks
      fb <- matrix(0,nb,nrow(res$Xf)) 
      for (i in 1:nb) fb[i,] <- renew(res$Xf,bb[i,],deriv=FALSE)$z
    } else fb <- bb %*% t(res$Xf)
    peak <- apply(fb[,ii],1,function(x) which(x==max(x)))
    pt <- tabulate(peak)
    scale <- 0.1*yl/max(pt)
    for (i in 1:length(pt)) if (pt[i]>4) lines(c(i-lag,i-lag),c(0,pt[i]*scale),lwd=2,col=4)
    tp <- which(pt==max(pt)) - lag
    mtext(tp,side=1,line=0,at=tp,col=4,cex=.7)
  }  
  if (is.finite(lock.down)) {
    abline(v=lock.down,col=2) ## 1st day of lockdown
    mtext(lock.down,side=1,line=1,at=lock.down,col=2,cex=.7)
  }
  
} ## plot.ip


sanity.plot <- function(res,b,ylab,c1=1,c2=1,Dev=Dev0) {
  ## Sanity check the posterior mode infection profile.
  ## b is a simple gam death model fit, res is a full infection profile fit 
  d <- Dev(res$beta,res$deaths,res$Xf,res$Xw,res$B,theta=res$theta)
  fi <- round(d$f)
  n <- length(fi)
  n.rep <- 100
  death <- matrix(0,n.rep,n)
  for (j in 1:n.rep) for (i in 1:n) {
    t <- if (is.null(res$ei2d)) sample(1:length(res$d),fi[i],replace=TRUE,prob=res$d) else
      round(rlnorm(fi[i],res$ei2d,res$si2d)) ## sample durations
    t <- t[i+t<=n] ## discard deaths beyond end of data
    if (length(t)>0) {
      dd <- tabulate(t) ## count up durations, starting at 1
      ii <- 1:length(dd)+i # -1 
      death[j,ii] <- death[j,ii] + dd
    }  
  }
  lag <- 21 ## get day zero timing right at 13th March  
  day <- 1:length(res$deaths)-lag
  lag2 <- 34
  plot(day+ lag2,death[1,],type="l",ylim=c(min(res$deaths),max(res$deaths)+25),col="grey",xlab="Days since 31st December",ylab=ylab,cex.lab=c1,cex.axis=c2)
  for (j in 2:n.rep) lines(day+lag2,death[j,],col="grey")
  
  X <- model.matrix(b);X[,((ncol(X)-4):ncol(X))] <- 0    ## UK model matrix, weekly removed
  betab <- coef(b); Vbb <- vcov(b); Xj <- X
  fv <- Xj%*%betab                 ## death rates - link scale
  se <- rowSums(Xj*(Xj%*%Vbb))^.5  ## corresponding s.e.
  n <- length(fv)
  ii <- (length(day)-n+1):length(day)
  lines(day[ii]+lag2,exp(fv))
  lines(day[ii]+lag2,exp(fv+2*se),lty=2)
  lines(day[ii]+lag2,exp(fv-2*se),lty=2)
  points(day+lag2,res$deaths,col="blue")
} ## sanity.plot

r.plot <- function(res,mcmc=TRUE,last.day=NULL,ylab="r",c1=1) {
  ## 'r' plot...
  day <- 1:length(res$deaths) - 21 #- 1 ## no instant death
  n <- length(day);ii <- 1:ncol(res$Xg)
  if (mcmc) {
    r <- res$Xg %*% t(res$bs[,ii])
    rq <- apply(r,1,quantile,probs=c(.025,.16,.5,.84,.975)) ## get profile CIs
  } else {
    rq <- matrix(0,5,n)
    rq[3,] <- r <- res$Xg %*% res$beta[ii]
    se <- rowSums(res$Xg*(res$Xg%*%res$Vb[ii,ii]))^.5
    rq[1,] <- r - 2*se;rq[2,] <-  r - se
    rq[5,] <- r + 2*se;rq[4,] <-  r + se
  }
  xlim <- if (is.null(last.day)) range(day) else c(min(day),last.day)
  plot(day,rq[3,],type="l",ylim=range(rq),xlim=xlim,ylab=ylab,cex.lab=c1)
  polygon(c(day,day[n:1]),c(rq[1,],rq[5,n:1]),col="lightgrey",border=NA)
  polygon(c(day,day[n:1]),c(rq[2,],rq[4,n:1]),col="grey",border=NA)
  lines(day,rq[3,])
  abline(v=11,col=2) ## 1st day of lockdown
  abline(0,0,col=4)
} ## r.plot

month.axis <- function(start=1,stop=425,origin=44,cex=1) {
  ## plot month scale on top axis... origin is day zero for original plot 13th March is 73
  end <- c(0,31,60,91,121,152,182,213,244,274,305,335,366,397,425, 456, 486, 517, 547, 578)
  month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar", "Apr", "May", "Jun", "Jul")
  k0 <- min(which(end > start))
  k1 <- max(which(end < stop))+1
  for (k in k0:k1) {
    at <- c(end[k-1]+1,end[k]) - origin
    axis(3,at=at,tck=-.04,labels=FALSE,lwd.ticks=2)
    axis(3,at=at[1]:at[2],tck=-.01,labels=FALSE)
    if (mean(at)>start-origin&&mean(at)<stop-origin) mtext(month[k-1],3,at=mean(at),line=.5,cex=cex)
  }
} ## month.axis

npi.plot <- function(ypos,adj=1, k = 1:6,kl=NULL,origin=73,cex=1) {
  
  times <- c(64,73,76,80,84,132,310,371,167,245,186,258,288,336)-origin
  lab <- c("public information","symptomatic self isolation","home working","schools close","lockdown","initial easing","lockdown 2", "lockdown 3","shops re-open","schools re-open","restaurants re-open","rule of 6","tier system",
           "lockdown end")
  col <- c(1,1,1,1,2,"blue",2,2,"blue","blue","blue",1,1,"blue")
  lty <- c(3,3,3,3,1,2,1,1,2,2,2,3,3,2)
  if (is.null(kl)) kl <- k
  for (i in k) abline(v=times[i],col=col[i],lty=lty[i])
  for (i in kl) text(times[i]+adj,ypos,lab[i],srt=90,pos=4,col=col[i],cex=cex)
}

#setwd("~sw283/research.notes/covid19/death-series/biometrics/code")

