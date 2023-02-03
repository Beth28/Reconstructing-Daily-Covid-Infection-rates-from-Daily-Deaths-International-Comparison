source('Data_code.R')
source('functions_for_complex.R')

choose.data <- function(k) {
  ## somewhat sloppy data selecting code...
  data.set <- c("spain", "belgium","italy", "scotland", "denmark", "sweden", "switzerland", 
                "portugal", "netherlands", "england")[k]
  cat(data.set,"\n")
  if (data.set=="spain") { ## starts 13th feb (day zero)
    dat <- data.frame(deaths=ed_ESP,day=1:length(ed_ESP),dow = rep(1:7,100)[1:length(ed_ESP)])
    deaths <- c(rep(0,9),ed_ESP)
  } else if (data.set == "belgium") { #starts 10th March, 26 days diff
    deaths <- c(rep(0, 35), ed_BEL)
    dat <- data.frame(deaths = ed_BEL, day =1:length(ed_BEL), dow = rep(1:7,100)[1:length(ed_BEL)])
  } else if (data.set == "italy"){ ## starts 24th feb (11 days diif)
    deaths <- c(rep(0, 20), ed_ITA)
    dat <- data.frame(deaths = ed_ITA, day=1:length(ed_ITA),dow = rep(1:7,100)[1:length(ed_ITA)])
  } else if (data.set == "scotland"){ # starts 11th march
    deaths <- c(rep(0, 36), ed_SCOT)
    dat <- data.frame(deaths = ed_SCOT, day=1:length(ed_SCOT),dow = rep(1:7,100)[1:length(ed_SCOT)])
  } else if (data.set == "denmark"){ # 11TH March
    deaths <- c(rep(0, 36), ed_DEN)
    dat <- data.frame(deaths = ed_DEN, day=1:length(ed_DEN),dow = rep(1:7,100)[1:length(ed_DEN)])
  } else if (data.set == "sweden"){ # 9th Mar
    deaths <- c(rep(0, 34), ed_SWED)
    dat <- data.frame(deaths = ed_SWED, day=1:length(ed_SWED),dow = rep(1:7,100)[1:length(ed_SWED)])
  } else if (data.set == "switzerland"){ # 5th Mar
    deaths <- c(rep(0, 30), ed_SWIT)
    dat <- data.frame(deaths = ed_SWIT, day=1:length(ed_SWIT),dow = rep(1:7,100)[1:length(ed_SWIT)])
  } else if (data.set == "portugal"){ # 16th Mar
    deaths <- c(rep(0, 41), ed_POR)
    dat <- data.frame(deaths = ed_POR, day=1:length(ed_POR),dow = rep(1:7,100)[1:length(ed_POR)])
  } else if (data.set == "netherlands") { # 27th Feb
    deaths <- c(rep(0, 23), ed_NLD)
    dat <- data.frame(deaths = ed_NLD, day=1:length(ed_NLD),dow = rep(1:7,100)[1:length(ed_NLD)])
  } else { # England, starts 2nd March
    deaths <- c(rep(0, 27), ed_ENG)
    dat <- data.frame(deaths = ed_ENG, day=1:length(ed_ENG),dow = rep(1:7,100)[1:length(ed_ENG)])
  }
  list(deaths=deaths,dat=dat)
}



## England

dum <- choose.data(10) ## England
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
ks <- 120; bs <- "ad"
beng <- gam(deaths~s(day,k = ks, bs=bs)+s(dow,k=7,bs="cc"),family=nb(),data=dat,
             knots=list(dow=c(0,7)))
theta <- beng$family$getTheta(TRUE)  ## Use this negative binomial theta for full model
nmcmc <- 1000
resl_eng <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=3.235,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)

day <- 1:nc+34 # re-scale day to plot from 31st december, all death vectors start from 4th Feb
par(mfrow=c(2,1),mar=c(5,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_eng,approx=F,last.day=430, lock.down = NA,ylab="England Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=430, origin = 0 ,cex=c0)
abline(v=lock_eng,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_eng,last.day=430, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=425, origin = 0,cex=c0)
abline(v=lock_eng,col=2)




## Scotland


dum <- choose.data(4) ## Scotland
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
ks <- 120; bs <- "ad"
bscot <- gam(deaths~s(day,k = ks, bs=bs)+s(dow,k=7,bs="cc"),family=nb(),data=dat,
            knots=list(dow=c(0,7)))
theta <- bscot$family$getTheta(TRUE)  ## Use this negative binomial theta for full model
nmcmc <- 1000
resl_scot <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=3.235,si2d=.415,
                     full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)

day <- 1:nc+34
par(mfrow=c(2,1),mar=c(5,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_scot,approx=F,last.day=558, lock.down = NA,ylab="Scotland Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_scot,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_scot,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_scot,col=2)


## Belgium


dum <- choose.data(2) ## Belgium
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
ks <- 120; bs <- "ad"
bbel <- gam(deaths~s(day,k = ks, bs=bs)+s(dow,k=7,bs="cc"),family=nb(),data=dat,
            knots=list(dow=c(0,7)))
theta <- bbel$family$getTheta(TRUE)  ## Use this negative binomial theta for full model
nmcmc <- 1000
resl_bel <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=3.235,si2d=.415,
                     full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)



day <- 1:nc+34
par(mfrow=c(2,1),mar=c(5,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_bel,approx=F,last.day=558, lock.down = NA,ylab="Belgium Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_bel,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_bel,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_bel,col=2)
#npi.plot(-2,0,1:14,kl=5:14)



## Italy

dum <- choose.data(3) ## Italy
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
ks <- 120; bs <- "ad"
bita <- gam(deaths~s(day,k = ks, bs=bs)+s(dow,k=7,bs="cc"),family=nb(),data=dat,
             knots=list(dow=c(0,7)))
theta <- bita$family$getTheta(TRUE)  ## Use this negative binomial theta for full model
nmcmc <- 1000
resl_ita <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=3.235,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)


day <- 1:nc+34
par(mfrow=c(2,1),mar=c(5,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_ita,approx=F,last.day=558, lock.down = NA,ylab="Italy Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_ita,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_ita,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_ita,col=2)




## Spain

dum <- choose.data(1) ## Spain
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
ks <- 120; bs <- "ad"
besp <- gam(deaths~s(day,k = ks, bs=bs)+s(dow,k=7,bs="cc"),family=nb(),data=dat,
             knots=list(dow=c(0,7)))
theta <- besp$family$getTheta(TRUE)  ## Use this negative binomial theta for full model
nmcmc <- 1000
resl_esp <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=3.235,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)




day <- 1:nc+34
par(mfrow=c(2,1),mar=c(5,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_esp,approx=F,last.day=558, lock.down = NA,ylab="Spain Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_esp,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_esp,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_esp,col=2)

## Denmark

dum <- choose.data(5) ## Denmark
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
ks <- 50; bs <- "ad"
bdnk <- gam(deaths~s(day,k = ks, bs=bs)+s(dow,k=7,bs="cc"),family=nb(),data=dat,
             knots=list(dow=c(0,7)))
theta <- bdnk$family$getTheta(TRUE)  ## Use this negative binomial theta for full model
nmcmc <- 1000
resl_dnk <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=3.235,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)




day <- 1:nc+34
par(mfrow=c(2,1),mar=c(5,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_dnk,approx=F,last.day=558, lock.down = NA,ylab="Denmark Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_dnk,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_dnk,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_dnk,col=2)




## Sweden 

dum <- choose.data(6) ## Sweden
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
ks <- 120; bs <- "ad"
bswed <- gam(deaths~s(day,k = ks, bs=bs)+s(dow,k=7,bs="cc"),family=nb(),data=dat,
             knots=list(dow=c(0,7)))
theta <- bswed$family$getTheta(TRUE)  ## Use this negative binomial theta for full model
nmcmc <- 1000
resl_swed <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=3.235,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)


day <- 1:nc+34
par(mfrow=c(2,1),mar=c(5,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_swed,approx=F,last.day=558, lock.down = NA,ylab="Sweden Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
points(day,deaths,cex=.5,col="grey")
plotR(resl_ita,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)





## Switzerland

dum <- choose.data(7) ## Switzerland
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
ks <- 120; bs <- "ad"
bswit <- gam(deaths~s(day,k = ks, bs=bs)+s(dow,k=7,bs="cc"),family=nb(),data=dat,
             knots=list(dow=c(0,7)))
theta <- bswit$family$getTheta(TRUE)  ## Use this negative binomial theta for full model
nmcmc <- 1000
resl_swit <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=3.235,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)


day <- 1:nc+34
par(mfrow=c(2,1),mar=c(5,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_swit,approx=F,last.day=558, lock.down = NA,ylab="Switzerland Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_swit,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_swit,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_swit,col=2)

## Portugal

dum <- choose.data(8) ## Portugal
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
ks <- 120; bs <- "ad"
bpor <- gam(deaths~s(day,k = ks, bs=bs)+s(dow,k=7,bs="cc"),family=nb(),data=dat,
             knots=list(dow=c(0,7)))
theta <- bpor$family$getTheta(TRUE)  ## Use this negative binomial theta for full model
nmcmc <- 1000
resl_por <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=3.235,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)



day <- 1:nc+34
par(mfrow=c(2,1),mar=c(5,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_por,approx=F,last.day=558, lock.down = NA,ylab="Portugal Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_por,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_por,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_por,col=2)





## Netherlands

dum <- choose.data(9) ## Netherlands
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
ks <- 120; bs <- "ad"
bnld <- gam(deaths~s(day,k = ks, bs=bs)+s(dow,k=7,bs="cc"),family=nb(),data=dat,
             knots=list(dow=c(0,7)))
theta <- bnld$family$getTheta(TRUE)  ## Use this negative binomial theta for full model
nmcmc <- 1000
resl_nld <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=3.235,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)




day <- 1:nc+34
par(mfrow=c(2,1),mar=c(5,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_nld,approx=F,last.day=558, lock.down = NA,ylab="Netherlands Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_nld,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_nld,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_nld,col=2)
