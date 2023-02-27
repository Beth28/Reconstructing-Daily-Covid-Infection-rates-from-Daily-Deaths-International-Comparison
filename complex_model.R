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


dum <- choose.data(10) ## England
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
ks <- 120; bs <- "ad"
beng <- gam(deaths~s(day,k = ks, bs=bs)+s(dow,k=7,bs="cc"),family=nb(),data=dat,
             knots=list(dow=c(0,7)))
theta <- beng$family$getTheta(TRUE)  ## Use this negative binomial theta for full model
nmcmc <- 1000
resl_eng <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)
lambda_eng <- resl_eng$lambda
resl_eng1 <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                     full.mcmc=TRUE, ks=ks, bs=bs, lambda = lambda_eng/40)


day <- 1:nc+34 # re-scale day to plot from 31st december, all death vectors start from 4th Feb
par(mfrow=c(2,1),mar=c(4,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_eng1,approx=F,last.day=430, lock.down = NA,ylab="England Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=430, origin = 0 ,cex=c0)
abline(v=lock_eng,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_eng1,last.day=430, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=425, origin = 0,cex=c0)
abline(v=lock_eng,col=2)


par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_eng1,beng, ylab = 'England Fatal Infections') 

## Plot all of england together

par(mfrow = c(3,1), mar = c(4,5,2,1))
dum <- choose.data(10) ## England
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
ks <- 120; bs <- "ad"

plot(b_eng,rug=FALSE,scheme=1,n=500,ylim=c(0,1800),ylab="England Daily infections/Deaths",
     xlab="Days since 31 Dec 2019",select=1,cex.lab=0.8);
abline(v=lock_eng);abline(0,0,lty=2)
with(dat_eng,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_eng$tag,fitted(b_eng),col="grey",lwd=2)
month.axis(start=31,stop=420,origin = 0, cex=0.8)

day <- 1:nc+34 # re-scale day to plot from 31st december, all death vectors start from 4th Feb
c1 <- 1.1;c0=.8
plot.ip(resl_eng1,approx=F,last.day=430, lock.down = NA,ylab="England Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=430, origin = 0 ,cex=c0)
abline(v=lock_eng,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_eng1,last.day=430, ylim=c(-2,2),cex=c1)
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
resl_scot <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                     full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)
lambda_scot <- resl_scot$lambda
resl_scot1 <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                       full.mcmc=TRUE, ks=ks, bs=bs, lambda = lambda_scot/40)


day <- 1:nc+34
par(mfrow=c(2,1),mar=c(4,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_scot1,approx=F,last.day=558, lock.down = NA,ylab="Scotland Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_scot,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_scot1,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_scot,col=2)

par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_scot1,bscot, ylab = 'Scotland Fatal Infections') 


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
lambda_bel <- resl_bel$lambda
resl_bel1 <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=3.235,si2d=.415,
                       full.mcmc=TRUE, ks=ks, bs=bs, lambda = lambda_bel/40)



day <- 1:nc+34
par(mfrow=c(2,1),mar=c(4,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_bel1,approx=F,last.day=558, lock.down = NA,ylab="Belgium Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_bel,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_bel1,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_bel,col=2)
#npi.plot(-2,0,1:14,kl=5:14)

par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_bel1,bbel, ylab = 'Belgium Fatal Infections') 


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
lambda_ita <- resl_ita$lambda
resl_ita1 <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=3.235,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = lambda_ita/40)


day <- 1:nc+34
par(mfrow=c(2,1),mar=c(4,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_ita1,approx=F,last.day=558, lock.down = NA,ylab="Italy Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_ita,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_ita1,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_ita,col=2)


par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_ita1,bita, ylab = 'Italy Fatal Infections') 



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
lambda_esp <- resl_esp$lambda
resl_esp1 <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=3.235,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = lambda_esp/40)



day <- 1:nc+34
par(mfrow=c(2,1),mar=c(4,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_esp1,approx=F,last.day=558, lock.down = NA,ylab="Spain Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_esp,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_esp1,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_esp,col=2)

par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_esp1,besp, ylab = 'Spain Fatal Infections') 



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
resl_dnk <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)
lambda_dnk <- resl_dnk$lambda
resl_dnk1 <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                     full.mcmc=TRUE, ks=ks, bs=bs, lambda = lambda_dnk/40)





day <- 1:nc+34
par(mfrow=c(2,1),mar=c(4,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_dnk1,approx=F,last.day=558, lock.down = NA,ylab="Denmark Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_dnk,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_dnk1,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_dnk,col=2)

par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_dnk1,bdnk, ylab = 'Denmark Fatal Infections')



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
resl_swed <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)
lambda_swed <- resl_swed$lambda
resl_swed1 <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = lambda_swed/40)


day <- 1:nc+34
par(mfrow=c(2,1),mar=c(4,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_swed1,approx=F,last.day=558, lock.down = NA,ylab="Sweden Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
points(day,deaths,cex=.5,col="grey")
plotR(resl_swed1,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)


par(mfrow=c(1,1),mar=c(4,5,2,1))

sanity.plot(resl_swed1,bswed, ylab = 'Sweden Fatal Infections') 
points(day,deaths,cex=.5,col="grey")



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
lambda_swit <- resl_swit$lambda
resl_swit1 <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=3.235,si2d=.415,
                       full.mcmc=TRUE, ks=ks, bs=bs, lambda = lambda_swit/20)


day <- 1:nc+34
par(mfrow=c(2,1),mar=c(4,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_swit1,approx=F,last.day=558, lock.down = NA,ylab="Switzerland Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_swit,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_swit1,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_swit,col=2)

par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_swit1,bswit, ylab = 'Switzerland Fatal Infections') 


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
lambda_por <- resl_por$lambda
resl_por1 <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=3.235,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = lambda_por/40)



day <- 1:nc+34
par(mfrow=c(2,1),mar=c(4,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_por1,approx=F,last.day=558, lock.down = NA,ylab="Portugal Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_por,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_por1,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_por,col=2)

par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_por1,bpor, ylab = 'Portugal Fatal Infections') 




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
resl_nld <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)
lambda_nld <- resl_nld$lambda
resl_nld1 <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = lambda_nld/40)




day <- 1:nc+34
par(mfrow=c(2,1),mar=c(4,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_nld1,approx=F,last.day=558, lock.down = NA,ylab="Netherlands Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_nld,col=2);
points(day,deaths,cex=.5,col="grey")
plotR(resl_nld1,last.day=558, ylim=c(-2,2),cex=c1)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_nld,col=2)

par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_nld,bnld, ylab = 'Netherlands Fatal Infections') 


### Plotting in sets of 3 ###

par(mfrow=c(3,1),mar=c(5,5,3,3))

dum <- choose.data(8) ## Portugal
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_por1,approx=F,last.day=558, lock.down = NA,ylab="Portugal Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_por,col=2);
points(day,deaths,cex=.5,col="grey")

dum <- choose.data(4) ## Scotland
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_scot1,approx=F,last.day=558, lock.down = NA,ylab="Scotland Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_scot,col=2);
points(day,deaths,cex=.5,col="grey")

dum <- choose.data(7) ## Switzerland
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_swit1,approx=F,last.day=558, lock.down = NA,ylab="Switzerland Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_swit,col=2);
points(day,deaths,cex=.5,col="grey")

####

par(mfrow=c(3,1),mar=c(5,5,3,3))

dum <- choose.data(8) ## Portugal
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plotR(resl_por1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'Portugal log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_por,col=2)


dum <- choose.data(4) ## Scotland
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plotR(resl_scot1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'Scotland log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_scot,col=2)

dum <- choose.data(7) ## Switzerland
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plotR(resl_swit1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'Switzerland log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_swit,col=2)

###

par(mfrow=c(3,1),mar=c(5,5,3,3))

dum <- choose.data(2) ## Belgium
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_bel1,approx=F,last.day=558, lock.down = NA,ylab="Belgium Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_bel,col=2);
points(day,deaths,cex=.5,col="grey")


dum <- choose.data(3) ## Italy
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_ita1,approx=F,last.day=558, lock.down = NA,ylab="Italy Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_ita,col=2);
points(day,deaths,cex=.5,col="grey")

dum <- choose.data(1) ## Spain
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_esp1,approx=F,last.day=558, lock.down = NA,ylab="Spain Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_esp,col=2);
points(day,deaths,cex=.5,col="grey")



###

par(mfrow=c(3,1),mar=c(5,5,3,3))

dum <- choose.data(2) ## Belgium
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plotR(resl_bel1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'Belgium log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_bel,col=2)


dum <- choose.data(3) ## Italy
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plotR(resl_ita1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'Italy log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_ita,col=2)

dum <- choose.data(1) ## Spain
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plotR(resl_esp1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'Spain log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_esp,col=2)

####
par(mfrow=c(3,1),mar=c(5,5,3,3))

dum <- choose.data(5) ## Denmark
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_dnk1,approx=F,last.day=558, lock.down = NA,ylab="Denmark Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_dnk,col=2);
points(day,deaths,cex=.5,col="grey")


dum <- choose.data(9) ## Netherlands
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_nld1,approx=F,last.day=558, lock.down = NA,ylab="Netherlands Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_nld,col=2);
points(day,deaths,cex=.5,col="grey")


dum <- choose.data(6) ## Sweden
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_swed1,approx=F,last.day=558, lock.down = NA,ylab="Sweden Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
points(day,deaths,cex=.5,col="grey")

###
par(mfrow=c(3,1),mar=c(5,5,3,3))

dum <- choose.data(5) ## Denmark
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plotR(resl_dnk1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'Denmark log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_dnk,col=2)

dum <- choose.data(9) ## Netherlands
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plotR(resl_nld1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'Netherlands log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_nld,col=2)

dum <- choose.data(6) ## Sweden
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plotR(resl_swed1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'Sweden log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)
