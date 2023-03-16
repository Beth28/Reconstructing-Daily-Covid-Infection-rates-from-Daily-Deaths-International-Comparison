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

## England ##

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


## Scotland ##


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



## Belgium ##

dum <- choose.data(2) ## Belgium
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
ks <- 120; bs <- "ad"
bbel <- gam(deaths~s(day,k = ks, bs=bs)+s(dow,k=7,bs="cc"),family=nb(),data=dat,
            knots=list(dow=c(0,7)))
theta <- bbel$family$getTheta(TRUE)  ## Use this negative binomial theta for full model
nmcmc <- 1000
resl_bel <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                     full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)
lambda_bel <- resl_bel$lambda
resl_bel1 <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                       full.mcmc=TRUE, ks=ks, bs=bs, lambda = lambda_bel/40)


## Denmark ##

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



## Italy ##

dum <- choose.data(3) ## Italy
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
ks <- 120; bs <- "ad"
bita <- gam(deaths~s(day,k = ks, bs=bs)+s(dow,k=7,bs="cc"),family=nb(),data=dat,
             knots=list(dow=c(0,7)))
theta <- bita$family$getTheta(TRUE)  ## Use this negative binomial theta for full model
nmcmc <- 1000
resl_ita <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)
lambda_ita <- resl_ita$lambda
resl_ita1 <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = lambda_ita/40)

## Netherlands ##

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

## Portugal ##

dum <- choose.data(8) ## Portugal
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
ks <- 120; bs <- "ad"
bpor <- gam(deaths~s(day,k = ks, bs=bs)+s(dow,k=7,bs="cc"),family=nb(),data=dat,
            knots=list(dow=c(0,7)))
theta <- bpor$family$getTheta(TRUE)  ## Use this negative binomial theta for full model
nmcmc <- 1000
resl_por <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                     full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)
lambda_por <- resl_por$lambda
resl_por1 <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = lambda_por/40)



## Spain ##

dum <- choose.data(1) ## Spain
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
ks <- 120; bs <- "ad"
besp <- gam(deaths~s(day,k = ks, bs=bs)+s(dow,k=7,bs="cc"),family=nb(),data=dat,
             knots=list(dow=c(0,7)))
theta <- besp$family$getTheta(TRUE)  ## Use this negative binomial theta for full model
nmcmc <- 1000
resl_esp <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)
lambda_esp <- resl_esp$lambda
resl_esp1 <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = lambda_esp/40)



## Sweden ##

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



## Switzerland ##

dum <- choose.data(7) ## Switzerland
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
ks <- 120; bs <- "ad"
bswit <- gam(deaths~s(day,k = ks, bs=bs)+s(dow,k=7,bs="cc"),family=nb(),data=dat,
             knots=list(dow=c(0,7)))
theta <- bswit$family$getTheta(TRUE)  ## Use this negative binomial theta for full model
nmcmc <- 1000
resl_swit <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                      full.mcmc=TRUE, ks=ks, bs=bs, lambda = NULL)
lambda_swit <- resl_swit$lambda
resl_swit1 <- full.fit(deaths,day,dow,theta,dilation=0,mcmc=nmcmc,ei2d=NULL,si2d=.415,
                       full.mcmc=TRUE, ks=ks, bs=bs, lambda = lambda_swit/20)






