testing <- read.csv('Data/England_testing_data.csv', header = TRUE, col.names = c('Date', 'period', 'pos_test', 'LB', 'UB'))

testing <- testing %>%
  mutate(date_time = julian(as.Date(Date, format = "%d/%m/%Y"),origin=as.Date("2019-12-31", format = "%Y-%m-%d")))

par(mar=c(5,5,1,1),mfrow=c(1,1))
plot(testing$date_time, testing$pos_test, xlab = "Days since 31/12/2019", ylab = "Number of positive tests")
arrows(x0 = testing$date_time, y0 = testing$LB, x1 = testing$date_time, y1 = testing$UB, code =3, angle = 90, length = .01)


day <- 1:nc+34 # re-scale day to plot from 31st december, all death vectors start from 4th Feb
par(mfrow=c(2,1),mar=c(4,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_eng1,approx=F,last.day=430, lock.down = NA,ylab="England Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=430, origin = 0 ,cex=c0)
abline(v=lock_eng,col=2)
#points(day,deaths,cex=.5,col="grey")
plot(testing$date_time, testing$pos_test, cex = .5, col = "black", xlim = c(35, 430))
abline(v=lock_eng,col=2)
arrows(x0 = testing$date_time, y0 = testing$LB, x1 = testing$date_time, y1 = testing$UB, code =3, angle = 90, length = .01)
month.axis(start=26,stop=430, origin = 0 ,cex=c0)




testing_scot <- read.csv('Data/Scotland_testing_data.csv', header = TRUE, col.names = c('Date', 'period', 'pos_test', 'LB', 'UB'))

testing_scot <- testing_scot %>%
  mutate(date_time = julian(as.Date(Date, format = "%d/%m/%Y"),origin=as.Date("2019-12-31", format = "%Y-%m-%d")))

par(mar=c(5,5,1,1),mfrow=c(1,1))
plot(testing_scot$date_time, testing_scot$pos_test, xlab = "Days since 31/12/2019", ylab = "Number of positive tests")
arrows(x0 = testing_scot$date_time, y0 = testing_scot$LB, x1 = testing_scot$date_time, y1 = testing_scot$UB, code =3, angle = 90, length = .01)


day <- 1:nc+34
par(mfrow=c(2,1),mar=c(4,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_scot1,approx=F,last.day=558, lock.down = NA,ylab="Scotland Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_scot,col=2);
points(day,deaths,cex=.5,col="grey")
plot(testing_scot$date_time, testing_scot$pos_test, xlab = "Days since 31/12/2019", ylab = "Number of positive tests", cex = .5, col = "black", xlim = c(35, 558))
arrows(x0 = testing_scot$date_time, y0 = testing_scot$LB, x1 = testing_scot$date_time, y1 = testing_scot$UB, code =3, angle = 90, length = .01)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_scot,col=2)



## Comparing distributions

Dp <- 80 ## how many lags 


## params of ISARIC based infection to death distribution 
## lognorm -  mean = exp(mu + sig2/2); var = (exp(sig2)-1)mean^2
##            sig2 = log(v/m^2+1); mu = log(m) - sig2/2


ei2d=3.235
si2d=.415
lag <- 1:Dp-1 
dln <- dlnorm(lag,ei2d,si2d) ## evaluate inf to death dist
pd <- dconv(dlnorm(0:100,2.840799,0.5719524),dlnorm(0:40,1.63,.5)) # convolution
dln_2 <- pd[1:Dp]

par(mfrow=c(1,1),mar=c(5,5,2,1))
plot(lag, dln_2, xlab = "Day", ylab = "Density", pch=16)
points(lag, dln, col = "grey", pch = 16)
legend("topright", legend = c("Convolution", "lognormal(3.235, 0.415)"), col = c("black", "grey"), pch = 16)


### PLOTTING ###

### England ###
par(mfrow=c(3,1),mar=c(5,5,3,3))
plot(b_eng,rug=FALSE,scheme=1,n=500,xlim = c(35,430),ylim=c(0,1800),ylab="Simple model infection trajectory",
     xlab="Days since 31st December",select=1,cex.lab=0.8);
abline(v=lock_eng, col=2);abline(0,0,lty=2)
with(dat_eng,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_eng$tag,fitted(b_eng),col="grey",lwd=2)
month.axis(start=31,stop=420,origin = 0, cex=0.8)


dum <- choose.data(10) ## England
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34 
c1 <- 1.1;c0=.8
plot.ip(resl_eng1,approx=F,last.day=430, lock.down = NA,ylab="Complex model infection trajectory", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=430, origin = 0 ,cex=c0)
abline(v=lock_eng,col=2);
points(day,deaths,cex=.5,col=1,pch=19)
lines(dat_eng$tag,fitted(beng),col="grey",lwd=2)


dum <- choose.data(10) ## England
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34 
c1 <- 1.1;c0=.8
plotR(resl_eng1,last.day=430, ylim=c(-2,2),cex=c1, ylab = 'log(R)')
month.axis(start=26,stop=425, origin = 0,cex=c0)
abline(v=lock_eng,col=2)

## England sanity ##
dum <- choose.data(10) ## England
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34 
c1 <- 1.1;c0=.8
par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_eng1,beng, ylab = 'England Fatal Infections')

### Scotland ###
par(mfrow=c(3,1),mar=c(5,5,3,3))
plot(b_scot,rug=FALSE,scheme=1,n=500,xlim = c(35, 558),ylim=c(0,175),ylab="Simple model infection trajectory",xlab = "Days since 31st December",select=1,cex.lab=1.2);
abline(v=lock_scot, col=2);abline(0,0,lty=2)
with(dat_scot,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_scot$tag,fitted(b_scot),col="grey",lwd=2)
month.axis(start=31,stop=547,origin = 0, cex=0.8)


dum <- choose.data(4) ## Scotland
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_scot1,approx=F,last.day=558, lock.down = NA,ylab="Complex model infection trajectory", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_scot,col=2);
points(day,deaths,cex=.5,col=1, pch=19)
lines(dat_scot$tag,fitted(bscot),col="grey",lwd=2)


dum <- choose.data(4) ## Scotland
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plotR(resl_scot1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_scot,col=2)

## Scotland sanity ##
dum <- choose.data(4) ## Scotland
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_scot1,bscot, ylab = 'Scotland Fatal Infections') 

### Belgium ###
par(mfrow=c(3,1),mar=c(5,5,3,3))
plot(b_bel,rug=FALSE,scheme=1,n=500,xlim = c(35, 558),ylim=c(0,600),ylab="Simple model infection trajectory", xlab = "Days since 31st December",select=1,cex.lab=1.2);
abline(v=lock_bel, col=2);abline(0,0,lty=2)
with(dat_bel,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_bel$tag,fitted(b_bel),col="grey",lwd=2)
month.axis(start=31,stop=547,origin = 0, cex=0.8)


dum <- choose.data(2) ## Belgium
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_bel1,approx=F,last.day=558, lock.down = NA,ylab="Complex model infection trajectory", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_bel,col=2);
points(day,deaths,cex=.5,col=1,pch=19)
lines(dat_bel$tag,fitted(bbel),col="grey",lwd=2)


dum <- choose.data(2) ## Belgium
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plotR(resl_bel1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_bel,col=2)

## Belgium sanity ##
dum <- choose.data(2) ## Belgium
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_bel1,bbel, ylab = 'Belgium Fatal Infections') 

### Denmark ###
par(mfrow=c(3,1),mar=c(5,5,4.15,3))
plot(b_dnk,rug=FALSE,scheme=1,n=500,xlim = c(35, 547), ylim=c(0,50),ylab="Simple model infection trajectory",
     xlab="Days since 31st December",select=1,cex.lab=1.1);
abline(v=lock_dnk, col=2);abline(0,0,lty=2)
with(dat_dnk,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_dnk$tag,fitted(b_dnk),col="grey",lwd=2)
month.axis(start=26,stop=547,origin = 0, cex=0.8)


dum <- choose.data(5) ## Denmark
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_dnk1,approx=F,last.day=558, lock.down = NA,ylab="Complex model infection trajectory", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_dnk,col=2);
points(day,deaths,cex=.5,col=1, pch=19)
lines(dat_dnk$tag,fitted(bdnk),col="grey",lwd=2)

plotR(resl_dnk1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_dnk,col=2)

## Denmark sanity ##
dum <- choose.data(5) ## Denmark
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_dnk1,bdnk, ylab = 'Denmark Fatal Infections')



### Italy ###
par(mfrow=c(3,1),mar=c(5,5,3,3))
plot(b_ita,rug=FALSE,scheme=1,n=500,xlim = c(35, 558),ylim=c(0,1800),ylab="Simple model infection trajectory",
     xlab="Days since 31st December",select=1,cex.lab=1.2);
abline(v=lock_ita, col=2);abline(0,0,lty=2)
with(dat_ita,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_ita$tag,fitted(b_ita),col="grey",lwd=2)
month.axis(start=31,stop=547,origin = 0, cex=0.8)

dum <- choose.data(3) ## Italy
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_ita1,approx=F,last.day=558, lock.down = NA,ylab="Complex model infection trajectory", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_ita,col=2);
points(day,deaths,cex=.5,col=1,pch = 19)
lines(dat_ita$tag,fitted(bita),col="grey",lwd=2)


dum <- choose.data(3) ## Italy
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plotR(resl_ita1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_ita,col=2)

## Italy sanity ##
dum <- choose.data(3) ## Italy
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_ita1,bita, ylab = 'Italy Fatal Infections') 

### Netherlands ###

par(mfrow=c(3,1),mar=c(5,5,3,3))
plot(b_nld,rug=FALSE,scheme=1,n=500,xlim = c(35, 558),ylim=c(0,325),ylab="Simple model infection trajectory",
     xlab="Days since 31st December",select=1,cex.lab=1.1);
abline(v=lock_nld,col=2);abline(0,0,lty=2)
with(dat_nld,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_nld$tag,fitted(b_nld),col="grey",lwd=2)
month.axis(start=26,stop=547,origin = 0, cex=0.8)

dum <- choose.data(9) ## Netherlands
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_nld1,approx=F,last.day=558, lock.down = NA,ylab="Complex model infection trajectory", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_nld,col=2);
points(day,deaths,cex=.5,col=1, pch = 19)
lines(dat_nld$tag,fitted(bnld),col="grey",lwd=2)

dum <- choose.data(9) ## Netherlands
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plotR(resl_nld1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_nld,col=2)

## Netherlands sanity ##
dum <- choose.data(9) ## Netherlands
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_nld,bnld, ylab = 'Netherlands Fatal Infections') 



### Portugal ###

par(mfrow=c(3,1),mar=c(5,5,3,3))


plot(b_por,rug=FALSE,scheme=1,n=500,xlim = c(35, 558),ylim=c(0,500),ylab="Simple model infection trajectory",
     xlab="Days since 31st December",select=1,cex.lab=1.2);
abline(v=lock_por, col=2);abline(0,0,lty=2)
with(dat_por,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_por$tag,fitted(b_por),col="grey",lwd=2)
month.axis(start=31,stop=547,origin = 0, cex=0.8)

dum <- choose.data(8) ## Portugal
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_por1,approx=F,last.day=558, lock.down = NA,ylab="Complex model infection trajectory", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_por,col=2);
points(day,deaths,cex=.5,col=1, pch=19)
lines(dat_por$tag,fitted(bpor),col="grey",lwd=2)


dum <- choose.data(8) ## Portugal
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plotR(resl_por1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_por,col=2)

## Portugal sanity ##
dum <- choose.data(8) ## Portugal
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_por1,bpor, ylab = 'Portugal Fatal Infections') 

### Spain ###
par(mfrow=c(3,1),mar=c(5,5,3,3))
plot(b_esp,rug=FALSE,scheme=1,n=500,xlim = c(35, 558),ylim=c(0,2100),ylab="Simple model infection trajectory",
     xlab="Days since 31st December",select=1,cex.lab=1.2);
abline(v=lock_esp,col=2);abline(0,0,lty=2)
with(dat_esp,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_esp$tag,fitted(b_esp),col="grey",lwd=2)
month.axis(start=31,stop=547,origin = 0, cex=0.8)

dum <- choose.data(1) ## Spain
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_esp1,approx=F,last.day=558, lock.down = NA,ylab="Complex model infection trajectory", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_esp,col=2);
points(day,deaths,cex=.5,col=1, pch=19)
lines(dat_esp$tag,fitted(besp),col="grey",lwd=2)


dum <- choose.data(1) ## Spain
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plotR(resl_esp1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_esp,col=2)

## Spain sanity ##
dum <- choose.data(1) ## Spain
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_esp1,besp, ylab = 'Spain Fatal Infections') 


### Sweden ###

# simple
par(mfrow=c(3,1),mar=c(5,5,3,3))
plot(b_swed,rug=FALSE,scheme=1,n=500,xlim = c(35, 558),ylim=c(0,175),ylab="Simple model infection trajectory",
     xlab="Days since 31st December",select=1,cex.lab=1.1);
with(dat_swed,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_swed$tag,fitted(b_swed),col="grey",lwd=2)
month.axis(start=26,stop=547,origin = 0, cex=0.8)


# complex
dum <- choose.data(6) ## Sweden
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths)
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_swed1,approx=F,last.day=558, lock.down = NA,ylab="Complex model infection trajectory", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
points(day,deaths,cex=.5,col=1, pch = 19)
lines(dat_swed$tag,fitted(bswed),col="grey",lwd=2)

# R
plotR(resl_swed1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)


## Sweden sanity ##
dum <- choose.data(6) ## Sweden
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths)
day <- 1:nc+34
c1 <- 1.1;c0=.8
par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_swed1,bswed, ylab = 'Sweden Fatal Infections') 




### Switzerland ###
par(mfrow=c(3,1),mar=c(5,5,3,3))
plot(b_swit,rug=FALSE,scheme=1,n=500,xlim = c(35, 558),ylim=c(0,175),ylab="Simple model infection trajectory",
     xlab="Days since 31st December",select=1,cex.lab=1.2);
abline(v=lock_swit, col=2);abline(0,0,lty=2)
with(dat_swit,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_swit$tag,fitted(b_swit),col="grey",lwd=2)
month.axis(start=31,stop=547,origin = 0, cex=0.8)


dum <- choose.data(7) ## Switzerland
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_swit1,approx=F,last.day=558, lock.down = NA,ylab="Complex model infection trajectory", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_swit,col=2);
points(day,deaths,cex=.5,col=1, pch=19)
lines(dat_swit$tag,fitted(bswit),col="grey",lwd=2)


dum <- choose.data(7) ## Switzerland
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
plotR(resl_swit1,last.day=558, ylim=c(-2,2),cex=c1, ylab = 'log(R)')
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_swit,col=2)

## Switzerland sanity ##
dum <- choose.data(7) ## Switzerland
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
day <- 1:nc+34
c1 <- 1.1;c0=.8
par(mfrow=c(1,1),mar=c(4,5,2,1))
sanity.plot(resl_swit1,bswit, ylab = 'Switzerland Fatal Infections') 






