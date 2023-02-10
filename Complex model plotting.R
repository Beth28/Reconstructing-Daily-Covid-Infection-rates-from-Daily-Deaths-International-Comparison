#belgium, italy, spain
par(mfrow=c(3,1),mar=c(5,5,2,1))
dum <- choose.data(2) ## Belgium
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_bel,approx=F,last.day=558, lock.down = NA,ylab="Belgium Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_bel,col=2);
points(day,deaths,cex=.5,col="grey")


dum <- choose.data(3) ## Italy
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_ita,approx=F,last.day=558, lock.down = NA,ylab="Italy Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_ita,col=2);
points(day,deaths,cex=.5,col="grey")


dum <- choose.data(1) ## Spain
deaths <- dum$deaths; dat <- dum$dat; rm(dum)
nc <- length(deaths);day <- 1:nc-21
dow <- rep(1:7,100)[1:nc] ## day of week
day <- 1:nc+34
c1 <- 1.1;c0=.8
plot.ip(resl_esp,approx=F,last.day=558, lock.down = NA,ylab="Spain Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_esp,col=2);
points(day,deaths,cex=.5,col="grey")

