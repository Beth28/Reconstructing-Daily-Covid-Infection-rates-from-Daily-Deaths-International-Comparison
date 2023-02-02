source('Functions_for_simple.R')
source('Data_code.R')

## England

lock_eng <- c(julian(as.Date("2020-3-24"),origin=as.Date("2019-12-31")),
              julian(as.Date("2020-11-5"),origin=as.Date("2019-12-31")),
              julian(as.Date("2021-1-5"),origin=as.Date("2019-12-31")))

dat_eng <- simple_model_dat(ed_ENG, "2020-03-02", 1) # returns data in required format for plotting
b_eng <- simple_model_b(ed_ENG, "2020-03-02", 1) # fits simple model

par(mar=c(5,5,1,1),mfrow=c(1,1))
plot(b_eng,rug=FALSE,scheme=1,n=500,ylim=c(0,175),ylab="Daily infections/Deaths",
     xlab="Days since 31 Dec 2019",select=1,cex.lab=1.2);
abline(v=lock_eng);abline(0,0,lty=2)
with(dat_eng,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_eng$tag,fitted(b_scot),col="grey",lwd=2)


## Scotland

lock_scot <- c(julian(as.Date("2020-3-24"),origin=as.Date("2019-12-31")),
              julian(as.Date("2021-01-05"),origin=as.Date("2019-12-31")))

dat_scot <- simple_model_dat(ed_SCOT, "2020-03-11", 4)
b_scot <- simple_model_b(ed_SCOT, "2020-03-11", 4)




## Belgium

lock_bel <- c(julian(as.Date("2020-3-18"),origin=as.Date("2019-12-31")),
              julian(as.Date("2020-11-2"),origin=as.Date("2019-12-31")))


dat_bel <- simple_model_dat(ed_BEL, "2020-03-10", 2)
b_bel <- simple_model_b(ed_BEL, "2020-03-10", 2)




## Spain

lock_esp <- c(julian(as.Date("2020-3-14"),origin=as.Date("2019-12-31")),
              julian(as.Date("2020-10-25"),origin=as.Date("2019-12-31")))

dat_esp <- simple_model_dat(ed_ESP, "2020-02-13", 4)
b_esp <- simple_model_b(ed_ESP, "2020-02-13", 4)



## Italy

lock_ita <- c(julian(as.Date("2020-3-11"),origin=as.Date("2019-12-31")),
              julian(as.Date("2020-11-06"),origin=as.Date("2019-12-31")))

dat_ita <- simple_model_dat(ed_ITA, "2020-02-24", 1)
b_ita <- simple_model_b(ed_ITA, "2020-02-24", 1)


## Denmark

lock_dnk <- c(julian(as.Date("2020-3-18"),origin=as.Date("2019-12-31")),
              julian(as.Date("2020-11-06"),origin=as.Date("2019-12-31")))

dat_dnk <- simple_model_dat(ed_DEN, "2020-03-11", 1)
b_dnk <- simple_model_b(ed_DEN, "2020-03-11", 1)




## Sweden

dat_swed <- simple_model_dat(ed_SWED, "2020-03-9", 4)
b_swed <- simple_model_b(ed_SWED, "2020-03-9", 4)

## Switzerland

lock_swit <- c(julian(as.Date("2020-3-19"),origin=as.Date("2019-12-31")))

dat_swit <- simple_model_dat(ed_SWIT, "2020-03-5", 4)
b_swit <- simple_model_b(ed_SWIT, "2020-03-5", 4)

## Portugal

lock_por <- c(julian(as.Date("2020-3-18"),origin=as.Date("2019-12-31")),
              julian(as.Date("2021-1-15"),origin=as.Date("2019-12-31")),
              julian(as.Date("2021-3-11"),origin=as.Date("2019-12-31")))

dat_por <- simple_model_dat(ed_POR, "2020-03-16", 4)
b_por <- simple_model_b(ed_POR, "2020-03-16", 4)



## Netherlands

lock_nld <- c(julian(as.Date("2020-03-23"),origin=as.Date("2019-12-31")),
              julian(as.Date("2020-12-15"),origin=as.Date("2019-12-31")))

dat_nld <- simple_model_dat(ed_NLD, "2020-02-27", 4)
b_nld <- simple_model_b(ed_NLD, "2020-02-27", 4)

## Plotting code

## England
par(mar=c(5,5,3,3),mfrow=c(1,1))
plot(b_eng,rug=FALSE,scheme=1,n=500,ylim=c(0,1800),ylab="England Daily infections/Deaths",
     xlab="Days since 31 Dec 2019",select=1,cex.lab=1.2);
abline(v=lock_eng);abline(0,0,lty=2)
with(dat_eng,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_eng$tag,fitted(b_eng),col="grey",lwd=2)
month.axis(start=31,stop=,origin = 0, cex=0.8)


## Belgium, Italy and Spain
par(mar=c(5,5,3,3),mfrow=c(3,1))
# 1
plot(b_bel,rug=FALSE,scheme=1,n=500,ylim=c(0,600),ylab="Belgium Daily infections/Deaths", xlab = "Days since 31 Dec 2019",select=1,cex.lab=1.2);
abline(v=lock_bel);abline(0,0,lty=2)
with(dat_bel,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_bel$tag,fitted(b_bel),col="grey",lwd=2)
month.axis(start=31,stop=547,origin = 0, cex=0.8)

# 2
plot(b_ita,rug=FALSE,scheme=1,n=500,ylim=c(0,1800),ylab="Italy Daily infections/Deaths",
     xlab="Days since 31 Dec 2019",select=1,cex.lab=1.2);
abline(v=lock_ita);abline(0,0,lty=2)
with(dat_ita,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_ita$tag,fitted(b_ita),col="grey",lwd=2)
month.axis(start=31,stop=547,origin = 0, cex=0.8)

# 3
plot(b_esp,rug=FALSE,scheme=1,n=500,ylim=c(0,2100),ylab="Spain Daily infections/Deaths",
     xlab="Days since 31 Dec 2019",select=1,cex.lab=1.2);
abline(v=lock_esp);abline(0,0,lty=2)
with(dat_esp,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_esp$tag,fitted(b_esp),col="grey",lwd=2)
month.axis(start=31,stop=547,origin = 0, cex=0.8)




## Portugal, Scotland and Switzerland
par(mar=c(5,5,3,3),mfrow=c(3,1))
# 4
plot(b_por,rug=FALSE,scheme=1,n=500,ylim=c(0,500),ylab="Portugal Daily infections/Deaths",
     xlab="Days since 31 Dec 2019",select=1,cex.lab=1.2);
abline(v=lock_por);abline(0,0,lty=2)
with(dat_por,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_por$tag,fitted(b_por),col="grey",lwd=2)
month.axis(start=31,stop=547,origin = 0, cex=0.8)

# 5
plot(b_scot,rug=FALSE,scheme=1,n=500,ylim=c(0,175),ylab="Scotland Daily infections/Deaths",xlab = "",select=1,cex.lab=1.2);
abline(v=lock_scot);abline(0,0,lty=2)
with(dat_scot,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_scot$tag,fitted(b_scot),col="grey",lwd=2)
month.axis(start=31,stop=547,origin = 0, cex=0.8)

# 6
plot(b_swit,rug=FALSE,scheme=1,n=500,ylim=c(0,175),ylab="Switzerland Daily infections/Deaths",
     xlab="Days since 31 Dec 2019",select=1,cex.lab=1.2);
abline(v=lock_swit);abline(0,0,lty=2)
with(dat_swit,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_swit$tag,fitted(b_swit),col="grey",lwd=2)
month.axis(start=31,stop=547,origin = 0, cex=0.8)



## Denmark, Netherlands and Sweden
par(mar=c(5,5,3,3),mfrow=c(3,1))
# 7
plot(b_dnk,rug=FALSE,scheme=1,n=500,ylim=c(0,50),ylab="Denmark Daily infections/Deaths",
     xlab="Days since 31 Dec 2019",select=1,cex.lab=1.2);
abline(v=lock_dnk);abline(0,0,lty=2)
with(dat_dnk,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_dnk$tag,fitted(b_dnk),col="grey",lwd=2)
month.axis(start=31,stop=547,origin = 0, cex=0.8)

# 8
plot(b_nld,rug=FALSE,scheme=1,n=500,ylim=c(0,325),ylab="Netherlands Daily infections/Deaths",
     xlab="Days since 31 Dec 2019",select=1,cex.lab=1.2);
abline(v=lock_nld);abline(0,0,lty=2)
with(dat_nld,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_nld$tag,fitted(b_nld),col="grey",lwd=2)
month.axis(start=31,stop=547,origin = 0, cex=0.8)

# 9
plot(b_swed,rug=FALSE,scheme=1,n=500,ylim=c(0,175),ylab="Sweden Daily infections/Deaths",
     xlab="Days since 31 Dec 2019",select=1,cex.lab=1.2);
with(dat_swed,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat_swed$tag,fitted(b_swed),col="grey",lwd=2)
month.axis(start=31,stop=547,origin = 0, cex=0.8)
