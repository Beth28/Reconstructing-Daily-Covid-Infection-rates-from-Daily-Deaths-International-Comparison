par(mar=c(5,5,3,3),mfrow=c(3,1))
# 1
plot(b,rug=FALSE,scheme=1,n=500,ylim=c(0,600),ylab="Belgium Daily infections/Deaths", xlab = "Days since 31 Dec 2019",select=1,cex.lab=1.2);
abline(v=lock_bel);abline(0,0,lty=2)
with(dat,points(tag,tote,pch=19,col=1,cex=.5))
lines(dat$tag,fitted(b),col="grey",lwd=2)
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