testing <- read.csv('Data/England_testing_data.csv', header = TRUE, col.names = c('Date', 'period', 'pos_test', 'LB', 'UB'))

testing <- testing %>%
  mutate(date_time = julian(as.Date(Date, format = "%d/%m/%Y"),origin=as.Date("2019-12-31", format = "%Y-%m-%d")))

par(mar=c(5,5,1,1),mfrow=c(1,1))
plot(testing$date_time, testing$pos_test, xlab = "Days since 31/12/2019", ylab = "Number of positive tests")
arrows(x0 = testing$date_time, y0 = testing$LB, x1 = testing$date_time, y1 = testing$UB, code =3, angle = 90, length = .01)


day <- 1:nc+34 # re-scale day to plot from 31st december, all death vectors start from 4th Feb
par(mfrow=c(2,1),mar=c(4,5,2,1))
c1 <- 1.1;c0=.8
plot.ip(resl_eng,approx=F,last.day=430, lock.down = NA,ylab="England Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=430, origin = 0 ,cex=c0)
abline(v=lock_eng,col=2);
points(day,deaths,cex=.5,col="grey")
plot(testing$date_time, testing$pos_test, cex = .5, col = "black", xlim = c(35, 430))
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
plot.ip(resl_scot,approx=F,last.day=558, lock.down = NA,ylab="Scotland Fatal Infections", xlab = "Days since 31st December",c1=c1,plot.peak=FALSE)
month.axis(start=26,stop=547, origin = 0 ,cex=c0)
abline(v=lock_scot,col=2);
points(day,deaths,cex=.5,col="grey")
plot(testing_scot$date_time, testing_scot$pos_test, xlab = "Days since 31/12/2019", ylab = "Number of positive tests", cex = .5, col = "black", xlim = c(35, 558))
arrows(x0 = testing_scot$date_time, y0 = testing_scot$LB, x1 = testing_scot$date_time, y1 = testing_scot$UB, code =3, angle = 90, length = .01)
month.axis(start=26,stop=547, origin = 0,cex=c0)
abline(v=lock_scot,col=2)
