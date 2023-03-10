### Comparison between ONS incidence rates ###

# England 

incidence_eng <- read.csv('Data/England_incidence.csv', header = TRUE, col.names = c('period', 'date', 'incidence', 'LB', 'UB'))

incidence_eng <- incidence_eng %>%
  mutate(date_time = julian(as.Date(date, format = "%d/%m/%Y"),origin=as.Date("2019-12-31", format = "%Y-%m-%d")))



par(mar=c(5,5,2,1),mfrow=c(1,1))
plot(incidence_eng$date_time, incidence_eng$incidence, cex = .5, col = "black", xlim = c(150, 430), xlab = "Days since 31st December", ylab = "Estimated incidence rate per 10,000 people")
abline(v=lock_eng,col=2)
arrows(x0 = incidence_eng$date_time, y0 = incidence_eng$LB, x1 = incidence_eng$date_time, y1 = incidence_eng$UB, code =3, angle = 90, length = .01)
month.axis(start=150,stop=430, origin = 0 ,cex=c0)



# Scotland

incidence_scot <- read.csv('Data/Scotland_incidence.csv', header = TRUE, col.names = c('period', 'date', 'incidence', 'LB', 'UB'))

incidence_scot <- incidence_scot %>%
  mutate(date_time = julian(as.Date(date, format = "%d/%m/%Y"),origin=as.Date("2019-12-31", format = "%Y-%m-%d")))

par(mar=c(5,5,2,1),mfrow=c(1,1))
plot(incidence_scot$date_time, incidence_scot$incidence,xlab = "Days since 31st December", ylab = "Estimated incidence rate per 10,000 people", cex = .5, col = "black", xlim = c(290, 558), ylim = c(0, 12))
arrows(x0 = incidence_scot$date_time, y0 = incidence_scot$LB, x1 = incidence_scot$date_time, y1 = incidence_scot$UB, code =3, angle = 90, length = .01)
month.axis(start=290,stop=547, origin = 0,cex=c0)
abline(v=lock_scot,col=2)




## Comparing distributions, log-normal vs convolution ##

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