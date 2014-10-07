#Part 1
#6
#a
Spruce <- read.csv(file = url("http://www1.appstate.edu/~thomleyje/data-files/Spruce.csv"))
htc <- Spruce$Ht.change
summary(htc)
#b
hist(htc)
qqnorm(htc)
#c
dich <- Spruce$Di.change
fert <- subset(Spruce,Fertilizer=='F')
nonfert <- subset(Spruce,Fertilizer=='NF')
boxplot(Fertilized$Di.change,NonFertilized$Di.change,
        main="Diameter Change by Fertilization",
        xlab="Diameter",
        ylab="Fertilization",
        horizontal=TRUE)
axis(2, 1:2, labels=c("Fertilized","Non-Fertilized"))
#d
tapply(Spruce$Di.change,Spruce$Fertilizer, summary)
#e
plot(dich, htc,main="Diameter vs Height Change",xlab="Diameter Change",
     ylab="Height Change")

#7
#a
x<-c(1:5)
y<-c(6:10)
xMean<-mean(x)
yMean<-mean(y)
xMedian<-median(x)
yMedian<-median(y)
xySumMean<-mean(x+y)
xySumMedian<-median(x+y)
xMeanYMean<-xMean+yMean
xMedianYMedian<-xMedian+yMedian
xySumMean
xMeanYMean
#b
xySumMedian
xMedianYMedian
#10
#a
qnorm(0.3,10,17)
qnorm(0.6,10,17)
#b
qnorm(0.1,25,32)
qnorm(0.9,25,32)

#11
#12
#13
#14
#a
x <- rnorm(15)
par(mfrow=c(2,1))
qqnorm(x)
qqline(x)
hist(x)

###Part 2
#1
pnorm(69.5,100,15)-pnorm(79,100,15)
pnorm(89,100,15)-pnorm(80,100,15)
pnorm(109,100,15)-pnorm(90,100,15)
pnorm(119,100,15)-pnorm(110,100,15)
pnorm(129,100,15)-pnorm(120,100,15)
#2
pnorm(137,100,15)
#3
qnorm(.98,100,15)
#4
qnorm(.999,100,15)

###Part 3
Oscarages <- read.csv(file=url("http://www1.appstate.edu/~thomleyje/data-files/Oscarages.csv"))
#1
rd <- 2
actress=Oscarages$Actress.Age
sactress=Oscarages$S.Actress.Age
actor=Oscarages$Actor.Age
sactor=Oscarages$S.Actor.Age


#actress data
n.actress <- sum(complete.cases(actress))
mean.actress <- mean(actress,na.rm=T)
trmean.actress <- mean(actress,.05,na.rm=T)
sd.actress <- sd(actress,na.rm=T)
cv.actress <- (sd.actress/mean.actress)*100
library(moments)
skew.actress<-skewness(actress,na.rm=T)
kurt.actress<-kurtosis(actress,na.rm=T)-3
fivenum.actress <- fivenum(actress,na.rm=T)
IQR.actress <- IQR(actress, na.rm=T)
range.actress <- fivenum.actress[5]-fivenum.actress[1]
mad.actress <- mad(actress, constant=1,na.rm=T)
actress.s <- sort(actress)
outliers <- boxplot.stats(actress.s)[4]
#Support actress data
n.sactress <- sum(complete.cases(sactress))
mean.sactress <- mean(sactress,na.rm=T)
trmean.sactress <- mean(sactress,.05,na.rm=T)
sd.sactress <- sd(sactress,na.rm=T)
cv.sactress <- (sd.sactress/mean.sactress)*100
skew.sactress<-skewness(sactress,na.rm=T)
kurt.sactress<-kurtosis(sactress,na.rm=T)-3
fivenum.sactress <- fivenum(sactress,na.rm=T)
IQR.sactress <- IQR(sactress, na.rm=T)
range.sactress <- fivenum.sactress[5]-fivenum.sactress[1]
mad.sactress <- mad(sactress, constant=1,na.rm=T)
sactress.s <- sort(sactress)
outliers <- boxplot.stats(sactress.s)[4]
#actor data
n.actor <- sum(complete.cases(actor))
mean.actor <- mean(actor,na.rm=T)
trmean.actor <- mean(actor,.05,na.rm=T)
sd.actor <- sd(actor,na.rm=T)
cv.actor <- (sd.actor/mean.actor)*100
skew.actor<-skewness(actor,na.rm=T)
kurt.actor<-kurtosis(actor,na.rm=T)-3
fivenum.actor <- fivenum(actor,na.rm=T)
IQR.actor <- IQR(actor, na.rm=T)
range.actor <- fivenum.actor[5]-fivenum.actor[1]
mad.actor <- mad(actor, constant=1,na.rm=T)
actor.s <- sort(actor)
outliers <- boxplot.stats(actor.s)[4]
#Support actor data
n.sactor <- sum(complete.cases(sactor))
mean.sactor <- mean(sactor,na.rm=T)
trmean.sactor <- mean(sactor,.05,na.rm=T)
sd.sactor <- sd(sactor,na.rm=T)
cv.sactor <- (sd.sactor/mean.sactor)*100
skew.sactor<-skewness(sactor,na.rm=T)
kurt.sactor<-kurtosis(sactor,na.rm=T)-3
fivenum.sactor <- fivenum(sactor,na.rm=T)
IQR.sactor <- IQR(sactor, na.rm=T)
range.sactor <- fivenum.sactor[5]-fivenum.sactor[1]
mad.sactor <- mad(sactor, constant=1,na.rm=T)
sactor.s <- sort(sactor)
outliers <- boxplot.stats(sactor.s)[4]

descriptives <- data.frame(Stats=c("n",
                                   "Mean",
                                   "5%Tr",
                                   "SD",
                                   "CV%",
                                   "Skew",
                                   "Kurt",
                                   "Min",
                                   "Q1",
                                   "Med",
                                   "Q3",
                                   "Maactress",
                                   "MAD",
                                   "IQR",
                                   "Range"),
                           Actress=format(round(c(n.actress,
                                                 mean.actress,
                                                 trmean.actress,
                                                 sd.actress,
                                                 cv.actress,
                                                 skew.actress,
                                                 kurt.actress,
                                                 fivenum.actress[1],
                                                 fivenum.actress[2],
                                                 fivenum.actress[3],
                                                 fivenum.actress[4],
                                                 fivenum.actress[5],
                                                 mad.actress,
                                                 IQR.actress,
                                                 range.actress),
                                               rd),
                                         scientific=F,
                                         big.mark=","),
                           SActress=format(round(c(n.sactress,
                                                  mean.sactress,
                                                  trmean.sactress,
                                                  sd.sactress,
                                                  cv.sactress,
                                                  skew.sactress,
                                                  kurt.sactress,
                                                  fivenum.sactress[1],
                                                  fivenum.sactress[2],
                                                  fivenum.sactress[3],
                                                  fivenum.sactress[4],
                                                  fivenum.sactress[5],
                                                  mad.sactress,
                                                  IQR.sactress,
                                                  range.sactress),
                                                rd),
                                          scientific=F,
                                          big.mark=","),
                           Actor=format(round(c(n.actor,
                                                  mean.actor,
                                                  trmean.actor,
                                                  sd.actor,
                                                  cv.actor,
                                                  skew.actor,
                                                  kurt.actor,
                                                  fivenum.actor[1],
                                                  fivenum.actor[2],
                                                  fivenum.actor[3],
                                                  fivenum.actor[4],
                                                  fivenum.actor[5],
                                                  mad.actor,
                                                  IQR.actor,
                                                  range.actor),
                                                rd),
                                          scientific=F,
                                          big.mark=","),
                           Actor=format(round(c(n.sactor,
                                                  mean.sactor,
                                                  trmean.sactor,
                                                  sd.sactor,
                                                  cv.sactor,
                                                  skew.sactor,
                                                  kurt.sactor,
                                                  fivenum.sactor[1],
                                                  fivenum.sactor[2],
                                                  fivenum.sactor[3],
                                                  fivenum.sactor[4],
                                                  fivenum.sactor[5],
                                                  mad.sactor,
                                                  IQR.sactor,
                                                  range.sactor),
                                                rd),
                                          scientific=F,
                                          big.mark=","))
descriptives

#2
boxplot(actress,sactress,actor,sactor,main="Ocsar Ages", horizontal=TRUE,xlab="Ages",names=c("Actress","S Actress","Actor", "S Actor"))
#3
boxplot(actress~Oscarages$Decade,main="Best Actress ages by decade",ylab="Ages",xlab="Decades")

###Part 4
Lottery <- read.csv(file=url("http://www1.appstate.edu/~thomleyje/data-files/Lottery.csv"))
Shipsanitation<- read.csv(file=url("http://www1.appstate.edu/~thomleyje/data-files/Shipsanitation.csv"))
FlightDelays<- read.csv(file=url("http://www1.appstate.edu/~thomleyje/data-files/FlightDelays.csv"))
Anthropometric<- read.csv(file=url("http://www1.appstate.edu/~thomleyje/data-files/Anthropometric.csv"))

#1
win = Lottery$Win
hist(win,main="Winning Lottery Ticket Numbers",xlab="Numbers")
boxplot(win,main="Winning Lottery Ticket Numbers",horizontal=T,xlab="Numbers")
qqnorm(win)
qqline(win)
plot.ecdf(win, col="red")
abline(v=mean(win,na.rm=T), col="red")
skewness(win)
kurtosis(win)-3

#2
score = Shipsanitation$Score
hist(score,main="Sanitation Score or Cruise ship kitchens")
boxplot(score,main="Sanitation Score or Cruise ship kitchens",horizontal=T,xlab="Score")
qqnorm(score)
qqline(score)
plot.ecdf(score, col="red")
abline(v=mean(score,na.rm=T), col="red")
skewness(score)
kurtosis(score)-3

#3
delays = FlightDelays$Delay
hist(delays,main="Minutes flight delayed",xlab="minutes")
boxplot(delays,main="Minutes flight delayed",horizontal=T,xlab="Minutes")
qqnorm(delays)
qqline(delays)
plot.ecdf(delays, col="red")
abline(v=mean(delays,na.rm=T), col="red")
skewness(delays)
kurtosis(delays)-3

#4
id = Anthropometric$Ideal
ht = Anthropometric$Height
hist(id,main="Ideal Heights",xlab="inches")
boxplot(id,main="Ideal Heights",horizontal=T,xlab="inches")
qqnorm(id)
qqline(id)
plot.ecdf(id, col="red")
abline(v=mean(id,na.rm=T), col="red")
skewness(id)
kurtosis(id)-3

hist(ht,main="Actual Heights",xlab="inches")
boxplot(ht,main="Actual Heights",horizontal=T,xlab="inches")
qqnorm(ht)
qqline(ht)
plot.ecdf(ht, col="red")
abline(v=mean(ht,na.rm=T), col="red")
skewness(ht)
kurtosis(ht)-3
