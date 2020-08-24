rm(list=objects())
library(zoo)
library(timeDate)
library(forecast)
library(xts)
library(mgcv)
setwd("/Volumes/annie/earded/Global\ summaries\ files")
library(sp) ; library(rgdal) ; library(raster);library(ncdf4);library(cmsaf);library(RNetCDF)
filename<-"temi.amoc.max.nc"
data<-nc_open(filename)
dname<-names(data$var)
t <- ncvar_get(data, "t")
X<-ncvar_get(data,dname)
plot(X,tmp_array,type='l',main="Max AMOC 21ka BP")

date1<- strptime("01/01/0001", "%m/%d/%Y")
date2<- strptime("01/01/0500", "%m/%d/%Y")
Date<-seq.POSIXt(date1,date2, by = "year")


#######Simple Moving Average####### estimate of trend + smoothing

plot(X,type='l',main='Simple Moving Average (SMA)',ylab='Max amoc')
lines(rollmean(X,3),col='blue')
lines(rollmean(X,40),col='red')
legend(0,28,col=c('black','blue', 'red'),legend=c('Raw', 'SMA 5', 'SMA 40'),lty=1,cex=0.8)
##############TMA#############
p=5
plot(rollmean(X,p),type='l',main='Simple vs Triangular Moving Average',ylab='Discoveries')
lines(rollmean(X,10),col='red')
lines(rollmean(rollmean(X,5),5),col='blue')
legend(206,25,col=c('black','red','blue'),legend=c('SMA 5', 'SMA 10','TMA 5'),lty=1,cex=0.8)
#####kernel smoothing ######
p=5
b=4
plot(rollmean(X,p),type='l',main='Simple Moving Average vs Kernel Smoothing',ylab='Discoveries')
lines(rollmean(X,10),col='red')
lines(ksmooth(time(X),X,'normal',bandwidth=b),type='l',col='blue')
legend(206,25,col=c('black','red','blue'),legend=c('SMA 5', 'SMA 10', 'Kernel,b=4'),lty=1,cex=0.8)


#################moyenne mobile##########
X.ts<-xts(X,order.by = Date)
mb<-filter(X, filter=array(1/50,dim=50),method = c("convolution"),sides = 2, circular = F)
plot(X,type='l', main='max AMOC')
mb<-na.omit(mb)
lines(mb,col='red')
legend(183,27,legend=c('Raw','moving average'),col=c('black','red'),cex=0.8,lty=1)

########## estimation paramétrique de la tendance #########
time <- c(1:length(Date))
reg <- lm(X ~ time + I(time^2) + I(time^3), data = X.ts)
par(mfrow = c(1, 2))
plot(Date,X.ts, type = "l", xlab = "",ylab = "Max AMOC", col = "blue") 
lines(Date, reg$fitted, col = "red", lwd = 2) 
plot(Date, X - reg$fitted, type = "l",xlab = "", ylab = "Max AMOC detrend", col = "orangered2")
                                                                                                              

################# noyau Gaussien  estimateur non paramétrique de la tendance ##########
noyau <- ksmooth(time(X.ts), X.ts, kernel = c("normal"), bandwidth = 10)
par(mfrow = c(1, 2))
plot(Date, X.ts, type = "l", xlab = "", ylab = "Ind. Prix. Conso. Ménages (INSEE)", col = "blue")
lines(Date, noyau$y, col = "red", lwd = 2)
plot(Date, X.ts - noyau$y, type = "l",xlab = "", ylab = "Ind. Prix. - detrend", col = "orangered2")                                                                                                                                   
#################polynomes locaux ##########
par(mfrow = c(1, 1))
lo<-loess(X~t, degree=1, span=0.9)
ychap.lo<-xts(lo$fitted,order.by=Date)
plot(X,type='l')
lines(lo$fitted,col='red')
?loess

#################regression sur bases de splines
library(mgcv)
g<-gam(X~s(t, k=3))
summary(g)
ychap.gam<-xts(g$fitted,order.by=Date)
plot(X,type='l')
lines(ychap.gam,col='red')

###################################################estimation de la partie saisonniere
#################regression
X.detrend<-X-ychap.lm
plot(X.detrend)


w=2*pi/50
fourier<-cbind(cos(w*t), sin(w*t))
K<-20
for(i in c(2:K))
{
  fourier<-cbind(fourier,cos(i*w*t), sin(i*w*t))
}
matplot(fourier,type='l')
dim(fourier)


reg<-lm(X.detrend~fourier[,1:2])
ychap.lm.season<-xts(as.numeric(reg$fitted),order.by=Date)
plot(X.detrend,type='l')
lines(ychap.lm.season,col='red')
lines(S,col='blue')

#################moyenne mobile
K <- 15
mb.season<-filter(X.detrend, filter=array(1/K,dim=K), method = c("convolution"), sides = 2, circular = TRUE)
mb.season<-xts(mb.season,order.by=Date)

plot(X.detrend,type='l')
lines(mb.season,col='red')
lines(S,col='blue')


#################noyau Gaussien
h=50
x<-seq(1,max(t),length=n)
W<-matrix(unlist(lapply(x,function(x){dnorm(x-t,0,sd=sqrt(h/2))/sum(dnorm(x-t,0,sd=sqrt(h/2)))})),ncol=n,nrow=n,byrow=F)
plot(W[,10])
ychap.kernel.season<-colSums(as.numeric(X.detrend)*W)
ychap.kernel.season<-xts(ychap.kernel.season,order.by=Date)

plot(X.detrend,type='l')
lines(ychap.kernel.season,col='red')
lines(S,col='blue')


#################polynomes locaux
lo<-loess(X.detrend~t, degree=1,span=0.25)
ychap.lo.season<-xts(lo$fitted,order.by=Date)
plot(X.detrend,type='l')
lines(ychap.lo.season,col='red')
lines(S,col='blue')

#################régression sur bases de splines cycliques
cycle<-c(rep(c(1:50),2),1)
plot(cycle)

plot(cycle, X.detrend, pch=20)

g<-gam(X.detrend~s(cycle,k=20, bs='cc'))
summary(g)
ychap.gam.season<-xts(g$fitted,order.by=Date)
plot(X.detrend,type='l')
lines(ychap.gam.season,col='red')
lines(S,col='blue')



######################comparaison des méthodes
plot(X-eps,type='l',ylim=range(X))
lines(X-eps,lwd=2)
lines(X,col='grey')
lines(ychap.lm+ychap.lm.season,col='purple')
lines(ychap.lm+ychap.kernel.season,col='red')
lines(ychap.lm+ychap.lo.season,col='blue')
lines(ychap.lm+ychap.gam.season,col='turquoise2')
lines(ychap.lm+mb.season,col='violetred1')

epschap <- X-(ychap.lm+ychap.lm.season)
plot(epschap, type='l')

acf(X-(ychap.lm+ychap.lm.season))
acf(X-(ychap.lm+ychap.kernel.season))
acf(X-(ychap.lm+ychap.lo.season))
acf(X-(ychap.lm+ychap.gam.season))
acf(X-(ychap.lm+mb.season),na.action = na.omit)





#################################################exercice 2
setwd("/Users/yannig/Documents/Enseignement/2019_2020/M1_serie_chro/Datasets/")
beer<-read.csv("beer2.csv",header=TRUE,skip=1)

#######creation de la date
date1<- strptime(c("01/01/91"), "%m/%d/%y")
date2<- strptime(c("08/01/95"), "%m/%d/%y")
Date<-seq(date1,date2,by = "1 month")
Time<-c(1:length(Date))
beer<-data.frame(Date,beer$BeerProd,Time)
names(beer)<-c("Date","BeerProd","Time")
plot(beer$Date,beer$BeerProd,type='l')

#################################################regression sur base de splines
Month<-as.numeric(format(Date,"%m"))
beer<-data.frame(beer,Month)

g<-gam(BeerProd~s(Time,k=10)+s(Month,k=4,bs='cc'),data=beer)
ychap.gam<-g$fitted

plot(beer$Date,beer$BeerProd,type='l')
lines(beer$Date,ychap.gam,col='red')

plot(g)
terms<-predict(g, newdata=beer, type="terms")

plot(beer$Date,beer$BeerProd-mean(beer$BeerProd),type='l')
lines(beer$Date,terms[,1],col='blue')
lines(beer$Date,terms[,2],col='red')



