library(BayesSpec);library(mvtnorm);library(pscl);library(trust)


#### Stationary process ####

n<-256
t<-c(1:n)
eps<-rnorm(n,0,sd=sigma)
y <- arima.sim(list(order=c(3,0,0), ar=c(1.4256,-0.7344,0.1296)),256,sd=1)
plot.ts(y)               
results<-adaptspec(nloop=10000, nwarmup=2000,nexp_max=4,nbasis=10,x=as.numeric(y),plotting=TRUE)

##### piecewise stationary process ####

data(simulated_piecewise)   
model1 <- adaptspec(nloop = 10000, nwarmup = 2000,nexp_max=4,nbasis=10, x = simulated_piecewise, plotting = TRUE) 
str(model1)
plot(simulated_piecewise, type='l')
summary(model1$nexp_curr)
plot(model1$nexp_curr)


#### climate data ####

setwd("/Volumes/annie/earded") #Set path files

############# Read data #############
filename<-"temi.amoc.max.nc"
data<-nc_open(filename)
print(data)
dname<-names(data$var)
tmp_array<-ncvar_get(data,dname)
plot(tmp_array,type='l')

########### Bayesian analysis with BayesSpec #############
results_amoc<-adaptspec(nloop=10000, nwarmup=3500,nexp_max=4, nbasis=10, x=as.numeric(tmp_array),plotting=TRUE)
plot.ts(tmp_array,type='l',main="temi amoc")



######## Bayesian analysis after smoothing ######
results_amoc<-adaptspec(nloop=10000, nwarmup=2000,nexp_max=4, nbasis=10, x=mb,plotting=TRUE)

results_amoc<-adaptspec(nloop=10000, nwarmup=2000,nexp_max=4, nbasis=10, x=reg$fitted,plotting=TRUE)



