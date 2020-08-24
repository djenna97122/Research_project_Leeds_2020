#loading of data
library(ggplot2)
setwd("/Volumes/annie/earded/Global\ summaries\ files/")
filename1 <- "amoc.nc"
filename2 <- "temi.amoc.max.nc"

filename5 <- "deglg.vn1_0.iceconc.mean.ANN.001.nc"
filename6 <- "deglg.vn1_0.sat.mean.ANN.001.nc"
filename7 <- "deglg.vn1_0.sst_mean.ANN.001.nc"
filename8<- "deglh.vn1_0.iceconc.mean.ANN.001.nc"
filename9 <- "deglh.vn1_0.sat.mean.ANN.001.nc"
filename10 <- "deglh.vn1_0.sst.mean.ANN.001.nc"
filename11 <- "deglh.amoc.max.nc"
filename12 <- "deglg.amoc.max.nc"

filename13 <- "temiA1.merid_Atlantic_ym_dpth.annual.nc"
filename14<- "temiA1.temp_mm_1_5m.monthly.nc"
filename15<- "temiA1.temp_mm_uo.monthly.nc"


data1<-nc_open(filename1)
data2<-nc_open(filename2)
data3<-nc_open(filename3)
data4<-nc_open(filename4)
data5<-nc_open(filename5)
data6<-nc_open(filename6)
data7<-nc_open(filename7)
data8<-nc_open(filename8)
data9<-nc_open(filename9)
data10<-nc_open(filename10)
data11<-nc_open(filename11)
data12<-nc_open(filename12)
data13<-nc_open(filename13)
data14<-nc_open(filename14)
data15<-nc_open(filename15)




dname1<-names(data1$var)
dname2<-names(data2$var)
dname3<-names(data3$var)
dname4<-names(data4$var)
dname5<-names(data5$var)
dname6<-names(data6$var)
dname7<-names(data7$var)
dname8<-names(data8$var)
dname9<-names(data9$var)
dname10<-names(data10$var)
dname11<-names(data11$var)
dname12<-names(data12$var)
dname13<-names(data13$var)
dname14<-names(data14$var)
dname15<-names(data15$var)

tmp_array1<-ncvar_get(data1,dname1)
tmp_array2<-ncvar_get(data2,dname2)
tmp_array3<-ncvar_get(data3,dname3)
tmp_array4<-ncvar_get(data4,dname4)
tmp_array5<-ncvar_get(data5,dname5)
tmp_array6<-ncvar_get(data6,dname6)
tmp_array7<-ncvar_get(data7,dname7)
tmp_array8<-ncvar_get(data8,dname8)
tmp_array9<-ncvar_get(data9,dname9)
tmp_array10<-ncvar_get(data10,dname10)
tmp_array11<-ncvar_get(data11,dname11)
tmp_array12<-ncvar_get(data12,dname12)
tmp_array13<-ncvar_get(data13,dname13)
tmp_array14<-ncvar_get(data14,dname14)
tmp_array15<-ncvar_get(data15,dname15)

##### SAT treatment #######
t <- ncvar_get(data6, "t")
plot(t,tmp_array6,type='l',main="Area-weighted mean of surface air temperature vs time",ylab="sat in °C",col='brown1')
lines(t,tmp_array9, col='darkslategray3')
legend(-23000,8.4,col=c('brown1', 'darkslategray3'),legend=c('deglg','deglh'),lty=1,cex=0.8)
sat<- c(tmp_array6,tmp_array9)
###BOXPLOTS###
model<-c(rep('deglg',5000),rep('deglh',5000))
sat_frame<-data.frame(sat,model)
sat_frame$model <- as.factor(sat_frame$model)
p<-ggplot(sat_frame, aes(x=model, y=sat,color=model))+
geom_boxplot() +
labs(title="Boxplot of Area-weighted mean of SAT per type of simulation" )
p
####BARPLOTS#### 
hist(coredata(tmp_array6),main = "Barplot - Area-weighted mean of Surface air temperature (deglg)", xlab="sat", ylab = "frequency",col = "brown1")
hist(coredata(tmp_array9),main = "Barplot - Area-weighted mean of Surface air temperature (deglh)", xlab="sat", ylab = "frequency",col = "darkslategray3")



##### SST treatment #######
t <- ncvar_get(data10, "t")
plot(t,tmp_array7,type='l',main="Area-weighted mean of sea surface temperature vs time",ylab="sst in °C",col='brown1')
lines(t,tmp_array10, col='darkslategray3')
legend(-23000,15.1,col=c('brown1', 'darkslategray3'),legend=c('deglg','deglh'),lty=1,cex=0.8)
sst<- c(tmp_array7,tmp_array10)
###BOXPLOTS###
model<-c(rep('deglg',5000),rep('deglh',5000))
sst_frame<-data.frame(sst,model)
sst_frame$model <- as.factor(sst_frame$model)
p2<-ggplot(sat_frame, aes(x=model, y=sst,color=model))+
  geom_boxplot() +
  labs(title="Boxplot of Area-weighted mean of SST per type of simulation" )
p2
####BARPLOTS#### 
hist(coredata(tmp_array7),main = "Barplot - Area-weighted mean of sea surface temperature (deglg)", xlab="sst", ylab = "frequency",col = "brown1")
hist(coredata(tmp_array10),main = "Barplot - Area-weighted mean of sea surface temperature (deglh)", xlab="sst", ylab = "frequency",col = "darkslategray3")




##### SIC treatment #######
t <- ncvar_get(data5, "t")
plot(t,tmp_array5,type='l',main="Area-weighted mean of sea ice concentration vs time",ylab="sic in °C",col='brown1')
lines(t,tmp_array8, col='darkslategray3')
legend(-20000,0.1325,col=c('brown1', 'darkslategray3'),legend=c('deglg','deglh'),lty=1,cex=0.8)
sic<- c(tmp_array5,tmp_array8)
###BOXPLOTS###
model<-c(rep('deglg',5000),rep('deglh',5000))
sic_frame<-data.frame(sic,model)
sic_frame$model <- as.factor(sic_frame$model)
p3<-ggplot(sat_frame, aes(x=model, y=sic,color=model))+
  geom_boxplot() +
  labs(title="Boxplot of Area-weighted mean of sea ice concentration per type of simulation" )
p3
####BARPLOTS#### 
hist(coredata(tmp_array5),main = "Barplot - Area-weighted mean of sea ice concentration (deglg)", xlab="sic", ylab = "frequency",col = "brown1")
hist(coredata(tmp_array8),main = "Barplot - Area-weighted mean of sea ice concentration (deglh)", xlab="sic", ylab = "frequency",col = "darkslategray3")


###### AMOC treatment #####


