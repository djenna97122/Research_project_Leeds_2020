setwd("/Volumes/annie/earded/Global\ summaries\ files/Temi")

######## Reading data ######
library(sp) ; library(rgdal) ; library(raster);library(ncdf4);library(cmsaf);library(RNetCDF)
filename1<-"temiA.iceconc_mm_srf.monthly.nc"
filename2<-"temiA1.iceconc_mm_srf.monthly.nc"
data1<-nc_open(filename1)
data2<-nc_open(filename2)
print(data1)
print(data2)
dname1<-names(data1$var)
tmp_array1<-ncvar_get(data1,dname1)
dname2<-names(data2$var)
tmp_array2<-ncvar_get(data2,dname2)

t<-c(1:500)
t1 <- ncvar_get(data1, "time")
t2<-ncvar_get(data2, "time")
dim(t1)
dim(t2)

####Get the area weighted mean on lat & long ######
latr<-apply(lat,1,deg2rad)
w<-apply(as.matrix(latr),2,cos)
iceconc_mean1<-t1
iceconc_mean2<-t2

for (tim in 1:dim(t1)){
  vt<-tmp_array1[ , ,tim]#matrix lon = rows, lat= col
  data_mean<-apply(vt,2,mean,na.rm=TRUE)
  data_mean<-weighted.mean(as.matrix(data_mean),w,na.rm=TRUE)
  iceconc_mean1[tim]<-data_mean
};

for (tim in 1:dim(t2)){
  vt<-tmp_array2[ , ,tim]#matrix lon = rows, lat= col
  data_mean<-apply(vt,2,mean,na.rm=TRUE)
  data_mean<-weighted.mean(as.matrix(data_mean),w,na.rm=TRUE)
  iceconc_mean2[tim]<-data_mean
};
iceconc_mean<-merge(iceconc_mean1,iceconc_mean2)
plot(t1,iceconc_mean,type='l',main="Area-weighted mean of ice concentration vs time (temi) ", xlab='time', ylab='iceconc')

concat<-merge(tmp_array1,tmp_array2)
###Creation of the new file ####
dim_new<-ncdim_def("t",units="year",vals=t,create_dimvar=TRUE, calendar="360_day",)
var<-ncvar_def(dname1,units="Sv",dim=dim_new)
concat<-nc_create("temi.iceconc_mean.nc",var)
print(concat)
ncvar_put(concat,"temi.iceconc_mean",iceconc_mean) #upload the values in the table
nc_close(concat)
