install.packages("ncdf4") 
install.packages("sp")
install.packages("mosaic")
library(sp); library(ncdf4);library(mosaic);
############# Read data #############
setwd("/Volumes/annie/earpal/database/experiments/deglg/vn1_0/merid_Atlantic_ym_dpth/001")
filename <- "deglg.vn1_0.merid_Atlantic_ym_dpth.annual.ANN.001.nc"
data<-nc_open(filename)
print(data)
dname<-names(data$var)
tmp_array<-ncvar_get(data,dname,start=c(1,1,1,1),count=c(-1,-1,-1,5000))
############# Get the variable & attributes #############
lat<-ncvar_get(data,"latitude")
lon<-ncvar_get(data, "longitude")
m_v<-ncatt_get( data, dname, "missing_value", verbose=FALSE)
v_max<-ncatt_get(data, dname, "valid_max")
v_min<-ncatt_get(data, dname, "valid_min")
sbmdl<-ncatt_get( data, dname, "submodel")
s_c<-ncatt_get(data, dname, "stash_code")
F_V<-ncatt_get(data, dname, "_FillValue")
p<-ncatt_get(data, dname, "processing")
pcmdi_name<-ncatt_get( data, dname,"pcmdi_name" )
pp_name<-ncatt_get( data, dname, "pp_name")
std_name<-ncatt_get( data, dname, "standard_name")
ln<-ncatt_get( data, dname, "long_name")
time<-ncatt_get( data, dname, "time")
t <- ncvar_get(data, "t")
head(t)
dim(t)
t1<-t[1:5000]
summary(t1)
tim=1
############# Get the area weighted mean on lat & long #############
latr<-apply(lat,1,deg2rad)
w<-apply(as.matrix(latr),2,cos)
sst_mean<-t1

for (tim in 1:dim(t1)){
  vt<-tmp_array[ , ,tim]#matrix lon = rows, lat= col
  dim(vt)
  dim(lat)
  data_mean<-apply(vt,2,mean,na.rm=TRUE)
  length(data_mean)
  data_mean
  head(vt)
  data_mean<-weighted.mean(as.matrix(data_mean),w,na.rm=TRUE)
  sst_mean[tim]<-data_mean
};
sst_mean
plot(t1,sst_mean,type='l',main="Area-weighted mean of sea surface temperature vs time (deglg) ", xlab='time', ylab='sst')
#plot(t1,temps_mean_icecon,type='l',main="deglg.ANN.001 -ice concentration vs time ")

############# Create the new files #############

setwd("/Volumes/annie/earded/Global\ summaries\ files/")
dim_new<-ncdim_def("t",units="year",vals=t1,create_dimvar=TRUE, calendar="360_day") #create time dimension 
var<-ncvar_def("sst.mean",units="K",dim=dim_new) #create the variable: table with the global summaries
nc_file<-nc_create("deglg.vn1_0.sst.mean.ANN.001.nc",var) #create the ncdef files
print(nc_file) 
ncvar_put(nc_file,"sst.mean",sst_mean) #upload the values in the table 
ncvar_get(nc_file,"sst.mean")
############# Create all attributes #############
ncatt_put(nc_file,"sst.mean","missing_value",m_v$value)
ncatt_put(nc_file,"sst.mean","valid_max",-200)
ncatt_put(nc_file,"sst.mean","valid_min",200)
ncatt_put(nc_file,"sst.mean","submodel",sbmdl$value)
ncatt_put(nc_file,"sst.mean","stash_code",s_c$value)
ncatt_put(nc_file,"sst.mean","_FillValue",F_V$value)
ncatt_put(nc_file,"sst.mean","processing",p$value)
ncatt_put(nc_file,"sst.mean","pcmdi_name",pcmdi_name$value)
ncatt_put(nc_file,"sst.mean","pp_name",pp_name$value)
ncatt_put(nc_file,"sst.mean","standard_name",std_name$value)
ncatt_put(nc_file,"sst.mean","long_name",ln$value)
ncatt_put(nc_file,"sst.mean","time",time$value)
ncatt_put(nc_file,"sst.mean","title",ln$value)
ncatt_put(nc_file,"sst.mean","name","sst.mean")
print(nc_file)
nc_close(nc_file)
nc_file<-nc_open("deglg.vn1_0.sst.mean.ANN.001.nc")
tmp_array2<-ncvar_get(nc_file,"sst.mean")
(tmp_array2)



################## AMOC###################

############# Read data #############
setwd("/Volumes/annie/earpal/database/experiments/deglg/vn1_0/merid_Atlantic_ym_dpth/001")
filename <- "deglg.vn1_0.merid_Atlantic_ym_dpth.annual.ANN.001.nc"
data<-nc_open(filename)
print(data)
dname<-names(data$var)
tmp_array<-ncvar_get(data,dname,start=c(1,1,1),count=c(-1,-1,5000))
############# Get the variable & attributes #############
lat<-ncvar_get(data, "latitude")
m_v<-ncatt_get( data, dname, "missing_value", verbose=FALSE)
ln<-ncatt_get( data, dname, "long_name")
F_V<-ncatt_get(data, dname, "_FillValue")
unit<-ncatt_get(data, dname, "units")
time<-ncatt_get( data, dname, "time")
t <- ncvar_get(data, "t")
depth<-ncvar_get(data, "depth")
head(t)
dim(t)
summary(t)
tim=1
############# Get the maxamoc long #############
t1<-t[1:5000]
amoc_max<-t1

for (tim in 1:5000){
  vt<-tmp_array[72:143,,tim]#matrix lat = rows, depth= col
  data_max<-apply(vt,2,max,na.rm=TRUE)
  data_max<-apply(as.matrix(data_max),2,max,na.rm=TRUE)
  amoc_max[tim]<-data_max
};
amoc_max
plot(t1,amoc_max,type='l',main="Max amoc vs time(deglg) ")
#plot(t1,temps_mean_icecon,type='l',main="deglh.ANN.001 -ice concentration vs time ")

############# Create the new files #############

setwd("/Volumes/annie/earded")
dim_new<-ncdim_def("t",units="year",vals=t1,create_dimvar=TRUE, calendar="360_day") #create time dimension
var<-ncvar_def("amoc_max",units=unit$value,dim=dim_new) #create the variable: table with the global summaries
nc_file<-nc_create("deglh.amoc.max.nc",var) #create the ncdef files
print(nc_file) 
ncvar_put(nc_file,"amoc_max",amoc_max) #upload the values in the table 
############# Create all attributes #############
ncatt_put(nc_file,"amoc_max","missing_value",m_v$value)
ncatt_put(nc_file,"amoc_max","_FillValue",F_V$value)
ncatt_put(nc_file,"amoc_max","long_name",ln$value)
ncatt_put(nc_file,"amoc_max","title",ln$value)
ncatt_put(nc_file,"amoc_max","name","amoc_max")
print(nc_file)
nc_close(nc_file)
nc_file<-nc_open("deglh.vn1_0.amoc_max.ANN.001.nc")
tmp_array2<-ncvar_get(nc_file,"amoc_max")
(tmp_array2)



nc_close(nc_file)

