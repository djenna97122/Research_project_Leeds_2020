install.packages("ncdf4") 
install.packages("sp")
install.packages("raster")
install.packages("mosaic")
library(sp); library(ncdf4);library(mosaic);
############# Read data #############
setwd("/Volumes/annie/earpal/database/experiments/deglg/vn1_0/temp_mm_uo/001")
filename<-"deglg.vn1_0.temp_mm_uo.monthly.ANN.001.nc"
#filename <- "deglg.vn1_0.merid_Atlantic_ym_dpth.annual.ANN.001.nc"
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
plot(t1,sst_mean,type='l',main="deglg.ANN.001 - surface air temperature vs time ")
#plot(t1,temps_mean_icecon,type='l',main="deglg.ANN.001 -ice concentration vs time ")

############# Create the new files #############

setwd("/Volumes/annie/earded")
dim_new<-ncdim_def("t",units="year",vals=t1,create_dimvar=TRUE, calendar="360_day") #create time dimension 
var<-ncvar_def("temp_mm_uo.mean",units="K",dim=dim_new) #create the variable: table with the global summaries
nc_file<-nc_create("deglg.vn1_0.temp_mm_uo.mean.ANN.001",var) #create the ncdef files
print(nc_file) 
ncvar_put(nc_file,"temp_uo.mean",sst_mean) #upload the values in the table 

############# Create all attributes #############
ncatt_put(nc_file,"temp_uo.mean","missing_value",m_v$value)
ncatt_put(nc_file,"temp_uo.mean","valid_max",400)
ncatt_put(nc_file,"temp_uo.mean","valid_min",200)
ncatt_put(nc_file,"temp_uo.mean","submodel",sbmdl$value)
ncatt_put(nc_file,"temp_uo.mean","stash_code",s_c$value)
ncatt_put(nc_file,"temp_uo.mean","_FillValue",F_V$value)
ncatt_put(nc_file,"temp_uo.mean","processing",p$value)
ncatt_put(nc_file,"temp_uo.mean","pcmdi_name",pcmdi_name$value)
ncatt_put(nc_file,"temp_uo.mean","pp_name",pp_name$value)
ncatt_put(nc_file,"temp_uo.mean","standard_name",std_name$value)
ncatt_put(nc_file,"temp_uo.mean","long_name","MEAN TEMPERATURE AT 1.5M")
ncatt_put(nc_file,"temp_uo.mean","time",time$value)
ncatt_put(nc_file,"temp_uo.mean","title","MEAN TEMPERATURE AT 1.5M")
ncatt_put(nc_file,"temp_uo.mean","name","temp_uo.mean")
print(nc_file)
nc_close(nc_file)
nc_file<-nc_open("deglg.vn1_0.temp_uo.mean.ANN.001")
tmp_array<-ncvar_get(nc_file,"temp_uo.mean")
head(tmp_array)
