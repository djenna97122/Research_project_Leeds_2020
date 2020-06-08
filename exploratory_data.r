install.packages("ncdf4") 
install.packages("sp")
install.packages("rgdal") 
install.packages("raster")
install.packages("cmsaf")
install.packages("RNetCDF")
library(sp) ; library(rgdal) ; library(raster);library(ncdf4);library(cmsaf);library(RNetCDF)
##### Read data #######
climate_fields<-list( "temp_mm_1_5m","temp_mm_uo","merid_Atlantic_ym_dpth","iceconc_mm_srf")
setwd("/Volumes/annie/earpal/database/experiments/deglg/vn1_0/temp_mm_uo/001")
filename <- "deglg.vn1_0.temp_mm_uo.monthly.ANN.001.nc"
data<-nc_open(filename)
print(data)
att<-attributes(data$var)
att_name<-att$names
########## Get the dimensions #########

lat<-ncvar_get(data, "latitude")
lon<-ncvar_get(data, "longitude")
t <- ncvar_get(data, "t")
t1<-t[1:5000]
tunits <- ncatt_get(data, "t", "units")

##### Get the variable & attributes #############


tmp_array <- ncvar_get(data,att_name)
dim(tmp_array)
temps_mean<-t1
tim=1
###### Get the area weighted mean on lat & long
for (tim in 1:dim(t1)){
  vt<-tmp_array[ , ,tim] #matrix lon = rows, lat= col
  gridt=raster(vt) 
  hasValues(gridt)
  values(gridt)[1:10]
  a<-area(gridt,na.rm=TRUE, weights=TRUE) 
  temps_mean[tim] <-weighted.mean(vt, values(a), na.rm = FALSE)
};
plot(t1,temps_mean,type='l')


######## Create a new ncdf file #########
dim_new<-ncdim_def("t",units="year",vals=t1,create_dimvar=TRUE, calendar="360_day",)
var<-ncvar_def("temp_mean_1_5m",units="K",dim=dim_new)
global_summary<-nc_create("global_summary_temps",var)
print(global_summary)

m_v<-ncatt_get( data, att_name, "missing_value", verbose=FALSE )
v_max<-ncatt_get(data, att_name, "valid_max")
v_min<-ncatt_get(data, att_name, "valid_min")
sbmdl<-ncatt_get( data, att_name, "submodel")
s_c<-ncatt_get( data, att_name, "stash_code")
F_V<-ncatt_get( data, att_name, "_FillValue")
p<-ncatt_get( data, att_name, "processing")
pcmdi_name<-ncatt_get( data, att_name,"pcmdi_name" )
pp_name<-ncatt_get( data, att_name, "pp_name")
std_name<-ncatt_get( data, att_name, "standard_name")
ln<-ncatt_get( data, att_name, "long_name")
time<-ncatt_get( data, att_name, "time")
ncatt_put(global_summary,"temp_mean_1_5m","missing_value",m_v$value)
ncatt_put(global_summary,"temp_mean_1_5m","valid_max",400)
ncatt_put(global_summary,"temp_mean_1_5m","valid_min",200)
ncatt_put(global_summary,"temp_mean_1_5m","submodel",sbmdl$value)
ncatt_put(global_summary,"temp_mean_1_5m","stash_code",s_c$value)
ncatt_put(global_summary,"temp_mean_1_5m","_FillValue",F_V$value)
ncatt_put(global_summary,"temp_mean_1_5m","processing",p$value)
ncatt_put(global_summary,"temp_mean_1_5m","pcmdi_name",pcmdi_name$value)
ncatt_put(global_summary,"temp_mean_1_5m","pp_name",pp_name$value)
ncatt_put(global_summary,"temp_mean_1_5m","standard_name",std_name$value)
ncatt_put(global_summary,"temp_mean_1_5m","long_name","MEAN TEMPERATURE AT 1.5M")
ncatt_put(global_summary,"temp_mean_1_5m","time",time$value)
ncatt_put(global_summary,"temp_mean_1_5m","title","MEAN TEMPERATURE AT 1.5M")
ncatt_put(global_summary,"temp_mean_1_5m","name","temp_mean_1_5m")
print(global_summary)
ncatted(global_summary,"missin_value","temp_mean_1_5m",d)
nc_close(global_summary)
nc_open("global_summary_temps")

