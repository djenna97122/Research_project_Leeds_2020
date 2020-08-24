import xarray as xarray
import matplotlib.pyplot as plt
import cartopy.crs as ccrs

ds=xr.open_dataset('/nfr/see-fs-01_.nc')
ice= ds.iceconc_ym_uo.isel(t=0).isel(unspecified=0)#unspecified = depth
lon=ice.longitude.values
lat=ice.latitude.values
plt.pcolormesh(ice)

projection_map=ccrs.PlateCarree()

figMap = plt.figure(figsize=(5,5),dpi=300)
axMap=figMap.add_subplot(1,1,1,projection=projection_map)

axMap.pcolormesh(lon,lat ice,transform=ccrs.PlateCarree(),cmap='Blues)
axMap.contourf(lon,lat ice,transform=ccrs.PlateCarree())
axMap.coastlines()
axMap.set_global()

-> see cartopy website
