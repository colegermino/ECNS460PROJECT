library(terra)
library(prism)
library(polite)


prism_dl_dir = "/Users/jonas/Library/CloudStorage/OneDrive-MontanaStateUniversity/ECNS 460/teton_nf_prism_download"
prism_set_dl_dir("/Users/jonas/Library/CloudStorage/OneDrive-MontanaStateUniversity/ECNS 460/teton_nf_prism_download")

start_date = as.Date("2020-01-01")
simple_start_date = format(start_date, "%Y%m%d")
end_date = as.Date("2023-12-31")
simple_end_date = format(end_date, "%Y%m%d")

date_range = seq(as.Date(start_date), as.Date(end_date), by = "day")
date_range = sapply(date_range, function(x) {format(x, "%Y%m%d")})

#This is the vector file of teton NF boundaries
teton_nf_boundary = vect("raw_data/grte_tracts/grte_boundary.shp")
#The CRS is NAD83

CRS_use = "NAD83"

weather_variables = c("tmean", "tmin", "tmax", "ppt", "tdmean", "vpdmin", "vpdmax")

#download weather data from PRISM for each variable of interest
#do not rerun this -- it's a HUGE download
'''

for (wvar in weather_variables){
  get_prism_dailys(type = wvar,
                   minDate = start_date,
                   maxDate = end_date)
}

'''

#makes a teton NP specific version of the PRISM data for date and variable
get_raster = function(variable, date){
  weather_rast = rast(paste(prism_dl_dir, "/PRISM_",variable,"_stable_4kmD2_",date,"_bil/PRISM_",variable,"_stable_4kmD2_",date,"_bil.bil", sep = ""))
  weather_rast = project(weather_rast, "NAD83")
  weather_rast_masked = mask(weather_rast, teton_nf_boundary)
  weather_rast_masked = crop(weather_rast_masked, teton_nf_boundary)
  
  return(weather_rast_masked)
}

make_variable_raster = function(variable, date_range){
  weather_var_raster = sapply(date_range, function(x){get_raster(variable, x)})
  weather_var_raster = rast(weather_var_raster)
  
  return(weather_var_raster)
}

weather_data = sapply(weather_variables, function(x) {make_variable_raster(x, date_range)})

weather_data = sds(weather_data)

#This could mess shit up to run if weather_data is not in memory(idrk)
'''
writeRaster(weather_data[1], "raw_data/teton_weather_rasters/teton_tmean_raster.tif", overwrite = T)
writeRaster(weather_data[2], "raw_data/teton_weather_rasters/teton_tmin_raster.tif", overwrite = T)
writeRaster(weather_data[3], "raw_data/teton_weather_rasters/teton_tmax_raster.tif", overwrite = T)
writeRaster(weather_data[4], "raw_data/teton_weather_rasters/teton_ppt_raster.tif", overwrite = T)
writeRaster(weather_data[5], "raw_data/teton_weather_rasters/teton_tdmean_raster.tif", overwrite = T)
writeRaster(weather_data[6], "raw_data/teton_weather_rasters/teton_vpdmin_raster.tif", overwrite = T)
writeRaster(weather_data[7], "raw_data/teton_weather_rasters/teton_vpdmax_raster.tif", overwrite = T)
'''

#This is just a test
test = rast("/Users/jonas/Library/CloudStorage/OneDrive-MontanaStateUniversity/ECNS 460/ECNS460PROJECT/data/raw_data/teton_weather_rasters/teton_tmax_raster.tif")

plot(test, 180)
