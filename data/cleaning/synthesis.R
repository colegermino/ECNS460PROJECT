library(terra)

tmean = rast("raw_data/teton_weather_rasters/teton_tmean_raster.tif")
tmax = rast("raw_data/teton_weather_rasters/teton_tmax_raster.tif")
tmin = rast("raw_data/teton_weather_rasters/teton_tmin_raster.tif")
ppt = rast("raw_data/teton_weather_rasters/teton_ppt_raster.tif")
tdmean = rast("raw_data/teton_weather_rasters/teton_tdmean_raster.tif")
vpdmin = rast("raw_data/teton_weather_rasters/teton_vpdmin_raster.tif")
vpdmax = rast("raw_data/teton_weather_rasters/teton_vpdmax_raster.tif")

weather_data = sds(tmean, tmax, tmin, ppt, tdmean, vpdmin, vpdmax)

read.csv2()