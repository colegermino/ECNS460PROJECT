library(terra)
library(tidyverse)

#Bring in weather rasters
tmean = rast("raw_data/teton_weather_rasters/teton_tmean_raster.tif")
tmax = rast("raw_data/teton_weather_rasters/teton_tmax_raster.tif")
tmin = rast("raw_data/teton_weather_rasters/teton_tmin_raster.tif")
ppt = rast("raw_data/teton_weather_rasters/teton_ppt_raster.tif")
tdmean = rast("raw_data/teton_weather_rasters/teton_tdmean_raster.tif")
vpdmin = rast("raw_data/teton_weather_rasters/teton_vpdmin_raster.tif")
vpdmax = rast("raw_data/teton_weather_rasters/teton_vpdmax_raster.tif")

#Creates a dataset of all of them together
weather_data = sds(tmean, tmax, tmin, ppt, tdmean, vpdmin, vpdmax)

#add time to raster
start_date = as.Date("2020-01-01")
end_date = as.Date("2023-12-31")
date_range = seq(as.Date(start_date), as.Date(end_date), by = "day")
terra::time(weather_data) = date_range

#Teton National Park avalanche data
avalanches_pre = read.csv("raw_data/MasterAVYdata.csv")

#needed a little bit more cleaning to be compatible----
pattern = c("Lat: ", latitude = ".*?", "Lng: ", longitude = ".*?")
date_pattern = c(tmonth = ".*?", "/", tday = ".*?", "/", tyear = ".*?")

avalanches = avalanches_pre |>
  separate_wider_regex(cols = Location,
                       patterns = pattern,
                       too_few = "align_start") |>
  separate_wider_regex(cols = Date,
                       patterns = date_pattern,
                       too_few = "align_start") |>
  mutate(date = paste("20", tyear, "/", tmonth, "/", tday, sep = "")) |>
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude),
         date = as.Date(date),
         simple_date = format(date, "%Y%m%d"),
         date_minus30 = date - 30,
         simple_date_minus30 = format(date_minus30, "%Y%m%d"))|>
  filter(date < end_date)

#Make points from avalanches----

#making a vector of points where avalanche observations take place
ava_vect = vect(avalanches, geom = c("longitude", "latitude"), crs = "NAD83")

#extract data from the raster for each observation
avalanches_tmean = terra::extractRange(tmean, ava_vect, first = as.character(ava_vect$simple_date_minus30), last = as.character(ava_vect$simple_date))

avalanches_tmean_df = bind_rows(avalanches_tmean)

rename_weather_variables = function(df, variable){
  renamed_df = df |>
    rename_all()
}

#test
avalanches_tmean[[1]] |> view()


