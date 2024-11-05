library(terra)
library(tidyverse)
library(purrr)

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
formatted_date_range = format(date_range, "%Y%m%d")
names(weather_data) = formatted_date_range

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
  filter(date < end_date) |>
  filter(date > start_date + 30) |> 
  mutate(ID = row_number())

#Make points from avalanches----

#making a vector of points where avalanche observations take place
ava_vect = vect(avalanches, geom = c("longitude", "latitude"), crs = "NAD83")


#Changes the names of a tibble to the names I want
name_weather_vars = function(df, variable){
  var_names = sapply(seq(30, 0, -1), function(x) paste(variable, "_lead", as.character(x), sep= ""))
  setNames(df, var_names)
}

#extract data from appropriate raster for each avalanche
make_weather_df = function(raster, avalanches_vector, var_name){
  fun_extract = terra::extractRange(raster,
                                    avalanches_vector,
                                    first = avalanches_vector$simple_date_minus30,
                                    last = avalanches_vector$simple_date,
                                    ID = TRUE)

# turn the weather data from each individual into a tibble
fun_extract = lapply(fun_extract, tibble)

#Change the names of the tibbles and Add Id row back in
fun_extract = lapply(fun_extract, function(x) name_weather_vars(x, var_name))
fun_extract = bind_rows(fun_extract) |>
  mutate(id = row_number())
}

#These are each a table of lead weather variables for each avalanches observation
avalanches_tmean = make_weather_df(tmean, ava_vect, "tmean")
avalanches_tmax = make_weather_df(tmax, ava_vect, "tmax")
avalanches_tmin = make_weather_df(tmin, ava_vect, "tmin")
avalanches_ppt = make_weather_df(ppt, ava_vect, "ppt")
avalanches_tdmean = make_weather_df(tdmean, ava_vect, "tdmean")
avalanches_vpdmin = make_weather_df(vpdmin, ava_vect, "vpdmin")
avalanches_vpdmax = make_weather_df(vpdmax, ava_vect, "vpdmax")


