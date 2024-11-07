library(terra)
library(tidyverse)
library(purrr)
library(elevatr)
library(sf)
library(tmap)

#Preparing Raster Data----

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

#Creating Random Avalanche Points-----

#get extent
avalanche_ext = vect("raw_data/grte_tracts/grte_boundary.shp")

#sample points
random_control = spatSample(avalanche_ext,
                            size = 1000,
                            method = "random")

crs(random_control) = "NAD83"

#generate random dates to apply to points
set.seed(12345)
random_dates = sample(seq((start_date + 30), end_date, by = "day"), length(random_control))
simple_random_dates = format(random_dates, "%Y%m%d")

random_dates_minus30 = random_dates - 30
simple_random_dates_minus30 = format(random_dates_minus30, "%Y%m%d")

#apply dates to random points
random_control$date = random_dates
random_control$date_minus30 = random_dates_minus30
random_control$simple_date = simple_random_dates
random_control$simple_date_minus30 = simple_random_dates_minus30
random_control$ID = seq(1,length(random_control))


#Extract data from the raster----

name_weather_vars = function(df, variable){
  var_names = sapply(seq(30, 0, -1), function(x) paste(variable, "_lead", as.character(x), sep = ""))
  setNames(df, var_names)
}

#makes a dataframe of weather data
make_weather_df = function(raster, avalanches_vector, var_name){
  fun_extract = terra::extractRange(raster,
                                    avalanches_vector,
                                    first = avalanches_vector$simple_date_minus30,
                                    last = avalanches_vector$simple_date,
                                    ID = TRUE)
  
  # turn the weather data from each individual into a tibbles
  fun_extract = lapply(fun_extract, tibble)
  
  #Change the names of the tibbles and Add Id row back in
  fun_extract = lapply(fun_extract, function(x) name_weather_vars(x, var_name))
  fun_extract = bind_rows(fun_extract) |>
    mutate(ID = row_number())
}

random_table = as_tibble(random_control)

#Extract elevation for each point
elevation = get_elev_point(locations = st_as_sf(random_control), prj = "NAD83", src = "epqs")|>
  select(ID, elevation)

#Extract weather variables for each point

#These are each a table of lead weather variables for each avalanches observation
avalanches_tmean = make_weather_df(tmean, random_control, "tmean")
random_table = left_join(random_table, avalanches_tmean)

avalanches_tmax = make_weather_df(tmax, random_control, "tmax")
random_table = left_join(random_table, avalanches_tmax)

avalanches_tmin = make_weather_df(tmin, random_control, "tmin")
random_table = left_join(random_table, avalanches_tmin)

avalanches_ppt = make_weather_df(ppt, random_control, "ppt")
random_table = left_join(random_table, avalanches_ppt)

avalanches_tdmean = make_weather_df(tdmean, random_control, "tdmean")
random_table = left_join(random_table, avalanches_tdmean)

avalanches_vpdmin = make_weather_df(vpdmin, random_control, "vpdmin")
random_table = left_join(random_table, avalanches_vpdmin)

avalanches_vpdmax = make_weather_df(vpdmax, random_control, "vpdmax")
random_table = left_join(random_table, avalanches_vpdmax)

random_table = left_join(random_table, elevation)


#messing with visuals
tmap_mode("plot")

tm_shape(st_as_sf(avalanche_ext)) + tm_borders() + 
  tm_shape(st_as_sf(random_control)) + tm_dots() +
  tm_shape(st_as_sf(ava_vect)) + tm_dots(col = "red")

big_tibble = bind_rows(list(big_avalanche, random_table), .id = "dcat")

big_tibble |> ggplot(aes(elevation)) +
  geom_density(aes(color = dcat))

big_tibble |> ggplot(aes(ppt_lead2)) +
  geom_density(aes(color = dcat))