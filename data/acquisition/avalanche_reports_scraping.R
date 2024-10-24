#Scrapes Avalanche Reports from GFNAC

#last updated 10/23/2024 by Jonas


library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)
library(rvest)
library(polite)


url_22 = "https://www.mtavalanche.com/avalanche-activity/archive/441"
url_21 = "https://www.mtavalanche.com/avalanche-activity/archive/440"
url_20 = "https://www.mtavalanche.com/avalanche-activity/archive/439"
url_19 = "https://www.mtavalanche.com/avalanche-activity/archive/438"
url_18 = "https://www.mtavalanche.com/avalanche-activity/archive/43"

urls = c(url_22, url_21, url_20, url_19, url_18)

htmls = lapply(urls, read_html)

reports_selector = 'div.incidents-table'

date_selector = 'div.inc-date'
region_selector = 'div.inc-region'
location_selector = 'div.inc-location'



#build a function
dates = htmls[[1]] |>
  html_elements(css = reports_selector) |>
  html_element(css = date_selector) |>
  html_text2()
  

regions = htmls[[1]] |>
  html_elements(css = reports_selector) |>
  html_element(css = region_selector) |>
  html_text2()

locations = htmls[[1]] |>
  html_elements(css = reports_selector) |>
  html_element(css = location_selector) |>
  html_text2()

view(locations)
view(regions)

view(dates)

locations[119]

#Gets one variable from the GFNAC HTML code
get_gfnac_var = function(html, var_selector){
  reports_selector = 'div.incidents-table'
  variable = html |>
    html_elements(css = reports_selector) |>
    html_element(css = var_selector) |>
    html_text2()
  return(variable)
}

#Gets variables of interest from GFNAC HTML code
get_gfnac_vars = function(html){
  reports_selector = 'div.incidents-table'
  date_selector = 'div.inc-date'
  region_selector = 'div.inc-region'
  location_selector = 'div.inc-location'
  
  date = get_gfnac_var(html, date_selector)
  region = get_gfnac_var(html, region_selector)
  location = get_gfnac_var(html, location_selector)
  
  tibble(dates, regions, locations)
}

av2023 = get_gfnac_vars(htmls[[1]])

reports = bind_rows(lapply(htmls, get_gfnac_vars))

write_csv2(reports, "raw_data/avalanche_reports_f18_to_s23.csv", append = FALSE)