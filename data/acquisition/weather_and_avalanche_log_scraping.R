#Webscraping data weather and incident logs from GFNAC

#Last updated 10/23/2024 by Jonas

library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)
library(rvest)
library(polite)

#setwd("/Users/jonas/Library/CloudStorage/OneDrive-MontanaStateUniversity/ECNS 460/ECNS460PROJECT/data")

url_22 = "https://www.mtavalanche.com/weather/wx-avalanche-log/archive/441"
url_21 = "https://www.mtavalanche.com/weather/wx-avalanche-log/archive/440"
url_20 = "https://www.mtavalanche.com/weather/wx-avalanche-log/archive/439"
url_19 = "https://www.mtavalanche.com/weather/wx-avalanche-log/archive/438"
url_18 = "https://www.mtavalanche.com/weather/wx-avalanche-log/archive/43"

urls = c(url_22, url_21, url_20, url_19, url_18)

htmls = lapply(urls, read_html)

#Test
html_22 = read_html(url_22)

incidents_22 = html_22 |> html_element("table") |> html_table()

#function to make lapply work

get_table = function(html_page){
  incidents = html_page |> html_element("table") |> html_table()
}

incidents = lapply(htmls, get_table)


all_incidents = bind_rows(incidents)

write_csv2(all_incidents, "raw_data/weather_and_avalanche_logs_f18_to_s23.csv", append = FALSE)
