library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)
library(rvest)


#Scrapes avalanche data from GFNAC
avalanche_url = "https://www.mtavalanche.com/avalanche-activity/archive/439"

#if (avalanche_page) stop()
avalanche_page = read_html(avalanche_url)

selector1 = "div.ui-accordion-content ui-corner-bottom ui-helper-reset ui-widget-content"
selector2 = "div.inc-details-left"

records = avalanche_page |>
  html_elements(selector2)|>
  html_text2()

#Converts list to data frame
avalanches0 = as.data.frame(records)

#replace \n with random (not in data) number
str_count(avalanches0$records, "143")

new_line = regex("143")

stopifnot(sum(str_count(avalanches0$records, new_line)) == 0)


avalanches1 = avalanches0 |>
  mutate(records = str_replace_all(records, "\n", new_line))


#Attmepts to clean. Still needs a lot of work
pattern = c(location = ".*?",
            new_line, ".*",
            "Elevation:",
            elevation = ".*?",
            new_line,
            ".*",
            "Aspect:",
            aspect = ".*?",
            new_line,
            ".*",
            "Coordinates:",
            coordinates = ".*?",
            new_line,
            ".*")


avalanches2 = separate_wider_regex(avalanches1,
                                   cols = records,
                                   patterns = pattern,
                                   too_few = "align_start"
)

write_csv(avalanches2,"avalanches.csv")