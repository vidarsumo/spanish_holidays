
# Extracting information from scraped data

# 1.0 LIBRARIES AND DATA ----
library(tidyverse)
library(lubridate)


holiday_tbl <- read_rds("01_data/spanish_holidays.rds")
url_tbl <- read_csv("01_data/url_list.csv")

no_days_per_url <- map_dbl(holiday_tbl, length)


# 2.0 Wranling ----

url_tbl <- url_tbl %>% 
  mutate(partial = str_remove(url, "https://calendarios.ideal.es/laboral/andalucia/")) %>% 
  separate(partial, c("Province","Municipality", "year"), sep = "/") %>% 
  select(-year)

url_large_tbl <- tibble(url = rep(url_tbl$url, no_days_per_url))

url_large_tbl <- url_large_tbl %>% 
  mutate(partial = str_remove(url, "https://calendarios.ideal.es/laboral/andalucia/")) %>% 
  separate(partial, c("Province","Municipality", "year"), sep = "/") %>% 
  select(-year)


# 3.0 EXTRACTION ----

text_tbl <- tibble(text = unlist(holiday_tbl))

text_tbl <- text_tbl %>% 
  mutate(
    month = 
      case_when(str_detect(text, "enero")      ~ 1,
                str_detect(text, "febrero")    ~ 2,
                str_detect(text, "marzo")      ~ 3,
                str_detect(text, "abril")      ~ 4,
                str_detect(text, "mayo")       ~ 5,
                str_detect(text, "junio")      ~ 6,
                str_detect(text, "julio")      ~ 7,
                str_detect(text, "agosto")     ~ 8,
                str_detect(text, "septiembre") ~ 9,
                str_detect(text, "octubre")    ~ 10,
                str_detect(text, "noviembre")  ~ 11,
                str_detect(text, "diciembre")  ~ 12
                ),
    
    day  = parse_number(text),
    year = str_extract_all(text, "\\(?[0-9.]+\\)?")[[1]][2]
    ) %>% 
  mutate(date = make_date(year, month, day)) %>% 
  select(date)
  

text_tbl <- text_tbl %>% 
  bind_cols(url_large_tbl %>% select(-url)) %>% 
  drop_na(date) %>% 
  mutate(date_week = floor_date(date, "week", week_start = 1))

text_tbl %>% 
  write_csv("01_data/date_province_data.csv")
