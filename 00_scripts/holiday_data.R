
# Get Spanish holidays with focus on Andalusia


# 1.0 Libraries ----

library(tidyverse)
library(rvest)
library(XML)
library(tictoc)

# 2.0 Setup ----

cities <- c("granada", "almeria", "cadiz", "cordoba", "huelva", "jaen", "malaga", "sevilla") #jarez, tarifa ?

base_url      <- "https://calendarios.ideal.es"
andalusia_url <- paste0(base_url, "/laboral/andalucia/")
full_url      <- paste0(andalusia_url, cities)
partial_url   <- str_remove(andalusia_url, base_url)


years <- 2017:2030
full_url_all_years <- paste0(full_url, "/", rep(years, each = length(full_url)))


# 2.1 Get all URL
url_list <- list()

for(i in seq_along(full_url_all_years)) {

  temp_partial_url <- str_remove(full_url_all_years[i], base_url)
  temp_partial_url <- str_replace_all(temp_partial_url, "[:digit:]", "")
  
  tmp_data <- full_url_all_years[i] %>% 
    read_html() %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    as_tibble() %>% 
    filter(str_detect(value, temp_partial_url)) %>% 
    filter(str_detect(value, "[:digit:]"))
  
  url_list[[i]] <- tmp_data
  
  }

url_list_final <- unlist(url_list)
url_list_final <- paste0(base_url, url_list_final)

tibble(url = url_list_final) %>% write_csv("01_data/url_list.csv")


# 3.0 Content ----

holiday_data <- list()

tic()

for(i in seq_along(url_list_final)) {
  
  temp_holiday_data <- url_list_final[i] %>% 
    read_html() %>% 
    html_nodes(".voc-list-default") %>% 
    html_text()
  
  holiday_data[[i]] <- temp_holiday_data

}

toc()

holiday_data %>% 
  write_rds("01_data/spanish_holidays.rds")
