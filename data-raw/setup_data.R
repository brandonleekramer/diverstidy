# diversity dictionary --------------------------------------------------

library(dplyr)
diversity_dictionary <- readr::read_csv("data-raw/diverstidy - diversity_types.csv") %>% 
  select(-source)
readr::write_rds(diversity_dictionary, "R/diversity_dictionary.rds")
usethis::use_data(diversity_dictionary, overwrite = TRUE)
usethis::use_data(diversity_dictionary, internal = TRUE, overwrite = TRUE)

# pubmed sample ----------------------------------------------------------

library(RPostgreSQL)
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
pubmed_data <- dbGetQuery(conn, "SELECT fk_pmid, year, abstract, publication 
                                 FROM pubmed_2021.soc_diversity_abstracts_0721 LIMIT 1000;")
dbDisconnect(conn)
readr::write_rds(pubmed_data, "R/pubmed_data.rds")
usethis::use_data(pubmed_data, overwrite = TRUE)
usethis::use_data(pubmed_data, internal = TRUE, overwrite = TRUE)

# countries data ---------------------------------------------

library(tidyverse)
world_cities <- read_csv("data-raw/worldcities_raw.csv")
world_cities_edited <- world_cities %>% 
  arrange(country) %>% 
  mutate(city = tolower(city), 
         city_ascii = tolower(city_ascii)) %>% 
  unite("city_combined", c("city", "city_ascii"), sep = "|") %>% 
  mutate(city_combined = str_replace(city_combined, "\\.", " ")) %>% 
  select(city_combined, country) %>% 
  separate_rows(city_combined, sep = "\\|") %>%
  distinct(city_combined, country) %>% 
  group_by(country) %>% 
  mutate(city_combined = paste(city_combined, collapse = "|")) %>% 
  distinct(city_combined, country)
readr::write_csv(world_cities_edited, "data-raw/worldcities_collapsed.csv")

library(dplyr)
countries_data <- readr::read_csv("data-raw/diverstidy - countries.csv")
#readr::write_rds(countries_data, "R/countries_data.rds")
usethis::use_data(countries_data, overwrite = TRUE)
usethis::use_data(countries_data, internal = TRUE, overwrite = TRUE)

library(dplyr)
string_corrections <- readr::read_csv("data-raw/diverstidy - abb_syms.csv")
#readr::write_rds(string_corrections, "R/string_corrections.rds")
usethis::use_data(string_corrections, overwrite = TRUE)
usethis::use_data(string_corrections, internal = TRUE, overwrite = TRUE)




