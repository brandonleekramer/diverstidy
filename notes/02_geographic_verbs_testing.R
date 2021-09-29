 
##### location testing 

rm(list=ls())
library("tidyverse")
library("RPostgreSQL")
load_all()

github_users <- tidyorgs::github_users

# testing sub-algos first - they work 
text_to_countries_df <- github_users %>%
  text_to_countries(login, location, country_name)
email_to_countries_df <- github_users %>%
  email_to_countries(login, email, country_name)

load_all()
# joining classified academic country information first 
detected_countries_df <- github_users %>% 
#russian_classified <- github_users %>%
  #left_join(academic_classified, by = "login") %>% 
  #unite("location", c("location", "country"), sep = " ") %>% 
  #mutate(location = str_replace(location, " NA", "")) %>%
  #filter(grepl("russia", location)) %>% 
  detect_countries(login, location, country_name, email) 


#dictionary <- tidyorgs::countries_data %>% 
duplicates <- read_csv("data-raw/diverstidy - countries.csv") %>% 
  separate_rows(catch_terms, sep = "\\|") %>%
  janitor::get_dupes(catch_terms) %>% 
  select(catch_terms, country_name, dupe_count) %>% 
  group_by(catch_terms, dupe_count) %>% 
  mutate(country_name = paste0(country_name, collapse = "|")) %>% 
  distinct_all()

countries <- read_csv("data-raw/diverstidy - countries.csv") %>% 
  tidyr::unnest_legacy(catch_terms = base::strsplit(catch_terms, "\\|")) %>% 
  select(catch_terms, everything()) %>% 
  distinct(catch_terms, .keep_all = TRUE) %>% 
  group_by(recode_column, country_name, country_domain, iso2, iso3, checked_for_dupes, notes) %>% 
  mutate(catch_terms = paste0(catch_terms, collapse = "|")) %>% 
  distinct_all()

countries <- countries %>% 
  tidyr::unnest_legacy(recode_column = base::strsplit(recode_column, "\\|")) %>% 
  select(recode_column, everything()) %>% 
  distinct(recode_column, .keep_all = TRUE) %>% 
  group_by(catch_terms, country_name, country_domain, iso2, iso3, checked_for_dupes, notes) %>% 
  mutate(recode_column = paste0(recode_column, collapse = "|")) %>% 
  distinct_all()

readr::write_csv(countries, "data-raw/countries_dd.csv")



detect_countries(data, id, input, 
                 output = c("country_name","iso2","iso3"),
                 regions = T, cities = T, emails = email)


detect_us_states(data, id, input, output,
                 format = c("state_name","postal_abb","std_abb","lat_lon"),
                 cities = T, emails = email)







only_detected <- detected_countries_df %>% 
  drop_na(country_name)

not_detected_with_valid_data <- detected_countries_df %>% 
  ungroup() %>% 
  filter(is.na(country_name)) %>% 
  select(-email) %>% 
  mutate(location = na_if(location, "NA")) %>% 
  filter(!is.na(location) | !is.na(company))

bigrams <- not_detected %>% 
  tidytext::unnest_tokens(bigram, location, token = "ngrams", n = 2) %>% 
  group_by(bigram) %>% 
  count() %>% 
  arrange(-n)
         
trigrams <- not_detected %>% 
  tidytext::unnest_tokens(bigram, location, token = "ngrams", n = 3) %>% 
  group_by(bigram) %>% 
  count() %>% 
  arrange(-n)
  
detected_counts <- only_detected %>% 
  separate_rows(country_name, sep = "\\|") %>% 
  group_by(country_name) %>% 
  count() %>% 
  arrange(-n)

with_valid_data <- github_users %>%
  filter(!is.na(location) | !is.na(company) | !is.na(email))

nonvalid_data <- github_users %>%
  filter(is.na(location) & is.na(company) & is.na(email)) 

table_to_database <- detected_countries_df %>% 
  left_join(academic_classified, by = "login") %>% 
  rename(country_raw = country) %>% 
  select(login, organization, country_name, academic, email, company, location, country_raw) %>% 
  mutate(academic = replace_na(academic, 0)) 

# write that table to the database 
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
dbWriteTable(conn, c("gh", "ctrs_classified_0821"), 
             table_to_database, row.names = FALSE)
dbDisconnect(conn)













