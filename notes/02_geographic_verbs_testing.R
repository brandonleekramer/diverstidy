 


rm(list=ls())
library("tidyverse")
library("RPostgreSQL")
load_all()
github_users <- tidyorgs::github_users
text_to_countries_df <- github_users %>%
  detect_geographies(login, location, "country", email, regions = FALSE, 
                     cities = TRUE, denonyms = TRUE, abbreviations = FALSE)
detected_count <- text_to_countries_df %>% drop_na(country) %>% select(-company)
to_code <- text_to_countries_df %>% filter(is.na(country)) 


suppressMessages(correct_strings <- readr::read_csv("data-raw/diverstidy - correct_strings.csv"))
correct_strings <- correct_strings %>% 
  dplyr::mutate(recode_column = paste0(" ",recode_column," ")) %>%
  dplyr::select(original_string, recode_column) %>% tibble::deframe()
chk <- to_code %>% 
  mutate(location = tolower(location)) %>% 
  mutate(location_new = str_replace_all(location, correct_strings))

1960/2344

rm(list=ls())
library("tidyverse")
library("RPostgreSQL")
load_all()

conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
github_users <- dbGetQuery(conn, "SELECT login, company, location, email 
                           FROM gh.ctrs_clean_0821 LIMIT 10000")
dbDisconnect(conn)




try_this <- github_users %>% 
  filter(grepl("中|国", location))

try_this <- try_this %>% 
  dplyr::mutate(location2 = stringr::str_replace_all(location, foreign_dictionary)) 



##### location testing 

rm(list=ls())
library("tidyverse")
library("RPostgreSQL")
load_all()
github_users <- tidyorgs::github_users
text_to_countries_df <- github_users %>%
  text_to_countries(login, location, "country", regions = FALSE, 
                    cities = TRUE, denonyms = TRUE, abbreviations = FALSE)

text_to_countries_df <- github_users %>% 
  left_join(text_to_countries_df, by = "login")


email_to_countries_df <- github_users %>%
  email_to_countries(login, email, "region")

rm(list=ls())
library("tidyverse")
github_users <- tidyorgs::github_users
load_all()
detected_countries_df <- github_users %>% 
  detect_countries(login, location, "country", email, regions = TRUE, 
                   cities = TRUE, denonyms = FALSE, abbreviations = FALSE) 

detected_count <- detected_countries_df %>% drop_na(country)


chk <- github_users %>% 
  mutate(location = str_replace(location, ",", ""))
  tidytext::unnest_tokens(words, location, token="ngrams", n=3, to_lower = TRUE)




detect_us_states(data, id, input, output,
                 format = c("state_name","postal_abb","std_abb","lat_lon"),
                 cities = T, emails = email)








