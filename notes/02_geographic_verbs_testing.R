 

rm(list=ls())
#library("devtools")
library("tidyverse")
library("RPostgreSQL")
devtools::load_all()

# get data 
conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
#test <- dbGetQuery(conn, "SELECT * FROM gh.ctrs_raw")
github_users <- dbGetQuery(conn, "SELECT login, company, location, email FROM gh.ctrs_clean_0821 limit 100")
dbDisconnect(conn)

setwd("~/git/diverstidy/data-raw/")
saveRDS(text_to_countries_df, file = "classified.rds", compress = TRUE)

github_users <- tidyorgs::github_users
devtools::load_all()
chk <- github_users %>%
  detect_geographies(login, location, "country", email)


chk <- github_users %>%
  detect_geographies(login, location, "country", email) %>% 
  detect_geographies(login, country, "iso_2") %>% 
  detect_geographies(login, country, "flag") %>% 
  select(login, location, country, iso_2, flag)

 
##### TESTING 

library("tidyverse")
library("RPostgreSQL")
text_to_countries <- 
  read_rds("~/Documents/git/diverstidy/data-raw/working-data/text_to_countries.rds")
devtools::load_all()
start <- Sys.time()
text_to_countries_new <- text_to_countries %>%
  detect_geographies(login, location, "country", email)
diff = Sys.time() - start; diff


chk <- text_to_countries_new %>% 
  filter(grepl("\\|", country))



devtools::load_all()
text_to_countries_df2 <- chk %>% 
  slice(1:10000) %>% 
  select(-country_original) %>% 
  #filter(grepl("Argentina |Colombia ", country)) %>% 
  detect_geographies(login, location, "country", email) 

total_classified <- text_to_countries_new %>% 
  drop_na(country)

total_to_classify <- text_to_countries_new %>% 
  filter((!is.na(email) & !grepl(".com$", email)) | !is.na(location)) %>% 
  filter(!grepl("(?i)(earth|milky way|^/dev|/home|/etc|/usr|^0x|^3rd rock|^/bin127.0.0.1|^world|^@|^anywhere|^somewhere|mars|remote|internet|the internet|localhost|everywhere|global|home|cyberspace|moon|online|null|hell|the world|unknown|behind you|the universe|universe)", location))

1216298 / 1501252 # all valid email & location data 
1216298 / 1286886 # with earth etc removed 
1216298 / 1301728 # with .com removed 


country_count <- text_to_countries_new %>% 
  tidyr::unnest_legacy(country = strsplit(country, "\\|")) %>% 
  count(country) %>% 
  arrange(-n)

chk <- text_to_countries_chk %>% 
  filter(!is.na(email) | !is.na(location)) %>% 
  select(login, company, location, email) %>% 
  left_join(text_to_countries_df %>% select(login, country), by = "login") %>% 
  filter(is.na(country) & !grepl("\\.com$", email)) 


country_dictionary <- countries_data
country_dictionary <- country_dictionary %>%
  tidyr::drop_na(iso_domain) %>%
  tidyr::unnest_legacy(iso_domain = base::strsplit(iso_domain, "\\|")) %>% 
  dplyr::select(iso_domain, country) 
country_vector <- na.omit(country_dictionary$iso_domain)
country_dictionary <- country_dictionary %>%
  dplyr::mutate(beginning = "\\b(?i)(", ending = ")\\b", 
                iso_domain = str_replace(iso_domain, "\\.", ""),
                iso_domain = paste0(beginning, iso_domain, ending)) %>%
  dplyr::select(iso_domain, country) %>% tibble::deframe()


matched_by_email <- text_to_countries_new %>%
  tidyr::drop_na(email) %>% # drop missing emails
  dplyr::mutate(email = tolower(email)) %>% # all domains to lower
  dplyr::mutate(domain = sub('.*@', '', email)) %>% # extract domain info after @ sign
  dplyr::mutate(domain = sub('.*\\.', '.', domain)) %>% # extract the last .domain
  # matches all of the root domains with several exceptions (bc of industry appropriation)
  dplyr::filter(domain %in% country_vector 
                & domain != ".ag" & domain != ".ai" & domain != ".am" 
                & domain != ".as" & domain != ".cc" & domain != ".fm" 
                & domain != ".io" & domain != ".im" & domain != ".me") %>%  
  dplyr::mutate(domain = str_replace(domain, '\\.', '')) 
# 6c. uses str_replace_all() to recode all domains into countries
matched_by_email <- matched_by_email %>% 
  dplyr::mutate(geo_code = stringr::str_replace_all(domain, country_dictionary)) %>%
  dplyr::select(login, geo_code)










detected_count <- text_to_countries_df %>% 
  drop_na(country) %>% select(-company)
to_code <- text_to_countries_df %>% 
  filter(is.na(country) & 
           !grepl("(?i)(earth|milky way|^/dev|/home|/etc|/usr|^0x|^3rd rock|^/bin127.0.0.1|^world|^@|^anywhere|^somewhere|mars|remote|internet|the internet|localhost|everywhere|global|home|cyberspace|moon|online|null|hell|the world|unknown|behind you|the universe|universe)", location)) %>% 
  filter(grepl("[a-z]", location))

top_entries_1 <- to_code %>% 
  mutate(location = tolower(location)) %>% 
  count(location) %>% 
  arrange(-n)
sum(top_entries_1$n)




country_dictionary <- countries_data
country_dictionary <- country_dictionary %>%
  tidyr::drop_na(iso_domain) %>%
  tidyr::unnest_legacy(iso_domain = base::strsplit(iso_domain, "\\|")) %>% 
  dplyr::select(iso_domain, !!output) 
country_vector <- na.omit(country_dictionary$iso_domain)
country_dictionary <- country_dictionary %>%
  dplyr::mutate(beginning = "\\b(?i)(", ending = ")\\b", 
                iso_domain = str_replace(iso_domain, "\\.", ""),
                iso_domain = paste0(beginning, iso_domain, ending)) %>%
  dplyr::select(iso_domain, country) %>% tibble::deframe()

chk_emails <- text_to_countries_df %>% 
  filter(is.na(country)) %>% 
      tidyr::drop_na(email) %>% # drop missing emails
      dplyr::mutate(email = tolower(email)) %>% # all domains to lower
      dplyr::mutate(domain = sub('.*@', '', email)) %>% # extract domain info after @ sign
      dplyr::mutate(domain = sub('.*\\.', '.', domain)) %>% 
  dplyr::mutate(domain = str_replace(domain, '\\.', '')) %>% 
  dplyr::mutate(geo_code = stringr::str_replace_all(domain, country_dictionary))

lets_see <- github_users %>% 
  filter(!is.na(location) | !is.na(company) | !is.na(email))

load_all()
other_detection_df <- text_to_countries_df %>% 
  filter(is.na(country)) %>% 
  drop_na(location) %>% 
  drop_na(email) %>% 
  select(-country) %>% 
  detect_geographies(login, location, "country", email)
  
top_entries_2 <- other_detection_df %>% 
  filter(is.na(country)) %>% 
  mutate(location = tolower(location)) %>% 
  count(location) %>% 
  arrange(-n)
sum(top_entries_2$n)
(1195347 + 1459) / 1260266
(1195347 + 1459) / 3212546
756000 / 1260266

# if country exists must recode this with a warning that says "country_original"

suppressMessages(correct_strings <- readr::read_csv("data-raw/diverstidy - abb_syms.csv"))
correct_strings <- correct_strings %>% 
  dplyr::mutate(recode_column = paste0(" ",recode_column," ")) %>%
  dplyr::select(original_string, recode_column) %>% tibble::deframe()
chk <- to_code %>% 
  data.table::as.data.table() %>% 
  maditr::dt_mutate(geo_code = stringr::str_replace_all(location, correct_strings)) %>% 
  dplyr::as_data_frame()


chk <- to_code %>% 
  mutate(location = tolower(location)) %>% 
  mutate(location_new = str_replace_all(location, correct_strings))

valid_data <- github_users %>% 
  drop_na(location)

before_ab_corrections = 1184921/1260266 
before_ab_time = 38.6
after_ab_corrections = 1187575/1260266  
before_ab_time = 40.29796

rm(list=ls())
library("tidyverse")
library("RPostgreSQL")
load_all()



start <- Sys.time()
test_this <- github_users %>% 
  tidyr::drop_na(location) %>%
  filter(grepl(",", location)) %>% 
  mutate(location_new = str_replace_all(location, string_corrections))
diff = Sys.time() - start; diff

2062/2344

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


country_count <- text_to_countries_df %>% 
  tidyr::unnest_legacy(country = strsplit(country, "\\|")) %>% 
  count(country) %>% 
  arrange(-n)


library(progress)

for (i in 1:10) {
  pb$tick()
  Sys.sleep(1 / 10)
}



