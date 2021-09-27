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






