
rm(list=ls())
library(tidyverse)
load_all()
data(pubmed_data)


pubmed_data %>% 
  slice_head() %>% 
  funnel_through(fk_pmid, abstract, sex_gender) %>% 
  select(fk_pmid, abstract, sex_gender)
