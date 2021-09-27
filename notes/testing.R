
rm(list=ls())
library(tidyverse)
load_all()
data(pubmed_data)

chk <- pubmed_data %>% 
  #slice_head() %>% 
  detect_ancestry_terms(fk_pmid, abstract) %>% 
  detect_cultural_terms(fk_pmid, abstract) %>% 
  detect_disability_terms(fk_pmid, abstract) %>% 
  detect_discrimination_terms(fk_pmid, abstract) %>% 
  detect_diversity_terms(fk_pmid, abstract) %>% 
  detect_disability_terms(fk_pmid, abstract) %>%
  detect_sexgender_terms(fk_pmid, abstract)
