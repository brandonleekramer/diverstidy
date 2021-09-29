
rm(list=ls())
library(tidyverse)
library(tidytext)
library(diverstidy)
library(igraph)
library(ggraph)
library(tidygraph)
load_all()
data(pubmed_data)
data(diversity_dictionary)

pubmed_data %>%
  detect_racialethnic_terms(fk_pmid, abstract) %>%
  detect_sexgender_terms(fk_pmid, abstract) %>% 
  detect_socialclass_terms(fk_pmid, abstract) %>% 
  group_by(year) %>% 
  summarize(racial_ethnic = sum(racial_ethnic),
            sex_gender = sum(sex_gender),
            social_class = sum(social_class)) %>% 
  pivot_longer(!year, names_to = "category", 
               values_to = "count") %>% 
  ggplot(aes(x=year, y=count, group=category)) +
  geom_line(aes(color=category), size = 1) +
  theme_bw() +
  ggtitle("Change in Diversity-Related Terms Over Time")

# graphing 

pubmed_graph <- pubmed_data %>%
  unnest_tokens(bigram, abstract, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  count(word1, word2, sort = TRUE) %>%
  filter(n > 100) %>%
  graph_from_data_frame() 

dictionary_terms <- diversity_dictionary %>% 
  unnest_legacy(name = strsplit(catch_terms, "\\|")) %>% 
  select(name, category)
  
nodelist <- data.frame(id = c(1:(igraph::vcount(pubmed_graph))), name = igraph::V(pubmed_graph)$name)
nodelist <- nodelist %>% 
  left_join(dictionary_terms, by = "name") %>% 
  mutate(category = replace_na(category, "nothing"),
         category = str_replace(category, "race/ethnicity\\|us omb terms", "us omb terms"))
V(pubmed_graph)$category <- nodelist$category

graph_tbl <- pubmed_graph %>% 
  as_tbl_graph() %>% 
  activate(nodes) %>% 
  mutate(degree  = centrality_degree()) %>% 
  mutate(new_name = ifelse(str_detect(
    name, str_c("\\b(?i)(",paste0(dictionary_terms$name, collapse = "|"),")\\b")), name, no = ""))

cols_f <- colorRampPalette(c("#D3D3D3", RColorBrewer::brewer.pal(9, 'Spectral')))

layout <- create_layout(graph_tbl, layout = 'igraph', algorithm = 'nicely')
ggraph(layout) +
  geom_edge_fan(aes(alpha = ..index..), show.legend = F) + 
  geom_node_point(aes(size = degree, 
                      color = as.factor(category)), 
                  show.legend = F) +
  geom_node_text(aes(label = new_name), vjust = 1, hjust = 1) +
  scale_color_manual(limits = as.factor(layout$category), 
                     values = cols_f(nrow(layout))) +  
  theme_void()







  






