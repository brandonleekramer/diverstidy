





funnel_through <- function(data, id, input, category){

  id <- enquo(id)
  input <- enquo(input)
  category <- enquo(category)
  `%notin%` <- Negate(`%in%`)
  
  category_string <- as.character(category)
  
  dictionary <- readr::read_rds(file = "R/diversity_dictionary.rds") %>% 
    mutate(category = stringr::str_replace(category, "/", "_")) %>% 
    filter(category == category_string)
  
  max_n <- dictionary %>%
    tidyr::unnest_legacy(catch_terms = base::strsplit(catch_terms, "\\|")) %>%
    dplyr::mutate(word_count = lengths(base::strsplit(catch_terms, "\\W+"))) 
  max_n <- max(max_n$word_count)  
  
  funnelized <- data.frame()
  data <- data %>% dplyr::mutate("{{input}}" := tolower(!!input))
  
  for (n_word in max_n:2) {
    # note: 12 is an arbitrary number that will eventually correspond to largest n in dictionary
    subdictionary <- dictionary %>%
      tidyr::unnest_legacy(catch_terms = base::strsplit(catch_terms, "\\|")) %>%
      dplyr::mutate(word_count = lengths(base::strsplit(catch_terms, "\\W+"))) %>%
      dplyr::filter(word_count == n_word)
    subdictionary <- na.omit(subdictionary$catch_terms)
    funnelized <- data %>%
      tidytext::unnest_tokens(words, !!input, token="ngrams", n=n_word, to_lower = TRUE) %>%
      dplyr::filter(words %in% subdictionary) %>%
      dplyr::mutate("{{category}}" := 1) %>%
      dplyr::filter(!!category == 1) %>%
      dplyr::select(!!id, words, !!category) %>%
      dplyr::bind_rows(funnelized)
  }
  
  subdictionary <- dictionary %>%
    tidyr::unnest_legacy(catch_terms = base::strsplit(catch_terms, "\\|")) %>%
    dplyr::mutate(word_count = lengths(base::strsplit(catch_terms, "\\W+"))) %>%
    dplyr::filter(word_count == 1)
  subdictionary <- na.omit(subdictionary$catch_terms)
  funnelized <- data %>%
    tidytext::unnest_tokens(words, !!input) %>%
    dplyr::filter(words %in% subdictionary) %>%
    dplyr::mutate("{{category}}" := 1) %>%
    dplyr::select(!!id, !!category) %>%
    dplyr::bind_rows(funnelized) %>% 
    dplyr::group_by(!!id) %>% 
    dplyr::summarise("{{category}}" := sum(!!category))
  funnelized <- data %>% 
    left_join(funnelized) %>% 
    dplyr::mutate("{{category}}" := replace_na(!!category, 0))
  funnelized

}









