#' Match messy text data to countries using a 'funneling' method
#'
#' This function allocates entries of unstructured text data to countries 
#' extracting and iterating through consecutive word sequences (or n-grams). 
#' To do this, the function extracts n-grams using the tidytext 
#' package, matching all sequences in the unstructured data that have n words 
#' and then 'funneling' through all sequences of n-1, n-2, etc. words before 
#' matching the single tokens. This function returns a dataframe akin to the 
#' original dataframe with one additional column aligning with the "output" parameter.
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param id A numeric or character vector unique to each entry.
#' @param input Character vector of messy or unstructured text that will
#' be unnested as n-grams and matched to dictionary of organizations in specified sector.
#' @param output Output column. Options include 'country','iso2','iso3','continent', 'region', 'subregion' and 'intermediate_region'.
#' 'region','subregion', or 'intermediate_region'.
#' @param regions Optional argument to detect major regions (e.g., states) in each country. Defaults to TRUE. 
#' @param cities Optional argument to detect major cities in each country. Defaults to TRUE.
#' @param denonyms Optional argument to detect denonyms of inhabitants of each country. Defaults to TRUE.
#' @param abbreviations Optional argument to detect abbreviations used for countries, regions, and cities. 
#' Options include 'all', 'only', 'omit', and 'prob_match' (default).
#'
#' @examples
#'
#' library(tidyverse)
#' library(diverstidy)
#' data(github_users)
#'
#' classified_by_text <- github_users %>%
#'   detect_geographies(login, location, country, )
#'
#' @export
detect_geographies <- function(data, id, input, 
                              output = c("country", "iso_2", "iso_3", 
                                         "iso_numeric", "iso_domain", 
                                         "continent", "flag", "region", 
                                         "sub_region", "int_region", "lat_lon", 
                                         "country_english", "country_chinese", 
                                         "country_russian", "country_french", 
                                         "country_spanish", "country_arabic"), 
                              email, #regions = TRUE, 
                              cities = TRUE, denonyms = TRUE 
                              #abbreviations = TRUE # c("prob_match", "all", "only", "omit"), 
                              ){
  # 2. convert all vars with enquos
  id <- enquo(id)
  input <- enquo(input)
  output <- rlang::arg_match(output)
  `%notin%` <- Negate(`%in%`)
  # 1. prep the dictionary procedure 
  # 1a. pull in countries dictionary 
  dictionary <- countries_data
  # 1b. create all the conditions to detect countries, regions and cities 
  dictionary <- dictionary %>% 
    dplyr::rename(catch_terms = countries, recode_column = recode_countries)
  
  if (missing(id)) { 
    return(print("Error: 'id' column requires numeric or character vector."))
  } else if (missing(input)) { 
    return(print("Error: 'input' column requires character vector."))
  } else if (missing(email)) { 
    return(print("Error: 'email' column requires character vector."))
  #} else if (regions == TRUE) {dictionary <- dictionary %>% 
  #  tidyr::unite(catch_terms, c("catch_terms", "regions"), sep="|") %>% 
  #  tidyr::unite(recode_column, c("recode_column", "recode_regions"), sep="|") %>% 
  #  dplyr::mutate(catch_terms = stringr::str_replace_all(catch_terms, "\\|NULL", "")) %>% 
  #  dplyr::mutate(recode_column = stringr::str_replace_all(recode_column, "\\|NULL", ""))
  } else if (cities == TRUE) {dictionary <- dictionary %>% 
    tidyr::unite(catch_terms, c("catch_terms", "cities"), sep="|") %>% 
    tidyr::unite(recode_column, c("recode_column", "recode_cities"), sep="|") %>% 
    dplyr::mutate(catch_terms = stringr::str_replace_all(catch_terms, "\\|NULL", "")) %>% 
    dplyr::mutate(recode_column = stringr::str_replace_all(recode_column, "\\|NULL", ""))
  } else if (denonyms == TRUE) {dictionary <- dictionary %>% 
    tidyr::unite(catch_terms, c("catch_terms", "denonyms"), sep="|") %>% 
    tidyr::unite(recode_column, c("recode_column", "recode_denonyms"), sep="|") %>% 
    dplyr::mutate(catch_terms = stringr::str_replace_all(catch_terms, "\\|NULL", "")) %>% 
    dplyr::mutate(recode_column = stringr::str_replace_all(recode_column, "\\|NULL", ""))
  #} else if (abbreviations == TRUE) {dictionary <- dictionary %>% 
  #  tidyr::unite(catch_terms, c("catch_terms", "abbreviations"), sep="|") %>% 
  #  tidyr::unite(recode_column, c("recode_column", "recode_abbreviations"), sep="|") %>% 
  #  dplyr::mutate(catch_terms = stringr::str_replace_all(catch_terms, "\\|NULL", "")) %>% 
  #  dplyr::mutate(recode_column = stringr::str_replace_all(recode_column, "\\|NULL", ""))
  } else { dictionary }
  
  # 1d. prior to running the for loop, we need the max string length 
  max_n <- dictionary %>%
    tidyr::unnest_legacy(catch_terms = base::strsplit(catch_terms, "\\|")) %>%
    dplyr::mutate(word_count = lengths(base::strsplit(catch_terms, "\\W+"))) 
  max_n <- max(max_n$word_count) 
  
  # 3. drop missing, convert to lower case, convert foreign characters to english
  suppressMessages(correct_strings <- readr::read_csv("data-raw/diverstidy - abb_syms.csv"))
  correct_strings <- correct_strings %>% 
    dplyr::mutate(recode_column = paste0(" ",recode_column," ")) %>%
    dplyr::select(original_string, recode_column) %>% tibble::deframe()

  data <- data %>% 
    tidyr::drop_na(!!input) %>%
    dplyr::mutate(cached_input = !!input, 
                  "{{input}}" := tolower(!!input),
                  "{{input}}" := stringr::str_replace_all(!!input, "/", " "),
                  "{{input}}" := stringr::str_replace_all(!!input, "\\.", " "),
                  "{{input}}" := stringr::str_replace_all(!!input, "·", " "),
                  "{{input}}" := stringr::str_replace_all(!!input, "\\b(:)\\b", " "),
                  "{{input}}" := stringr::str_replace_all(!!input, "u\\.s\\.","united states"),
                  "{{input}}" := stringr::str_replace_all(!!input, "u\\.s\\.a\\.","united states"),
                  "{{input}}" := stringr::str_replace_all(location, correct_strings),
                  "{{input}}" := stringr::str_replace_all(!!input, ",", " "))
  
  # 4. use a for loop to funnel match n-grams of lengths 2-12 
  funnelized <- data.frame()
  for (n_word in max_n:2) {
    # note: 6 is the longest string in the location data (as of 08-25-2021)
    subdictionary <- dictionary %>%
      tidyr::unnest_legacy(catch_terms = base::strsplit(catch_terms, "\\|")) %>%
      dplyr::mutate(word_count = lengths(base::strsplit(catch_terms, "\\W+"))) %>%
      dplyr::filter(word_count == n_word)
    subdictionary <- na.omit(subdictionary$catch_terms)
    funnelized <- data %>%
      tidytext::unnest_tokens(words, !!input, token="ngrams", n=n_word, to_lower = TRUE) %>%
      dplyr::filter(words %in% subdictionary) %>%
      dplyr::select(!!id, words) %>%
      dplyr::bind_rows(funnelized)
  }
  # 5. funnel match on all of the single tokens 
  subdictionary <- dictionary %>%
    tidyr::unnest_legacy(catch_terms = base::strsplit(catch_terms, "\\|")) %>%
    dplyr::mutate(word_count = lengths(base::strsplit(catch_terms, "\\W+"))) %>%
    dplyr::filter(word_count == 1)
  subdictionary <- na.omit(subdictionary$catch_terms)
  funnelized <- data %>%
    #dplyr::filter(!!id %notin% ids_to_filter) %>%
    tidytext::unnest_tokens(words, !!input) %>%
    dplyr::filter(words %in% subdictionary) %>%
    dplyr::select(!!id, words) %>%
    dplyr::bind_rows(funnelized) %>% 
    dplyr::select(!!id, words) 
  dictionary <- dictionary %>%
    dplyr::mutate(original_string = paste0("\\b(?i)(",recode_column,")\\b")) %>%
    dplyr::select(original_string, !!output) %>% tibble::deframe()
  all_matched_data <- funnelized %>%
    dplyr::mutate(geo_code = stringr::str_replace_all(words, dictionary)) %>% 
    dplyr::select(!!id, geo_code) 
  all_matched_data <- all_matched_data %>% 
    dplyr::distinct(across(everything())) %>%
    dplyr::group_by(!!id, geo_code) %>%
    dplyr::mutate(geo_code = paste(geo_code, collapse = "|")) %>% 
    dplyr::distinct(across(everything())) %>%
    dplyr::mutate(geo_code = dplyr::na_if(geo_code, "NA")) %>% 
    dplyr::rename("{{output}}" := geo_code) %>% 
    dplyr::rename_all(~stringr::str_replace_all(.,"\"",""))

  if (missing(email)) { 
    
    all_matched_data 
    
  } else {
    
    email <- enquo(email)
    
    # 2. pull out all of the country domains from the dictionary 
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
      dplyr::select(iso_domain, !!output) %>% tibble::deframe()
    
    # 3. drop missing emails, all domains to lower, extract domain info after @ sign
    matched_by_email <- data %>%
      tidyr::drop_na(!!email) %>% # drop missing emails
      dplyr::mutate("{{ email }}" := tolower(!!email)) %>% # all domains to lower
      dplyr::mutate(domain = sub('.*@', '', !!email)) %>% # extract domain info after @ sign
      dplyr::mutate(domain = sub('.*\\.', '.', domain)) %>% # extract the last .domain
      # matches all of the root domains with several exceptions (bc of industry appropriation)
      dplyr::filter(domain %in% country_vector 
                    & domain != ".ag" & domain != ".ai" & domain != ".am" & domain != ".as" & domain != ".cc"   
                    & domain != ".fm" & domain != ".io" & domain != ".im" & domain != ".me") %>%  
      dplyr::mutate(domain = str_replace(domain, '\\.', '')) 
    #4. uses str_replace_all() to recode all domains into countries
    matched_by_email <- matched_by_email %>% 
      dplyr::mutate(geo_code = stringr::str_replace_all(domain, country_dictionary)) %>%
      dplyr::select(!!id, geo_code)
    # 5. removes all of the duplicates and combines those with multiple countries
    matched_by_email <- matched_by_email %>% 
      dplyr::distinct(across(everything())) %>%
      dplyr::group_by(!!id, geo_code) %>%
      dplyr::mutate(geo_code = paste(geo_code, collapse = "|")) %>% 
      dplyr::distinct(across(everything())) %>%
      dplyr::mutate("{{output}}" := dplyr::na_if(geo_code, "NA")) %>% 
      dplyr::rename_all(~stringr::str_replace_all(.,"\"","")) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(-geo_code)
    all_matched_data <- dplyr::bind_rows(all_matched_data, matched_by_email)
  } 
  suppressMessages(
    data <- data %>% 
      dplyr::left_join(all_matched_data) %>%
      dplyr::rename(geo_code = !!output) %>% 
      dplyr::distinct(across(everything())) %>%
      dplyr::group_by(!!id, !!input, !!email) %>%
      dplyr::mutate(geo_code =  paste0(geo_code, collapse = "|")) %>% 
      dplyr::distinct(across(everything())) %>%
      dplyr::mutate("{{output}}" := dplyr::na_if(geo_code, "NA")) %>% 
      dplyr::rename_all(~stringr::str_replace_all(.,"\"","")) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate("{{ input }}" := cached_input) %>% 
      dplyr::select(-geo_code, -cached_input) 
  ) 
  data
}