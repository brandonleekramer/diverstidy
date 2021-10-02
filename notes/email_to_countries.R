#' Match emails to countries 
#'
#' This function matches email domains to countries based on email sub-domains. 
#' For example, users with an email domain ending in ".br" will be matched to Brazil. 
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param id A numeric or character vector unique to each entry.
#' @param input Character vector of emails or email domains.
#' @param output Desired name of classified organization column.
#'
#' @examples
#'
#' library(tidyverse)
#' library(tidyorgs)
#' data(github_users)
#'
#' classified_by_email <- github_users %>%
#'   email_to_countries(login, email, organization)
#'
#' @export
email_to_countries <- function(data, id, input, 
                               # UPDATE EMAIL PARSING BY "|"
                               output = c("country", "iso_2", "iso_3", "iso_numeric", "iso_domain", 
                                          "continent", "region", "sub_region", "int_region", "lat_lon", 
                                          "country_english", "country_chinese", "country_russian", 
                                          "country_french", "country_spanish", "country_arabic")){ 
  # 0. check for errors 
  if (missing(id)) { 
    return(print("Error: 'id' column requires numeric or character vector."))
  } else if (missing(input)) { 
    return(print("Error: 'input' column requires character vector."))
  } 
    
  # 1. convert all vars with enquos
  id <- enquo(id)
  input <- enquo(input)
  output <- rlang::arg_match(output)
  
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
  all_domains_df <- data %>%
    tidyr::drop_na(!!input) %>% # drop missing emails
    dplyr::mutate("{{ input }}" := tolower(!!input)) %>% # all domains to lower
    dplyr::mutate(domain = sub('.*@', '', !!input)) %>% # extract domain info after @ sign
    dplyr::mutate(domain = sub('.*\\.', '.', domain)) %>% # extract the last .domain
    # matches all of the root domains with several exceptions (bc of industry appropriation)
    dplyr::filter(domain %in% country_vector 
                  & domain != ".ag" & domain != ".ai" & domain != ".as" & domain != ".cc"   
                  & domain != ".fm" & domain != ".io" & domain != ".im" & domain != ".me") %>%  
    dplyr::mutate(domain = str_replace(domain, '\\.', '')) 
  #4. uses str_replace_all() to recode all domains into countries
  all_domains_df <- all_domains_df %>% 
    dplyr::mutate(geo_code = stringr::str_replace_all(domain, country_dictionary)) %>%
    dplyr::select(!!id, geo_code)
  # 5. removes all of the duplicates and combines those with multiple countries
  all_domains_df <- all_domains_df %>% 
    dplyr::distinct(across(everything())) %>%
    dplyr::group_by(!!id, geo_code) %>%
    dplyr::mutate(geo_code = paste(geo_code, collapse = "|")) %>% 
    dplyr::distinct(across(everything())) %>%
    dplyr::mutate("{{output}}" := dplyr::na_if(geo_code, "NA")) %>% 
    dplyr::rename_all(~stringr::str_replace_all(.,"\"","")) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-geo_code)
  all_domains_df
}