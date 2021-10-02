#' Convert messy text and email data into standardized country names or country codes
#'
#' This function standardizes messy text data that contains city, state, and/or country names 
#' as well as email domains into standardized country names. The detect_countries() function 
#' integrates tidyorgs's text_to_countries() and email_to_countries() together, providing 
#' the capacity to match users to countries for organizational or geographic analysis in a tidy framework.
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param id A numeric or character vector unique to each entry.
#' @param input Character vector of text data that includes the name of cities, states, 
#' and/or countries that will be standardized into country names or country codes.
#' @param output Output column. Options include 'country','iso2','iso3','continent', 'region', 'subregion' and 'intermediate_region'. 
#' If multiple countries are detected, they will be separated by the "|" symbol.
#' @param email Character vector of email or email domain information.
#' @param regions Optional argument to detect major regions (e.g., states) in each country. Defaults to TRUE. 
#' @param cities Optional argument to detect major cities in each country. Defaults to TRUE.
#' @param denonyms Optional argument to detect denonyms of inhabitants of each country. Defaults to TRUE.
#'
#' @examples
#'
#' library(tidyverse)
#' library(tidyorgs)
#' data(github_users)
#'
#' classified_users <- github_users %>%
#'   detect_countries(login, location, country_name, email)
#'   
#' @export
detect_countries <- function(data, id, input, 
                             output = c("country", "iso_2", "iso_3", "iso_numeric", "iso_domain", 
                                        "continent", "region", "sub_region", "int_region", "lat_lon", 
                                        "country_english", "country_chinese", "country_russian", 
                                        "country_french", "country_spanish", "country_arabic"), 
                             email, regions = FALSE, cities = FALSE, denonyms = FALSE, abbreviations = FALSE){ 
  # TODO: need to add an if clause in the case that email = FALSE
  # 1. convert all vars with enquos
  id <- enquo(id)
  input <- enquo(input)
  output <- rlang::arg_match(output)
  email <- enquo(email)
  regions <- rlang::arg_match(regions)
  cities <- rlang::arg_match(cities)
  denonyms <- rlang::arg_match(denonyms)
  abbreviations <- rlang::arg_match(abbreviations)
  `%notin%` <- Negate(`%in%`)
  # 2. match by text entries 
  all_matched_data <- data %>%
    text_to_countries(!!id, !!input, output, !!regions, !!cities, !!denonyms, !!abbreviations)
  already_classified <- all_matched_data[,1]
  # 3. match by all emails 
  matched_by_email <- data %>%
    dplyr::filter(!!id %notin% already_classified) %>% # check
    email_to_countries(!!id, !!email, output) 
  all_matched_data <- dplyr::bind_rows(all_matched_data, matched_by_email) 
  # 4. join back to the original dataset and remove duplicates
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
    dplyr::select(-geo_code)
  ) 
  data
}