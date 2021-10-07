#' Detect socioeconomic and other social class terms in unstructured text data 
#'
#' This function detects social class terms in unstructured text data. The input 
#' will be a character vector of text data such as a biomedical abstract, a Twitter bio, 
#' or a chapter from a novel. The output column will provide the number of social class 
#' terms detected in the entry. 
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param id A numeric or character vector unique to each entry.
#' @param input Character vector of text data for social class terms to be detected.
#'
#' @examples
#'
#' library(tidyverse)
#' library(diverstidy)
#' data(pubmed_data)
#'
#' detected_terms <- pubmed_data %>%
#'   detect_socialclass_terms(fk_pmid, abstract)
#'   
#' @export
detect_socialclass_terms <- function(data, id, input){
  id <- dplyr::enquo(id)
  input <- dplyr::enquo(input)
  data <- data %>% 
    diverstidy::funnel_match(!!id, !!input, social_class, "social class")
  data 
}