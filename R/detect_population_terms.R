#' Detect group or population-related terms in unstructured text data 
#'
#' This function detects group/population terms in unstructured text data. The input 
#' will be a character vector of text data such as a biomedical abstract, a Twitter bio, 
#' or a chapter from a novel. The output column will provide the number of group/population terms
#' detected in the entry. 
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param id A numeric or character vector unique to each entry.
#' @param input Character vector of text data for group/population terms to be detected.
#'
#' @examples
#'
#' library(tidyverse)
#' library(diverstidy)
#' data(pubmed_data)
#'
#' detected_terms <- pubmed_data %>%
#'   detect_population_terms(fk_pmid, abstract)
#'   
#' @export
detect_population_terms <- function(data, id, input){
  id <- dplyr::enquo(id)
  input <- dplyr::enquo(input)
  data <- data %>% 
    diverstidy::funnel_match(!!id, !!input, population, "population")
  data 
}