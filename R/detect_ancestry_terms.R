#' Detect ancestry and heritage terms in unstructured text data 
#'
#' This function detects ancestry and heritage terms in unstructured text data. The input 
#' will be a character vector of text data such as a biomedical abstract, a Twitter bio, 
#' or a chapter from a novel. The output column will provide the number of ancestry and 
#' heritage terms detected in the entry. 
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param id A numeric or character vector unique to each entry.
#' @param input Character vector of text data for ancestry terms to be detected.
#'
#' @examples
#'
#' library(tidyverse)
#' library(diverstidy)
#' data(pubmed_data)
#'
#' detected_terms <- pubmed_data %>%
#'   detect_ancestry_terms(fk_pmid, abstract)
#'   
#' @export
detect_ancestry_terms <- function(data, id, input){
  id <- dplyr::enquo(id)
  input <- dplyr::enquo(input)
  data <- data %>% 
    diversity::funnel_match(!!id, !!input, ancestry, "ancestry")
  data 
}