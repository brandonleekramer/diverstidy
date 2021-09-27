detect_racialethnic_terms <- function(data, id, input){
  id <- enquo(id)
  input <- enquo(input)
  data <- data %>% 
    funnel_match(!!id, !!input, racial_ethnic, "race_ethnicity")
  data 
}