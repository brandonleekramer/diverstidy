detect_lifecourse_terms <- function(data, id, input){
  id <- enquo(id)
  input <- enquo(input)
  data <- data %>% 
    funnel_match(!!id, !!input, lifecourse, "lifecourse")
  data 
}