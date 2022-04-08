make_matrix_safe <- function(x) {
  if ((class(x) == "numeric") | (class(x) == "integer")) {
    x <- data.frame(t(as.matrix(x, nrow=1))) # t returns transpose of the data frame
    x
  }else {
  x
  }}
  
  