# function to display hashtable as a table

hash.astable <- function(hashtable) {
  keys <- hashkeys(hashtable)
  values <- c()
  for(i in keys) {
    values <- c(values, hashtable[[i]])
  }
  df <- data.frame(keys = unlist(keys), values = values)
  return(df)
}
