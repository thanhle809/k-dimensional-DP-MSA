#'@title display hash table as a dataframe
#'@param hashtable a hash table as a result from hashtab function


hash.astable <- function(hashtable) {
  keys <- hashkeys(hashtable)
  values <- c()
  for (i in keys) {
    values <- c(values, hashtable[[i]])
  }
  df <- data.frame(keys = unlist(keys), values = values)
  return(df)
}
