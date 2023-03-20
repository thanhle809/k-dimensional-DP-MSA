#' @title Build a hashtable with all possible ids with empty score
#' @param idlist list of possible id resulting from the generate_id function
#' @return a hash table
build.dict <- function(idlist) {
  dict <- hashtab()
  for (i in idlist) {
    sethash(h = dict, key = i, value = NULL)
  }
  return(dict)
}
