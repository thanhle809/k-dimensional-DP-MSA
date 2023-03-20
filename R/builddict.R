# build.dict: receive sequence list, create a hash table as a dictionary to store id, score, and origin
build.dict <- function(idlist) {
  dict <- hashtab()
  for (i in idlist) {
    sethash(h = dict, key = i, value = NULL)
  }
  return(dict)
}
