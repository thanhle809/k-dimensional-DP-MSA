# this function traces back the dict.pred to generate the MSA visualiztion
# dict.pred is a hashtable with keys being the cells and values being the predecessor
returnPath <- function(dict.pred) {
  dict.pred <- hash.astable(dict.pred) %>%
    arrange(., desc(keys))

  # a function that takes in the current cell, look up the predecessor and move the
  # current pointer to the predecessort
  lookup <- function(current) {
    index <- which(dict.pred$keys == current)
    return(index)
  }

  current <- dict.pred[1, ]$keys
  path <- c(current)
  index <- lookup(current)

  while (dict.pred[index, ]$values != "NA") {
    predecessor <- dict.pred[index, ]$values
    path<- c(path, predecessor)
    index <- lookup(predecessor)
  }

  return(path)
}
