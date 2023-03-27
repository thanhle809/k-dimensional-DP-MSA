#' @title Generate trace back path for MSA visualization
#' @param dict.pred a hash table of cellid:predecessor
#' @import dplyr
#' @return path

returnPath <- function(dict.pred) {
  dict.pred <- hash.astable(dict.pred) %>%
    arrange(desc(eval(parse(text="keys"))))

  # a function that takes in the current cell, look up the predecessor and move the
  # current pointer to the predecessort
  lookup <- function(current) {
    index <- which(eval(parse(text="dict.pred$keys == current")))
    return(index)
  }

  current <- eval(parse(text="dict.pred[1, ]$keys"))
  path <- c(current)
  index <- lookup(current)

  while (dict.pred[index, ]$values != "NA") {
    predecessor <- dict.pred[index, ]$values
    path <- c(path, predecessor)
    index <- lookup(predecessor)
  }

  return(path)
}
