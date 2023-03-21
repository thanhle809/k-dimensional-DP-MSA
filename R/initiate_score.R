#' @title Generate the scores along the axes
#' @param dict hashtable of cell id and score
#' @param listofseqlength vector of sequence length

initiate.score <- function(dict, listofseqlength) {
  noofseq <- length(listofseqlength)
  for (i in sequence(noofseq)) {
    before <- i - 1
    after <- noofseq - i
    for (g in seq(2, listofseqlength[i])) {
      onebefore <- rep("1", before)
      oneafter <- rep("1", after)
      gappen <- (g - 1) * (-2)
      vec <- c(onebefore, g, oneafter)
      id <- paste(vec, collapse = "-")
      sethash(dict, id, gappen)
    }
  }
  root <- paste(rep("1", noofseq), collapse = "-")
  sethash(dict, root, 0)
  return(dict)
}
