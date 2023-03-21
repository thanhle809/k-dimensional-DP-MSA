#' @title Get sequence length vectors
#' @param seqtable a table with column 1 being seqname and 2 being sequence
return.seqlengths <- function(seqtable) {
  sequence <- seqtable$sequence
  lengthvec <- c()
  for (i in sequence) {
    lengthvec <- c(lengthvec, str_length(i))
  }
  return(lengthvec)
}
