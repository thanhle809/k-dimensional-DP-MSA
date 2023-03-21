#' @title update the score of a cell
#' @param current current cell id whose score needs to be filled
#' @param dict hashtable containing cellid-score, to retrieve Fscore
#' @param dict.pred hashtable containing cellid-predecessor
#' @param sscoretab hashtable of predecessor-sscore

update.score <- function(current, dict, dict.pred, sscoretab) {
  # first cell
  noofseq <- length(unlist(str_split(current, pattern = "-")))
  firstcell <- paste0(rep(1, noofseq), collapse = "-")

  predecessors <- hashkeys(sscoretab)
  score <- -Inf
  for (i in predecessors) {
    Fscore <- dict[[i]]
    sscore <- sscoretab[[i]]
    if (score < Fscore + sscore) {
      predecessor <- i
      score <- Fscore + sscore
    }
    dict[[current]] <- score
    dict.pred[[current]] <- predecessor
  }
  dict.pred[[firstcell]] <- "NA"
}
