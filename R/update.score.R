# This function takes in the current id and the list of predecessors with their s.score
# , get the max score, and update the score in the main hashtable

  # retrive Fscore of predecessor
  # sum Fscore and sscore
  # go to current cell and update with the max
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
