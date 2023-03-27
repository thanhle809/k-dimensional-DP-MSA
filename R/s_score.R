#' @title Calculate the sscore of a column in the MSA table
#' @description Return the sscore of a column, based on residue vector and gap penalty
#' @param scoremat scoring matrix
#' @param gappen vector of gap penalties
#' @param resvec list of residues in that column
#' @export
#' @return sscore
s.score <- function(scoremat, resvec, gappen) {
  scorelist <- c()
  score <- 0

  if (length(resvec) != 1) {
    for (i in sequence(length(resvec) - 1)) {
      for (j in seq(from = i + 1, to = length(resvec))) {
        score <- scoremat[resvec[i], resvec[j]]
        scorelist <- c(scorelist, score)
      }
    }
    scorelist <- c(scorelist, gappen)
    score <- mean(scorelist)
  }
  return(score)
}
