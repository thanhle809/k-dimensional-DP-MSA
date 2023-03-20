# s.score: the function that takes in a vector of all residues in a column
# and calculate their average match score
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
