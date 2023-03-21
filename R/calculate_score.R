#' @title Calculate the sscore for possible predecessors.
#' @description the function returns a hash table of the pair predecessor - sscore
#' @param current id of the current cell
#' @param predecessoridlist list of the id of the possible predecessors
#' @param listofseq list of sequence strings
#' @param scoremat matrix of substitution score
#' @return a hash table of the predecessor - sscore

calculate.score <- function(current, predecessoridlist, listofseq, scoremat) {
  # process id of current and predecessors
  current <- as.numeric(unlist(str_split(current, pattern = "-")))
  predecessorlist <- c()
  for (i in predecessoridlist) {
    predecessor <- as.numeric(unlist(str_split(i, pattern = "-")))
    predecessorlist <- c(predecessorlist, list(predecessor))
  }

  scorelist <- c()

  # calculate the score of possible predecessors
  for (i in predecessorlist) {
    # locate the gaps and matches
    gapindex <- which((current - i) == 0)
    matchindex <- current[-gapindex]

    # calculate gap penalty
    gappen <- (-2) * length(gapindex)

    # calculate match score (S.score)
    # extract the letters from sequences
    lettervec <- c()
    for (i in sequence(length(current))) {
      if (i %in% gapindex) {
        next
      } else {
        sequence <- listofseq[i]
        letter <- substr(sequence, start = current[i], stop = current[i])
        lettervec <- c(lettervec, letter)
      }
    }

    sscore <- s.score(scoremat = BLOSUM62, resvec = lettervec, gappen = gappen)
    scorelist <- c(scorelist, sscore)
  }

  sscoretab <- hashtab()
  for (i in sequence(length(predecessoridlist))) {
    sethash(sscoretab, key = predecessoridlist[i], value = scorelist[i])
  }
  return(sscoretab)
}
