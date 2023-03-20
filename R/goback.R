# goback: this functions takes in the id of the cell we're trying to score (current cell)
# and gap indices and return the cell to trace back to

goback <- function(id, gapindices) {
  current <- unlist(str_split(id, pattern = "-"))

  nogap <- as.numeric(current) - 1

  predecessorlist <- c()

  for (i in gapindices) {
    predecessor <- nogap
    for (j in i) {
      predecessor[j] <- nogap[j] + 1
    }

    # the predecessor with id contains 0 is nonexistent so has to be removed
    if (0 %in% predecessor) {
      next
    } else {
      predecessorlist <- c(predecessorlist, list(predecessor))
    }
  }

  if (0 %in% nogap == FALSE) {
    predecessorlist <- c(predecessorlist, list(nogap))
  }

  predecessor.idlist <- c()
  for (i in predecessorlist) {
    predecessor.idlist <- c(predecessor.idlist, paste(i, collapse = "-"))
  }
  return(predecessor.idlist)
}
