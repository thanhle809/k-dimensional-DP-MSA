# a function that receives a path and a list of sequences and return MSA
getMSA <- function(path, seqlist) {
  path <- sort(path, decreasing = F)

  # process path idlist to numeric list
  pathlist <- c()
  for (i in path) {
    pathid <- as.numeric(unlist(str_split(i, pattern = "-")))
    pathlist <- c(pathlist, list(pathid))
  }

  # MSA has columns to be each residue and each row to be a sequence
  letter <- c()
  msa <- matrix(nrow = length(seqlist), ncol = 1)

  # for each of the current cell
  for (i in sequence(length(pathlist))) {
    if (i <= length(pathlist) - 1) {
      gaps <- which(pathlist[[i]] - pathlist[[i + 1]] == 0)
    } else {
      gaps <- c("NA")
    }

    column <- c()

    # for each of the sequence id in the current cell id
    for (j in sequence(length(pathlist[[i]]))) {
      if (j %in% gaps) {
        letter <- "-"
      } else {
        letter <- str_sub(string = seqlist[j], start = pathlist[[i]][j], end = pathlist[[i]][j])
      }
      column <- c(column, letter)
    }
    msa <- cbind(msa, column)
  }
  return(msa)
}
