# gap.indices: this function takes in the number of sequence and return all possible positions of gaps
gap.indices <- function(noofseq) {
  gapindices <- c()

  for (g in seq(from = 1, to = noofseq - 1)) {

    indices.mat <- combn(sequence(noofseq), g)

    for (i in sequence(ncol(indices.mat))) {
      gapindices <- c(gapindices, list(indices.mat[,i]))
    }
  }
  return(gapindices)
}
