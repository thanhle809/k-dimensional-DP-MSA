#' @title Read fasta file into table of seqname and sequences
#' @param filename name of the fasta file

read.fasta <- function(filename) {
  lines <- readLines(con = filename)
  namebag <- c()
  seqbag <- c()
  for (i in sequence(length(lines))) {
    if (i %% 2 == 1) {
      namebag <- c(namebag, lines[i])
    } else {
      seqbag <- c(seqbag, lines[i])
    }
  }
  table <- data.frame(seqname = namebag, sequence = seqbag)
  return(table)
}
