# a function that receives a fasta-derived table of 2 columns - name and sequence - and
# return a vector of sequence lengths
return.seqlengths <- function(seqtable) {
  sequence <- seqtable$sequence
  lengthvec <- c()
  for (i in sequence) {
    lengthvec <- c(lengthvec, str_length(i))
  }
  return(lengthvec)
}
