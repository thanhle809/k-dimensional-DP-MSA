# a function that writes the script after accepting user's specified parameters

#the file containing the script is constructloops.R

# read in the sequences
# write the prerequisite for calculating the scores - list of sequence, scoring matrix
library(stringr)

filename <- readline(prompt = "Enter the fasta file name: ")
seqtable <- read.fasta(filename)
listofseq <- seqtable$sequence
noofseqlength <- return.seqlengths(seqtable)
numofseq <- length(listofseq)

seqchar <- paste0("'", listofseq, "'")

write(x = paste0("listofseq <- c(", paste0(seqchar, collapse = ", "), ")"), file = "script.R", append = T)
write(x = paste0("noofseqlength <- c(", paste0(noofseqlength, collapse = ", "), ")"), file = "script.R", append = T)
write(x = paste0("numofseq <- ", numofseq), file = "script.R", append = T)
write(x = paste0("dict <- build.dict(generate.id(noofseqlength))"), file = "script.R", append = T)
write(x = paste0("dict.pred <- build.dict(generate.id(noofseqlength))"), file = "script.R", append = T)
write(x = paste0("dict <- initiate.score(dict, noofseqlength)"), file = "script.R", append = T)


characterset <- c()
characterset <- paste0("a", sequence(numofseq))

# move() writes the command under each for loop statement
# generate the current id, write the command of what to execuse with the current id
move <- function(charindex, seqindex, maxseqindex) {
  # last sequence index (innermost loop to consider for current id)
  # concatenate string to the text string above
  if (seqindex == maxseqindex) {
    # generate the current id word <- paste0(a1, a2, a3)
    text <- paste0("currentid <- paste0(", paste0(characterset, collapse = ", "), ")")
    # retrieve predecessor
    text <- paste0(text,
                   "\n", strrep("\t", seqindex+1),
                   "predecessor <- goback(id = currentid, gapindices = gap.indices(noofseq = ", seqindex, "))")
    # calculate score
    text <- paste0(text,
                   "\n", strrep("\t", seqindex+1),
                   "sscoretab <- calculate.score(currentid, predecessor, listofseq, BLOSUM62)")
    # update the score in the table
    text <- paste0(text,
                   "\n", strrep("\t", seqindex+1),
                   "update.score(currentid, dict, dict.pred, sscoretab)")
  } else {
    # generate current id
    text <- paste0(charindex, " <- paste0(", charindex, ", '-')")
  }
  return(text)

}

# content() write the 'for' loop statement
content <- function(seqindex, maxseqindex) {
  sparse <- strrep("\t", times = seqindex)
  starter <- paste0(sparse, "for (", characterset[seqindex]," in sequence(", noofseqlength[seqindex], ")) {")
  starter <- paste0(starter, "\n", sparse, "\t", move(characterset[seqindex], seqindex, maxseqindex))
}

# write the commands in a script file
for (i in sequence(numofseq)) {
  write(
    x = content(i, numofseq),
    file = "script.R",
    append = TRUE
  )
}

write(
  x=strrep("}", times = numofseq),
  file = "script.R",
  append = TRUE)

write(x = "path <- returnPath(dict.pred)", file = "script.R", append = T)
write(x = "msa <- getMSA(path, listofseq)", file = "script.R", append = T)
