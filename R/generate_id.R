#' @title Generate cells id
#' @description receive a vector of sequence lengths and return the all possible combinations of score id
#' @param listofseqlength a vector of sequence lengths
#' @export
generate.id <- function(listofseqlength) {
  veclist <- c()

  for (i in listofseqlength) {
    veclist <- c(veclist, list(sequence(i)))
  }

  idlist <- c()
  newwordlist <- c()

  for (i in sequence(length(listofseqlength))) {
    if (i == 1) {
      idlist <- c(idlist, veclist[[i]])
    } else {
      for (j in veclist[[i]]) {
        newword <- str_c(idlist, "-", j)
        newwordlist <- c(newwordlist, newword)
      }
      idlist <- c()
      idlist <- newwordlist
      newwordlist <- c()
    }
  }
  return(sort(idlist))
}
