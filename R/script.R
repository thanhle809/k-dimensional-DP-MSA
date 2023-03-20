listofseq <- c('MAAPRAVLLL', 'MAVPSAVLLL', 'TVICFSGKRK', 'TAIPSAVLLL')
noofseqlength <- c(10, 10, 10, 10)
numofseq <- 4
dict <- build.dict(generate.id(noofseqlength))
dict.pred <- build.dict(generate.id(noofseqlength))
dict <- initiate.score(dict, noofseqlength)
	for (a1 in sequence(10)) {
		a1 <- paste0(a1, '-')
		for (a2 in sequence(10)) {
			a2 <- paste0(a2, '-')
			for (a3 in sequence(10)) {
				a3 <- paste0(a3, '-')
				for (a4 in sequence(10)) {
					currentid <- paste0(a1, a2, a3, a4)
					predecessor <- goback(id = currentid, gapindices = gap.indices(noofseq = 4))
					sscoretab <- calculate.score(currentid, predecessor, listofseq, BLOSUM62)
					update.score(currentid, dict, dict.pred, sscoretab)
}}}}
path <- returnPath(dict.pred)
msa <- getMSA(path, listofseq)
