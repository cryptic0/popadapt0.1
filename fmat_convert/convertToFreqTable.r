#' Converts a table with genotype counts to a new table with allele frequencies
#' @param popNamesListFilePath Path to a file with the name of each population on each line
#' @param individualsFilePath Path to a file with both the name of each individual and the population
#' they belong to (separated by whitespace) on each line
#' @param vcfFilePath Path to the VCF file
#' @param snpListFilePath Path to the list of SNP that you would like to generate frequencies for
#' @param frequencyOutputDirectory Optional. The output directory you would like to have the population
#' frequency files in
#' @param newPopFilesDirectory Optional. The output directory you would like to store the population files 
#' with individuals
#' @export

convertToFreqTable <- function(popNamesListFilePath, individualsFilePath, vcfFilePath, snpListFilePath=NULL, frequencyOutputDirectory, newPopFilesDirectory) {
	message("\n\n*** Dependencies: vcftools. Please make sure that you have vcftools installed in your terminal ***\n\n")
 	# Make sure that directories end in a slash
	if (substring(frequencyOutputDirectory,
		nchar(frequencyOutputDirectory)) != "/") {
		frequencyOutputDirectory <- paste0(
			frequencyOutputDirectory, "/")
	}
	if (substring(newPopFilesDirectory, 
                nchar(newPopFilesDirectory)) != "/") {
                newPopFilesDirectory <- paste0(
                        newPopFilesDirectory, "/")
        }

	# Make population files
	dir.create(newPopFilesDirectory)
	popNamesIn = file(popNamesListFilePath, "r")
	while ( TRUE ) {
		line = readLines(popNamesIn, n = 1)
		if ( length(line) == 0 ) {
      			break
    		}
  		newPopFilePath <- paste0(newPopFilesDirectory, line, ".pop")
    		file.create(newPopFilePath)
    		matches <- grep(paste0(".*", line, "$"), 
			readLines(individualsFilePath), value=TRUE)
    		individuals = c()
    		for (i in matches) {
      			individual <- strsplit(i, "\\s+")
      			individuals = c(individuals, individual[[1]][1])
    		}
    		newPopFileIn <- file(newPopFilePath)
   		writeLines(individuals, newPopFileIn)
    		close(newPopFileIn)
  	}
	close(popNamesIn)
  	# Make frequency files
  	dir.create(frequencyOutputDirectory)
  	popFilesVector <- list.files(path = newPopFilesDirectory, 
		pattern = "*.pop")
  	for (file in popFilesVector) {
    		vcfCommand <- paste0("vcftools --vcf ", vcfFilePath)
    		vcfCommand <- paste0(vcfCommand, " --keep ", 
			newPopFilesDirectory, file)
    		if (!is.null(snpListFilePath)) {
    		vcfCommand <- paste0(vcfCommand, " --positions ", 
			snpListFilePath)
    		}
    		vcfCommand <- paste0(vcfCommand, " --freq --out ", 
			frequencyOutputDirectory, file)
    		#print(paste0(frequencyOutputDirectory, file))
	    	system(vcfCommand)
  	}
	# newTable <- data.frame(pop=character(), loci=character(), A=integer(), T=integer())
	# for (i in 1:nrow(inputTable)) {
	# 	row <- inputTable[i,]
	# 	pop <- row[1]
	# 	loci <- row[2]
	# 	popsize <- row[3]
	# 	hom1 <- row[4]
	# 	het <- row[5]
	# 	hom2 <- row[6]
	# 	aFreq = sum(hom1*2, het)/(popsize * 2)
	#        	tFreq = sum(hom2*2, het)/(popsize * 2)
	# 	newTable[i,] <- c(pop, loci, aFreq, tFreq)
	# }
	# print(newTable)
}

