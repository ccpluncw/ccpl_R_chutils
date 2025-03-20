#' This function calculates summary statistics for demographic data.
#'
#' Function that calculates summary statistics for demographic data.
#'
#' @param data A dataframe with the demographic information.  It needs - at minimum - a subject column and one other of the specified columns.
#' @param subjectColumn A string specifying the column of data that holds the subject identifier.
#' @param ageColumn A string specifying the column of data that holds the age information.  DEFAULT = NULL.
#' @param sexColumn A string specifying the column of data that holds the sex information.  DEFAULT = NULL.
#' @param genderColumn A string specifying the column of data that holds the gender information.  DEFAULT = NULL.
#' @param ethnicityColumn A string specifying the column of data that holds the ethnicity information.  DEFAULT = NULL.
#' @param outputFileName A sting specifying the name of the output file that will contain the summary statistics.  Default is "demographics.txt".
#' @param append A boolean specifying whether to append to end of an existing file or to overwrite any existing file with the same name.  DEFAULT = FALSE (overwrite).
#' @param roundValue An integer that specifies how many digits the output should be rounded to.  DEFAULT = 2.
#''
#' @return nothing - a text file is saved.
#' @keywords demographics
#' @export
#' @examples ch.getDemographics (data, "sn", "age", genderColumn = "gender",outputFileName = "output.txt", append = T)

ch.getDemographics <- function (data, subjectColumn, ageColumn = NULL, sexColumn = NULL, genderColumn = NULL, ethnicityColumn = NULL,outputFileName = "demographics.txt", append = F, roundValue = 2) {

	genPresent <- FALSE
	sexPresent <- FALSE

	cols <- c(subjectColumn, ageColumn, sexColumn, genderColumn, ethnicityColumn)
	data.1 <- unique(data[, cols])
	data.raw <- data.frame(sn = data.1[[subjectColumn]])
	if(!is.null(ageColumn))	data.raw$Birth <- data.1[[ageColumn]]
	if(!is.null(sexColumn))	data.raw$Sex <- data.1[[sexColumn]]
	if(!is.null(genderColumn))	data.raw$Gender <- data.1[[genderColumn]]
	if(!is.null(ethnicityColumn))	data.raw$Ethnicity <- data.1[[ethnicityColumn]]

	sink(outputFileName, append = append)

	if("Birth" %in% colnames(data.raw))
	{
		rawN <- nrow(data.raw)

		age.mean.r <- round(mean(data.raw$Birth, na.rm = T), roundValue)
		age.sd.r <- round(sd(data.raw$Birth, na.rm = T), roundValue)
		cat("\n ******************** Age ******************** \n")
		cat("\t\t N =", rawN,"\n\n")
		cat("\t\t Mean =", age.mean.r,"\n")
		cat("\t\t SD =", age.sd.r,"\n")
		agePresent <- TRUE
	} else {
		cat("\n No Age Data \n")
	}


	if("Gender" %in% colnames(data.raw))
	{

		data.raw$Gender <- ifelse(is.na(data.raw$Gender), "not collected", data.raw$Gender)

		rawN <- with(data.raw, sum(unlist(table(Gender))))

		gender.tbl.r <- with(data.raw, round(prop.table(table(Gender)), roundValue))
		cat("\n\n ******************** Gender ******************** \n")
		cat("\t\t N =", rawN,"\n\n")
		writeLines(paste0("\t\t", capture.output(print(gender.tbl.r, row.names = F))))
		genPresent <- TRUE
	} else {
		cat("\n No Gender Data \n")
	}

	if("Sex" %in% colnames(data.raw))
	{

		data.raw$Sex <- ifelse(is.na(data.raw$Sex), "not collected", data.raw$Sex)

		rawN <- with(data.raw, sum(unlist(table(Sex))))

		sex.tbl.r <- with(data.raw, round(prop.table(table(Sex)), roundValue))
		cat("\n\n ******************** Sex ******************** \n")
		cat("\t\t N =", rawN,"\n\n")
		writeLines(paste0("\t\t", capture.output(print(sex.tbl.r, row.names = F))))
		sexPresent <- TRUE
	} else {
		cat("\n No Sex Data \n")
	}

	if(sexPresent & genPresent)
	{

		data.raw$Sex <- ifelse(is.na(data.raw$Sex), "not collected", data.raw$Sex)
		data.raw$Gender <- ifelse(is.na(data.raw$Gender), "not collected", data.raw$Gender)

		rawN <- with(data.raw, sum(unlist(table(Sex,Gender))))

		sexGen.tbl.r <- with(data.raw, round(prop.table(table(Sex, Gender, dnn=c("Sex","Gender"))), roundValue))

		cat("\n\n ******************** Sex and Gender ******************** \n")
		cat("\t\t N =", rawN,"\n\n")
		writeLines(paste0("\t\t", capture.output(print(sexGen.tbl.r, row.names = F))))
	}

	if("Ethnicity" %in% colnames(data.raw))
	{

		data.raw$Ethnicity <- ifelse(is.na(data.raw$Ethnicity), "not collected", data.raw$Ethnicity)

		rawN <- with(data.raw, sum(unlist(table(Ethnicity))))

		ethnicity.tbl.r <- as.data.frame(with(data.raw, round(prop.table(table(Ethnicity)), roundValue)))
		cat("\n\n ******************** Ethnicity ******************** \n")
		cat("\t\t N =", rawN,"\n\n")
		writeLines(paste0("\t", capture.output(print(ethnicity.tbl.r, row.names = F))))
		ethPresent <- TRUE
	} else {
		cat("\n No Ethnicity Data \n")
	}

	sink(NULL)

}
