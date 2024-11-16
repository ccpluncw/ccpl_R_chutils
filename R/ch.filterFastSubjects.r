### A script to exclude participant whos RT is to fast

### Fast ms is the threshold for what RT is considered to fast
### trial_percent is the threshold for percentage of trails required to be slower than fast_ms
#' A function to exclude participant whos RT is to fast
#'
#' This function adds a legend to a plot.
#' @param data a dataframe with the trial by trial responses.
#' @param rtCol a string that specifies the name of the column in "data" that contains the RT data.
#' @param subCol a string that specifies the name of the column in "data" that contains the subject identifiers.
#' @param trialCol a string that specifies the name of the column in "data" that contains the trial number. It is used to plot the data.  DEFAULT = NULL (no plots)
#' @param fast_ms an integer identifying the ms threshold, equal to or below which are considered too fast. DEFAULT = 250
#' @param trial_percent a proportion (between 0-1) that identifies the proportion of trials below fast_ms that disqualify a participant. DEFAULT = 0.1
#' @param outputFileName a string specifying the output file name
#' @param append a boolean that specifies whether to append the ouput to the file if it exists. DEFAULT = TRUE.
#' @param plotFilename A string that identifies the name of file (.pdf) in which the plot will be saved. The default is NULL, whereby the plot will not be saved.
#' @param removeFastResponses a boolean that specifies whether to remove individual fast RT trials from final dataset. DEFAULT = FALSE.
#' @keywords sn fast RT subject
#' @return a list that contains datRemoved = the data that was removed; datKept = the data that was kept; numRemoved = the number of subjects removed; pRemoved = the percent of subjects removed
#' @export
#' @examples ch.filterFastSubjects(df.data, "rt", "sn")


ch.filterFastSubjects <- function(data,
															rtCol,
															subCol,
															trialCol = NULL,
															fast_ms = 250,
                              trial_percent = .10,
	                            outputFileName = NULL,
                              append = TRUE, plotFilename = NULL, removeFastResponses = FALSE){

				data.raw <- data
    ### Sample n before exclusion
        sample.n.pre <- length(unique(data[[subCol]]))

    ### Data process to create list of excluded participants
				data.raw$keybRT_less250 <- ifelse(data[[rtCol]] <= fast_ms, 1,0)

				df.tmp <- data.raw %>% dplyr::group_by(eval(parse(text=subCol))) %>% dplyr::summarize(mean_RT = mean(eval(parse(text=rtCol)), na.rm = T),
                             sd_RT = sd(eval(parse(text=rtCol)), na.rm = T),
                             percent_fastRT = sum(keybRT_less250)/dplyr::n())
				colnames(df.tmp)[1] <- subCol

        df.tmp.rm <- df.tmp %>%
            dplyr::filter(percent_fastRT >= trial_percent)
				df.tmp.kp <- df.tmp %>%
            dplyr::filter(percent_fastRT < trial_percent)

        exclusion.list <-  as.character(df.tmp.rm[[subCol]])
        num.excluded <- length(exclusion.list)

				pRemoved = num.excluded/sample.n.pre

   	 		data.keep <- data[!(data[[subCol]] %in% exclusion.list), ]
   	 		data.exclude <- data[(data[[subCol]] %in% exclusion.list), ]

        sample.n.after <- length(unique(data.keep[[subCol]]))

				############## Make Plots ##############
				if(!is.null(plotFilename) & !is.null(trialCol)) {
					pdf(plotFilename)
					sns <- unique(data.exclude[[subCol]])
					plot.new()
						text(x=.4, y=.9, "The Participants", font=2, cex=3)
						text(x=.4, y=.6, "Removed From", font=2, cex=3)
						text(x=.4, y=.3, "The Final Dataset", font=2, cex=3)
					for(sn in sns) {
						df.tmp <- data.exclude[data.exclude[[subCol]] == sn,]
						plot(df.tmp[[rtCol]] ~ df.tmp[[trialCol]], ylim = c(0,10000), main = sn)
						abline(h = fast_ms)
					}

					plot.new()
					text(x=.4, y=.9, "The Participants", font=2, cex=3)
					text(x=.4, y=.6, "Remaining In", font=2, cex=3)
					text(x=.4, y=.3, "The Final Dataset", font=2, cex=3)
					sns <- unique(data.keep[[subCol]])
					for(sn in sns) {
						df.tmp <- data.keep[data.keep[[subCol]] == sn,]
						plot(df.tmp[[rtCol]] ~ df.tmp[[trialCol]], ylim = c(0,10000), main = sn)
						abline(h = fast_ms)
					}
					dev.off()
				}
				############## End Make Plots ##############


				if(removeFastResponses) {
					numBeforeRemove <- nrow(data.keep)
					data.keep <- data.keep[data.keep[[rtCol]] > fast_ms, ]
					pFastResponsesRemoved <- (numBeforeRemove - nrow(data.keep))/numBeforeRemove
				}

   # Saving the output
    if(!is.null(outputFileName)) {
      sink(outputFileName,
           type = c("output", "message"),
           append = append)
        cat("\n")
        cat("#############################################\n")
        cat("Fast RT Exclusion\n")
        cat("#############################################\n")
        cat("\n")
        cat("The threshold for fast RT in ms:\n")
        cat(paste0(fast_ms))
        cat("\n")
        cat("\n")
        cat("The threshold of percent of trials under fast RT threshold to Remove Participant :\n")
        cat(paste0("%", trial_percent*100))
        cat("\n")
        cat("\n")
        cat("Sample size before exclusion:\n")
        cat(paste0(sample.n.pre))
        cat("\n")
        cat("\n")
        cat("Sample excluded for fast RT:\n")
        cat(paste0(num.excluded))
        cat("\n")
        cat("\n")
				print(data.frame(df.tmp.rm))
        cat("\n")
        cat("\n")
        cat("Sample size after fast RT exclusions:\n")
        cat(paste0(sample.n.after))
        cat("\n")
        cat("\n")
        print(data.frame(df.tmp.kp))
        cat("\n")
        cat("\n")

				cat("Were individual fast RT trials removed from remaining data:", removeFastResponses, "\n")
				if(removeFastResponses) {
					cat("Percent of individual trials removed because of fast RT:", pFastResponsesRemoved, "\n\n\n")
				}
        cat("#############################################\n")
        cat("End of Incomplete Trial Exclusion\n")
        cat("#############################################\n")

      sink()
    }


		outList <- list(datKept = data.keep, datRemoved = data.exclude, numRemoved = num.excluded, pRemoved = pRemoved)

    ### Return
    return(outList)

}
