#' This function calculates probMax for chMaxAveComb based on the percent influence of the maximum valued item
#'
#' This function calculates probMax for chMaxAveComb based on the percent influence of the maximum valued item
#'
#' @param list.data A list with each element filled with numbers. Each vactor in the list is a set of values for an item
#' @param percentMaxInfluenceOverall A probability that specifies the percent of influence that the column with the max(median()) has on the output vector over and above the average. When percentMaxInfluenceOverall = 0, then the max(median()) has no influence over and above the average. It's only influence is that it is part of the average.  When percentMaxInfluenceOverall > 0, then max(median()) has some influence on the average, but also some more influence over and above the average. When percentMaxInfluenceOverall = 1, then max(median()) has all the influence. pDEFAULT = 0.6 (this is the percent found with the biased average grouping model;
#' @param percentMaxInfluenceOnAve A probability that specifies the percent of influence that the column with the max(median()) has on the the average. When percentMaxInfluenceOnAve = 0, then the max(median()) has no influence the average.  When percentMaxInfluence > 0, then max(median()) has some influence on the average. When percentMaxInfluence = 1, then max(median()) has all the influence. pDEFAULT = NULL (this will force the column with the max(median()) to have an equal influence on the ave as all other columns);
#' @keywords combine function vectors
#' @return a probability to be input into ch.maxAveComb for the probMax option
#' @export
#' @examples ch.maxAveComb (list.data, percentMaxInfluence = .6)

ch.getProbMax <- function(list.data, percentMaxInfluenceOverall = 0.6, percentMaxInfluenceOnAve = NULL) {

if(percentMaxInfluenceOverall < 0 | percentMaxInfluenceOverall > 1) {
  stop ("percentMaxInfluence must be between 0 and 1")
}
numVectors <- length(list.data)
if(is.null(percentMaxInfluenceOnAve)) {
  percentMaxInfluenceOnAve <- 1/numVectors
}

if(numVectors > 1) {
  #find Column with the maximum median
  maxCol <-as.numeric(which.max(lapply(list.data,median)))
  #get the minimum number of elements in each column. this will serve as the standardized column length
  aveN <- min(lengths(list.data))
  #get the total number of elements in the list including the max column
  if(percentMaxInfluenceOnAve <=0) {
    sumN <- aveN * (numVectors - 1)
    maxN <- 0
  } else {
    sumN <- aveN/percentMaxInfluenceOnAve
    maxN <- aveN
  }
  #calculate the percent of the average items
  probAve <- 1-percentMaxInfluenceOverall
  #calculate the totalN
  if(probAve <=0) {
    probMax <- 1
  } else {
    totalN <- sumN/probAve
    #calculate the extra max items
    maxNextra <- totalN - sumN
    #calculate probMax
    probMax <- (maxNextra + maxN)/totalN
  }
} else {
  probMax <- 1
}
  return(probMax)
}
