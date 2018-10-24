#' A function to calculate D Prime statistics
#'
#' This function calculated D Prime statistics.
#' @param data a dataframe with the trial by trial responses.
#' @param grpCols a vector containing strings that specifies the names of the columns in "data" that you want the data grouped by.
#' @param correctCol a string that specifies the name of the column in "data" that contains the whether or not the participant responded correctly.
#' @param correctVals a vector of two values that specifies the "correct" value (index 1) and the "incorrect" value (index 2). e.g, c("yes", "no")
#' @param targetPresentCol a string that specifies the name of the column in "data" that contains the whether or not the target was presented.
#' @param targetPresentVals a vector of two values that specifies the "target present" value (index 1) and the "target absent" value (index 2). e.g, c("yes", "no")
#' @param addCorrection a boolean that specifies whether you want a .5 correction to be added the total hits, FAs, misses, and CRs. This corrects for 0 and 1 values for FA and Hits. DEFAULT = T.
#' @keywords dprime analysis
#' @return dataframe with the dprime statistics:
#' @export
#' @examples ch.moralsSnRTpHit (data=moralsData,"sn", "trial", "RT", "res.RT", "fit.RT", "overlap", "keyDef", c("Yes", "No"), "correct", params=parameters)


ch.calculateDprimeStats <- function (data, grpCols, correctCol, correctVals = c(TRUE, FALSE), targetPresentCol, targetPresentVals = c(TRUE, FALSE), addCorrection = TRUE) {

    if (addCorrection) {
      correction <- 0.5
    } else {
      correction <- 0.0
    }

    df.Dprime <- as.data.frame(data %>% dplyr::group_by_(grpCols) %>% dplyr::summarise(
      N.Hit = sum(eval(parse(text=correctCol))==correctVals[1] & eval(parse(text=targetPresentCol))==targetPresentVals[1])+correction,
      N.Miss = sum(eval(parse(text=correctCol))==correctVals[2] & eval(parse(text=targetPresentCol))==targetPresentVals[1])+correction,
      N.CR = sum(eval(parse(text=correctCol))==correctVals[1] & eval(parse(text=targetPresentCol))==targetPresentVals[2])+correction,
      N.FA = sum(eval(parse(text=correctCol))==correctVals[2] & eval(parse(text=targetPresentCol))==targetPresentVals[2])+correction,
      N.targetPresent = sum(eval(parse(text=targetPresentCol))==targetPresentVals[1])+(2*(correction)),
      N.targetAbsent = sum(eval(parse(text=targetPresentCol))==targetPresentVals[2])+(2*(correction)),
    ))

    df.Dprime$pHit <- df.Dprime$N.Hit/df.Dprime$N.targetPresent
    df.Dprime$pMiss <- df.Dprime$N.Miss/df.Dprime$N.targetPresent
    df.Dprime$pFA <- df.Dprime$N.FA/df.Dprime$N.targetAbsent
    df.Dprime$pCR <- df.Dprime$N.CR/df.Dprime$N.targetAbsent
    df.Dprime$dPrime <- qnorm(df.Dprime$pHit)-qnorm(df.Dprime$pFA)
    df.Dprime$beta <- -.5*(qnorm(df.Dprime$pHit)+qnorm(df.Dprime$pFA))
    df.Dprime$RelBeta <- df.Dprime$beta/df.Dprime$dPrime
    df.Dprime$varDp <- mapply(ch.getDprimeVariance,df.Dprime$N.targetPresent, df.Dprime$N.targetAbsent, df.Dprime$pFA, df.Dprime$pHit)

    zOut <- mapply(ch.ZtestDprimeEqualZero,df.Dprime$N.targetPresent, df.Dprime$N.targetAbsent, df.Dprime$pFA, df.Dprime$pHit)
    zOut <- t(zOut)
    df.Dprime$zVal <- zOut[,'zVal']
    df.Dprime$pVal <- zOut[,'pVal']

    return(df.Dprime)
}
