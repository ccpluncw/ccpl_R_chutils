#' A function to create 67 different line type values for plotting
#'
#' This function creates 67 different line type values for plotting.
#' @keywords plot lty 67 different
#' @return a vector of lty values
#' @export
#' @examples ch.get70diffLtys()

ch.get67diffLtys <- function () {

  smallBreak <- 3
  largeBreak <- 6
  r1 <- rep(c(1,3,5,7,9), 14)
  r2 <- rep(rep(c(smallBreak,largeBreak), each=5), 7)
  r3 <- rep(1,60)
  r4 <- rep(rep(c(smallBreak,largeBreak), each=10), 3)
  r5 <- rep(1,40)
  r6 <- rep(c(smallBreak,largeBreak), each=20)

  out <- NULL
  for(i in 1:length(r1)) {
    out[i] <- paste(r1[i], r2[i], sep="")
    if(i>=11) {
      out[i] <- paste(out[i], r3[i-10], r4[i-10], sep="")
    }
    if(i>=31) {
      out[i] <- paste(out[i], r5[i-30], r6[i-30], sep="")
    }
  }
  out[1] <- "solid"
  out <- out[-31]
  out <- out[-26]
  out <- out[-64]
  return(out)
}
