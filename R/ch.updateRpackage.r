#' A function to update an R package
#'
#' This function updates an R package easily.
#' @param packageRoot the directory in which the R package directory resides. DEFAULT = my computers packageRoot.
#' @param packageName the R package directory name.
#' @keywords morals dbfile read
#' @return a list with all the parameterd
#' @export
#' @examples ch.readMoralsDBfile ("myDBfile.txt")

ch.updateRpackage <- function(packageRoot = "~/Dropbox/UNCW/Active UNCW/research/Programming/R Packages", packageName = NULL) {
  if (is.null(packageName)){
    stop("packageName must be specified")
  }

  library(devtools)
  library(roxygen2)

  wd <- getwd()
  packageDir <- paste(packageRoot, packageName, sep="/")
  setwd(packageDir)
  document()
  setwd(packageRoot)
  install(packageName)
  setwd(wd)

}
