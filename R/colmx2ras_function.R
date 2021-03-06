#' Convert bycol-matrix to raster
#'
#' Convert a matrix with origin in upper left corner and with cell order from
#' top to bottom (bycol) to a raster with origin in lower left corner
#' @param mx Input matrix
#' @return Raster object
#' @keywords matrix raster conversion bycol transposition coordinate origin
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

colmx2ras <- function(mx){
  #require(raster)
  mx <- t(mx[, ncol(mx):1])
  ras <- raster(mx, xmx=ncol(mx), ymx=nrow(mx))
  return(ras)
}