#' Calculate spatial indices for points in 2D space
#'
#' Function that returns spatial indices (plot numbers) for given coordinate pairs.
#' E.g. for hectares res = 100 m
#' @param xcor X-coordinate of the point
#' @param ycor Y-coordinate of the point
#' @param res side length of one spatial subunit
#' @return spatial index for the input point
#' @keywords spatial index plot number
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

calc.spatial.index <- function(xcor, ycor, res=1){
  # Convert absolute to relative coordinates
  xcor <- xcor - floor(min(xcor, na.rm=T))
  ycor <- ycor - floor(min(ycor, na.rm=T))
  # Count number of cells in X-direction
  # %/%: integer division operator
  nx <- max(xcor, na.rm=T) %/% res + 1
  # For each coordinate calculate the cell number in X- and Y-direction
  myx <- xcor %/% res + 1
  myy <- ycor %/% res + 1
  # Calculate the total index
  index <- myx + nx * (myy - 1)
  return(index)
}


