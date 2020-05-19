#' @title A matrix melting functions
#' @description This function melts a matrix into a format that can then be read in igraph or visNetwork for data visualization
#' @param x melt output
#' @keywords melt, matrix
#' @export
#' @examples
#' meltme(avian)
meltme <- function(x){
  colnames(x) <- c("names", 1:length(x)-1)
  x <- dplyr::select(x, -names)
  x <- as.matrix(x)
  x[lower.tri(x, diag=TRUE)] <- NA
  x <- reshape2::melt(x)
  x <- stats::setNames(x, c('ind1', 'ind2', 'values'))
  x <- dplyr::filter(x, !is.na(values))
  return(x)
}
