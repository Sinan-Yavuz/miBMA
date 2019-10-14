
#' mice.impute.miBMA Function
#'
#' This function allows you to run miBMA
#' @param y.
#' @param ry
#'@author this is a different version of mice.impute.norm which was written
#'by Stef van Buuren, Karin Groothuis-Oudshoorn, 2017
#' @export

mice.impute.miBMA <- function(y, ry, x, wy = NULL, ...) {
  if (is.null(wy)) wy <- !ry
  x <- cbind(1, as.matrix(x)) #Keep as it is, have this "1" for an intercept
  parm <- .norm.draw.miBMA(y, ry, x, ...)
  return(x[wy, ] %*% parm$beta + rnorm(sum(wy)) * parm$sigma)
}
