#' rollcor
#'
#' performs running correlation analysis
#' @param x a vector
#' @param y a vector
#' @param ... argument to pass into cor function
#' @param width The width of the sliding window, which must be odd number.
#' @param show If TRUE, the result will be plotted.

rollcor <- function (x, y, width, show = TRUE, ...) {
  x <- as.vector(x)
  y <- as.vector(y)
  stopifnot(length(x) == length(y))
  stopifnot((width %% 2) != 0)

  len <- length(x)
  halfWidth <- (width - 1) / 2
  lenCC <- len - 2 * halfWidth
  cc <- rep(0, lenCC)
  names(cc) <- (halfWidth + 1) : (len - halfWidth)
  for (i in 1 : lenCC) {
    start <- i
    end <- start + width - 1
    # cc[i] <- cor(coredata(x)[start : end], coredata(y)[start : end], ...)
    cc[i] <- cor(x[start : end], y[start : end], ...)
  }

  return(cc)
}
