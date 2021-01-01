#' Compute epsilon
#'
#' @param x An numerix scalar or vector
#'
#' @return An error tolerance
#'
eps <- function(x = 1.0) {
    # from https://raw.githubusercontent.com/cran/pracma/master/R/eps.R
    stopifnot(is.numeric(x))

    x <- max(abs(x))

    if (x <  .Machine$double.xmin) {
        e <- .Machine$double.xmin
    } else {
        e <- 2^floor(log2(x)) * .Machine$double.eps
    }
    e
}
