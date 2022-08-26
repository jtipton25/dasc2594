#' Generate a random matrix
#'
#' @param n The number of rows
#' @param p The number of columns
#'
#' @return A n by p matrix with random values sampled with replacement from `values`
#' @export
#'
#' @examples
#' library(dasc2594)
#'
#' # simulate a random vector
#' u <- random_matrix(3, 1)
#'
#' # simulate a random matrix
#' A <- random_matrix(3, 3)
#'
#' # simulate a random matrix with only positive values
#' B <- random_matrix(3, 3, values = 1:9)
#'
random_matrix <- function(n, p, values = -9:9) {
    return(matrix(sample(values, n * p, replace = TRUE), n, p))
}
