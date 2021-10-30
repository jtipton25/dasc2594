#' Generate a random basis for R^n
#'
#' @param n The dimension of the basis generated
#'
#' @return A n by n matrix where each column is a basis vector
#' @export
#'
#' @examples
#' library(dasc2594)
#' B <- make_basis(4)
#' # check that columns of B are linearly independent
#' all.equal(rref(B), diag(4))
make_basis <- function(n) {
    # generate a basis for R^n
    B <- matrix(sample(-9:9, n^2, replace = TRUE), n, n)
    while(!is_basis(B)) {
        B <- matrix(sample(-9:9, n^2, replace = TRUE), n, n)
    }
    return(B)
}
