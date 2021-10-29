#' Checks if the columns of a square matrix define a basis
#'
#' @param B A \eqn{n \times n}{n by n} matrix
#'
#' @return A logical of whether the matrix is a basis
#' @export
#'
is_basis <- function(B) {
    if (!is.matrix(B))
        stop("B must be a square numeric matrix.")
    if (!is.numeric(B))
        stop("B must be a square numeric matrix.")
    if (nrow(B) != ncol(B))
        stop("B must be a square numeric matrix.")


    n <- nrow(B)
    return(isTRUE(all.equal(rref(B), diag(n))))
}
