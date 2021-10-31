#' Make a matrix with integer eigen factorizations
#'
#' @param n The dimension of the matrix
#'
#' @return A list with three components. A is the matrix that has eigen
#' decomposition A = V D V^{-1} where V is a matrix with eigenvectors
#'  as columns and D = diag(d) is a matrix with corresponding eigenvalues d
#'  on the diagonal.
#' @export
#'
#' @examples
#'
#' e <- make_eigen(3)
#' e$A
#' e$V
#' e$d

make_eigen <- function(n) {
    # eigenvalues are not unique
    # eigenvalues with the same multiplicity are not unique but
    # must span the same eigenspace

    if (!is_positive_integer(n, 1))
        stop("n must be a positive integer")

    V <- matrix(sample(-4:4, n^2, replace = TRUE), n, n)
    d <- sample(-9:9, n, replace = TRUE)
    idx <- order(d)
    V <- V[, idx]
    d <- d[idx]

    A <- V %*% diag(d) %*% solve(V)

    return(list(A = A, V = V, d = d))
}


