#' Check if the matrix equation Ax = b has a unique solution
#'
#' @param A The matrix A
#' @param b The vector b
#'
#' @return This function returns TRUE if there is a unique solution to the equation Ax = b and FALSE if there is not a unique solution to the equation Ax = b. If there is no solution to the system of equations, the function will return an error.
#'
#' @return
#' @export
#'
#' @examples
#'
#' library(dasc2594)
#' A <- diag(2)
#' b <- 1:2
#' # expect TRUE
#' is_consistent(A, b)
#' # expect TRUE
#' is_unique(A, b)
#'
#' A <- matrix(c(1, 0, 0, 1, 1, 1), 2, 3)
#' b <- 1:2
#' # expect FALSE
#' is_unique(A, b)
is_unique <- function(A, b) {
    if (!is.matrix(A))
        stop("A must be a numeric matrix.")
    if (!is.numeric(A))
        stop("A must be a numeric matrix.")
    if (!is.vector(b) & !is.matrix(b))
        stop("b must be a numeric vector.")
    if (is.matrix(b))
        if (ncol(b) != 1)
            stop("b must be a numeric vector.")
    if (!is.numeric(b))
        stop("b must be a numeric vector.")
    if (nrow(A) != length(b))
        stop("A and b must have the same number of rows.")
    if (!is_consistent(A, b))
        stop("The system of equations Ax = b must be consistent.")

    # determine if the solution to the system of equations Ax = b is unique

    isTRUE(all.equal(rref(cbind(A)), diag(max(nrow(A), ncol(A)))[1:nrow(A), 1:ncol(A)]))
}
