#' Check if the matrix equation Ax = b is consistent
#'
#' @param A The matrix A
#' @param b The vector b
#'
#' @return This function returns TRUE if there is a solution to the equation Ax = b and FALSE if there is not a solution to the equation Ax = b
#' @export
#'
#' @examples
#'
#' library(dasc2594)
#' A <- diag(2)
#' b <- 1:2
#' # expect TRUE
#' is_consistent(A, b)
#'
#' A <- matrix(c(1, 0, 1, 0), 2, 2)
#' b <- 1:2
#' # expect FALSE
#' is_consistent(A, b)

is_consistent <- function(A, b) {
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

    return(qr(A)$rank == qr(cbind(A, b))$rank)
}

