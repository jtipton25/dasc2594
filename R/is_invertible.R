#' Test if a matrix A is invertible
#'
#' @param A a matrix
#'
#' @return TRUE if A is invertible and FALSE if A is not invertible
#' @export
#'
#' @examples
#'
#' library(dasc2594)
#' # An invertible matrix
#' A <- diag(4)
#' is_invertible(A) # returns TRUE
#'
#' # A non-invertible matrix
#' A <- matrix(1, 3, 3)
#' is_invertible(A) # returns FALSE

is_invertible <- function(A) {
    if (!is.matrix(A))
        stop("A must be a square numeric matrix")
    if (!is.numeric(A))
        stop("A must be a square numeric matrix")
    if (nrow(A) != ncol(A))
        stop("A must be a square numeric matrix")

    # if there are complex eigenvalues
    if (any(is.complex(eigen(A)$values))) {
        return(!(any(abs(Re(eigen(A)$values)) < .Machine$double.eps^0.5 &
                         abs(Im(eigen(A)$values)) < .Machine$double.eps^0.5)))
    }
    return(!any(eigen(A)$values == 0))
}
