#' Swap two rows
#'
#' @param A The matrix of which to swap rows
#' @param row1 The first row to swap
#' @param row2 The second row to swap
#'
#' @return The matrix A of which to swap rows
#'
#' A[c(row1, row2), ] <- A[c(row2, row1), ]
#'
#' @export
#'
#' @examples
#'
#' A <- diag(3)
#' swap_rows(A, 2, 3)
#'

row_swap <- function(A, row1, row2) {
    if (!is_valid_row(A, row1))
        stop("The rows to be swapped must be valid rows for the matrix A")
    if (!is_valid_row(A, row2))
        stop("The rows to be swapped must be valid rows for the matrix A")
    if (row1 == row2)
        stop("The rows to be swapped must be different")

    # swap the rows
    A[c(row1, row2), ] <- A[c(row2, row1), ]
    return(A)
}


#' Multiply one row by a scalar
#'
#' @param A The matrix of which to multiply one row
#' @param row The row to be modified
#' @param a The number to multiply by \code{row}
#'
#' @return The matrix A where row1 is added to a times row 2
#'
#' A[row, ] <- a * A[row, ]
#'
#' @export
#'
#' @examples
#'
#' A <- diag(3)
#' multiply_row(A, 2, -2)
#'

row_multiply <- function(A, row, a) {
    if (!is_valid_row(A, row))
        stop("The row to be multiplied must be valid for the matrix A")
    if (length(a) != 1)
        stop("a must be a non-zero number")
    if (is.null(a))
        stop("a must be a non-zero number")
    if(!(is.numeric(a) & a != 0))
        stop("a must be a non-zero number")
    A[row, ] <- a * A[row, ]
    return(A)
}



#' Add one row to a multiple of another row
#'
#' @param A The matrix of which to add multiple of one row to another
#' @param row1 The row to be modified
#' @param row2 The row to be multiplied and added to row1
#' @param a The number to multiply by row 2
#'
#' @return The matrix A where row1 is added to a times row 2
#'
#' A[row1, ] <- A[row1, ] + a * A[row2, ]
#'
#' @export
#'
#' @examples
#'
#' A <- diag(3)
#' add_rows(A, 2, 3, -2)
#'

row_add <- function(A, row1, row2, a) {

    if (!is_valid_row(A, row1))
        stop("The rows to be added must be valid rows for the matrix A")
    if (!is_valid_row(A, row2))
        stop("The rows to be added must be valid rows for the matrix A")
    if (row1 == row2)
        stop("The rows to be added must be different")
    if (length(a) != 1)
        stop("a must be a non-zero number")
    if (is.null(a))
        stop("a must be a non-zero number")
    if(!(is.numeric(a) & a != 0))
        stop("a must be a non-zero number")

    # perform the row addition
    A[row1, ] <- A[row1, ] + a * A[row2, ]
    return(A)
}







#' Check if the row is valid for the matrix A
#'
#' @param A The matrix A
#' @param row The row to check if it is valid
#'
#' @return \code{is_valid_row()} returns TRUE if \code{row} is a valid row for \code{A} and \code{FALSE} otherwise
#' @export
#'
#' @examples
#' A <- diag(4)
#' # expect TRUE
#' is_valid_row(A, 3)
#' # expect FALSE
#' is_valid_row(A, 5)
#'

is_valid_row <- function(A, row) {
    if (!is.matrix(A))
        stop("A must be a matrix")
    if (length(row) != 1)
        return(FALSE)
    if (!(row %in% 1:nrow(A)) | !is.numeric(row))
        return(FALSE)

    return(TRUE)

}
