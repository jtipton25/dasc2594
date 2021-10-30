#' Calculate a random elementary matrix
#'
#' @param n The size of the elementary matrix (an n times n matrix)
#' @param verbose A logical value that determines whether to print the output
#' @param operation If specified, operation is a character string specifying what kind of elementary row operation to perform. The options are "row swap", "scalar multiplication", and "row addition".
#' @param prob If specified, the probability of applying a row swap, row_multiplication, or row addition operation
#' @return An elementary matrix
#' @export
#'
elementary_matrix <- function(n, operation = NULL, verbose = FALSE, prob = c(1/6, 1/6, 2/3)) {
    # add in option to get a specific row operation

    # check if a valid operation
    if (is.null(operation)) {
        operation <- sample(c("row swap", "scalar multiplication", "row addition"), 1, prob = prob)
    }
    if (!(operation %in% c("row swap", "scalar multiplication", "row addition")))
        stop('operation must be either "row swap", "scalar multiplication", or "row addition"')
    # initialize the elementary matrix
    E <- diag(n)

    if (operation == "scalar multiplication"){
        if (verbose)
            message("scalar multiplication")
        #scalar row mult
        row_idx <- sample(1:n, 1, replace = FALSE)
        E[row_idx, ] <- sample((-4:4)[-5], 1) * E[row_idx, ]
    } else if (operation == "row swap") {
        if (verbose)
            message("row swap")
        #row swaps
        #choose two rows, getting two distinct rows
        row_index <- sample(1:n, 2, replace = FALSE)
        E[row_index, ] <- E[rev(row_index), ]
    } else {
        if (verbose)
            message("row multiplication and addition")
        # row mult and addition, mult one row by something, mult by other row, and add to other
        # sample(-9:9, 1)
        # row_index[1]
        # row_index[2]
        row_index <- sample(1:n, 2, replace = FALSE)
        E[row_index[1],] <- E[row_index[1],] + sample((-9:9)[-10], 1) * E[row_index[2],]
    }

    return(E)

}

#' Calculate a random elementary matrix
#'
#' @param n The size of the elementary matrix (an n times n matrix)
#' @param k The number of elemenatry row operations to perform
#' @param verbose A logical value that determines whether to print the output
#' @return An product of elementary matrices, the inverse of the produce of elementary matrices, and the full sequence of elementary matrices
#' @export
#'
elementary_matrix_sequence <- function(n, k, verbose = FALSE) {
    # n is the size of the matrix, k is the number of operations to perform
    E <- diag(n)
    E_inv <- diag(n)
    matrix_sequence <- vector(mode = "list", length = k)
    for (j in 1:k) {
        matrix_sequence[[j]] <- elementary_matrix(n, verbose = verbose)
        E <- E %*% matrix_sequence[[j]]
        E_inv <- solve(matrix_sequence[[j]]) %*% E_inv
    }
    return(list(E = E, E_inv = E_inv, matrix_sequence = matrix_sequence))
}


