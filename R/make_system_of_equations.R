#' Simulate a system of equations Ax = b
#' Simulate a systemt of equations Ax = b that is (usually) easy to solve using RREF without needing many complicated fractions.
#'
#' @param n_equations The number of equations in the system (the number of rows for the matrix A).
#' @param n_variables The number of variables in the system (the number of columns for the matrix A).
#' @param dim_col The dimension of the column space. Must be between 1 and the minimum of the number of n_equations and n_variables.
#' @param dim_null The dimension of the column space. Must be between 1 and the minimum of the number of n_equations and n_variables.
#' @param is_consistent Is the system of equations Ax = b consistent (Is there a solution)?
#' @param is_homogeneous Is the vector b equal to the 0 vector?
#'
#' @return A list that contains the matrix A and the vector b. If the system of equations is not homogeneous or consistent, the list will also contain the solution vector x
#' @export
#'
#' @examples
#' n_equations <- 7
#' n_variables <- 5
#' dim_col = min(n_equations, n_variables)
#' dim_null = n_variables - dim_col
#' eq <- make_system_of_equations(n_equations, n_variables, dim_col = 2, is_consistent = TRUE)
#' str(eq)
#' rref(cbind(eq$A, eq$b))
#' if(eq$is_consistent)
#'   all.equal(eq$A %*% eq$x, eq$b)

make_system_of_equations <- function(
    n_equations,
    n_variables,
    dim_col = min(n_equations, n_variables),
    dim_null = n_variables - dim_col,
    is_consistent = ifelse(dim_col == n_equations, TRUE, sample(c(TRUE, FALSE), 1)),
    is_homogeneous = FALSE) {

    # TODO
    # better error checking for possible values of dim_col and dim_null
    # require n_equations and n_variables to be greater than 1)

    if (!is_positive_integer(n_equations, 1))
        stop("n_equations must be a positive integer")
    if (!is_positive_integer(n_variables, 1))
        stop("n_variables must be a positive integer")
    if (!is_positive_integer(dim_col, 1))
        stop("dim_col must be a positive integer")
    if (!is_nonnegative_integer(dim_null, 1))
        stop("dim_null must be a positive integer")
    if (dim_col > min(n_equations, n_variables))
        stop("dim_col must be no larger than min(n_equations, n_variables)")
    if (dim_null > min(n_equations, n_variables) | dim_null < min(0, n_equations - n_variables))
        stop("dim_null must be between the minimum of 0 and n_equations - n_variables (min(0, n_equations - n_variables)) and the mimimum of n_equations and n_variables (min(n_equations, n_varialbes))")
    if (dim_col + dim_null != n_variables)
        stop("dim_col + dim_null must equal n_variables")
    if (length(is_consistent) != 1)
        stop("is_consistent must be either TRUE or FALSE")
    if (!is.logical(is_consistent) | is.na(is_consistent))
        stop("is_consistent must be either TRUE or FALSE")
    if (dim_col == n_equations & !is_consistent)
        stop("is_consistent must be TRUE when dim_col = n_equations")
    if (length(is_homogeneous) != 1)
        stop("is_homogeneous must be either TRUE or FALSE")
    if (!is.logical(is_homogeneous) | is.na(is_homogeneous))
        stop("is_homogeneous must be either TRUE or FALSE")

    # maximum number of matrices to try
    max_iter <- 10e4
    iter <- 0
    A1 <- matrix(sample(-9:9, n_equations * dim_col, replace = TRUE), n_equations, dim_col)
    while(!isTRUE(all.equal(rref(A1), diag(max(n_equations, n_variables))[1:n_equations, 1:dim_col, drop = FALSE])) & iter < max_iter) {
        A1 <- matrix(sample(-9:9, n_equations * dim_col, replace = TRUE), n_equations, dim_col)
        iter <- iter + 1
    }
    if (iter == max_iter)
        stop("The function failed to simulate a matrix with linearly indepdent columns")
    A2 <- A1 %*% matrix(sample((-1:1)[-2], dim_col * dim_null, replace = TRUE), dim_col, dim_null)
    # A2 <- rbind(matrix(sample(-9:9, dim_null * dim_col), dim_col, dim_null), matrix(0, n_equations - dim_col, dim_null))
    A <- NULL
    if (ncol(A2) == 0) {
        A <- A1
    } else {
        A <- cbind(A1, A2)
    }

    # E <- elementary_matrix_sequence(n_equations, num_operations)
    # A <- E$E %*% A

    x <- NULL
    b <- NULL
    if (is_homogeneous) {
        b <- rep(0, n_equations)
    } else {
        # non-homogeneous system of equations
        if (is_consistent) {
            # consistent system of equations
            x <- c(sample(-9:9, dim_col), rep(0, dim_null))
            b <- A %*% x
        } else {
            # inconsistent system of equations
            # if (dim_null > 0) {
            #     # the solution lies in the nullspace
            #     x <- sample(-9:9, dim_null)
            #     b <- rbind(rref(A)[, (dim_col+1):(dim_col + dim_null), drop = FALSE][1:(n_equations - dim_null), , drop = FALSE], diag(dim_null)) %*% x
            # } else {
                # the system of equations does not lie in the nullspace
                # n_equations > n_variables
            if (n_equations > n_variables) {
                x <- c(sample(-9:9, dim_col), rep(0, dim_null))
                b <- A %*% x +
                    rbind(matrix(0, n_variables, n_equations - n_variables), diag(n_equations - n_variables))%*% sample(-9:9, n_equations - n_variables)
            } else {
                x <- c(sample(-9:9, dim_col), rep(0, dim_null))
                b <- A %*% x +
                    rbind(matrix(0, n_equations - dim_col, dim_null), diag(dim_null)) %*% sample(-9:9, dim_null)
            }
            # }
        }
    }

    # permute the columns
    col_idx <- sample(1:n_variables, n_variables)
    if (is_consistent | is_homogeneous) {
        return(list(A = A[, col_idx], b = b, x = x[col_idx], is_consistent = is_consistent))
    } else {
        return(list(A = A[, col_idx], b = b, is_consistent = is_consistent))
    }
}

