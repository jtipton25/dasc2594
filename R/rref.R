#' Compute the reduced row echelon form
#'
#' @param A a matrix
#' @param num_flops Should the number of floating point operations be returned?
#'
#' @return
#' A matrix in reduced row echelon form
#'
#' @export
#'
#' @examples
#'
#' A <- matrix(c(4, 3, 2, 5, 3, 5, 5, 3, 4, 4, 6, 5), 3, 4)
#' B <- rref(A)
rref <- function(A, num_flops = FALSE) {
    ## From the pracma gitHub page https://raw.githubusercontent.com/cran/pracma/master/R/rref.R
    stopifnot(is.numeric(A))
    if (!is.matrix(A))
        stop("Input parameter 'A' must be a matrix.")

    nr <- nrow(A)
    nc <- ncol(A)
    tol <- eps() * max(nr, nc) * max(abs(A))

    flops <- 0
    r <- 1
    for (i in 1:nc) {
        pivot <- which.max(abs(A[r:nr, i]))
        pivot <- r + pivot - 1
        m <- abs(A[pivot, i])

        #count flops
        # flops <- flops + 1
        if (m <= tol) {
            # warning("The algorithm might be numerically unstable. Treat the output with skepticism.")
            A[r:nr, i] <- 0  # zeros(nr-r+1, 1)
            # flops <- flops + length(r:nr)
        } else {
            A[c(pivot, r), i:nc] <- A[c(r, pivot), i:nc]
            # flops <- flops + length(i:nc)
            A[r, i:nc] <- A[r, i:nc] / A[r, i]
            # flops <- flops + length(i:nc) * 2

            # count flops 14
            flops <- flops + length(i:nc)

            if (r == 1) {
                ridx <- c((r + 1):nr)
            } else if (r == nr) {
                ridx <- c(1:(r - 1))
            } else {
                ridx <- c(1:(r - 1), (r + 1):nr)
            }
            # flops <- flops + length(ridx) + length(ridx) * length(i:nc)

            # count flops: matrix multiplication and addition
            flops <- flops + length(ridx) * length(i:nc) + length(i:nc)
            A[ridx, i:nc] <- A[ridx, i:nc] -
                A[ridx, i, drop = FALSE] %*% A[r, i:nc, drop = FALSE]
            if (r == nr) break
            r <- r + 1
        }
    }
    # if (any(abs(A) < tol))
    #     warning("The algorithm might be numerically unstable. Treat the output with skepticism.")
    A[abs(A) < tol] <- 0
    if(num_flops) {
        message("The number of flops to get RREF was ", flops)
    }
    return(A)
}
