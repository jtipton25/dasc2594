#' Convert an R array into a \LaTeX system of equations for easy writing in Rmarkdown
#'
#' @param A An array to convert to Latex format
#' @param b A vector to convert to Latex format
#'
#' @return A string that can be put into an Rmarkdown document by using `cat()` to direct the output
#' @export
#'
#' @examples
#' latex_string <- array_to_latex(matrix(1:6, 3, 2))
#'
#' # within Rmarkdown chunk
#' # cat(latex_string)
array_to_matrix_equation <- function(A, b) {
    # From https://data-and-the-world.onrender.com/posts/matrix-to-latex/
    if (!is.matrix(A))
        stop("Input parameter 'A' must be a matrix.")
    if (!is.vector(b))
        stop("Input parameter 'b' must be a vector.")
    if (nrow(A) != length(b))
        stop("A and b must have the same number of rows")

    nrows <- nrow(A)
    ncols <- ncol(A)
    if (ncols < 2)
        stop("ncols must be at least 2")
    variables <- paste0("x_", 1:ncols)

    eqn_A <- array_to_latex(A)
    eqn_x <- array_to_latex(as.matrix(variables), fraction = FALSE)
    eqn_b <- array_to_latex(as.matrix(b))
    eqn_tex <- paste(eqn_A, eqn_x, " & =", eqn_b, collapse = "")

    return(paste("\\begin{align*}", eqn_tex, "\\end{align*}"))
}

