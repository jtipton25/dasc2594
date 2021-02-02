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
array_to_vector_equation <- function(A, b) {
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
    eqn_tex <- ""
    for (j in 1:(ncols-1)) {
        eqn_tex <- paste(eqn_tex, variables[j], "\\begin{pmatrix}",
              paste(A[, j], collapse = " \\\\ "),
              "\\end{pmatrix} + ")

    }
    eqn_tex <- paste(eqn_tex, variables[ncols], "\\begin{pmatrix}",
                             paste(A[, ncols], collapse = " \\\\ "),
                             "\\end{pmatrix} =")
    eqn_tex <- paste(eqn_tex, "\\begin{pmatrix}",
                     paste(b, collapse = " \\\\ "),
                     "\\end{pmatrix}")
    return(paste("\\begin{align*}", eqn_tex, "\\end{align*}"))
}

