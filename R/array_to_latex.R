#' Convert an R array into \LaTeX for easy writing in Rmarkdown
#'
#' @param A An array to convert to Latex format
#' @param fraction An option of whether to print the result as a decimal or fracion
#'
#' @return A string that can be put into an Rmarkdown document by using `cat()` to direct the output
#' @export
#' @importFrom MASS fractions
#'
#' @examples
#' latex_string <- array_to_latex(matrix(1:6, 3, 2))
#'
#' # within Rmarkdown chunk
#' # cat(latex_string)
array_to_latex <- function(A, fraction = TRUE){
    # From https://data-and-the-world.onrender.com/posts/matrix-to-latex/
    if (!is.matrix(A))
        stop("Input parameter 'A' must be a matrix.")
    if (!is.logical(fraction))
        stop("fraciton must be a logical input.")
    rows <- NULL
    if (fraction) {
        rows <- apply(as.character(fractions(A)), MARGIN = 1, paste, collapse = " & ")
    } else {
        rows <- apply(A, MARGIN = 1, paste, collapse = " & ")
    }
    matrix_string <- paste(rows, collapse = " \\\\ ")
    return(paste("\\begin{pmatrix}", matrix_string, "\\end{pmatrix}"))
}
