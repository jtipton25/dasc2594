#' Convert an R array into a \LaTeX system of equations for easy writing in Rmarkdown
#'
#' @param A An array to convert to Latex format
#'
#' @return A string that can be put into an Rmarkdown document by using `cat()` to direct the output
#' @export
#'
#' @examples
#' latex_string <- array_to_latex(matrix(1:6, 3, 2))
#'
#' # within Rmarkdown chunk
#' # cat(latex_string)
array_to_system <- function(A) {
    # From https://data-and-the-world.onrender.com/posts/matrix-to-latex/
    if (!is.matrix(A))
        stop("Input parameter 'A' must be a matrix.")
    nrows <- nrow(A)
    ncols <- ncol(A)
    row_strings <- rep("", nrows)
    for (j in 1:nrows) {
        seperator <- rep("", ncols-1)
        variables <- paste0("x_", 1:(ncols-1))
        values <- as.character(abs(A[j, ]))
        for (k in 1:(ncols - 1)) {
            if (k == (ncols - 1)) {
                seperator[k]  <- "& {}={} &"
            } else {
                if (A[j, k+1] >= 0) {
                    seperator[k] <- "& {}+{} &"
                } else {
                    seperator[k] <- "& {}-{} &"
                }
            }
            if (A[j, k] == 0) {
                variables[k] <- ""
            }
            if (A[j, k+1] == 0 & k != (ncols-1)) {
                seperator[k] <- "& {}{} &"
            }
            if (A[j, k] %in% c(-1, 0, 1)) {
                values[k] <- ""
            }
            row_strings[j] <- paste(row_strings[j], values[k], variables[k], seperator[k])

        }
        row_strings[j] <- paste(row_strings[j], values[ncols])

    }
    row_strings <- paste(row_strings, collapse = "\\\\")
    return(paste(paste0("\\begin{alignat*}{", ncol(A), "}"), row_strings, "\\end{alignat*}"))
}

