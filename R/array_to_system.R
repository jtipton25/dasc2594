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
array_to_system <- function(A, b) {
    # From https://data-and-the-world.onrender.com/posts/matrix-to-latex/
    if (!is.matrix(A))
        stop("Input parameter 'A' must be a matrix.")
    if (!is.vector(b))
        stop("Input parameter 'b' must be a vector.")
    if (nrow(A) != length(b))
        stop("A and b must have the same number of rows")

    nrows <- nrow(A)
    ncols <- ncol(A)
    row_strings <- rep("", nrows)
    for (j in 1:nrows) {
        seperator <- rep("", ncols)
        variables <- paste0("x_", 1:ncols)
        values <- as.character(abs(A[j, ]))
        for (k in 1:ncols) {
            if (k == ncols) {
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
            if (k != ncols) {
                if (A[j, k+1] == 0) {
                    seperator[k] <- "& {}{} &"
                }
            }
            if (A[j, k] %in% c(-1, 0, 1)) {
                values[k] <- ""
            }

            if (k == 1) {
                if (A[j, k] < 0) {
                    if (A[j, k] == -1) {
                        values[k] <- "-"
                    } else {
                        values[k] <- A[j, k]
                    }
                }
            }
            row_strings[j] <- paste(row_strings[j], values[k], variables[k], seperator[k])

        }
        row_strings[j] <- paste(row_strings[j], b[j])

    }
    row_strings <- paste(row_strings, collapse = "\\\\")
    return(paste(paste0("\\begin{alignedat}{", ncol(A) + 1, "}"), row_strings, "\\end{alignedat}"))
}

