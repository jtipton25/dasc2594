#' Write an equation in Rmarkdown to a LaTeX format for compiling into a png equation image
#'
#' @param A An array to convert to Latex format
#' @param b A vector to convert to Latex format
#' @param file The file location to save the pdf output
#'
#' @return A string that can be put into an Rmarkdown document by using `cat()` to direct the output
#' @export
#'
#' @examples
#' latex_string <- array_to_latex(matrix(1:6, 3, 2))
#'
#' # within Rmarkdown chunk
#' # cat(latex_string)
latex_equation_to_png <- function(A, b, file = NULL, format = "system of equations") {
    # From https://data-and-the-world.onrender.com/posts/matrix-to-latex/
    if (!is.matrix(A))
        stop("Input parameter 'A' must be a matrix.")
    if (!is.vector(b))
        stop("Input parameter 'b' must be a vector.")
    if (nrow(A) != length(b))
        stop("A and b must have the same number of rows")
    ## add in matrix equation, system of equations
    # if (format != "vector equation")
    #     stop('format must be equal to "vector equation"')
    if (!(format %in% c("matrix equation", "vector equation", "system of equations")))
        stop('format must be equal to "matrix equation", "vector equation", or "system of equations"')

    eqn_out <- NULL
    if (format == "matrix equation") {
        eqn_out <- array_to_matrix_equation(A, b)
    }
    if (format == "vector equation") {
        eqn_out <- array_to_vector_equation(A, b)
    }
    if (format == "system of equations") {
        eqn_out <- array_to_system(A, b)
    }

    eqn_latex <- paste(
        # "\\documentclass[convert={density=600,size=1600x900,outext=.png}]{standalone}",
        "\\documentclass[convert={density=600,size=1600x900,outext=.png}, preview]{standalone}",
        "\\usepackage{amsmath}",
        # "\\usepackage{standalone}",
        # "\\standaloneconfig{convert={convertexe={convert}}}",
        "\\pagestyle{empty}",
        "\\begin{document}",
        eqn_out,
        "\\end{document}", sep = "\n"
    )
    if (is.null(file)) {
        return(eqn_latex)
    } else {
        write(eqn_latex, file = file)
    }
}

