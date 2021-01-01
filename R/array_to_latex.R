array_to_latex <- function(arr){
    # From https://data-and-the-world.onrender.com/posts/matrix-to-latex/
    rows <- apply(arr, MARGIN = 1, paste, collapse = " & ")
    matrix_string <- paste(rows, collapse = " \\\\ ")
    return(paste("\\begin{bmatrix}", matrix_string, "\\end{bmatrix}"))
}
