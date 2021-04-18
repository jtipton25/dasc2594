#'
#' Evaluate a tangent plane for a function \eqn{f(x, y)} at the point \eqn{(a, b)}.
#'
#' @param x A vector of x-axis coordinates.
#' @param y A vector of y-axis coordinates.
#' @param a The point at which to evaluate the tangent plane in the x-axis coordinates.
#' @param b The point at which to evaluate the tangent plane in the y-axis coordinates.
#' @param target_fun The function \eqn{f(x, y)} of interest.
#' @param grad_fun The gradient \eqn{\nabla f(x, y)}{} of the function \eqn{f(x, y)}.
#'
#' @return A vector of values that represent the tangent plane at each input value of `x` and `y`.
#' @export
#'
tangent_plane <- function(x, y, a, b, target_fun, grad_fun) {

    if(!is_numeric_vector(x, length(x)))
        stop("x must be a numeric vector.")
    if(!is_numeric_vector(y, length(y)))
        stop("y must be a numeric vector.")
    if (length(x) != length(y))
        stop("x and y must be numeric vectors of the same length.")
    if (!is_numeric(a, 1))
        stop("a must be a scalar.")
    if (!is_numeric(b, 1))
        stop("b must be a scalar.")

    grad <- grad_fun(a, b)
    target_fun(a, b) + grad[1] * (x - a) + grad[2] * (y - b)
}


#' Plot the function \eqn{f(x, y)} and the tangent plane of \eqn{f(x, y)} at a point \eqn{(a, b)}
#'
#' @param target_fun The function \eqn{f(x, y)} of interest.
#' @param grad_fun The gradient \eqn{\nabla f(x, y)} of the function \eqn{f(x, y)}.
#' @param a The point at which to evaluate the tangent plane in the x-axis coordinates.
#' @param b The point at which to evaluate the tangent plane in the y-axis coordinates.
#' @param n The number of grid points at which to evaluate the function \eqn{f(x, y)} and the tangent plane of \eqn{f(x, y)}
#' @param xlim The range of points in the x-axis at which to evaluate the function \eqn{f(x, y)} and the tangent plane of \eqn{f(x, y)}
#' @param ylim The range of points in the y-axis at which to evaluate the function \eqn{f(x, y)} and the tangent plane of \eqn{f(x, y)}
#'
#' @return A plotly plot of the function of interest and the
#' @export
#'
#' @import tidyverse
#' @importFrom plotly plot_ly add_surface add_trace
#'
#' @examples
#' target_fun <- function(x, y) {
#'     return(x^2 + y^2)
#' }
#'   grad_fun <- function(x, y) {
#'     c(2 * x, 2 * y)
#' }
#' plot_tangent_plane(target_fun = target_fun, grad_fun = grad_fun, a=-1, b = 1)

plot_tangent_plane <- function(target_fun, grad_fun, a = 3, b = 2, n = 50, xlim = c(-4, 4), ylim = c(-4, 4)) {
    library(tidyverse)
    library(plotly)

    # tangent plane
    if (!is_numeric(a, 1))
        stop("a must be a scalar.")

    # generate a grid over which to plot
    x <- seq(xlim[1], xlim[2], length = n)
    y <- seq(ylim[1], ylim[2], length = n)
    dat <- expand_grid(x, y)


    dat <- dat %>%
        # apply the function to the grid
        mutate(z = target_fun(x, y)) %>%
        # calculate the tangent plane
        mutate(z2 = tangent_plane(x, y, a, b, target = target_fun, grad = grad_fun))


    plot_ly(x = x, y = y, z = matrix(dat$z, n, n)) %>%
        add_surface(
            contours = list(
                z = list(
                    show=TRUE,
                    usecolormap=TRUE,
                    highlightcolor="#ff0000",
                    project=list(z=TRUE)
                )
            ),
            colorbar = list(title = "Function"), showscale = FALSE
        ) %>%
        add_surface(x = x, y = y, z = matrix(dat$z2, n, n),
                    colorbar = list(title = "Tangent plane"), showscale = FALSE) %>%
        add_trace(x = a, y = b, z = target_fun(a, b),
                  mode = "markers", type = "scatter3d",
                  marker = list(size = 5, color = "red", symbol = 104),
                  name = paste0("(a=", a, ", b=", b, ")"))

}


