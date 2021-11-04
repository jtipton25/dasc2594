#' plot_change_basis
#'
#' Plot a change of basis from B1 to B2
#'
#' @param B1 2 by 2 matrix of the initial basis
#' @param B2 2 by 2 matrix of the initial basis
#' @return An animation created by `gganimate`.
#' @import ggplot2
#' @import gganimate
#' @import tidyverse
#' @import tidyr
#' @import tidyselect
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' if (requireNamespace("patchwork", quietly = TRUE)) {
#'   library(patchwork)
#' }
#' # create a 2D basis matrix
#' B1 <- matrix(c(1, 1, 0, 2), 2, 2)
#' # create a 2D basis matrix
#' B2 <- matrix(c(1, 2, 2, 1), 2, 2)
#'
#' # create a vector with coordinates with respect to the standard basis
#' x <- c(-1, -2)
#'
#' # plot the transformation of basis, and the intermediate transformation to the standard basis
#' p1 <- plot_change_basis(B1, B2) +
#'   facet_wrap(~ time,
#'                labeller = labeller(time = c("1" = "B1", "2" = "B2"))) +
#'   geom_segment(aes(x = 0, xend = -1, y = 0, yend = -2), color = "blue")
#'
#'  p2 <- plot_change_basis(B1, diag(2)) +
#'    facet_wrap(~ time,
#'                 labeller = labeller(time = c("1" = "B1", "2" = "I"))) +
#'    geom_segment(aes(x = 0, xend = -1, y = 0, yend = -2), color = "blue")
#'
#'
#'    p3 <- plot_change_basis(diag(2), B2) +
#'    facet_wrap(~ time,
#'               labeller = labeller(time = c("1" = "I", "2" = "B2"))) +
#'    geom_segment(aes(x = 0, xend = -1, y = 0, yend = -2), color = "blue")
#'
#' # generate the plot using patchwork
#' p1 / (p2 + p3)


#'

plot_change_basis <- function(B1, B2) {

  # change basis from B1 to B2
    if (!is_basis(B1) | nrow(B1) !=2)
        stop ("B1 must be a basis of dimension 2")
    if (!is_basis(B2) | nrow(B2) !=2)
        stop ("B2 must be a basis of dimension 2")

    ##
    ## Initialize a grid for the standard basis
    ##

    grid_standard <- construct_grid() %>%
        mutate(id = row_number())

    ##
    ## construct grids
    ##

    grid_start <- grid_standard %>%
        # need to `transform_df_coords()` twice as each segment is made up of 2 points
        transform_df_coords(.data$x, .data$y, m = B1) %>%
        transform_df_coords(.data$xend, .data$yend, m = B1)

    grid_end <- grid_standard %>%
        # need to `transform_df_coords()` twice as each segment is made up of 2 points
        transform_df_coords(.data$x, .data$y, m = B2) %>%
        transform_df_coords(.data$xend, .data$yend, m = B2)

    grid_all <- bind_rows(
        mutate(grid_start, time = 1),
        mutate(grid_end, time = 2)
    )

    ##
    ## construct basis functions
    ##

    basis_standard <- tibble(
        x = c(0, 0),
        y = c(0, 0),
        xend = c(1, 0),
        yend = c(0, 1),
        # `vec` is unnecessary, will just use to differentiate colors
        vec = c("i", "j")
    ) %>%
        mutate(id = nrow(grid_start) + row_number())

    basis_start <- basis_standard %>%
      transform_df_coords(.data$x, .data$y, m = B1) %>%
      transform_df_coords(.data$xend, .data$yend, m = B1)

    basis_end <- basis_standard %>%
        transform_df_coords(.data$x, .data$y, m = B2) %>%
        transform_df_coords(.data$xend, .data$yend, m = B2)

    basis_all <- bind_rows(
        mutate(basis_start, time = 1),
        mutate(basis_end, time = 2)
    )

    ##
    ## Define breaks in grid
    ##

    # If you just want to use the starting grid for the breaks, could do
    x_breaks <- unique(grid_start$x)
    y_breaks <- unique(grid_start$y)

    ##
    ## Define the animation
    ##
    p <- ggplot(aes(x = .data$x, y = .data$y, group = id), data = grid_all)+
        geom_segment(aes(xend = .data$xend, yend = .data$yend))+
        geom_segment(aes(xend = .data$xend, yend = .data$yend, colour = .data$vec), data = basis_all, arrow = arrow(length = unit(0.02, "npc")), size = 1.2)+
        scale_x_continuous(breaks = x_breaks, minor_breaks = NULL)+
        scale_y_continuous(breaks = y_breaks, minor_breaks = NULL)+
        coord_fixed()+
        theme_minimal()+
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              legend.position = "none")

    return(p)

}
