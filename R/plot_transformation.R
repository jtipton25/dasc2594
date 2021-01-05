#' plot_transformation
#'
#' @param transformation_matrix A matrix describing the transformation to animate
#' @return An animation created by `gganimate`.
#' @import ggplot2
#' @import gganimate
#' @import tidyverse
#' @import tidyr
#' @import tidyselect
#' @export
#'

plot_transformation <- function(transformation_matrix) {
    ##
    ## Initialize a grid
    ##
    grid_start <- construct_grid() %>%
        mutate(id = row_number())

    ##
    ## construct grids
    ##

    grid_trans <- grid_start %>%
        # need to `transform_df_coords()` twice as each segment is made up of 2 points
        transform_df_coords(x, y, m = transformation_matrix) %>%
        transform_df_coords(xend, yend, m = transformation_matrix)

    grid_all <- bind_rows(
        mutate(grid_start, time = 1),
        mutate(grid_trans, time = 2)
    )

    ##
    ## construct basis functions
    ##
    basis_start <- tibble(
        x = c(0, 0),
        y = c(0, 0),
        xend = c(1, 0),
        yend = c(0, 1),
        # `vec` is unnecessary, will just use to differentiate colors
        vec = c("i", "j")
    ) %>%
        mutate(id = nrow(grid_start) + row_number())

    basis_trans <- basis_start %>%
        transform_df_coords(x, y, m = transformation_matrix) %>%
        transform_df_coords(xend, yend, m = transformation_matrix)

    basis_all <- bind_rows(
        mutate(basis_start, time = 1),
        mutate(basis_trans, time = 2)
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
    p <- ggplot(aes(x = x, y = y, group = id), data = grid_all)+
        geom_segment(aes(xend = xend, yend = yend))+
        geom_segment(aes(xend = xend, yend = yend, colour = vec), data = basis_all, arrow = arrow(length = unit(0.02, "npc")), size = 1.2)+
        scale_x_continuous(breaks = x_breaks, minor_breaks = NULL)+
        scale_y_continuous(breaks = y_breaks, minor_breaks = NULL)+
        coord_fixed()+
        theme_minimal()+
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              legend.position = "none")

    return(p)
}
