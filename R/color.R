#' Data Point Palette
#'
#' This function returns the color palette that we used for Data Points.  It contains
#' colors from the CFPB palette, alternating between 'warm' and 'cool' colors as
#' the Design team recommended.
#' @param x A vector of numbers between 1 and 8
#' @return A hex representation of the appropriate color
#' @export
dp_palette <- function(x) {
  if (!is.numeric(x))
    stop('Must supply a numeric vector to dp_palette')
  if (any(x > 8) | any(x <= 0))
    stop('Invalid numbers supplied to dp_palette')

  my_palette <- c('#1FA049', '#DC7327', '#2763AF', '#755846',
                  '#267675', '#A02169', '#254B88', '#D14227')

  my_palette[x]
}

#' Data Point Theme
#'
#' This is the ggplot2 theme that I used to create the graphs in the Data Point.
#' @param base_family Font family to use (default = 'Helvetica')
#' @param base_size Size of the base font to use (default = 11)
#' @param light_family Font family to use where light text is required
#' @export
dp_theme <- function(base_family = 'Helvetica',
                     base_size = 11,
                     light_family = 'Helvetica-Narrow') {
  ret <- ggplot2::theme
}
