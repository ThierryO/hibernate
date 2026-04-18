#' Scale a display
#' Return a vector of the optimal width and height
#' @param display the output of `display_object()`, `display_part()` or
#' `display_importance()`.
#' @param scale The minimal scale.
#' This prevents small objects to be scaled up.
#' Decreasing the number will enlarge small objects.
#' Defaults to `200`, referring to a 1:200 scale.
#' @param height maximal height in mm.
#' @param width maximal width in mm.
#' @export
#' @importFrom assertthat assert_that is.number noNA
#' @importFrom sf st_bbox
scale_display <- function(display, scale = 200, width = 180, height = 260) {
  assert_that(
    inherits(display, "ggplot"), inherits(display$data, "sf"),
    is.number(scale), noNA(scale), scale > 0,
    is.number(width), noNA(width), width > 0,
    is.number(height), noNA(height), height > 0
  )
  bb <- st_bbox(display$data)
  delta <- c(
    dy = unname(bb["ymax"] - bb["ymin"]), dx = unname(bb["xmax"] - bb["xmin"])
  )
  landscape <- FALSE
  ratio <- delta["dy"] / delta["dx"]
  if (1000 * delta["dy"] / scale < height) {
    if (1000 * delta["dx"] / scale < width) {
      fig_dim <- 1000 * delta / scale
    } else {
      fig_dim <- c(ratio, 1) * width
    }
  } else {
    if (ratio < 0.95) {
      if (ratio * height < width) {
        landscape <- TRUE
        fig_dim <- c(ratio, 1) * height
      } else {
        fig_dim <- c(ratio, 1) * width
      }
    } else {
      fig_dim <- height / c(1, ratio)
    }
  }
  fig_dim <- unname(fig_dim / 25.4)
  attr(fig_dim, "landscape") <- landscape
  return(fig_dim)
}
