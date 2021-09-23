#' Display the importance of the spaces based on a model
#' @param model the output of `ìmputation_model()`.
#' @param labels the labels for the relative changes.
#' @param legend_title title for the colour legend
#' @inheritParams display_part
#' @export
#' @importFrom dplyr %>% arrange distinct filter group_by inner_join left_join
#' mutate select summarise transmute ungroup
#' @importFrom ggnewscale new_scale_fill
#' @importFrom ggplot2 coord_sf ggplot geom_sf scale_fill_viridis_c
#' scale_fill_manual
#' @importFrom rlang .data
#' @importFrom sf read_sf st_bbox st_drop_geometry st_transform
#' @importFrom tidyr pivot_longer extract
display_importance <- function(
  model, labels = c(-0.8, -0.5, 0, 2, 5), crs = 31370,
  legend_title = "relative\nimportance"
) {
  root <- attr(model, "root")
  model$.args$data %>%
    filter(.data$link == 2) %>%
    distinct(.data$part, .data$space, .data$space_int) %>%
    inner_join(model$summary.random$space_int, by = c("space_int" = "ID")) %>%
    transmute(
      space = as.character(.data$space),
      .data$mean, lcl = .data$`0.025quant`, ucl = .data$`0.975quant`
    ) %>%
    inner_join(read_vc("space", root = root), by = c("space" = "code")) %>%
    select(.data$id, .data$space, .data$mean, .data$lcl, .data$ucl) -> rf
  file.path(attr(model, "root"), "floor_plan.geojson") %>%
    read_sf() %>%
    inner_join(read_vc("floor_plan", root = root), by = "id") %>%
    inner_join(read_vc("part", root = root), by = c("part" = "id")) %>%
    inner_join(read_vc("object", root = root), by = c("object" = "id")) %>%
    st_transform(crs = crs) -> floorplan
  bb <- st_bbox(floorplan)
  floorplan %>%
    st_drop_geometry() %>%
    select(.data$part, starts_with("offset")) %>%
    distinct() %>%
    pivot_longer(starts_with("offset"), names_to = "var") %>%
    extract(
      .data$var, into = c("floor", "dir"), "offset([0-9]+)_([x|y])",
      convert = TRUE
    ) %>%
    inner_join(
      data.frame(
        dir = c("x", "y"),
        centre = c(mean(c(bb$xmin, bb$xmax)), mean(c(bb$ymin, bb$ymax)))
      ),
      by = "dir"
    ) %>%
    transmute(
      .data$part, .data$floor, .data$dir, value = .data$value - .data$centre
    ) %>%
    arrange(.data$dir) %>%
    group_by(.data$part, .data$floor) %>%
    summarise(shift = list(.data$value), .groups = "drop") %>%
    ungroup() -> shift
  rotate <- floorplan$angle[1]
  floorplan %>%
    inner_join(shift, by = c("part", "floor")) %>%
    mutate(
      geometry = (.data$geometry + .data$shift) * rotation(rotate)
    ) %>%
    select(.data$id, .data$structure) %>%
    left_join(rf, by = "id") -> rotated
  breaks <- log(labels + 1)
  labels <- sprintf("%+.0f%%", 100 * labels)
  ggplot(rotated) +
    geom_sf(aes(fill = structure), colour = NA, show.legend = FALSE) +
    scale_fill_manual(
      values = c(
        space = "transparent", wall = "black", collapsed = "transparent"
      )
    ) +
    new_scale_fill() +
    geom_sf(aes(fill = mean)) +
    scale_fill_viridis_c(
      legend_title, na.value = NA, labels = labels, breaks = breaks
    ) +
    coord_sf(datum = crs)
}
