#' Display the object
#' @inheritParams aggregate_observations
#' @inheritParams display_part
#' @inheritParams ggplot2::discrete_scale
#' @param extra_data an optional data.frame to fill the spaces.
#' @param extra_var the name of the variable in `extra_data` to fill the spaces.
#' @param legend_title title for the colour legend
#' @param shift shift parts with different floors to avoid overlap.
#' Defaults to `TRUE`.
#' @export
#' @importFrom assertthat assert_that has_name is.flag is.string noNA
#' @importFrom dplyr %>% arrange filter group_by inner_join left_join semi_join
#' starts_with summarise ungroup
#' @importFrom ggplot2 ggplot coord_sf element_blank geom_sf scale_fill_manual
#' scale_fill_viridis_c scale_fill_viridis_d theme waiver
#' @importFrom ggnewscale new_scale_fill
#' @importFrom git2rdata repository read_vc
#' @importFrom rlang .data
#' @importFrom sf st_bbox st_transform read_sf
#' @importFrom tidyr extract pivot_longer
display_object <- function(
  object_name, root, extra_data, extra_var, legend_title = extra_var,
  labels = waiver(), breaks = waiver(), shift = TRUE, rotate = TRUE, crs = 31370
) {
  assert_that(is.string(object_name), noNA(object_name))
  if (!inherits(root, "git_repository")) {
    root <- repository(root)
  }
  assert_that(is.flag(shift), noNA(shift))
  assert_that(is.flag(rotate), noNA(rotate))
  read_vc("object", root = root) %>%
    filter(.data$name == object_name) -> angle
  assert_that(nrow(angle) == 1, msg = "no object found with matching name")
  read_vc("part", root = root) %>%
    semi_join(angle, by = c("object" = "id")) -> parts
  file.path(root$path, "..", "floor_plan.geojson") %>%
    read_sf() %>%
    inner_join(read_vc("floor_plan", root = root), by = "id") %>%
    semi_join(parts, by = c("part" = "id")) %>%
    st_transform(crs = crs) -> geoms
  if (shift) {
    parts %>%
      select("id", starts_with("offset")) |>
      pivot_longer(-.data$id) %>%
      extract(
        .data$name, c("floor", "direction"), "offset(.*)_(.)", convert = TRUE
      ) %>%
      arrange(.data$direction) %>%
      group_by(.data$id, .data$floor) %>%
      summarise(shift = list(.data$value), .groups = "drop") %>%
      ungroup() %>%
      inner_join(x = geoms, by = c("part" = "id", "floor")) %>%
      mutate(geometry = .data$geometry + .data$shift) -> geoms
  }
  if (rotate) {
    st_bbox(geoms) %>%
      matrix(ncol = 2) %>%
      rowMeans() -> bb_center
    geoms %>%
      mutate(
        geometry = (.data$geometry - bb_center) * rotation(angle$angle)
      ) -> geoms
  }
  if (missing(extra_data)) {
    return(
      ggplot(geoms) +
        geom_sf(aes(fill = .data$structure), show.legend = FALSE) +
        scale_fill_manual(
          values = c(
            space = "transparent", wall = "grey25", collapsed = "grey75"
          )
        ) +
        coord_sf(datum = crs)
    )
  }
  assert_that(
    is.string(extra_var), noNA(extra_var),
    inherits(extra_data, "data.frame"), has_name(extra_data, c("id", extra_var))
  )
  extra_data %>%
    select("id", extra = !!extra_var) %>%
    left_join(x = geoms, by = c("id")) -> final
  if (missing(legend_title)) {
    legend_title <- extra_var
  }
  p <- ggplot(final) +
    geom_sf(aes(fill = structure), colour = NA, show.legend = FALSE) +
    scale_fill_manual(
      values = c(
        space = "transparent", wall = "grey75", collapsed = "transparent"
      )
    ) +
    new_scale_fill() +
    geom_sf(aes(fill = .data$extra), size = 0.1)
  if (is.numeric(final$extra)) {
    p <- p +
      scale_fill_viridis_c(
        legend_title, na.value = NA, labels = labels, breaks = breaks
      )
  } else {
    p <- p + scale_fill_viridis_d(
      legend_title, na.value = NA, labels = labels, breaks = breaks
    )
  }
  p +
    coord_sf(datum = crs) +
    theme(panel.background = element_blank())
}
