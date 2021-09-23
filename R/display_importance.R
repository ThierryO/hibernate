#' Display the importance of the spaces based on a model
#' @param model the output of `ìmputation_model()`.
#' @param labels the labels for the relative changes.
#' @inheritParams display_part
#' @inheritParams display_object
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
  display_object(
    object_name = attr(model, "object_name"), root = attr(model, "root"),
    extra_data = rf, extra_var = "mean", legend_title = legend_title,
    breaks = log(labels + 1), labels = sprintf("%+.0f%%", 100 * labels),
    shift = TRUE, rotate = TRUE, crs = crs
  )
}
