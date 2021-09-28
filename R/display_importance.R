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
  model,
  labels = c(
    0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 0.75, 1, 1.33, 2, 5, 10, 20, 50, 100
  ),
  crs = 31370, legend_title = "relative\nimportance"
) {
  root <- attr(model, "root")
  model$.args$data %>%
    filter(.data$link == 2) %>%
    distinct(.data$part, .data$space, .data$count_space) %>%
    inner_join(
      model$summary.random$count_space, by = c("count_space" = "ID")
    ) %>%
    transmute(
      space = as.character(.data$space), .data$mean
    ) %>%
    inner_join(read_vc("space", root = root), by = c("space" = "code")) -> tmp
  extreme <- exp(range(tmp$mean))
  if (extreme[1] < min(labels)) {
    labels <- c(extreme[1], labels)
  }
  if (max(labels) < extreme[2]) {
    labels <- c(labels, extreme[2])
  }
  tmp %>%
    transmute(
      .data$id, .data$space,
      mean = cut(
        .data$mean, breaks = log(labels),
        labels = paste(head(labels, -1), tail(labels, -1), sep = " - "),
        include.lowest = TRUE
      )
    ) -> rf
  display_object(
    object_name = attr(model, "object_name"), root = attr(model, "root"),
    extra_data = rf, extra_var = "mean", legend_title = legend_title,
    breaks = levels(rf$mean), shift = TRUE, rotate = TRUE, crs = crs
  )
}
