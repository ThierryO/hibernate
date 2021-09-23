#' Display a binary map with the surveyed spaces
#' @inheritParams display_object
#' @inheritParams validate_data
#' @inheritParams ggplot2::discrete_scale
#' @param year Year of the survey
#' @export
#' @importFrom assertthat assert_that is.count is.string noNA
#' @importFrom dplyr %>% filter inner_join mutate semi_join
#' @importFrom ggplot2 waiver
#' @importFrom git2rdata read_vc
#' @importFrom lubridate round_date year
#' @importFrom rlang .data
display_surveyed <- function(
  object_name, year, root, legend_title, labels = waiver(), breaks = waiver(),
  crs = 31370
) {
  assert_that(is.string(object_name), noNA(object_name))
  assert_that(is.count(year))
  read_vc("object", root = root) %>%
    filter(.data$name == object_name) -> object
  assert_that(nrow(object) == 1, msg = "no matching object found.")
  read_vc("part", root = root) %>%
    semi_join(object, by = c("object" = "id")) %>%
    semi_join(x = read_vc("space", root = root), by = c("part" = "id")) %>%
    semi_join(
      x = read_vc("survey_space", root = root), by = c("space" = "id")
    ) %>%
    inner_join(read_vc("survey", root = root), by = c("survey" = "id")) %>%
    transmute(
      id = .data$space,
      bats = .data$total > 0,
      survey_year = round_date(.data$date, unit = "year") %>%
        year()
    ) %>%
    filter(.data$survey_year == year) -> surveyed
  if (missing(legend_title)) {
    legend_title <- sprintf("surveyed in %i", year)
  }
  display_object(
    object_name = object_name, root = root, extra_data = surveyed,
    extra_var = "bats", legend_title = legend_title, breaks = breaks,
    labels = labels
  )
}
