#' Aggregate observations at a species level
#' @param species id of the species
#' @param object_name the name of the object
#' @inheritParams validate_data
#' @export
#' @importFrom assertthat assert_that is.count
#' @importFrom dplyr %>% filter group_by inner_join left_join semi_join
#' summarise transmute
#' @importFrom git2rdata read_vc repository
#' @importFrom rlang .data
#' @importFrom tidyr replace_na
aggregate_observations <- function(species, object_name, root) {
  assert_that(is.count(species))
  if (!inherits(root, "git_repository")) {
    root <- repository(root)
  }
  assert_that(validate_data(root))
  all_species <- read_vc("species", root = root)
  assert_that(any(all_species$id == species))
  selected_species <- all_species %>%
    filter(.data$id %in% species | .data$parent %in% species)
  while (length(species) < nrow(selected_species)) {
    species <- selected_species$id
    selected_species <- all_species %>%
      filter(.data$id %in% species | .data$parent %in% species)
  }
  read_vc("survey_space_species", root) %>%
    filter(.data$species %in% selected_species$id) %>%
    group_by(.data$survey_space) %>%
    summarise(count = sum(.data$count)) %>%
    left_join(
      x = read_vc("survey_space", root), by = c("id" = "survey_space")
    ) %>%
    transmute(.data$survey, .data$space, count = replace_na(.data$count, 0)) %>%
    inner_join(read_vc("survey", root = root), by = c("survey" = "id")) %>%
    inner_join(read_vc("space", root = root), by = c("space" = "id")) %>%
    inner_join(read_vc("part", root), by = c("part" = "id")) %>%
    semi_join(
      read_vc("object", root) %>%
        filter(.data$name == object_name),
      by = c("object" = "id")
    ) %>%
    transmute(
      .data$date, part = factor(.data$name), space_id = .data$space,
      space = factor(.data$code.x), .data$count
    )
}
