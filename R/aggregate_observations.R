#' Aggregate observations at a species level
#' @param species id of the species
#' @param object_name the name of the object
#' @inheritParams validate_data
#' @export
#' @importFrom assertthat assert_that is.string noNA
#' @importFrom dplyr %>% filter group_by inner_join left_join semi_join
#' summarise transmute
#' @importFrom git2rdata read_vc repository
#' @importFrom rlang .data
#' @importFrom tidyr replace_na
aggregate_observations <- function(species_code, object_name, root) {
  assert_that(is.string(species_code), noNA(species_code))
  assert_that(is.string(object_name), noNA(object_name))
  if (!inherits(root, "git_repository")) {
    root <- repository(root)
  }
  assert_that(validate_data(root))
  all_species <- read_vc("species", root = root)
  assert_that(any(all_species$code == species_code))
  species_id <- all_species$id[all_species$code == species_code]
  selected_species <- all_species %>%
    filter(.data$id %in% species_id | .data$parent %in% species_id)
  while (length(species_id) < nrow(selected_species)) {
    species_id <- selected_species$id
    selected_species <- all_species %>%
      filter(.data$id %in% species_id | .data$parent %in% species_id)
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
