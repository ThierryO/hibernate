#' Add a survey
#' @param date the date of the survey
#' @param observers a vector of observer ids.
#' @param lead_observer the observer id of the lead observer.
#' @param space a vector of space codes.
#' @param space_species a data.frame with space, species and count
#' @inheritParams validate_data
#' @export
#' @importFrom assertthat assert_that has_name is.count noNA
#' @importFrom dplyr %>% bind_rows filter group_by inner_join left_join mutate
#' row_number select summarise transmute
#' @importFrom git2rdata read_vc write_vc
#' @importFrom rlang .data
add_survey <- function(
  date, observers, lead_observer, space, space_species, root
) {
  assert_that(noNA(date), length(date) == 1)
  survey <- read_vc("survey", root)
  assert_that(!date %in% survey$date)
  assert_that(
    inherits(observers, "numeric"), noNA(observers), !anyDuplicated(observers)
  )
  assert_that(is.count(lead_observer), lead_observer %in% observers)
  date <- as.Date(date)
  assert_that(all(observers %in% read_vc("observer", root)$id))
  assert_that(
    is.character(space), noNA(space), !anyDuplicated(space), length(space) > 0
  )
  existing_space <- read_vc("space", root)
  assert_that(all(space %in% existing_space$code))
  species <- read_vc("species", root)
  assert_that(
    inherits(space_species, "data.frame"),
    all(has_name(space_species, c("species", "space"))),
    all(space_species$space %in% existing_space$code),
    all(space_species$species %in% species$code)
  )
  survey_id <- max(survey$id) + 1L
  survey %>%
    bind_rows(data.frame(id = survey_id, date = date)) %>%
    write_vc(file = "survey", root = root)
  data.frame(
    survey = survey_id,
    observer = as.integer(observers),
    lead = observers %in% lead_observer
  ) %>%
    bind_rows(read_vc("survey_observer", root = root)) %>%
    write_vc(file = "survey_observer", root = root)
  existing_ss <- read_vc("survey_space", root = root)
  existing_space %>%
    filter(.data$code %in% space) %>%
    transmute(
      space = .data$id, survey = survey_id,
      id = row_number() + max(existing_ss$id)
    ) -> new_ss
  existing_space_species <- read_vc(file = "survey_space_species", root = root)
  space_species %>%
    inner_join(species, by = c("species" = "code")) %>%
    select(species = .data$id, .data$space, .data$count) %>%
    inner_join(existing_space, by = c("space" = "code")) %>%
    select(.data$species, space = .data$id, .data$count) %>%
    inner_join(new_ss, by = "space") -> new_sss
  new_sss %>%
    group_by(.data$id) %>%
    summarise(total = as.integer(sum(.data$count))) %>%
    left_join(x = new_ss, by = "id") %>%
    mutate(total = replace_na(.data$total, 0L)) %>%
    bind_rows(existing_ss) %>%
    write_vc(file = "survey_space", root = root)
  new_sss %>%
    transmute(
      .data$species, survey_space = .data$id, count = as.integer(.data$count),
      id = row_number() + max(existing_space_species$id)
    ) %>%
    bind_rows(existing_space_species) %>%
    write_vc(file = "survey_space_species", root = root)
  return(invisible(NULL))
}
