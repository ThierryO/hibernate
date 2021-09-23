#' Validata the raw data repository
#' @param root Either a `git_repository` object or the path to a git repository
#' @export
#' @importFrom assertthat assert_that noNA
#' @importFrom dplyr anti_join
#' @importFrom git2rdata read_vc repository
validate_data <- function(root) {
  if (!inherits(root, "git_repository")) {
    root <- repository(root)
  }
  survey <- read_vc("survey", root = root)
  observer <- read_vc("observer", root = root)
  survey_observer <- read_vc("survey_observer", root = root)
  assert_that(
    nrow(anti_join(survey_observer, survey, by = c("survey" = "id"))) == 0,
    msg = "Some rows in `survery_observer` has no matching id in `observer`"
  )
  object <- read_vc("object", root = root)
  part <- read_vc("part", root = root)
  assert_that(
    nrow(anti_join(part, object, by = c("object" = "id"))) == 0,
    msg = "Some rows in `part` has no matching id in `object`"
  )
  space <- read_vc("space", root = root)
  assert_that(
    nrow(anti_join(space, part, by = c("part" = "id"))) == 0,
    msg = "Some rows in `space` has no matching id in `part`"
  )
  survey_space <- read_vc("survey_space", root = root)
  assert_that(
    nrow(anti_join(survey_space, survey, by = c("survey" = "id"))) == 0,
    msg = "Some rows in `survey_space` has no matching id in `survey`"
  )
  assert_that(
    nrow(anti_join(survey_space, space, by = c("space" = "id"))) == 0,
    msg = "Some rows in `survey_space` has no matching id in `space`"
  )
  assert_that(noNA(survey_space$total), all(survey_space$total >= 0))
  species <- read_vc("species", root = root)
  survey_space_species <- read_vc("survey_space_species", root = root)
  assert_that(
    nrow(
      anti_join(
        survey_space_species, survey_space, by = c("survey_space" = "id")
      )
    ) == 0,
msg = "Some rows in `survey_space_species` has no matching id in `survey_space`"
  )
  assert_that(
    nrow(
      anti_join(survey_space_species, species, by = c("species" = "id"))
    ) == 0,
    msg = "Some rows in `survey_space_species` has no matching id in `species`"
  )
}
