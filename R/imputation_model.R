#' Fit imputation model
#' @inheritParams aggregate_observations
#' @inheritParams validate_data
#' @inheritParams get_network
#' @export
#' @importFrom dplyr %>% bind_rows distinct filter group_by inner_join mutate
#' transmute ungroup
#' @importFrom git2rdata repository
#' @importFrom INLA inla
#' @importFrom lubridate round_date year
#' @importFrom tidyr complete
imputation_model <- function(
  species_code, object_name, root, direct_only = FALSE
) {
  if (!inherits(root, "git_repository")) {
    root <- repository(root)
  }
  aggregate_observations(
    species_code = species_code, object_name = object_name, root = root
  ) %>%
    group_by(.data$space) %>%
    mutate(mc = max(.data$count)) %>%
    ungroup() -> survey_data
  relevant <- as.character(unique(survey_data$space[survey_data$mc > 0]))
  network <- get_network(
    object_name = object_name, root = root, direct_only = direct_only
  )
  singleton <- which(colSums(network) == 1)
  remove <- singleton[!names(singleton) %in% relevant]
  network <- network[-remove, -remove]
  important <- colnames(network) %in% relevant
  extra <- which(rowSums(network[!important, important]) > 0)
  while (length(extra)) {
    relevant <- c(relevant, names(extra))
    important <- colnames(network) %in% relevant
    extra <- which(rowSums(network[!important, important]) > 0)
  }
  network <- network[important, important]
  survey_data %>%
    filter(.data$space %in% colnames(network)) %>%
    transmute(
      year = round_date(.data$date, unit = "years") %>%
        year(),
      space = factor(.data$space, levels = colnames(network)),
      observed = .data$count
    ) %>%
    complete(year = min(.data$year):max(.data$year), .data$space) %>%
    mutate(cyear = .data$year - max(.data$year)) %>%
    inner_join(
      survey_data %>%
        distinct(.data$part, .data$space),
      by = "space"
    ) -> model_data
  model_data %>%
    transmute(
      zero_year = .data$year, zero_cyear = .data$cyear, .data$observed,
      zero_space = factor(.data$space), zero_intercept = 1,
      zero = as.integer(.data$observed == 0), link = 1,
      zero_space_int = as.integer(.data$space), zero_part = factor(.data$part)
    ) %>%
    bind_rows(
      model_data %>%
        mutate(
          space = factor(.data$space), part = factor(.data$part),
          count = ifelse(.data$observed > 0, .data$observed, NA),
          link = 2, intercept = 1, space_int = as.integer(.data$space)
        )
    ) -> analysis_data
  model <- inla(
    cbind(zero, count) ~ 0 +
      zero_intercept + intercept +
      f(
        zero_cyear, model = "rw2",
        hyper = list(theta = list(prior = "pc.prec", param = c(0.02, 0.05)))
      ) +
      f(
        zero_space, model = "iid",
        hyper = list(theta = list(prior = "pc.prec", param = c(0.1, 0.05)))
      ) +
      f(
        cyear, model = "rw1",
        hyper = list(theta = list(prior = "pc.prec", param = c(0.1, 0.05)))
      ) +
      f(
        space_int, model = "besagproper", graph = network
      ),
    family = c("binomial", "zeroinflatednbinomial0"),
    data = analysis_data,
    control.family = list(
      list(),
      list(hyper = list(theta = list(intial = -10, fixed = TRUE)))
    ),
    control.predictor = list(link = analysis_data$link),
    control.compute = list(waic = TRUE, config = TRUE, graph = TRUE)
  )
  attr(model, "species_code") <- species_code
  attr(model, "object_name") <- object_name
  attr(model, "root") <- root
  return(model)
}
