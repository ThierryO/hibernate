#' impute missing observations
#' @inheritParams display_importance
#' @param level the required level of aggregation.
#' Defaults to `"object"`.
#' @param n_sim number of simulations.
#' @export
#' @importFrom assertthat assert_that is.count noNA
#' @importFrom dplyr %>% across bind_cols filter full_join mutate n select
#' @importFrom INLA inla.posterior.sample
#' @importFrom purrr map map2 map_dfr
#' @importFrom stats plogis quantile rbinom rnbinom
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr replace_na
impute <- function(model, level = c("object", "part", "space"), n_sim = 999) {
  assert_that(is.count(n_sim), noNA(n_sim))
  level <- match.arg(level)
  raw_sims <- inla.posterior.sample(n = n_sim, result = model)
  raw_sims %>%
    map_dfr("hyperpar") %>%
    `[[`("size for nbinomial zero-inflated observations[2]") -> size
  raw_sims %>%
    map("latent") %>%
    map2(
      seq_len(n_sim),
      function(x, y) {
        colnames(x) <- sprintf("sample_%04i", y)
        return(as.data.frame(x))
      }
    ) %>%
    bind_cols() %>%
    rownames_to_column("latent") %>%
    filter(grepl("^Predictor:", .data$latent)) %>%
    bind_cols(
      model$.args$data %>%
        transmute(
          .data$observed, .data$link,
          year = ifelse(
            .data$link == 1, .data$zero_year,
            ifelse(.data$link == 2, .data$year, .data$undet_year)
          ),
          space = ifelse(
            .data$link == 1, .data$zero_space,
            ifelse(.data$link == 2, .data$space, .data$undet_space)
          )
        )
    ) %>%
    filter(is.na(.data$observed)) %>%
    pivot_longer(starts_with("sample_"), names_to = "sample") %>%
    extract(sample, "sample", "sample_(.*)", convert = TRUE) %>%
    select(
      .data$link, .data$year, .data$space, .data$sample, .data$value
    ) -> sims
  sims %>%
    filter(.data$link == 1) %>%
    select(-.data$link, zero = .data$value) %>%
    inner_join(
      sims %>%
        filter(.data$link == 2) %>%
        select(-.data$link, count = .data$value) %>%
        mutate(size = size[.data$sample]),
      by = c("year", "space", "sample")
    ) %>%
    inner_join(
      sims %>%
        filter(.data$link == 3) %>%
        select(-.data$link, undet = .data$value),
      by = c("year", "space", "sample")
    ) %>%
    mutate(
      zero = rbinom(n(), size = 1, prob = plogis(.data$zero)),
      count = rnbinom(n(), size = .data$size, mu = exp(.data$count)),
      imputed = (1 - .data$zero) * .data$count,
      undet = rbinom(n(), size = .data$imputed, prob = plogis(.data$undet)),
      space = factor(.data$space, labels = levels(model$.args$data$space))
    ) %>%
    inner_join(
      model$.args$data %>%
        distinct(.data$part, .data$space),
      by = "space"
    ) -> imputed
  switch(
    level,
    object = group_by(imputed, .data$year, .data$sample),
    part = group_by(imputed, .data$year, .data$part, .data$sample),
    group_by(imputed, .data$year, .data$part, .data$space, .data$sample)
  ) %>%
    summarise(
      total = sum(.data$imputed), undet = sum(.data$undet),
      .groups = "drop_last"
    ) %>%
    summarise(
      mean = mean(.data$total), lcl = quantile(.data$total, prob = 0.025),
      ucl = quantile(.data$total, prob = 0.975), undet_mean = mean(.data$undet),
      undet_lcl = quantile(.data$undet, prob = 0.025),
      undet_ucl = quantile(.data$undet, prob = 0.975), .groups = "drop"
    ) -> imputed_total
  model$.args$data %>%
    filter(.data$link == 2, !is.na(.data$observed)) %>%
    select(.data$year, .data$part, .data$space, .data$observed) %>%
    left_join(
      model$.args$data %>%
        filter(.data$link == 3, !is.na(.data$undet)) %>%
        select(year = .data$undet_year, .data$part, .data$space, .data$undet),
      by = c("year", "part", "space")
    ) %>%
    mutate(undet = replace_na(.data$undet, 0)) -> observed
  join_by <- switch(
    level, object = "year", part = c("year", "part"), c("year", "part", "space")
  )
  switch(
    level,
    object = group_by(observed, .data$year),
    part = group_by(observed, .data$year, .data$part),
    group_by(observed, .data$year, .data$part, .data$space)
  ) %>%
    summarise(
      observed = sum(.data$observed), undet = sum(.data$undet), .groups = "drop"
    ) %>%
    full_join(imputed_total, by = join_by) %>%
    mutate(
      mean = replace_na(.data$mean, 0) + replace_na(.data$observed, 0),
      lcl = replace_na(.data$lcl, 0) + replace_na(.data$observed, 0),
      ucl = replace_na(.data$ucl, 0) + replace_na(.data$observed, 0),
      undet_mean = replace_na(.data$undet_mean, 0) + replace_na(.data$undet, 0),
      undet_lcl = replace_na(.data$undet_lcl, 0) + replace_na(.data$undet, 0),
      undet_ucl = replace_na(.data$undet_ucl, 0) + replace_na(.data$undet, 0)
    )
}
