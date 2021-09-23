#' Create a network for the connected spaces
#' @inheritParams aggregate_observations
#' @inheritParams validate_data
#' @export
#' @importFrom assertthat assert_that is.flag is.string noNA
#' @importFrom dplyr %>% arrange filter left_join select semi_join
#' @importFrom git2rdata read_vc
#' @importFrom rlang .data
get_network <- function(object_name, direct_only = FALSE, root) {
  assert_that(is.string(object_name), noNA(object_name))
  assert_that(is.flag(direct_only), noNA(direct_only))
  assert_that(validate_data(root))
  connections <- read_vc("space_connection", root = root)
  if (direct_only) {
    connections <- filter(connections, .data$direct)
  }
  read_vc("object", root = root) %>%
    filter(.data$name == object_name) %>%
    semi_join(x = read_vc("part", root = root), by = c("object" = "id")) %>%
    semi_join(x = read_vc("space", root = root), by = c("part" = "id")) %>%
    select(.data$id, .data$code) %>%
    arrange(.data$code) -> space
  network <- diag(nrow(space))
  colnames(network) <- space$code
  rownames(network) <- space$code
  connections %>%
    left_join(space, by = c("from" = "id")) %>%
    left_join(space, by = c("to" = "id")) -> edges
  network[as.matrix(edges[, c("code.x", "code.y")])] <- 1
  network[as.matrix(edges[, c("code.y", "code.x")])] <- 1
  return(network)
}
