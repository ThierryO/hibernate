#' Display the floor plan of a part
#'
#' @inheritParams validate_data
#' @param part_id the id of the part
#' @param label Which labels to display on the labelled spaces?
#' `code` is the short code, `ìd` is the internal id, `none` hides the labels.
#' @param connection Display optional connections between the centroids.
#' @inheritParams sf::st_transform
#' @export
#' @importFrom assertthat assert_that is.count is.flag noNA
#' @importFrom dplyr %>% arrange bind_rows filter group_by inner_join left_join
#' mutate row_number select summarise
#' @importFrom ggplot2 aes aes_string coord_sf ggplot geom_sf geom_sf_label
#' labs scale_fill_manual
#' @importFrom git2rdata read_vc
#' @importFrom sf read_sf st_cast st_centroid st_transform
#' @importFrom tidyr extract pivot_longer starts_with
display_part <- function(
  part_id, root, label = c("code", "id", "none"), connection = FALSE,
  crs = 31370
) {
  assert_that(is.count(part_id))
  label <- match.arg(label)
  assert_that(is.flag(connection), noNA(connection))
  read_vc("part", root = root) %>%
    filter(.data$id == part_id) -> part_title
  part_title %>%
    pivot_longer(starts_with("offset"), names_to = "var") %>%
    extract(
      .data$var, into = c("floor", "dir"), "offset([0-9]+)_([x|y])",
      convert = TRUE
    ) %>%
    select(.data$floor, .data$dir, .data$value) %>%
    arrange(.data$dir) %>%
    group_by(.data$floor) %>%
    summarise(shift = list(.data$value)) -> shift
  file.path(root$path, "..", "floor_plan.geojson") %>%
    read_sf() %>%
    inner_join(
      read_vc("floor_plan", root = root) %>%
        filter(.data$part == part_id),
      by = "id"
    ) %>%
    left_join(read_vc("space", root = root), by = "id") %>%
    select("id", "structure", "floor", "code") %>%
    st_transform(crs = crs) %>%
    inner_join(shift, by = "floor") %>%
    mutate(geometry = .data$geometry + .data$shift) -> part_data
  part_data %>%
    filter(!is.na(.data$code)) %>%
    st_centroid() -> centroids
  p <- ggplot(part_data) +
    geom_sf(aes(fill = .data$structure), show.legend = FALSE) +
    scale_fill_manual(
      values = c(space = "transparent", wall = "grey25", collapsed = "grey75")
    )
  if (connection) {
    read_vc("space_connection", root = root) %>%
      mutate(connection_id = row_number()) -> connections
    centroids %>%
      inner_join(connections, by = c("id" = "from")) %>%
      bind_rows(
        centroids %>%
          inner_join(connections, by = c("id" = "to"))
      ) %>%
      select(.data$connection_id, .data$floor) -> connected
    if (nrow(connected) > 0) {
      connected %>%
        group_by(.data$connection_id) %>%
        summarise(
          delta = sprintf("%i - %i", min(.data$floor), max(.data$floor))
        ) %>%
        st_cast("LINESTRING") -> connected
      p <- p +
        geom_sf(data = connected, aes(linetype = .data$delta), color = "red")
    }
  }
  if (label != "none") {
    p <- p +
      geom_sf_label(
        data = centroids,
        aes_string(label = label)
      ) +
      labs(title = part_title$name)
  }
  return(p + coord_sf(datum = crs))
}
