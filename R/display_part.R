#' Display the floor plan of a part
#'
#' @inheritParams validate_data
#' @param part_id the id of the part
#' @param label Which labels to display on the labelled spaces?
#' `code` is the short code, `ìd` is the internal id, `none` hides the labels.
#' @param connection Display optional connections between the centroids.
#' @param rotate Rotate the part along the general direction of the object.
#' Default to `TRUE`.
#' @inheritParams sf::st_transform
#' @export
#' @importFrom assertthat assert_that is.count is.flag noNA
#' @importFrom dplyr %>% arrange bind_rows filter group_by inner_join left_join
#' mutate row_number select summarise
#' @importFrom ggplot2 aes aes_string element_blank coord_sf ggplot geom_sf
#' geom_sf_label labs scale_fill_manual theme
#' @importFrom git2rdata read_vc repository
#' @importFrom sf read_sf st_bbox st_cast st_centroid st_transform
#' @importFrom tidyr extract pivot_longer starts_with
display_part <- function(
  part_id, root, label = c("code", "id", "none"), connection = FALSE,
  rotate = TRUE, crs = 31370
) {
  assert_that(is.count(part_id))
  label <- match.arg(label)
  assert_that(is.flag(connection), noNA(connection))
  assert_that(is.flag(rotate), noNA(rotate))
  if (!inherits(root, "git_repository")) {
    root <- repository(root)
  }
  file.path(root$path, "..", "floor_plan.geojson") %>%
    read_sf() %>%
    inner_join(
      read_vc("floor_plan", root = root) %>%
        filter(.data$part == part_id),
      by = "id"
    ) %>%
    left_join(read_vc("space", root = root), by = "id") %>%
    select("id", "structure", "floor", "code") %>%
    st_transform(crs = crs) -> part_base
  read_vc("part", root = root) %>%
    filter(.data$id == part_id) -> part_title
  bb <- st_bbox(part_base)
  part_title %>%
    pivot_longer(starts_with("offset"), names_to = "var") %>%
    extract(
      .data$var, into = c("floor", "dir"), "offset([0-9]+)_([x|y])",
      convert = TRUE
    ) %>%
    inner_join(
      data.frame(
        dir = c("x", "y"),
        centre = c(mean(c(bb$xmin, bb$xmax)), mean(c(bb$ymin, bb$ymax)))
      ),
      by = "dir"
    ) %>%
    transmute(.data$floor, .data$dir, value = .data$value - .data$centre) %>%
    arrange(.data$dir) %>%
    group_by(.data$floor) %>%
    summarise(shift = list(.data$value)) -> shift
  read_vc("object", root = root) %>%
    semi_join(part_title, by = c("id" = "object")) -> angle
  part_base %>%
    inner_join(shift, by = "floor") %>%
    mutate(
      geometry = (.data$geometry + .data$shift) * rotation(angle$angle)
    ) -> part_data
  suppressWarnings({
    part_data %>%
      filter(!is.na(.data$code)) %>%
      st_centroid() -> centroids
  })
  p <- ggplot(part_data) +
    geom_sf(aes(fill = .data$structure), show.legend = FALSE) +
    scale_fill_manual(
      values = c(space = "transparent", wall = "grey25", collapsed = "grey75")
    ) +
    theme(panel.background = element_blank())
  if (connection) {
    read_vc("space_connection", root = root) %>%
      mutate(connection_id = row_number()) -> connections
    centroids %>%
      inner_join(connections, by = c("id" = "from")) %>%
      bind_rows(
        centroids %>%
          inner_join(connections, by = c("id" = "to"))
      ) %>%
      select(.data$connection_id, .data$floor, .data$direct) -> connected
    if (nrow(connected) > 0) {
      connected %>%
        group_by(.data$connection_id) %>%
        summarise(
          delta = sprintf("%i - %i", min(.data$floor), max(.data$floor)),
          direct = unique(.data$direct)
        ) %>%
        st_cast("LINESTRING") -> connected
      p <- p +
        geom_sf(
          data = connected, aes(linetype = .data$delta, colour = .data$direct)
        )
    }
  }
  if (label != "none") {
    p <- p +
      geom_sf_label(
        data = centroids, aes_string(label = label)
      ) +
      labs(title = part_title$name)
  }
  return(p + coord_sf(datum = crs))
}

#' @importFrom assertthat assert_that is.number
rotation <- function(degrees) {
  assert_that(is.number(degrees))
  radians <- degrees * pi / 180
  matrix(c(cos(radians), sin(radians), -sin(radians), cos(radians)), 2, 2)
}
