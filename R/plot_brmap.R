#' Plot a Brazilian map
#'
#' Creates a `ggplot` using [ggplot2::geom_sf()]. The returned object can be
#' extended with any regular ggplot2 layer, scale or label.
#'
#' @param map An `sf` or legacy `Spatial` polygon object.
#' @param data Optional data frame to join before plotting.
#' @param by Join specification accepted by [dplyr::left_join()].
#' @param fill_by Optional single column name mapped to polygon fill.
#' @param theme A complete or partial ggplot2 theme.
#' @param border_colour Polygon border colour.
#' @param border_linewidth Polygon border width.
#' @param fill Constant fill used when `fill_by` is `NULL`.
#' @param data_to_join,join_by,var Deprecated aliases for `data`, `by` and
#'   `fill_by`.
#'
#' @return A `ggplot` object.
#' @seealso [get_brmap()], [join_brmap()]
#' @importFrom rlang .data
#' @examples
#' data("pop2017")
#' south <- get_brmap(
#'   "municipality", year = 2023, filters = list(region = 4)
#' )
#' plot_brmap(
#'   south,
#'   data = pop2017,
#'   by = c("municipality_code" = "mun"),
#'   fill_by = "pop2017"
#' )
#' @export
plot_brmap <- function(
    map,
    data = NULL,
    by = NULL,
    fill_by = NULL,
    theme = theme_brmap(),
    border_colour = "grey30",
    border_linewidth = 0.15,
    fill = "white",
    data_to_join = NULL,
    join_by = NULL,
    var = NULL) {
  if (!is.null(data_to_join)) {
    if (!is.null(data)) {
      stop(
        "Use only one of `data` and deprecated `data_to_join`.",
        call. = FALSE
      )
    }
    .warn_deprecated("data_to_join", "data")
    data <- data_to_join
  }
  if (!is.null(join_by)) {
    if (!is.null(by)) {
      stop("Use only one of `by` and deprecated `join_by`.",
        call. = FALSE
      )
    }
    .warn_deprecated("join_by", "by")
    by <- join_by
  }
  if (!is.null(var)) {
    if (!is.null(fill_by)) {
      stop("Use only one of `fill_by` and deprecated `var`.",
        call. = FALSE
      )
    }
    .warn_deprecated("var", "fill_by")
    fill_by <- var
  }

  if (inherits(map, "Spatial")) {
    map <- sf::st_as_sf(map)
  }
  if (!inherits(map, "sf")) {
    stop("`map` must be an sf or Spatial polygon object.", call. = FALSE)
  }
  if (!is.null(data) && !is.data.frame(data)) {
    stop("`data` must be NULL or a data frame.", call. = FALSE)
  }
  if (!is.null(data) && nrow(data) > 0L) {
    map <- join_brmap(map, data, by = by)
  }
  if (!is.null(fill_by) &&
      (length(fill_by) != 1L || is.na(fill_by))) {
    stop(
      "`fill_by` must be NULL or a single column name.",
      call. = FALSE
    )
  }
  if (!is.null(fill_by) && !fill_by %in% names(map)) {
    stop(sprintf("Column `%s` was not found in `map`.", fill_by),
      call. = FALSE
    )
  }

  if (is.null(fill_by)) {
    plot <- ggplot2::ggplot(map) +
      ggplot2::geom_sf(
        colour = border_colour,
        linewidth = border_linewidth,
        fill = fill
      )
  } else {
    plot <- ggplot2::ggplot(map) +
      ggplot2::geom_sf(
        ggplot2::aes(fill = .data[[fill_by]]),
        colour = border_colour,
        linewidth = border_linewidth
      ) +
      ggplot2::labs(fill = fill_by)
  }
  plot + theme
}

#' Minimal theme for Brazilian maps
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @return A ggplot2 theme.
#' @export
theme_brmap <- function(base_size = 9, base_family = "") {
  ggplot2::theme_void(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      legend.justification = c(0, 0),
      legend.position = "inside",
      legend.position.inside = c(0.01, 0.01)
    )
}

#' @rdname theme_brmap
#' @usage theme_map(base_size = 9, base_family = "")
#' @export
theme_map <- function(base_size = 9, base_family = "") {
  .Deprecated("theme_brmap")
  theme_brmap(base_size = base_size, base_family = base_family)
}
