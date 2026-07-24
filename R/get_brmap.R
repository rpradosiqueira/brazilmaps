#' Get a Brazilian territorial map
#'
#' Returns a simplified territorial mesh shipped with the package. No network
#' connection is used. Municipal meshes can be selected by milestone edition
#' year; other levels represent the current edition.
#'
#' @param level Geographic level in lower snake case. One of `"country"`,
#'   `"region"`, `"state"`, `"state_hex"`, `"state_region"`,
#'   `"intermediate_region"`, `"immediate_region"`, `"mesoregion"`,
#'   `"microregion"` or `"municipality"`.
#' @param filters Optional named list of code filters. Level shorthands such as
#'   `list(region = 5, state = 50)` are accepted and combined with logical AND.
#' @param output Output type: `"sf"`, `"data.frame"` or `"sp"`.
#' @param year Bundled municipal mesh edition. `NULL` selects the most recent
#'   edition. Historical years are available only for municipalities.
#' @param geo,geo_filter,as Deprecated aliases for `level`, `filters` and
#'   `output`.
#' @param geo.filter,class Older deprecated aliases for `filters` and
#'   `output`.
#'
#' @return An `sf` object by default. `"data.frame"` removes the geometry;
#'   `"sp"` returns a legacy `Spatial` object and requires package `sp`.
#' @seealso [brmap_editions()], [join_brmap()], [plot_brmap()]
#' @examples
#' rio <- get_brmap("state", filters = list(state = 33))
#' municipalities_2010 <- get_brmap(
#'   "municipality", year = 2010, filters = list(region = 5)
#' )
#' plot_brmap(rio)
#' @export
get_brmap <- function(
    level = "country",
    filters = NULL,
    output = c("sf", "data.frame", "sp"),
    year = NULL,
    geo = NULL,
    geo_filter = NULL,
    as = NULL,
    geo.filter = NULL,
    class = NULL) {
  if (!is.null(geo)) {
    if (!missing(level)) {
      stop("Use only one of `level` and deprecated `geo`.", call. = FALSE)
    }
    .warn_deprecated("geo", "level")
    level <- geo
  }

  supplied_filters <- Filter(
    Negate(is.null),
    list(
      filters = filters,
      geo_filter = geo_filter,
      geo.filter = geo.filter
    )
  )
  if (length(supplied_filters) > 1L) {
    stop(
      "Use only one of `filters`, `geo_filter` and `geo.filter`.",
      call. = FALSE
    )
  }
  if (!is.null(geo_filter)) {
    .warn_deprecated("geo_filter", "filters")
    filters <- geo_filter
  }
  if (!is.null(geo.filter)) {
    .warn_deprecated("geo.filter", "filters")
    filters <- geo.filter
  }

  if (!is.null(as) && !is.null(class)) {
    stop("Use only one of deprecated `as` and `class`.", call. = FALSE)
  }
  if (!is.null(as)) {
    if (!missing(output)) {
      stop("Use only one of `output` and deprecated `as`.", call. = FALSE)
    }
    .warn_deprecated("as", "output")
    output <- as
  }
  if (!is.null(class)) {
    if (!missing(output)) {
      stop("Use only one of `output` and deprecated `class`.", call. = FALSE)
    }
    .warn_deprecated("class", "output")
    output <- if (identical(class, "SpatialPolygonsDataFrame")) {
      "sp"
    } else {
      class
    }
  }
  output <- match.arg(output)

  legacy_schema <- is.character(level) &&
    any(level %in% names(.legacy_level_aliases))
  level <- .normalize_levels(level)
  if (length(level) != 1L) {
    stop("`level` must contain exactly one geographic level.", call. = FALSE)
  }

  if (identical(level, "municipality")) {
    available <- brmap_editions()[["year"]]
    if (is.null(year)) {
      year <- max(available)
    }
    year <- .validate_year(year, available)
    map <- .read_city_map(year)
  } else {
    if (!is.null(year)) {
      stop(
        "`year` can be used only with `level = \"municipality\"`.",
        call. = FALSE
      )
    }
    map <- .read_current_map(level)
  }

  map <- .filter_rows(map, filters)
  if (legacy_schema) {
    map <- .as_legacy_map(map)
  }

  if (identical(output, "data.frame")) {
    return(as.data.frame(sf::st_drop_geometry(map)))
  }
  if (identical(output, "sp")) {
    if (!requireNamespace("sp", quietly = TRUE)) {
      stop("Package `sp` is required for `output = \"sp\"`.",
        call. = FALSE
      )
    }
    return(sf::as_Spatial(map))
  }
  map
}
