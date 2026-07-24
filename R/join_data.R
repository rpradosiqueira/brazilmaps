#' Join tabular data to a Brazilian map
#'
#' A small type-preserving wrapper around [dplyr::left_join()].
#'
#' @param map An `sf`, `SpatialPolygonsDataFrame` or `data.frame` object.
#' @param data A data frame containing the columns to add.
#' @param by Join specification accepted by [dplyr::left_join()].
#'
#' @return The same broad object type supplied in `map`.
#' @seealso [get_brmap()]
#' @examples
#' data("pop2017")
#' municipalities <- get_brmap("municipality", year = 2023)
#' municipalities <- join_brmap(
#'   municipalities, pop2017, by = c("municipality_code" = "mun")
#' )
#' @export
join_brmap <- function(map, data, by = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (inherits(map, "SpatialPolygonsDataFrame")) {
    map@data <- dplyr::left_join(map@data, data, by = by)
    return(map)
  }
  if (inherits(map, "sf") || is.data.frame(map)) {
    return(dplyr::left_join(map, data, by = by))
  }

  stop(
    "`map` must be an sf, SpatialPolygonsDataFrame or data.frame object.",
    call. = FALSE
  )
}

#' @rdname join_brmap
#' @usage join_data(map, data, by = NULL)
#' @export
join_data <- function(map, data, by = NULL) {
  .Deprecated("join_brmap")
  join_brmap(map, data, by = by)
}
