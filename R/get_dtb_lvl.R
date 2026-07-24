#' Relate levels of the Brazilian Territorial Division
#'
#' Returns codes and names for one or more territorial levels using the
#' hierarchy shipped with the package.
#'
#' @param levels Character vector containing any of `"municipality"`,
#'   `"state"`, `"region"`, `"immediate_region"`,
#'   `"intermediate_region"`, `"microregion"` and `"mesoregion"`.
#' @param filters Optional named list of codes used to filter the result.
#'   Conditions are combined with logical AND.
#'
#' @return A data frame with explicit code/name pairs for the requested levels.
#' @seealso [get_dtb()]
#' @examples
#' get_dtb_levels(c("municipality", "region"))
#' get_dtb_levels(c("state", "immediate_region"))
#' @references
#' \url{https://www.ibge.gov.br/geociencias/organizacao-do-territorio/estrutura-territorial/23701-divisao-territorial-brasileira.html}
#' @export
get_dtb_levels <- function(
    levels = c(
      "municipality", "state", "region", "immediate_region",
      "intermediate_region", "microregion", "mesoregion"
    ),
    filters = NULL) {
  levels <- .normalize_levels(levels)
  unsupported <- setdiff(
    levels,
    c(
      "municipality", "state", "region", "immediate_region",
      "intermediate_region", "microregion", "mesoregion"
    )
  )
  if (length(unsupported)) {
    stop(
      sprintf(
        "DTB relationships are unavailable for: %s.",
        paste(unsupported, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  depth <- c(
    region = 1L,
    state = 2L,
    intermediate_region = 4L,
    mesoregion = 4L,
    immediate_region = 5L,
    microregion = 5L,
    municipality = 7L
  )
  deepest <- names(which.max(depth[levels]))
  dtb <- .read_dtb()
  dtb <- dtb[dtb[["level"]] == deepest, , drop = FALSE]
  dtb <- .filter_rows(dtb, filters)

  wanted <- unlist(
    lapply(
      levels,
      function(level) {
        c(paste0(level, "_code"), paste0(level, "_name"))
      }
    ),
    use.names = FALSE
  )
  missing_columns <- setdiff(wanted, names(dtb))
  for (column in missing_columns) {
    dtb[[column]] <- NA
  }
  as.data.frame(dtb[, wanted, drop = FALSE])
}

#' Deprecated DTB level relationship helper
#'
#' `get_dtb_lvl()` is retained for compatibility. Use [get_dtb_levels()].
#'
#' @param geo Legacy geographic level names.
#' @param geo_filter Optional named list of legacy filters.
#' @return A data frame using the legacy DTB column names.
#' @export
get_dtb_lvl <- function(
    geo = c(
      "City", "State", "Region", "Immediate", "Intermediary",
      "MicroRegion", "MesoRegion"
    ),
    geo_filter = NULL) {
  .Deprecated("get_dtb_levels")
  levels <- .normalize_levels(geo, warn = FALSE)
  filters <- .normalize_filters(
    geo_filter,
    names(.read_dtb()),
    warn = FALSE
  )
  result <- get_dtb_levels(levels, filters = filters)
  .as_legacy_map(result)
}
