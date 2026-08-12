#' List bundled municipal mesh editions
#'
#' Returns the inventory generated with the spatial files. The package bundles
#' the latest official edition from each consecutive run with the same
#' municipality count. All listed editions are installed and read locally by
#' [get_brmap()].
#'
#' @return A data frame with one row per installed edition and columns `year`,
#'   `n_features`, `file_bytes`, `md5`, `crs`, `simplification` and `source`.
#'   `source` contains the exact official IBGE archive URL or URLs used by the
#'   maintenance pipeline.
#' @examples
#' brmap_editions()
#' @export
brmap_editions <- function() {
  path <- system.file(
    "maps", "municipality", "index.csv", package = "brazilmaps"
  )
  if (!nzchar(path)) {
    stop("The municipal mesh inventory is not installed.", call. = FALSE)
  }
  utils::read.csv(path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' @rdname brmap_editions
#' @usage brmap_years()
#' @export
brmap_years <- function() {
  .Deprecated("brmap_editions")
  brmap_editions()
}
