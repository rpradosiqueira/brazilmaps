#' List bundled municipal mesh editions
#'
#' Returns the inventory generated with the spatial files. The package bundles
#' the latest official edition from each consecutive run with the same
#' municipality count. All listed editions are installed and read locally by
#' [get_brmap()].
#'
#' @return A data frame with edition year, feature count, file size, checksum,
#'   source and processing metadata.
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
