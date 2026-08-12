#' Query the Brazilian Territorial Division
#'
#' Looks up territorial identifiers and their hierarchy in the version of the
#' Brazilian Territorial Division (DTB) shipped with the package.
#'
#' @param code Optional vector of IBGE territorial codes.
#' @param name Optional vector of territory names. Matching ignores case and
#'   surrounding whitespace.
#'
#' @return A data frame. If both arguments are `NULL`, the complete bundled
#'   snapshot is returned. If both are supplied, rows matching either
#'   condition are returned. A query with no matches returns zero rows while
#'   preserving the complete column schema.
#' @seealso [get_dtb_levels()]
#' @examples
#' get_dtb(code = c(50, 5002704, 1))
#' get_dtb(name = c("Campo Grande", "Recife"))
#' @references
#' \url{https://www.ibge.gov.br/geociencias/organizacao-do-territorio/estrutura-territorial/23701-divisao-territorial-brasileira.html}
#' @export
get_dtb <- function(code = NULL, name = NULL) {
  dtb <- .read_dtb()
  if (is.null(code) && is.null(name)) {
    return(dtb)
  }

  keep <- rep(FALSE, nrow(dtb))
  if (!is.null(code)) {
    query_codes <- suppressWarnings(as.numeric(code))
    if (anyNA(query_codes)) {
      stop(
        "Every value in `code` must be coercible to a numeric code.",
        call. = FALSE
      )
    }
    keep <- keep | dtb[["code"]] %in% query_codes
  }
  if (!is.null(name)) {
    query_names <- .normalize_name(name)
    keep <- keep | .normalize_name(dtb[["name"]]) %in% query_names
  }

  result <- dtb[keep, , drop = FALSE]
  if (!nrow(result)) {
    return(result)
  }
  used <- vapply(result, function(column) any(!is.na(column)), logical(1))
  result[, used, drop = FALSE]
}

#' Deprecated DTB query
#'
#' `get_dtb_info()` is retained for compatibility. Use [get_dtb()] with
#' argument `code` and lower-snake-case output columns.
#'
#' @param cod Optional vector of IBGE territorial codes.
#' @param name Optional vector of territory names.
#' @return A data frame using the legacy DTB column names.
#' @export
get_dtb_info <- function(cod = NULL, name = NULL) {
  .Deprecated("get_dtb")
  .as_legacy_dtb(get_dtb(code = cod, name = name))
}
