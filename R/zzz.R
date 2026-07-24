.read_dtb <- function() {
  path <- system.file("dtb", "dtb.rds", package = "brazilmaps")
  if (!nzchar(path)) {
    stop("The territorial division table is not installed.", call. = FALSE)
  }
  dtb <- .rename_columns(readRDS(path))
  legacy_values <- dtb[["level"]] %in% names(.legacy_level_aliases)
  dtb[["level"]][legacy_values] <- unname(
    .legacy_level_aliases[dtb[["level"]][legacy_values]]
  )
  names(dtb)[names(dtb) == "cod"] <- "code"
  names(dtb)[names(dtb) == "abbr"] <- "abbreviation"
  dtb
}

.normalize_name <- function(x) {
  trimws(tolower(enc2utf8(as.character(x))))
}

.as_legacy_dtb <- function(dtb) {
  inverse_levels <- stats::setNames(
    names(.legacy_level_aliases)[
      !duplicated(unname(.legacy_level_aliases))
    ],
    unname(.legacy_level_aliases)[
      !duplicated(unname(.legacy_level_aliases))
    ]
  )
  dtb[["level"]] <- unname(inverse_levels[dtb[["level"]]])
  names(dtb)[names(dtb) == "code"] <- "cod"
  names(dtb)[names(dtb) == "abbreviation"] <- "abbr"
  .as_legacy_map(dtb)
}
