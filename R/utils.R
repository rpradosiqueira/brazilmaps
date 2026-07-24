.brazilmaps_cache <- new.env(parent = emptyenv())

.canonical_levels <- c(
  "country", "region", "state", "state_hex", "state_region",
  "intermediate_region", "immediate_region", "mesoregion",
  "microregion", "municipality"
)

.legacy_level_aliases <- c(
  Brazil = "country",
  Region = "region",
  State = "state",
  StateHex = "state_hex",
  StateReg = "state_region",
  Intermediary = "intermediate_region",
  Immediate = "immediate_region",
  Imediate = "immediate_region",
  MesoRegion = "mesoregion",
  MicroRegion = "microregion",
  City = "municipality"
)

.level_file_stems <- c(
  country = "Brazil",
  region = "Region",
  state = "State",
  state_hex = "StateHex",
  state_region = "StateReg",
  intermediate_region = "Intermediary",
  immediate_region = "Imediate",
  mesoregion = "MesoRegion",
  microregion = "MicroRegion",
  municipality = "City"
)

.legacy_column_aliases <- c(
  Brazil = "country_code",
  Region = "region_code",
  Region_name = "region_name",
  State = "state_code",
  State_name = "state_name",
  State_abbr = "state_abbreviation",
  Intermediary = "intermediate_region_code",
  Intermediary_name = "intermediate_region_name",
  Immediate = "immediate_region_code",
  Immediate_name = "immediate_region_name",
  Imediate = "immediate_region_code",
  Imediate_name = "immediate_region_name",
  MesoRegion = "mesoregion_code",
  MesoRegion_name = "mesoregion_name",
  MicroRegion = "microregion_code",
  MicroRegion_name = "microregion_name",
  City = "municipality_code",
  City_name = "municipality_name"
)

.filter_key_columns <- c(
  country = "country_code",
  region = "region_code",
  state = "state_code",
  state_abbreviation = "state_abbreviation",
  intermediate_region = "intermediate_region_code",
  immediate_region = "immediate_region_code",
  mesoregion = "mesoregion_code",
  microregion = "microregion_code",
  municipality = "municipality_code"
)

.warn_deprecated <- function(old, new) {
  warning(
    sprintf("`%s` is deprecated; use `%s`.", old, new),
    call. = FALSE
  )
}

.normalize_levels <- function(levels, warn = TRUE) {
  if (!is.character(levels) || !length(levels) || anyNA(levels)) {
    stop("`levels` must be a non-empty character vector.", call. = FALSE)
  }

  legacy <- levels %in% names(.legacy_level_aliases)
  if (any(legacy) && warn) {
    .warn_deprecated(
      paste(unique(levels[legacy]), collapse = ", "),
      paste(unique(unname(.legacy_level_aliases[levels[legacy]])),
        collapse = ", "
      )
    )
  }
  levels[legacy] <- unname(.legacy_level_aliases[levels[legacy]])

  unknown <- setdiff(levels, .canonical_levels)
  if (length(unknown)) {
    stop(
      sprintf(
        "Unknown geographic level(s): %s. Available levels: %s.",
        paste(unknown, collapse = ", "),
        paste(.canonical_levels, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  unique(levels)
}

.validate_year <- function(year, available) {
  if (length(year) != 1L || is.na(year)) {
    stop("`year` must contain one available edition year.", call. = FALSE)
  }
  year <- suppressWarnings(as.integer(year))
  if (is.na(year) || !year %in% available) {
    stop(
      sprintf(
        "Municipal mesh year %s is unavailable. Use one of: %s.",
        deparse(year),
        paste(available, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  year
}

.rename_columns <- function(object, aliases = .legacy_column_aliases) {
  object_names <- names(object)
  matched <- match(object_names, names(aliases))
  replace <- !is.na(matched)
  object_names[replace] <- unname(aliases[matched[replace]])
  names(object) <- object_names
  object
}

.as_legacy_map <- function(map) {
  inverse <- stats::setNames(
    names(.legacy_column_aliases),
    unname(.legacy_column_aliases)
  )
  .rename_columns(map, inverse)
}

.normalize_filters <- function(filters, available_fields, warn = TRUE) {
  if (is.null(filters)) {
    return(NULL)
  }
  if (!is.list(filters) || is.null(names(filters)) ||
      any(!nzchar(names(filters)))) {
    stop("`filters` must be a named list.", call. = FALSE)
  }

  filter_names <- names(filters)
  legacy <- filter_names %in% names(.legacy_column_aliases)
  if (any(legacy) && warn) {
    replacements <- unname(.legacy_column_aliases[filter_names[legacy]])
    .warn_deprecated(
      paste(unique(filter_names[legacy]), collapse = ", "),
      paste(unique(replacements), collapse = ", ")
    )
  }
  filter_names[legacy] <- unname(
    .legacy_column_aliases[filter_names[legacy]]
  )

  shorthand <- filter_names %in% names(.filter_key_columns)
  filter_names[shorthand] <- unname(
    .filter_key_columns[filter_names[shorthand]]
  )
  if (anyDuplicated(filter_names)) {
    stop(
      "Each field may be supplied only once in `filters`.",
      call. = FALSE
    )
  }

  unknown <- setdiff(filter_names, available_fields)
  if (length(unknown)) {
    stop(
      sprintf(
        "Unknown filter field(s): %s. Available fields: %s.",
        paste(unknown, collapse = ", "),
        paste(available_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  names(filters) <- filter_names
  filters
}

.filter_rows <- function(data, filters, warn = TRUE) {
  filters <- .normalize_filters(filters, names(data), warn = warn)
  if (is.null(filters)) {
    return(data)
  }
  keep <- rep(TRUE, nrow(data))
  for (field in names(filters)) {
    keep <- keep & data[[field]] %in% filters[[field]]
  }
  data[keep, , drop = FALSE]
}

.read_topojson_gz <- function(path) {
  temporary <- tempfile(fileext = ".topojson")
  on.exit(unlink(temporary), add = TRUE)
  input <- gzfile(path, open = "rb")
  output <- file(temporary, open = "wb")
  repeat {
    bytes <- readBin(input, what = "raw", n = 1024L * 1024L)
    if (!length(bytes)) {
      break
    }
    writeBin(bytes, output)
  }
  close(input)
  close(output)
  map <- sf::st_read(temporary,
    quiet = TRUE,
    stringsAsFactors = FALSE
  )
  sf::st_crs(map) <- 4674
  if ("id" %in% names(map)) {
    map[["id"]] <- NULL
  }
  map <- .rename_columns(map)
  code_fields <- grep("_code$", names(map), value = TRUE)
  for (field in code_fields) {
    map[[field]] <- as.integer(map[[field]])
  }
  map
}

.read_current_map <- function(level) {
  cache_key <- paste0("current_", level)
  if (exists(cache_key, envir = .brazilmaps_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .brazilmaps_cache, inherits = FALSE))
  }

  stem <- .level_file_stems[[level]]
  topology_path <- system.file(
    "maps", paste0(stem, ".topojson.gz"),
    package = "brazilmaps"
  )
  if (nzchar(topology_path)) {
    map <- .read_topojson_gz(topology_path)
  } else {
    rds_path <- system.file(
      "maps", paste0(stem, ".rds"),
      package = "brazilmaps"
    )
    if (!nzchar(rds_path)) {
      stop("The requested map is not installed.", call. = FALSE)
    }
    map <- readRDS(rds_path)
    if (inherits(map, "sfc")) {
      map <- sf::st_sf(
        name = "Brazil",
        country_code = 0L,
        geometry = map
      )
    }
    map <- .rename_columns(map)
    sf::st_crs(map) <- 4674
  }

  assign(cache_key, map, envir = .brazilmaps_cache)
  map
}

.read_city_map <- function(year) {
  cache_key <- paste0("municipality_", year)
  if (exists(cache_key, envir = .brazilmaps_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .brazilmaps_cache, inherits = FALSE))
  }

  file_name <- sprintf("City_%d.topojson.gz", year)
  path <- system.file("maps", "municipality", file_name, package = "brazilmaps")
  if (!nzchar(path)) {
    stop(sprintf("Municipal mesh file for %d is not installed.", year),
      call. = FALSE
    )
  }

  map <- .read_topojson_gz(path)

  dtb_path <- system.file("dtb", "dtb.rds", package = "brazilmaps")
  if (nzchar(dtb_path)) {
    dtb <- .read_dtb()
    hierarchy <- dtb[dtb[["level"]] == "municipality", , drop = FALSE]
    index <- match(map[["municipality_code"]], hierarchy[["code"]])
    hierarchy_fields <- c(
      "region_code", "region_name", "state_code", "state_name",
      "microregion_code", "microregion_name",
      "mesoregion_code", "mesoregion_name",
      "immediate_region_code", "immediate_region_name",
      "intermediate_region_code", "intermediate_region_name"
    )
    for (field in hierarchy_fields) {
      if (!field %in% names(map)) {
        map[[field]] <- hierarchy[[field]][index]
      }
    }
  }
  assign(cache_key, map, envir = .brazilmaps_cache)
  map
}
