# Rebuild current non-municipal maps and the installed DTB hierarchy.
#
# Municipal geometry comes from the already generated current TopoJSON. The
# hierarchy comes from the official IBGE Localities API and is cached only for
# maintenance. Runtime users do not access the API.
#
# Maintenance dependencies:
# install.packages(c("jsonlite", "sf"))

stopifnot(
  requireNamespace("jsonlite", quietly = TRUE),
  requireNamespace("sf", quietly = TRUE)
)
options(timeout = max(1800, getOption("timeout")))

maintenance_cache_dir <- Sys.getenv(
  "BRAZILMAPS_CACHE_DIR",
  unset = file.path("data-raw", "cache")
)
maintenance_work_dir <- Sys.getenv(
  "BRAZILMAPS_WORK_DIR",
  unset = file.path("data-raw", "work")
)

replace_file <- function(candidate, destination) {
  backup <- paste0(destination, ".bak")
  if (file.exists(backup)) {
    if (!file.exists(destination)) {
      if (!file.rename(backup, destination)) {
        stop("Could not recover previous file ", destination, call. = FALSE)
      }
    } else {
      unlink(backup)
    }
  }
  had_destination <- file.exists(destination)
  if (had_destination && !file.rename(destination, backup)) {
    stop("Could not preserve existing file ", destination, call. = FALSE)
  }
  if (!file.rename(candidate, destination)) {
    if (had_destination) {
      file.rename(backup, destination)
    }
    stop("Could not move completed file to ", destination, call. = FALSE)
  }
  if (had_destination) {
    unlink(backup)
  }
  invisible(destination)
}

download_localities <- function(url, destination) {
  partial <- paste0(destination, ".part")
  unlink(partial)
  on.exit(unlink(partial), add = TRUE)
  status <- utils::download.file(
    url, partial, mode = "wb", quiet = TRUE
  )
  if (!identical(status, 0L) ||
      !file.exists(partial) ||
      file.info(partial)[["size"]] == 0) {
    stop("Download failed or was empty: ", url, call. = FALSE)
  }
  municipalities <- tryCatch(
    read_localities(partial),
    error = function(error) {
      stop(
        "The IBGE Localities response is not valid JSON: ",
        conditionMessage(error), call. = FALSE
      )
    }
  )
  replace_file(partial, destination)
  municipalities
}

read_localities <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

localities_url <- paste0(
  "https://servicodados.ibge.gov.br/api/v1/localidades/",
  "municipios?orderBy=id"
)
cache_dir <- file.path(maintenance_cache_dir, "current")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
localities_path <- file.path(cache_dir, "municipalities.json")
municipalities <- if (file.exists(localities_path) &&
    file.info(localities_path)[["size"]] > 0) {
  try(read_localities(localities_path), silent = TRUE)
} else {
  structure("cache missing", class = "try-error")
}
if (inherits(municipalities, "try-error")) {
  municipalities <- download_localities(localities_url, localities_path)
}

value_or_na <- function(object, field, type = c("integer", "character")) {
  type <- match.arg(type)
  value <- if (is.null(object)) NULL else object[[field]]
  if (is.null(value) || !length(value)) {
    return(if (type == "integer") NA_integer_ else NA_character_)
  }
  if (type == "integer") as.integer(value) else enc2utf8(as.character(value))
}

hierarchy_row <- function(municipality) {
  micro <- municipality[["microrregiao"]]
  meso <- if (is.null(micro)) NULL else micro[["mesorregiao"]]
  immediate <- municipality[["regiao-imediata"]]
  intermediary <- if (is.null(immediate)) {
    NULL
  } else {
    immediate[["regiao-intermediaria"]]
  }
  state <- if (!is.null(intermediary)) {
    intermediary[["UF"]]
  } else if (!is.null(meso)) {
    meso[["UF"]]
  } else {
    NULL
  }
  region <- if (is.null(state)) NULL else state[["regiao"]]

  data.frame(
    municipality_code = value_or_na(municipality, "id"),
    municipality_name = value_or_na(
      municipality, "nome", "character"
    ),
    microregion_code = value_or_na(micro, "id"),
    microregion_name = value_or_na(micro, "nome", "character"),
    mesoregion_code = value_or_na(meso, "id"),
    mesoregion_name = value_or_na(meso, "nome", "character"),
    immediate_region_code = value_or_na(immediate, "id"),
    immediate_region_name = value_or_na(
      immediate, "nome", "character"
    ),
    intermediate_region_code = value_or_na(intermediary, "id"),
    intermediate_region_name = value_or_na(
      intermediary, "nome", "character"
    ),
    state_code = value_or_na(state, "id"),
    state_name = value_or_na(state, "nome", "character"),
    state_abbreviation = value_or_na(state, "sigla", "character"),
    region_code = value_or_na(region, "id"),
    region_name = value_or_na(region, "nome", "character"),
    stringsAsFactors = FALSE
  )
}

hierarchy <- do.call(rbind, lapply(municipalities, hierarchy_row))
hierarchy <- hierarchy[
  order(hierarchy[["municipality_code"]]),
  ,
  drop = FALSE
]
stopifnot(
  nrow(hierarchy) == 5571L,
  !anyDuplicated(hierarchy[["municipality_code"]]),
  length(unique(stats::na.omit(
    hierarchy[["immediate_region_code"]]
  ))) == 510L,
  length(unique(stats::na.omit(
    hierarchy[["intermediate_region_code"]]
  ))) == 133L,
  5101837L %in% hierarchy[["municipality_code"]]
)

city_path <- file.path(
  "inst", "maps", "municipality", "City_2025.topojson.gz"
)
city_temporary <- tempfile(fileext = ".topojson")
city_input <- gzfile(city_path, open = "rb")
city_output <- file(city_temporary, open = "wb")
repeat {
  city_bytes <- readBin(city_input, what = "raw", n = 1024L * 1024L)
  if (!length(city_bytes)) break
  writeBin(city_bytes, city_output)
}
close(city_input)
close(city_output)
city <- sf::st_read(
  city_temporary, quiet = TRUE, stringsAsFactors = FALSE
)
unlink(city_temporary)
sf::st_crs(city) <- 4674
if ("id" %in% names(city)) {
  city[["id"]] <- NULL
}
city <- city[
  match(
    hierarchy[["municipality_code"]],
    city[["municipality_code"]]
  ),
  ,
  drop = FALSE
]
stopifnot(!anyNA(city[["municipality_code"]]))
for (field in setdiff(names(hierarchy), "municipality_code")) {
  city[[field]] <- hierarchy[[field]]
}
city[["name"]] <- city[["municipality_name"]]

previous_s2 <- sf::sf_use_s2(FALSE)
on.exit(sf::sf_use_s2(previous_s2), add = TRUE)

dissolve_level <- function(data, code, label, parents = character()) {
  keep <- !is.na(data[[code]])
  data <- data[keep, , drop = FALSE]
  groups <- split(seq_len(nrow(data)), data[[code]])
  rows <- lapply(groups, function(index) {
    values <- data[index[[1L]], c(code, label, parents), drop = FALSE]
    values <- sf::st_drop_geometry(values)
    names(values)[names(values) == label] <- "name"
    sf::st_sf(
      values,
      geometry = suppressMessages(
        suppressWarnings(sf::st_union(sf::st_geometry(data)[index]))
      )
    )
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result <- result[order(result[[code]]), , drop = FALSE]
  invalid <- !sf::st_is_valid(result)
  if (any(invalid)) {
    sf::st_geometry(result)[invalid] <- sf::st_make_valid(
      sf::st_geometry(result)[invalid]
    )
  }
  sf::st_crs(result) <- 4674
  result
}

current_maps <- list(
  State = dissolve_level(
    city, "state_code", "state_name",
    c("state_abbreviation", "region_code")
  ),
  Region = dissolve_level(city, "region_code", "region_name"),
  Immediate = dissolve_level(
    city, "immediate_region_code", "immediate_region_name",
    c("intermediate_region_code", "state_code", "region_code")
  ),
  Intermediary = dissolve_level(
    city, "intermediate_region_code", "intermediate_region_name",
    c("state_code", "region_code")
  )
)
current_maps[["Brazil"]] <- sf::st_sf(
  name = "Brasil",
  country_code = 0L,
  geometry = suppressMessages(
    suppressWarnings(sf::st_union(sf::st_geometry(city)))
  )
)
sf::st_crs(current_maps[["Brazil"]]) <- 4674

map_dir <- file.path("inst", "maps")

# Preserve curated cartograms and discontinued micro/mesoregions, while
# standardizing their columns and CRS metadata.
legacy_files <- c("StateHex", "StateReg", "MicroRegion", "MesoRegion")
legacy_columns <- c(
  State = "state_code",
  State_abbr = "state_abbreviation",
  Region = "region_code",
  Region_name = "region_name",
  MicroRegion = "microregion_code",
  MicroRegion_name = "microregion_name",
  MesoRegion = "mesoregion_code",
  MesoRegion_name = "mesoregion_name"
)

read_topojson_gz <- function(path) {
  temporary <- tempfile(fileext = ".topojson")
  on.exit(unlink(temporary), add = TRUE)
  input <- gzfile(path, open = "rb")
  output <- file(temporary, open = "wb")
  repeat {
    bytes <- readBin(input, what = "raw", n = 1024L * 1024L)
    if (!length(bytes)) break
    writeBin(bytes, output)
  }
  close(input)
  close(output)
  result <- sf::st_read(
    temporary, quiet = TRUE, stringsAsFactors = FALSE
  )
  if ("id" %in% names(result)) {
    result[["id"]] <- NULL
  }
  sf::st_crs(result) <- 4674
  result
}

read_legacy_map <- function(level) {
  rds_path <- file.path(map_dir, paste0(level, ".rds"))
  topology_path <- file.path(
    map_dir, paste0(level, ".topojson.gz")
  )
  if (file.exists(rds_path)) {
    return(readRDS(rds_path))
  }
  if (file.exists(topology_path)) {
    return(read_topojson_gz(topology_path))
  }
  stop("No installed source was found for ", level, call. = FALSE)
}

for (level in legacy_files) {
  object <- read_legacy_map(level)
  object_names <- names(object)
  matched <- match(object_names, names(legacy_columns))
  replace <- !is.na(matched)
  object_names[replace] <- unname(legacy_columns[matched[replace]])
  names(object) <- object_names
  sf::st_crs(object) <- 4674
  invalid <- !sf::st_is_valid(object)
  if (any(invalid)) {
    sf::st_geometry(object)[invalid] <- sf::st_make_valid(
      sf::st_geometry(object)[invalid]
    )
  }
  current_maps[[level]] <- object
}

expected_counts <- c(
  Brazil = 1L,
  Region = 5L,
  State = 27L,
  Immediate = 510L,
  Intermediary = 133L,
  MesoRegion = 137L,
  MicroRegion = 558L,
  StateHex = 27L,
  StateReg = 27L
)
file_stems <- c(
  Brazil = "Brazil",
  Region = "Region",
  State = "State",
  Immediate = "Imediate",
  Intermediary = "Intermediary",
  MesoRegion = "MesoRegion",
  MicroRegion = "MicroRegion",
  StateHex = "StateHex",
  StateReg = "StateReg"
)

mapshaper_script <- file.path(
  "data-raw", "node_modules", "mapshaper", "bin", "mapshaper"
)
if (!file.exists(mapshaper_script)) {
  stop(
    "mapshaper is not installed. Run `npm install --prefix data-raw`.",
    call. = FALSE
  )
}
node <- Sys.getenv("BRAZILMAPS_NODE", unset = Sys.which("node"))
if (!nzchar(node)) {
  stop(
    "Node.js was not found. Set BRAZILMAPS_NODE to its executable.",
    call. = FALSE
  )
}

topology_workspace <- file.path(
  maintenance_work_dir, "current", "mapshaper"
)
dir.create(topology_workspace, recursive = TRUE, showWarnings = FALSE)

run_mapshaper <- function(arguments, label) {
  status <- system2(
    node,
    c(
      "--max-old-space-size=8192",
      shQuote(normalizePath(mapshaper_script, winslash = "/")),
      arguments
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  command_status <- attr(status, "status")
  if (!is.null(command_status) && command_status != 0L) {
    stop(
      "mapshaper ", label, " failed:\n",
      paste(status, collapse = "\n"),
      call. = FALSE
    )
  }
}

write_current_topojson <- function(
    object, level, quantization = 1e6, max_area_error = 5e-4) {
  stem <- file_stems[[level]]
  input <- normalizePath(
    file.path(topology_workspace, paste0(stem, ".geojson")),
    winslash = "/", mustWork = FALSE
  )
  topology <- normalizePath(
    file.path(topology_workspace, paste0(stem, ".topojson")),
    winslash = "/", mustWork = FALSE
  )
  sf::st_write(
    object, input, driver = "GeoJSON", delete_dsn = TRUE,
    quiet = TRUE, layer_options = "RFC7946=NO"
  )
  run_mapshaper(
    c(
      shQuote(input),
      "-o", "force", "format=topojson",
      paste0("quantization=", format(quantization, scientific = FALSE)),
      shQuote(topology)
    ),
    paste0("serialization for ", level)
  )

  output <- file.path(map_dir, paste0(stem, ".topojson.gz"))
  partial <- paste0(output, ".part")
  unlink(partial)
  input_connection <- file(topology, open = "rb")
  output_connection <- gzfile(partial, open = "wb", compression = 9)
  repeat {
    bytes <- readBin(
      input_connection, what = "raw", n = 1024L * 1024L
    )
    if (!length(bytes)) break
    writeBin(bytes, output_connection)
  }
  close(input_connection)
  close(output_connection)

  candidate <- read_topojson_gz(partial)
  code_fields <- intersect(
    grep("_code$", names(object), value = TRUE),
    names(candidate)
  )
  if (length(code_fields)) {
    key <- code_fields[[1L]]
    candidate <- candidate[
      match(object[[key]], candidate[[key]]),
      ,
      drop = FALSE
    ]
  }
  original_area <- as.numeric(
    sf::st_area(sf::st_transform(object, 5880))
  )
  candidate_area <- as.numeric(
    sf::st_area(sf::st_transform(candidate, 5880))
  )
  feature_area_error <- abs(candidate_area - original_area) /
    pmax(abs(original_area), .Machine$double.eps)
  stopifnot(
    nrow(candidate) == expected_counts[[level]],
    setequal(names(candidate), names(object)),
    !any(sf::st_is_empty(candidate)),
    all(sf::st_is_valid(candidate)),
    max(feature_area_error, na.rm = TRUE) <= max_area_error
  )

  replace_file(partial, output)
  unlink(file.path(map_dir, paste0(stem, ".rds")))
  output
}

current_outputs <- character(length(current_maps))
names(current_outputs) <- names(current_maps)
for (level in names(current_maps)) {
  object <- current_maps[[level]]
  stopifnot(
    nrow(object) == expected_counts[[level]],
    !any(sf::st_is_empty(object)),
    all(sf::st_is_valid(object))
  )
  current_outputs[[level]] <- write_current_topojson(
    object, level
  )
}

dtb_columns <- c(
  "code", "name", "level", "abbreviation",
  "region_code", "region_name",
  "state_code", "state_name",
  "microregion_code", "microregion_name",
  "mesoregion_code", "mesoregion_name",
  "immediate_region_code", "immediate_region_name",
  "intermediate_region_code", "intermediate_region_name",
  "municipality_code", "municipality_name"
)

empty_dtb <- function(n) {
  result <- data.frame(
    code = rep(NA_real_, n),
    name = rep(NA_character_, n),
    level = rep(NA_character_, n),
    abbreviation = rep(NA_character_, n),
    region_code = rep(NA_integer_, n),
    region_name = rep(NA_character_, n),
    state_code = rep(NA_integer_, n),
    state_name = rep(NA_character_, n),
    microregion_code = rep(NA_integer_, n),
    microregion_name = rep(NA_character_, n),
    mesoregion_code = rep(NA_integer_, n),
    mesoregion_name = rep(NA_character_, n),
    immediate_region_code = rep(NA_integer_, n),
    immediate_region_name = rep(NA_character_, n),
    intermediate_region_code = rep(NA_integer_, n),
    intermediate_region_name = rep(NA_character_, n),
    municipality_code = rep(NA_integer_, n),
    municipality_name = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )
  result[dtb_columns]
}

make_dtb_level <- function(level, ancestors) {
  code <- paste0(level, "_code")
  label <- paste0(level, "_name")
  fields <- unique(c(code, label, ancestors))
  source <- hierarchy[!is.na(hierarchy[[code]]), fields, drop = FALSE]
  source <- source[!duplicated(source[[code]]), , drop = FALSE]
  result <- empty_dtb(nrow(source))
  result[["code"]] <- as.numeric(source[[code]])
  result[["name"]] <- source[[label]]
  result[["level"]] <- level
  if (identical(level, "state")) {
    state_match <- match(
      source[["state_code"]],
      hierarchy[["state_code"]]
    )
    result[["abbreviation"]] <-
      hierarchy[["state_abbreviation"]][state_match]
  }
  transferable <- intersect(names(source), names(result))
  for (field in transferable) {
    result[[field]] <- source[[field]]
  }
  result
}

municipality_dtb <- empty_dtb(nrow(hierarchy))
municipality_dtb[["code"]] <- hierarchy[["municipality_code"]]
municipality_dtb[["name"]] <- hierarchy[["municipality_name"]]
municipality_dtb[["level"]] <- "municipality"
for (field in intersect(names(hierarchy), names(municipality_dtb))) {
  municipality_dtb[[field]] <- hierarchy[[field]]
}

dtb <- rbind(
  municipality_dtb,
  make_dtb_level(
    "immediate_region",
    c(
      "immediate_region_code", "immediate_region_name",
      "intermediate_region_code", "intermediate_region_name",
      "state_code", "state_name", "region_code", "region_name"
    )
  ),
  make_dtb_level(
    "intermediate_region",
    c(
      "intermediate_region_code", "intermediate_region_name",
      "state_code", "state_name", "region_code", "region_name"
    )
  ),
  make_dtb_level(
    "microregion",
    c(
      "microregion_code", "microregion_name",
      "mesoregion_code", "mesoregion_name",
      "state_code", "state_name", "region_code", "region_name"
    )
  ),
  make_dtb_level(
    "mesoregion",
    c(
      "mesoregion_code", "mesoregion_name",
      "state_code", "state_name", "region_code", "region_name"
    )
  ),
  make_dtb_level(
    "state",
    c("state_code", "state_name", "region_code", "region_name")
  ),
  make_dtb_level(
    "region",
    c("region_code", "region_name")
  )
)
dtb <- dtb[order(dtb[["level"]], dtb[["code"]]), , drop = FALSE]
rownames(dtb) <- NULL
stopifnot(
  nrow(dtb) == 6941L,
  sum(dtb[["level"]] == "municipality") == 5571L,
  sum(dtb[["level"]] == "immediate_region") == 510L,
  sum(dtb[["level"]] == "intermediate_region") == 133L
)
saveRDS(
  dtb,
  file.path("inst", "dtb", "dtb.rds"),
  compress = "xz",
  version = 3
)

utils::write.csv(
  hierarchy,
  file.path("inst", "dtb", "municipality-hierarchy.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

message(
  "Built current maps and DTB: ",
  paste(
    paste(names(expected_counts), expected_counts, sep = "="),
    collapse = ", "
  ),
  "; DTB rows=", nrow(dtb), "."
)
