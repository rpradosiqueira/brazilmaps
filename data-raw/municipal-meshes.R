# Rebuild every selected municipal mesh shipped with brazilmaps.
#
# Runtime users never execute this script and never download map data.
# Maintainers run it from the package root when IBGE publishes a new edition.
#
# Maintenance dependencies:
# install.packages("sf")
# npm install --prefix data-raw

stopifnot(
  requireNamespace("sf", quietly = TRUE),
  requireNamespace("s2", quietly = TRUE)
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

ibge_base <- paste0(
  "https://geoftp.ibge.gov.br/organizacao_do_territorio/",
  "malhas_territoriais/malhas_municipais"
)
edition_selection <- utils::read.csv(
  file.path("data-raw", "municipal-edition-selection.csv"),
  colClasses = c(
    year = "integer",
    n_features = "integer",
    bundled = "logical",
    superseded_by = "integer"
  ),
  check.names = FALSE
)
official_municipal_years <- edition_selection[["year"]]
municipal_years <- edition_selection[["year"]][edition_selection[["bundled"]]]
expected_bundled <- c(
  edition_selection[["n_features"]][-nrow(edition_selection)] !=
    edition_selection[["n_features"]][-1L],
  TRUE
)
expected_superseded_by <- vapply(
  seq_len(nrow(edition_selection)),
  function(index) {
    if (edition_selection[["bundled"]][index]) {
      return(NA_integer_)
    }
    later <- seq.int(index + 1L, nrow(edition_selection))
    candidates <- later[
      edition_selection[["bundled"]][later] &
        edition_selection[["n_features"]][later] ==
          edition_selection[["n_features"]][index]
    ]
    if (length(candidates)) {
      edition_selection[["year"]][candidates[1L]]
    } else {
      NA_integer_
    }
  },
  integer(1)
)
if (
  anyDuplicated(official_municipal_years) ||
    is.unsorted(official_municipal_years, strictly = TRUE) ||
    !identical(edition_selection[["bundled"]], expected_bundled) ||
    !identical(
      edition_selection[["superseded_by"]],
      expected_superseded_by
    )
) {
  stop(
    "municipal-edition-selection.csv must retain the latest edition in ",
    "each consecutive run with the same municipality count.",
    call. = FALSE
  )
}
state_abbreviations <- tolower(c(
  "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
  "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN",
  "RO", "RR", "RS", "SC", "SE", "SP", "TO"
))
state_codes <- c(
  12L, 27L, 13L, 16L, 29L, 23L, 53L, 32L, 52L, 21L,
  31L, 50L, 51L, 15L, 25L, 26L, 22L, 41L, 33L, 24L,
  11L, 14L, 43L, 42L, 28L, 35L, 17L
)

municipal_urls <- function(year) {
  root <- sprintf("%s/municipio_%d", ibge_base, year)

  if (year %in% c(2000L, 2010L)) {
    return(sprintf(
      "%s/%s/%s_municipios.zip",
      root, state_abbreviations, state_abbreviations
    ))
  }
  if (year == 2001L) {
    return(sprintf(
      "%s/%s/%02dmu2500g.zip",
      root, state_abbreviations, state_codes
    ))
  }
  if (year %in% c(2013L, 2014L)) {
    return(sprintf(
      "%s/%s/%s_municipios.zip",
      root, toupper(state_abbreviations), state_abbreviations
    ))
  }

  suffix <- switch(
    as.character(year),
    `2005` = "escala_2500mil/proj_geografica/arcview_shp/brasil/55mu2500gc.zip",
    `2007` = "escala_2500mil/proj_geografica_sirgas2000/brasil/55mu2500gsr.zip",
    `2015` = "Brasil/BR/br_municipios.zip",
    `2016` = "Brasil/BR/br_municipios.zip",
    `2017` = "Brasil/BR/br_municipios.zip",
    `2018` = "Brasil/BR/br_municipios.zip",
    `2019` = "Brasil/BR/br_municipios_20200807.zip",
    `2020` = "Brasil/BR/BR_Municipios_2020.zip",
    `2021` = "Brasil/BR/BR_Municipios_2021.zip",
    `2022` = "Brasil/BR/BR_Municipios_2022.zip",
    `2023` = "Brasil/BR_Municipios_2023.zip",
    `2024` = "Brasil/BR_Municipios_2024.zip",
    `2025` = "Brasil/BR_Municipios_2025.zip",
    stop("No source is registered for ", year, call. = FALSE)
  )
  paste(root, suffix, sep = "/")
}

download_source <- function(url, year) {
  destination_dir <- file.path(maintenance_cache_dir, as.character(year))
  dir.create(destination_dir, recursive = TRUE, showWarnings = FALSE)
  destination <- file.path(destination_dir, basename(url))
  valid_zip <- function(path) {
    if (!file.exists(path) || file.info(path)[["size"]] <= 0) {
      return(FALSE)
    }
    contents <- try(utils::unzip(path, list = TRUE), silent = TRUE)
    !inherits(contents, "try-error") && nrow(contents) > 0L
  }
  zip_ok <- valid_zip(destination)
  if (!zip_ok) {
    message("Downloading ", url)
    partial <- paste0(destination, ".part")
    unlink(partial)
    on.exit(unlink(partial), add = TRUE)
    status <- utils::download.file(
      url, partial, mode = "wb", quiet = TRUE
    )
    if (!identical(status, 0L) || !valid_zip(partial)) {
      stop(
        "Download failed or was not a readable ZIP archive: ",
        url, call. = FALSE
      )
    }
    replace_file(partial, destination)
  }
  destination
}

read_zip_shape <- function(zip_path, year) {
  identifier <- tools::file_path_sans_ext(basename(zip_path))
  destination <- file.path(
    maintenance_work_dir, as.character(year), identifier
  )
  if (dir.exists(destination)) {
    unlink(destination, recursive = TRUE)
  }
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  extracted <- try(
    utils::unzip(zip_path, exdir = destination),
    silent = TRUE
  )
  if (inherits(extracted, "try-error")) {
    tar <- Sys.which("tar")
    if (!nzchar(tar)) {
      stop(
        "Could not extract ", zip_path,
        " with R and no `tar` executable was found.", call. = FALSE
      )
    }
    status <- system2(
      tar,
      c(
        "-xf",
        shQuote(normalizePath(zip_path, winslash = "/")),
        "-C",
        shQuote(normalizePath(destination, winslash = "/"))
      ),
      stdout = TRUE,
      stderr = TRUE
    )
    command_status <- attr(status, "status")
    if (!is.null(command_status) && command_status != 0L) {
      stop(
        "Could not extract ", zip_path, ":\n",
        paste(status, collapse = "\n"), call. = FALSE
      )
    }
  }
  shapes <- list.files(
    destination, pattern = "\\.shp$", full.names = TRUE,
    recursive = TRUE, ignore.case = TRUE
  )
  if (length(shapes) != 1L) {
    stop(
      "Expected exactly one shapefile in ", zip_path,
      "; found ", length(shapes), ".", call. = FALSE
    )
  }
  sf::st_read(shapes, quiet = TRUE, stringsAsFactors = FALSE)
}

detect_code_column <- function(data) {
  candidates <- c(
    "CD_MUN", "CD_GEOCMU", "CD_GEOCODI", "GEOCODIGO",
    "GEOCOD_MUN", "GEOCODIG_M", "CD_GEOCODM", "CD_GEOCOD"
  )
  exact <- names(data)[toupper(names(data)) %in% candidates]
  if (length(exact)) {
    return(exact[[1L]])
  }

  attributes <- sf::st_drop_geometry(data)
  scores <- vapply(attributes, function(column) {
    values <- gsub("\\.0$", "", trimws(as.character(column)))
    mean(grepl("^[0-9]{7}$", values) | is.na(values))
  }, numeric(1))
  if (!length(scores) || max(scores) < 0.9) {
    stop("Could not identify the municipality code column.", call. = FALSE)
  }
  names(which.max(scores))
}

detect_name_column <- function(data, code_column) {
  candidates <- c(
    "NM_MUN", "NM_MUNICIP", "NM_MUNIC", "NOME", "NOMEMUN",
    "NOME_MUNIC", "MUNICIPIO"
  )
  exact <- names(data)[toupper(names(data)) %in% candidates]
  exact <- setdiff(exact, code_column)
  if (length(exact)) {
    return(exact[[1L]])
  }

  attributes <- sf::st_drop_geometry(data)
  is_text <- vapply(attributes, is.character, logical(1))
  fields <- setdiff(names(attributes)[is_text], code_column)
  if (!length(fields)) {
    stop("Could not identify the municipality name column.", call. = FALSE)
  }
  scores <- vapply(attributes[fields], function(column) {
    length(unique(stats::na.omit(column))) / max(1L, length(column))
  }, numeric(1))
  names(which.max(scores))
}

standardize_municipal_mesh <- function(parts, year) {
  previous_s2 <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(previous_s2), add = TRUE)
  meshes <- lapply(parts, function(part) {
    code_column <- detect_code_column(part)
    name_column <- detect_name_column(part, code_column)
    code <- gsub("\\.0$", "", trimws(as.character(part[[code_column]])))
    result <- sf::st_sf(
      name = enc2utf8(as.character(part[[name_column]])),
      municipality_code = suppressWarnings(as.integer(code)),
      geometry = sf::st_geometry(part)
    )
    if (is.na(sf::st_crs(result))) {
      source_crs <- if (year <= 2005L) 4618 else 4674
      sf::st_crs(result) <- source_crs
    }
    result
  })
  mesh <- do.call(rbind, meshes)
  mesh <- mesh[!is.na(mesh[["municipality_code"]]), , drop = FALSE]
  mesh <- mesh[
    !mesh[["municipality_code"]] %in% c(4300001L, 4300002L),
    ,
    drop = FALSE
  ]

  if (anyDuplicated(mesh[["municipality_code"]])) {
    duplicate_codes <- unique(
      mesh[["municipality_code"]][duplicated(mesh[["municipality_code"]])]
    )
    message(
      "Combining multipart rows for ", length(duplicate_codes),
      " municipality codes in ", year, "."
    )
    single <- mesh[
      !mesh[["municipality_code"]] %in% duplicate_codes,
      ,
      drop = FALSE
    ]
    combined <- lapply(duplicate_codes, function(code) {
      rows <- mesh[
        mesh[["municipality_code"]] == code,
        ,
        drop = FALSE
      ]
      sf::st_sf(
        name = rows[["name"]][[1L]],
        municipality_code = code,
        geometry = suppressMessages(
          suppressWarnings(sf::st_union(sf::st_geometry(rows)))
        )
      )
    })
    mesh <- do.call(rbind, c(list(single), combined))
  }
  mesh[["state_code"]] <- as.integer(
    substr(sprintf("%07d", mesh[["municipality_code"]]), 1L, 2L)
  )
  mesh[["region_code"]] <- as.integer(
    substr(sprintf("%07d", mesh[["municipality_code"]]), 1L, 1L)
  )
  mesh[["year"]] <- as.integer(year)
  mesh <- mesh[c(
    "name", "municipality_code", "state_code", "region_code",
    "year", "geometry"
  )]
  mesh <- sf::st_transform(mesh, 4674)
  invalid <- !sf::st_is_valid(mesh)
  if (any(invalid)) {
    sf::st_geometry(mesh)[invalid] <- sf::st_make_valid(
      sf::st_geometry(mesh)[invalid]
    )
  }
  mesh[order(mesh[["municipality_code"]]), , drop = FALSE]
}

write_topojson <- function(mesh, year, keep = 0.05, quantization = 1e6) {
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

  workspace <- file.path(
    maintenance_work_dir, as.character(year), "mapshaper"
  )
  dir.create(workspace, recursive = TRUE, showWarnings = FALSE)
  input <- normalizePath(
    file.path(workspace, sprintf("City_%d.geojson", year)),
    winslash = "/", mustWork = FALSE
  )
  topology <- normalizePath(
    file.path(workspace, sprintf("City_%d.topojson", year)),
    winslash = "/", mustWork = FALSE
  )
  simplified_topology <- normalizePath(
    file.path(workspace, sprintf("City_%d_simplified.topojson", year)),
    winslash = "/", mustWork = FALSE
  )
  sf::st_write(
    mesh, input, driver = "GeoJSON", delete_dsn = TRUE,
    quiet = TRUE, layer_options = "RFC7946=NO"
  )
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

  build_candidate <- function(clean) {
    clean_argument <- if (clean) "-clean" else character()
    run_mapshaper(
      c(
        shQuote(input),
        clean_argument,
        "-simplify", paste0(format(100 * keep, scientific = FALSE), "%"),
        "keep-shapes",
        clean_argument,
        "-o", "format=topojson",
        paste0("quantization=", format(quantization, scientific = FALSE)),
        shQuote(simplified_topology)
      ),
      "simplification"
    )
    run_mapshaper(
      c(
        shQuote(simplified_topology),
        "-dissolve", "municipality_code",
        "copy-fields=name,state_code,region_code,year",
        clean_argument,
        "-o", "format=topojson",
        paste0("quantization=", format(quantization, scientific = FALSE)),
        shQuote(topology)
      ),
      "topology rebuild"
    )
  }

  candidate_is_valid <- function() {
    candidate <- try(
      sf::st_read(topology, quiet = TRUE, stringsAsFactors = FALSE),
      silent = TRUE
    )
    if (inherits(candidate, "try-error")) {
      return(FALSE)
    }
    sf::st_crs(candidate) <- 4674
    nrow(candidate) == nrow(mesh) &&
      !any(sf::st_is_empty(candidate)) &&
      all(sf::st_is_valid(candidate))
  }

  build_candidate(clean = TRUE)
  if (!candidate_is_valid()) {
    message(
      "Topology cleaning changed features or validity in ", year,
      "; rebuilding in feature-preserving mode."
    )
    build_candidate(clean = FALSE)
  }

  candidate <- sf::st_read(
    topology, quiet = TRUE, stringsAsFactors = FALSE
  )
  sf::st_crs(candidate) <- 4674
  invalid_spherical <- !sf::st_is_valid(candidate)
  if (any(invalid_spherical)) {
    message(
      "Rebuilding ", sum(invalid_spherical),
      " spherical geometry feature(s) in ", year, "."
    )
    unchecked <- s2::s2_geog_from_wkb(
      sf::st_as_binary(sf::st_geometry(candidate)[invalid_spherical]),
      check = FALSE
    )
    rebuilt <- s2::s2_rebuild(
      unchecked,
      s2::s2_options(
        snap = s2::s2_snap_precision(quantization),
        split_crossing_edges = TRUE,
        validate = FALSE
      )
    )
    sf::st_geometry(candidate)[invalid_spherical] <- sf::st_as_sfc(
      s2::s2_as_binary(rebuilt), crs = 4674
    )
    if (any(!sf::st_is_valid(candidate))) {
      stop(
        "Spherical geometry repair failed for ", year, ".",
        call. = FALSE
      )
    }

    repaired_input <- normalizePath(
      file.path(workspace, sprintf("City_%d_repaired.geojson", year)),
      winslash = "/", mustWork = FALSE
    )
    sf::st_write(
      candidate, repaired_input, driver = "GeoJSON",
      delete_dsn = TRUE, quiet = TRUE, layer_options = "RFC7946=NO"
    )
    run_mapshaper(
      c(
        shQuote(repaired_input),
        "-o", "format=topojson",
        paste0("quantization=", format(quantization, scientific = FALSE)),
        shQuote(topology)
      ),
      "spherical repair serialization"
    )
  }

  output_dir <- file.path("inst", "maps", "municipality")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output <- file.path(output_dir, sprintf("City_%d.topojson.gz", year))
  candidate_output <- paste0(output, ".part")
  unlink(candidate_output)
  input_connection <- file(topology, open = "rb")
  output_connection <- gzfile(
    candidate_output, open = "wb", compression = 9
  )
  repeat {
    bytes <- readBin(input_connection, what = "raw", n = 1024L * 1024L)
    if (!length(bytes)) {
      break
    }
    writeBin(bytes, output_connection)
  }
  close(input_connection)
  close(output_connection)
  candidate_output
}

validate_output <- function(path, year, expected_rows) {
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
  sf::st_crs(result) <- 4674
  errors <- character()
  if (nrow(result) != expected_rows) {
    errors <- c(errors, sprintf("expected %d rows, found %d", expected_rows, nrow(result)))
  }
  if (anyDuplicated(result[["municipality_code"]])) {
    errors <- c(errors, "duplicated municipality codes")
  }
  if (any(!sf::st_is_valid(result))) {
    errors <- c(errors, "invalid geometries")
  }
  if (!isTRUE(sf::st_crs(result) == sf::st_crs(4674))) {
    errors <- c(errors, "CRS is not EPSG:4674")
  }
  if (length(errors)) {
    stop(
      "Validation failed for ", year, ": ", paste(errors, collapse = "; "),
      call. = FALSE
    )
  }
  result
}

build_municipal_year <- function(year) {
  urls <- municipal_urls(year)
  archives <- vapply(
    urls, download_source, character(1), year = year,
    USE.NAMES = FALSE
  )
  parts <- lapply(archives, read_zip_shape, year = year)
  mesh <- standardize_municipal_mesh(parts, year)
  expected_features <- edition_selection[["n_features"]][
    match(year, edition_selection[["year"]])
  ]
  if (nrow(mesh) != expected_features) {
    stop(
      "Official feature count changed for ", year, ": expected ",
      expected_features, ", found ", nrow(mesh),
      ". Review municipal-edition-selection.csv.",
      call. = FALSE
    )
  }
  candidate_output <- write_topojson(mesh, year)
  on.exit(unlink(candidate_output), add = TRUE)
  installed <- validate_output(candidate_output, year, nrow(mesh))
  output <- sub("\\.part$", "", candidate_output)
  replace_file(candidate_output, output)

  data.frame(
    year = as.integer(year),
    n_features = nrow(installed),
    file_bytes = file.info(output)[["size"]],
    md5 = unname(tools::md5sum(output)),
    crs = "EPSG:4674",
    simplification = "weighted Visvalingam 5%; keep-shapes; dissolve; clean",
    source = paste(urls, collapse = " | "),
    stringsAsFactors = FALSE
  )
}

requested_years <- Sys.getenv("BRAZILMAPS_YEARS", unset = "")
years_to_build <- if (nzchar(requested_years)) {
  as.integer(strsplit(requested_years, ",", fixed = TRUE)[[1L]])
} else {
  municipal_years
}
if (anyNA(years_to_build) || any(!years_to_build %in% municipal_years)) {
  stop("BRAZILMAPS_YEARS contains an unsupported year.", call. = FALSE)
}

inventory <- do.call(rbind, lapply(years_to_build, build_municipal_year))
inventory <- inventory[order(inventory[["year"]]), , drop = FALSE]
index_path <- file.path("inst", "maps", "municipality", "index.csv")
installed_files <- file.path(
  "inst", "maps", "municipality",
  sprintf("City_%d.topojson.gz", municipal_years)
)
if (all(file.exists(installed_files))) {
  missing_inventory <- setdiff(municipal_years, inventory[["year"]])
  if (length(missing_inventory)) {
    recovered <- lapply(missing_inventory, function(year) {
      path <- file.path(
        "inst", "maps", "municipality",
        sprintf("City_%d.topojson.gz", year)
      )
      temporary <- tempfile(fileext = ".topojson")
      input <- gzfile(path, open = "rb")
      output <- file(temporary, open = "wb")
      repeat {
        bytes <- readBin(input, what = "raw", n = 1024L * 1024L)
        if (!length(bytes)) break
        writeBin(bytes, output)
      }
      close(input)
      close(output)
      object <- sf::st_read(
        temporary, quiet = TRUE, stringsAsFactors = FALSE
      )
      unlink(temporary)
      validate_output(path, year, nrow(object))
      data.frame(
        year = year,
        n_features = nrow(object),
        file_bytes = file.info(path)[["size"]],
        md5 = unname(tools::md5sum(path)),
        crs = "EPSG:4674",
        simplification = "weighted Visvalingam 5%; keep-shapes; dissolve; validated",
        source = paste(municipal_urls(year), collapse = " | "),
        stringsAsFactors = FALSE
      )
    })
    inventory <- rbind(inventory, do.call(rbind, recovered))
    inventory <- inventory[order(inventory[["year"]]), , drop = FALSE]
  }
  utils::write.csv(
    inventory,
    index_path,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
} else {
  message(
    "Partial build: index.csv was not replaced. Missing editions: ",
    paste(
      municipal_years[!file.exists(installed_files)],
      collapse = ", "
    )
  )
}

message(
  "Built ", nrow(inventory), " municipal editions (",
  format(sum(inventory[["file_bytes"]]), big.mark = ","), " bytes)."
)
