# Rate-limited smoke test for the two live IBGE source families.
#
# This script downloads the Localities API response once and sends one HEAD
# request to the newest municipal mesh archive. It must remain separate from
# regular package tests, which are fully offline.

smoke_test_sources <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package `jsonlite` is required for the source smoke test.",
      call. = FALSE
    )
  }
  options(timeout = max(60, getOption("timeout")))

  localities_url <- paste0(
    "https://servicodados.ibge.gov.br/api/v1/localidades/",
    "municipios?orderBy=id"
  )
  localities_file <- tempfile(fileext = ".json")
  on.exit(unlink(localities_file), add = TRUE)
  status <- utils::download.file(
    localities_url, localities_file, mode = "wb", quiet = TRUE
  )
  if (!identical(status, 0L) || file.info(localities_file)[["size"]] == 0) {
    stop("The IBGE Localities API download failed or was empty.",
      call. = FALSE
    )
  }

  municipalities <- jsonlite::fromJSON(
    localities_file, simplifyVector = FALSE
  )
  required_fields <- c("id", "nome", "regiao-imediata")
  missing_fields <- setdiff(
    required_fields,
    unique(unlist(lapply(municipalities, names), use.names = FALSE))
  )
  immediate_codes <- vapply(municipalities, function(municipality) {
    immediate <- municipality[["regiao-imediata"]]
    if (is.null(immediate[["id"]])) NA_integer_ else as.integer(immediate[["id"]])
  }, integer(1))
  intermediary_codes <- vapply(municipalities, function(municipality) {
    immediate <- municipality[["regiao-imediata"]]
    intermediary <- immediate[["regiao-intermediaria"]]
    if (is.null(intermediary[["id"]])) {
      NA_integer_
    } else {
      as.integer(intermediary[["id"]])
    }
  }, integer(1))
  fingerprint <- c(
    municipalities = length(municipalities),
    immediate_regions = length(unique(stats::na.omit(immediate_codes))),
    intermediate_regions = length(unique(stats::na.omit(intermediary_codes)))
  )
  expected <- c(
    municipalities = 5571L,
    immediate_regions = 510L,
    intermediate_regions = 133L
  )
  if (length(missing_fields) || !identical(fingerprint, expected)) {
    stop(
      "IBGE Localities schema/count fingerprint changed. Missing fields: ",
      if (length(missing_fields)) paste(missing_fields, collapse = ", ") else "none",
      "; observed counts: ",
      paste(names(fingerprint), fingerprint, sep = "=", collapse = ", "),
      ". Review the maintenance pipeline before refreshing bundled data.",
      call. = FALSE
    )
  }

  inventory <- utils::read.csv(
    file.path("inst", "maps", "municipality", "index.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  latest <- inventory[which.max(inventory[["year"]]), , drop = FALSE]
  source_urls <- strsplit(latest[["source"]], " | ", fixed = TRUE)[[1L]]
  if (length(source_urls) != 1L) {
    stop("The latest municipal edition must have exactly one source URL.",
      call. = FALSE
    )
  }

  curl <- Sys.which("curl")
  if (!nzchar(curl)) {
    stop("The `curl` executable is required for the archive HEAD request.",
      call. = FALSE
    )
  }
  headers <- system2(
    curl,
    c(
      "--fail", "--silent", "--show-error", "--location", "--head",
      "--connect-timeout", "15", "--max-time", "60", source_urls
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  command_status <- attr(headers, "status")
  if ((!is.null(command_status) && command_status != 0L) ||
      !any(grepl("^HTTP/[0-9.]+ (200|206)", headers))) {
    stop(
      "The latest municipal archive did not return HTTP 200/206: ",
      source_urls, "\n", paste(headers, collapse = "\n"),
      call. = FALSE
    )
  }

  summary <- c(
    "## brazilmaps source smoke test",
    "",
    paste0("- Localities fingerprint: ",
      paste(names(fingerprint), fingerprint, sep = "=", collapse = ", ")
    ),
    paste0("- Latest municipal archive: ", latest[["year"]], " (HEAD OK)"),
    paste0("- Source: `", source_urls, "`")
  )
  summary_path <- Sys.getenv("GITHUB_STEP_SUMMARY", unset = "")
  if (nzchar(summary_path)) {
    writeLines(summary, summary_path, useBytes = TRUE)
  }
  message(paste(summary[-c(1L, 2L)], collapse = "\n"))
}

smoke_test_sources()
