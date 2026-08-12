test_that("the selected municipal milestones are installed", {
  expected <- c(2000L, 2001L, 2007L, 2010L, 2023L, 2025L)
  inventory <- brmap_editions()

  expect_named(
    inventory,
    c(
      "year", "n_features", "file_bytes", "md5", "crs",
      "simplification", "source"
    )
  )
  expect_identical(inventory$year, expected)
  expect_identical(
    inventory$n_features,
    c(5508L, 5561L, 5564L, 5565L, 5570L, 5571L)
  )
  expect_true(all(inventory$n_features > 5000L))
  expect_true(all(inventory$file_bytes > 0))
  expect_true(all(nzchar(inventory$md5)))
  expect_true(all(nzchar(inventory$source)))
  expect_true(all(inventory$crs == "EPSG:4674"))
  expect_true(all(grepl("5%", inventory$simplification, fixed = TRUE)))

  paths <- system.file(
    "maps", "municipality",
    sprintf("City_%d.topojson.gz", expected),
    package = "brazilmaps"
  )
  expect_true(all(nzchar(paths)))
  expect_identical(
    unname(tools::md5sum(paths)),
    inventory$md5
  )

  meshes <- lapply(
    expected,
    function(edition) get_brmap("municipality", year = edition)
  )
  expect_identical(vapply(meshes, nrow, integer(1)), inventory$n_features)
  expect_true(all(vapply(meshes, function(mesh) {
    inherits(mesh, "sf") &&
      !anyDuplicated(mesh$municipality_code) &&
      identical(sf::st_crs(mesh), sf::st_crs(4674)) &&
      all(sf::st_is_valid(mesh))
  }, logical(1))))
})

test_that("the in-session cache does not change filtered results", {
  full_before <- get_brmap("state")
  selected <- get_brmap("state", filters = list(state = 50))
  full_after <- get_brmap("state")

  expect_equal(nrow(selected), 1L)
  expect_equal(selected$state_code, 50L)
  expect_identical(full_after, full_before)
})

test_that("current and historical meshes load locally", {
  current <- get_brmap("municipality")
  historical <- get_brmap("municipality", year = 2000)

  expect_s3_class(current, "sf")
  expect_equal(nrow(current), 5571L)
  expect_equal(nrow(historical), 5508L)
  expect_equal(sf::st_crs(current), sf::st_crs(4674))
  expect_equal(sf::st_crs(historical), sf::st_crs(4674))
  expect_equal(anyDuplicated(current$municipality_code), 0L)
  expect_equal(anyDuplicated(historical$municipality_code), 0L)
  expect_true(all(sf::st_is_valid(current)))
  expect_true(all(sf::st_is_valid(historical)))
  expect_true(5101837L %in% current$municipality_code)
  expect_false(5101837L %in% historical$municipality_code)
})

test_that("municipal filters are intersected and do not duplicate rows", {
  result <- get_brmap(
    "municipality",
    year = 2025,
    filters = list(region = 5, state = 50)
  )

  expect_gt(nrow(result), 0L)
  expect_true(all(result$region_code == 5L))
  expect_true(all(result$state_code == 50L))
  expect_equal(anyDuplicated(result$municipality_code), 0L)
})

test_that("invalid editions, levels and filters fail informatively", {
  expect_error(
    get_brmap("municipality", year = 2012),
    "unavailable"
  )
  expect_error(
    get_brmap("municipality", year = 2024),
    "unavailable"
  )
  expect_error(
    get_brmap("state", year = 2025),
    "only"
  )
  expect_error(
    get_brmap("municipality", filters = list(unknown = 1)),
    "Unknown filter"
  )
})

test_that("tabular output and legacy map calls remain compatible", {
  tabular <- get_brmap("state", output = "data.frame")
  expect_s3_class(tabular, "data.frame")
  expect_false(inherits(tabular, "sf"))
  expect_false("geometry" %in% names(tabular))
  expect_true("state_code" %in% names(tabular))

  expect_warning(
    old_map <- get_brmap(
      "State",
      filters = list(state = 50),
      output = "data.frame"
    ),
    "deprecated"
  )
  expect_true(all(old_map$State == 50L))
  expect_false("state_code" %in% names(old_map))

  expect_warning(
    old_name <- get_brmap(
      "Imediate",
      filters = list(state = 50)
    ),
    "deprecated"
  )
  expect_true(all(old_name$State == 50L))

  expect_warning(
    old_filter_argument <- get_brmap(
      "state",
      geo_filter = list(state = 50)
    ),
    "deprecated"
  )
  expect_true(all(old_filter_argument$state_code == 50L))

  expect_warning(
    old_filter_field <- get_brmap(
      "state",
      filters = list(State = 50)
    ),
    "deprecated"
  )
  expect_true(all(old_filter_field$state_code == 50L))

  expect_warning(
    old_output <- get_brmap("state", as = "data.frame"),
    "deprecated"
  )
  expect_s3_class(old_output, "data.frame")

  expect_warning(old_inventory <- brmap_years(), "deprecated")
  expect_identical(old_inventory$year, brmap_editions()$year)
})
