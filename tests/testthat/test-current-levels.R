test_that("current territorial levels have official counts", {
  expected <- c(
    country = 1L,
    region = 5L,
    state = 27L,
    immediate_region = 510L,
    intermediate_region = 133L
  )

  for (level in names(expected)) {
    object <- get_brmap(level)
    code_fields <- grep("_code$", names(object), value = TRUE)
    expect_equal(nrow(object), expected[[level]], info = level)
    expect_equal(sf::st_crs(object), sf::st_crs(4674), info = level)
    expect_true(all(sf::st_is_valid(object)), info = level)
    expect_true(
      all(vapply(
        code_fields,
        function(field) is.integer(object[[field]]),
        logical(1)
      )),
      info = level
    )
  }
})

test_that("current maps use compact local TopoJSON files", {
  stems <- c(
    "Brazil", "Region", "State", "StateHex", "StateReg",
    "Intermediary", "Imediate", "MesoRegion", "MicroRegion"
  )
  topology_paths <- system.file(
    "maps", paste0(stems, ".topojson.gz"),
    package = "brazilmaps"
  )
  legacy_paths <- system.file(
    "maps", paste0(stems, ".rds"),
    package = "brazilmaps"
  )

  expect_true(all(nzchar(topology_paths)))
  expect_false(any(nzchar(legacy_paths)))
  expect_lt(sum(file.info(topology_paths)$size), 3 * 1024^2)
})

test_that("maps expose explicit lower-snake-case columns", {
  expect_named(
    get_brmap("state"),
    c("state_code", "name", "state_abbreviation", "region_code", "geometry")
  )
  expect_named(
    get_brmap("immediate_region"),
    c(
      "immediate_region_code", "name", "intermediate_region_code",
      "state_code", "region_code", "geometry"
    )
  )
  expect_named(
    get_brmap("country"),
    c("name", "country_code", "geometry")
  )
})

test_that("DTB includes the corrected current hierarchy", {
  dtb <- get_dtb()
  expect_equal(nrow(dtb), 6941L)
  expect_equal(sum(dtb$level == "municipality"), 5571L)
  expect_equal(sum(dtb$level == "immediate_region"), 510L)
  expect_equal(sum(dtb$level == "intermediate_region"), 133L)

  boa_esperanca <- get_dtb(code = 5101837)
  expect_equal(boa_esperanca$name, "Boa Esperança do Norte")
  expect_equal(boa_esperanca$immediate_region_name, "Sorriso")

  escada <- get_dtb(code = 2605202)
  expect_match(escada$immediate_region_name, "Escada")
  expect_match(escada$immediate_region_name, "Ribeirão")

  campo_grande <- get_dtb(name = "  campo grande ")
  expect_true(5002704 %in% campo_grande$code)
})

test_that("DTB level relationships are type-stable", {
  result <- get_dtb_levels(
    c("municipality", "state", "region"),
    filters = list(state = 50)
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(result$state_code == 50L))
  expect_true(all(result$region_code == 5L))
  expect_type(result$municipality_code, "integer")
  expect_type(result$municipality_name, "character")
})

test_that("legacy DTB functions retain their previous schema", {
  expect_warning(
    legacy_dtb <- get_dtb_info(cod = 5101837),
    "deprecated"
  )
  expect_equal(legacy_dtb$level, "City")
  expect_true("Immediate_name" %in% names(legacy_dtb))

  expect_warning(
    legacy_levels <- get_dtb_lvl(
      c("City", "State"),
      geo_filter = list(State = 50)
    ),
    "deprecated"
  )
  expect_named(
    legacy_levels,
    c("City", "City_name", "State", "State_name")
  )
})
