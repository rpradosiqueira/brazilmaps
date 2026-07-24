test_that("join_brmap preserves sf and data frame classes", {
  map <- get_brmap("state")
  values <- data.frame(state_code = 11L, value = 1)

  joined_sf <- join_brmap(map, values, by = "state_code")
  expect_s3_class(joined_sf, "sf")
  expect_true("value" %in% names(joined_sf))

  joined_df <- join_brmap(
    sf::st_drop_geometry(map),
    values,
    by = "state_code"
  )
  expect_s3_class(joined_df, "data.frame")
  expect_false(inherits(joined_df, "sf"))
})

test_that("plot_brmap returns an extensible ggplot", {
  map <- get_brmap("state")
  plot <- plot_brmap(map)
  expect_s3_class(plot, "ggplot")

  map$value <- seq_len(nrow(map))
  filled <- plot_brmap(map, fill_by = "value")
  expect_s3_class(filled, "ggplot")
  expect_error(plot_brmap(map, fill_by = "missing"), "not found")
})

test_that("deprecated helper names and arguments remain compatible", {
  map <- get_brmap("state")
  values <- data.frame(state_code = 11L, value = 1)

  expect_warning(
    joined <- join_data(map, values, by = "state_code"),
    "deprecated"
  )
  expect_s3_class(joined, "sf")

  expect_warning(old_theme <- theme_map(), "deprecated")
  expect_s3_class(old_theme, "theme")

  expect_warning(
    old_data <- plot_brmap(
      map,
      data_to_join = values,
      by = "state_code"
    ),
    "deprecated"
  )
  expect_s3_class(old_data, "ggplot")

  expect_warning(
    old_by <- plot_brmap(
      map,
      data = values,
      join_by = "state_code"
    ),
    "deprecated"
  )
  expect_s3_class(old_by, "ggplot")

  expect_warning(
    old_fill <- plot_brmap(map, var = "state_code"),
    "deprecated"
  )
  expect_s3_class(old_fill, "ggplot")
})

test_that("example datasets contain decoded UTF-8 text", {
  data("deaths")
  data("gini2015")
  data("pop2017")

  text <- c(deaths$micro, gini2015$uf, pop2017$nome_mun)
  expect_false(any(grepl("\\\\u[0-9A-Fa-f]{4}", text)))
  expect_false(any(grepl("\\\\'", text)))
  expect_true("Guajará-Mirim" %in% deaths$micro)
  expect_true("Rondônia" %in% gini2015$uf)
  expect_true("Alta Floresta D'Oeste" %in% pop2017$nome_mun)
})
