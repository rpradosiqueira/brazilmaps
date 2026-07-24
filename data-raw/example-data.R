# Repair escaped Unicode sequences in the three installed example datasets.
#
# Maintenance dependency:
# install.packages("stringi")

stopifnot(requireNamespace("stringi", quietly = TRUE))

decode_data_frame <- function(data) {
  for (field in names(data)) {
    if (is.character(data[[field]])) {
      data[[field]] <- stringi::stri_unescape_unicode(data[[field]])
    }
  }
  data
}

for (path in list.files("data", pattern = "\\.rda$", full.names = TRUE)) {
  environment <- new.env(parent = emptyenv())
  loaded <- load(path, envir = environment)
  stopifnot(length(loaded) == 1L)
  object <- decode_data_frame(get(loaded, envir = environment))
  assign(loaded, object, envir = environment)
  save(
    list = loaded,
    file = path,
    envir = environment,
    compress = "xz",
    version = 3
  )
}
