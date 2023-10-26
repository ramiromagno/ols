flatten <- function(x, element = NULL, embedded = TRUE) {
  if (embedded) {
    x |>
      purrr::list_flatten() |>
      purrr::map(list("_embedded", element)) |>
      purrr::list_flatten()
  } else {
    purrr::list_flatten(x)
  }
}
