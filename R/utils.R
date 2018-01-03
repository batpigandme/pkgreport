
view <- function(x) {
  if (interactive()) {
    View(x)
  }
  invisible(x)
}

data_path <- function(...) {
  here::here("data", ...)
}
