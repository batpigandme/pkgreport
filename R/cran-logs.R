cranlogs_month <- function(pkg) {
  df <- cranlogs::cran_downloads(pkg, from = lubridate::today() - lubridate::weeks(4), to = lubridate::today())
  tibble(pkg = pkg, downloads = sum(df$count))
}
