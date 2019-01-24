## from: https://github.com/r-lib/usethis/blob/master/R/utils.R
## mimimalist, type-specific purrr::pluck()'s
pluck_chr <- function(l, what) vapply(l, `[[`, character(1), what)

## from: https://github.com/r-lib/usethis/blob/master/R/utils.R
## mimimalist, type-specific purrr::pluck()'s
pluck_num <- function(l, what) vapply(l, `[[`, numeric(1), what)
