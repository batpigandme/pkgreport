crandb_revdeps <- function(pkg) {
  url <- paste0("https://crandb.r-pkg.org/-/revdeps/", pkg)
  resp <- httr::GET(url)
  httr::stop_for_status(resp)

  json <- httr::content(resp, "parsed")
  if (length(json) == 0) {
    return(tibble())
  }

  json[[1]] %>%
    purrr::map_int(length) %>%
    tibble::enframe("type", "count") %>%
    tibble::add_column(pkg = pkg, .before = 0)
}

crandb_releases <- function(pkg) {
  url <- paste0("https://crandb.r-pkg.org/", pkg, "/all")
  resp <- httr::GET(url)

  if (httr::http_error(url)) {
    return(tibble::tibble())
  }

  json <- httr::content(resp, "parsed")
  versions <- unname(json$versions)

  tibble::tibble(
    pkg = pkg,
    version = versions %>% purrr::map_chr("Version"),
    major = major_version(version),
    date = versions %>%
      purrr::map_chr("crandb_file_date") %>%
      stringr::str_replace(" UTC|\n", "") %>%
      readr::parse_datetime() %>%
      lubridate::as_date()
  )
}


major_version <- function(version) {
  pieces <- stringr::str_split_fixed(version, stringr::fixed("."), n = 4)
  pieces[, 3] %in% c("", "0")
}
