# get package URL(s) from pkgsearch::cran_package()
# https://r-hub.github.io/pkgsearch/reference/cran_package.html
pkg_url <- function(pkg) {
  resp <- pkgsearch::cran_package(pkg)

  urls <- purrr::pluck(resp, "URL")

  if (is.null(urls)) {
    return(NA)
  }
  else {
    return(urls)
  }
}
