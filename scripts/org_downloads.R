library(tidyverse)

get_monthly_dls <- function(pkg) {
  url <- paste0("http://cranlogs.r-pkg.org/downloads/total/last-month/", pkg)
  resp <- httr::GET(url)
  json <- httr::content(resp, "parsed")
  unjson <- json[[1]]
  reunjson <- unlist(unjson)
  reunjson %>% pluck("downloads")
}

dl_df <- tibble("package" = org_repos$repo)

org_downloads <- dl_df %>%
  mutate(downloads = map_chr(.x = repo, get_monthly_dls))

org_downloads <- org_downloads %>%
  mutate(monthly_dls = as.integer(downloads)) %>%
  arrange(desc(monthly_dls))

write_csv(org_downloads, here::here("data", "org_downloads.csv"))
