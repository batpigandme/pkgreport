# assumes you recently ran overview.R
suppressPackageStartupMessages(library(tidyverse))
load(here::here("data", "overview.RData"))

`%nin%` <- Negate(`%in%`)

# get just the pacakges with CRAN releases
cran_pkgs <- repos %>%
  unique() %>%
  filter(!is.na(release_1st)) %>%
  filter(repo %nin% c("io", "err", "lookup")) # remove packages that are diff on CRAN

# get package URLs from CRAN metadata
pkgs_urls <- cran_pkgs %>%
  mutate(cran_urls = map_chr(repo, pkg_url))

# since we manually edited this, now we're reading in a new google sheet
library(googlesheets4)
pkg_inventory <- googledrive::drive_get("os_package_inventory") %>%
  read_sheet(sheet = 1)

# best guesses at news URLs based on primary url
url_type_repo <- pkg_inventory %>%
  filter(stringr::str_starts(primary_url, "https://github.com/")) %>%
  mutate(news_url = glue::glue("{primary_url}/blob/master/NEWS.md"))

url_type_pkgdown <- pkg_inventory %>%
  filter(stringr::str_starts(primary_url, "https://github.com/", negate = TRUE)) %>%
  mutate(news_url = glue::glue("{primary_url}/news/index.html"))



# check urls --------------------------------------------------------------
# source url exists function
source(here::here("scripts", "url_exists.R"))

pkgdown_news_exists <- url_type_pkgdown %>%
  mutate(news_works = map_lgl(news_url, url_exists))

repo_news_exists <- url_type_repo %>%
  mutate(news_works = map_lgl(news_url, url_exists))

pkgs_w_news_exists <- bind_rows(pkgdown_news_exists, repo_news_exists)

# gs4_create("pkgs_w_news_urls", sheets = list(pkgs_w_news_exists))
# read the above back in now that I've fixed the bad URLS
pkgs_w_news_urls <- googledrive::drive_get("pkgs_w_news_urls") %>%
  read_sheet(sheet = 1)

valid_news_urls <- pkgs_w_news_urls %>%
  select(c("repo", "valid_news_url"))

pkgs_w_news_links <- pkgs_w_news_exists %>%
  left_join(valid_news_urls) %>%
  select(c("owner", "repo", "cran_urls", "primary_url", "valid_news_url")) %>%
  rename("news_url" = "valid_news_url")

write_csv(pkgs_w_news_links, here::here("data", "pkgs_w_news_links.csv"))
# gs4_create("pkgs_w_news_links", sheets = list(pkgs_w_news_links))
