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


# source url exists function
source(here::here("scripts", "url_exists.R"))
