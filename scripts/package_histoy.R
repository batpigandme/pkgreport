library(tidyverse)
library(lubridate)
devtools::load_all(here::here())
library(pkgsearch)

# TODO: Include in test coverage
# TODO: add CRAN package status

orgs <- c("tidyverse", "r-lib", "r-dbi", "tidymodels")
org_repos <- orgs %>%
  map_df(github_repos) %>%
  filter(is_pkg) %>%
  select(-is_pkg)

extra <- read_lines(data_path("repos.txt")) %>% .[. != ""]
extra_repos <- extra %>% map_df(github_repo)

repos <- bind_rows(org_repos, extra_repos)

# Downloads and revdeps ------------------------------------------------------

cranlogs <- repos$repo %>% map_df(cranlogs_month)
repos <- repos %>% left_join(cranlogs, by = c("repo" = "pkg"))

# CRAN revdeps
revdeps <- repos$repo %>% map_df(crandb_revdeps)
revdeps_sum <- revdeps %>%
  group_by(pkg) %>%
  summarise(
    revdep = sum(count),
    revdep_hard = sum(count[type %in% c("Imports", "Depends")])
  )

repos <- repos %>% left_join(revdeps_sum, by = c("repo" = "pkg"))

# Release ages -------------------------------------------------------------

releases <- repos$repo %>% map_df(crandb_releases)

release_sum <- releases %>%
  mutate(age = round((date %--% today()) / days(1))) %>%
  group_by(pkg) %>%
  summarise(
    release = last(age),
    release_maj = age[last(which(major))],
    release_1st = first(age)
  )

repos <- repos %>% left_join(release_sum, by = c("repo" = "pkg"))

# CRAN package history ----------------------------------------------------

pkg_history <- repos$repo %>% map_df(pkgsearch::cran_package_history)

pkg_history <- pkg_history %>%
  select(-dependencies)

write_csv(pkg_history, here::here("data", "pkg_history.csv"))



# EDA ---------------------------------------------------------------------
pkg_history <- pkg_history %>%
  rename(date_publication = `Date/Publication`)


pkg_history <- pkg_history %>%
  filter(Package != "Design")

pkg_history <- pkg_history %>%
  mutate(date_publication = lubridate::as_date(date_publication))

releases_2019 <- pkg_history %>%
  filter(date_publication >= as_date("2019-01-01")) %>%
  distinct()

write_csv(releases_2019, here::here("data", "releases_2019.csv"))

pkg_2019_release_count <- releases_2019 %>%
  count(Package)
