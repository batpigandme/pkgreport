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

sum_pkgs <- release_sum$pkg

repos <- repos %>% left_join(release_sum, by = c("repo" = "pkg"))

# released since Jan 1, 2019
first_releases_2019 <- repos %>%
  filter(release_1st <= (today() - as_date("2019-01-01")))

write_csv(first_releases_2019, here::here("data", "first_releases_2019.csv"))
# CRAN package history ----------------------------------------------------

pkg_history <- repos$repo %>% map_df(pkgsearch::cran_package_history)

pkg_history <- pkg_history %>%
  select(-dependencies) %>%
  distinct()

write_csv(pkg_history, here::here("data", "pkg_history.csv"))



# EDA ---------------------------------------------------------------------
pkg_history <- pkg_history %>%
  rename(date_publication = `Date/Publication`)

# only keep packages that are in release sum
pkg_history <- pkg_history %>%
  filter(Package %in% sum_pkgs)

pkg_history <- pkg_history %>%
  mutate(date_publication = lubridate::as_date(date_publication))

releases_2019 <- pkg_history %>%
  filter(date_publication >= as_date("2019-01-01")) %>%
  distinct() %>%
  janitor::remove_empty("cols") %>%
  janitor::clean_names()

write_csv(releases_2019, here::here("data", "releases_2019.csv"))

pkg_2019_release_count <- releases_2019 %>%
  count(package)

# plot releases per week --------------------------------------------------
releases_2019 %>%
  mutate(release_week = lubridate::ymd(cut.Date(date_publication, breaks = "1 week"))) %>%
  count(release_week) %>%
  ggplot(aes(x = release_week, y = n)) +
  geom_line() +
  labs(title = "tidyverse / r-lib team CRAN releases per week",
       x = "week",
       y = "number of releases")
