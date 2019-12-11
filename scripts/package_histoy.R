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

# add major/minor version info
major_minor_releases_2019 <- releases_2019 %>%
  separate(version, c("major", "minor", "patch", "sub-patch"), "\\.", fill = "right") %>%
  replace_na(list(patch = 0)) %>%
  mutate(release = case_when(
    minor == 0 & patch == 0 ~ "major",
    patch == 0 ~ "minor",
    TRUE ~ "patch"
  )) %>%
  mutate(release_month = month(date_publication, label = TRUE, abbr = TRUE))

pkg_2019_release_count <- releases_2019 %>%
  count(package)


# release week df ---------------------------------------------------------

# make tibble of all weeks in 2019
weeks_2019 <- tibble::tibble(week_num = 1:52) %>%
  mutate(week_date = (lubridate::ymd("2018-12-31") + ((week_num - 1) * 7)))

# break 2019 releases into releases per week
releases_per_week <- releases_2019 %>%
  mutate(release_week = lubridate::ymd(cut.Date(date_publication, breaks = "1 week"))) %>%
  count(release_week)

releases_weeks_2019 <- weeks_2019 %>%
  left_join(releases_per_week, by = c("week_date" = "release_week")) %>%
  mutate(n = replace_na(n, 0))

# plot releases per week
releases_weeks_2019 %>%
  filter(week_date <= Sys.Date()) %>%
  ggplot(aes(x = week_date, y = n)) +
  geom_line() +
  labs(title = "tidyverse / r-lib team CRAN releases per week",
       x = "week",
       y = "releases") +
  hrbrthemes::theme_ipsum_rc() +
  theme(axis.title.y = element_text(angle = 0)) # unrotate y-axis lab

# monthly releases --------------------------------------------------------
releases_2019 %>%
  mutate(release_month = lubridate::ymd(cut.Date(date_publication, breaks = "1 month"))) %>%
  count(release_month) %>%
  ggplot(aes(x = release_month, y = n)) +
  geom_line() +
  labs(title = "tidyverse / r-lib team monthly CRAN releases",
       x = "month",
       y = "releases") +
  hrbrthemes::theme_ipsum_rc() +
  theme(axis.title.y = element_text(angle = 0))


monthly_release_type <- major_minor_releases_2019  %>%
  group_by(release) %>%
  count(release_month)


# package releases by month and type --------------------------------------
monthly_release_type %>%
  group_by(release_month) %>%
  ggplot(aes(x = release_month, y = n, fill = release)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "tidyverse / r-lib package releases by month and type",
       x = "month 2019",
       y = "count",
       fill = "release type") +
  hrbrthemes::theme_ipsum_rc() +
  theme(axis.title.y = element_text(angle = 0))

# total releases for each type
monthly_release_type %>%
  group_by(release) %>%
  summarise(total = sum(n))

