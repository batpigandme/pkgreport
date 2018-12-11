suppressPackageStartupMessages(library(tidyverse))
load(here::here("data", "overview.RData"))

repos <- repos %>%
  unique()

first_releases <- repos %>%
  filter(release_1st < 350) %>%
  arrange(release_1st)

links <- first_releases %>%
  mutate(repo_link = glue::glue("[{repo}](https://github.com/{owner}/{repo})")) %>%
  select(repo_link)

clipr::write_clip(glue::glue_collapse(links$repo_link, sep = ", ", last = ", and "))

major_releases <- repos %>%
  filter(release_maj < 350) %>%
  arrange(release_maj)

links <- major_releases %>%
  mutate(repo_link = glue::glue("[{repo}](https://github.com/{owner}/{repo})")) %>%
  select(repo_link)

clipr::write_clip(glue::glue_collapse(links$repo_link, sep = ", ", last = ", and "))

releases <- repos %>%
  filter(release < 350) %>%
  arrange(release)

links <- releases %>%
  mutate(repo_link = glue::glue("[{repo}](https://github.com/{owner}/{repo})")) %>%
  select(repo_link)

clipr::write_clip(glue::glue_collapse(links$repo_link, sep = ", ", last = ", and "))
