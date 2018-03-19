library(tidyverse)
library(lubridate)
load_all(here::here())

# TODO: Include in test coverage
# TODO: add CRAN package status

orgs <- c("tidyverse", "r-lib", "r-dbi")
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

# Issues ------------------------------------------------------------------

issues <- map2_df(repos$owner, repos$repo, github_issues)

issues <- issues %>% mutate(
  bug = labels %>% map_lgl(~ any("bug" %in% .)),
  feature = labels %>% map_lgl(~ any("feature" %in% .)),
  unlabelled = !pr & labels %>% map_lgl(is_empty),
)
issues %>% arrange(desc(comments))

# Determine which issues are unlabelled (i.e. untriaged)
# Number of bugs

reactions <- pmap_df(issues[c("owner", "repo", "number")], github_reactions)
reactions_sum <- reactions %>%
  group_by(owner, repo, number) %>%
  summarise(
    reaction_p1 = sum(n[reaction == "+1"]),
    reaction_other = sum(n[reaction != "+1"]),
  )



issues <- issues %>%
  left_join(reactions_sum, by = c("owner", "repo", "number")) %>%
  replace_na(list(reaction_p1 = 0L, reaction_other = 0L))

issues <- issues %>%
  arrange(desc(reaction_p1))

issues_sum <- issues %>%
  group_by(owner, repo) %>%
  summarise(
    issues = sum(!pr),
    bugs = sum(bug),
    features = sum(feature),
    unlabelled = sum(unlabelled),
    prs = sum(pr),
    reaction_p1 = sum(reaction_p1),
    reaction_other = sum(reaction_other)
  )

repos <- repos %>% left_join(issues_sum, by = c("owner", "repo"))

# Arrange repos by watchers ----------------------------------------------
repos <- repos %>%
  arrange(desc(watchers))

# Create issues df without labels column for csv ------------------------------
# Issues less labs

issues_no_labs <- issues %>%
  select(-labels) %>%
  arrange(desc(reaction_p1, comments, reaction_other))

# Share on googlesheets ---------------------------------------------------

write_csv(repos, data_path("overview.csv"), na = "")
# write_csv(issues, data_path("issues.csv"), na = "")
write_csv(issues_no_labs, data_path("issues.csv"), na = "")

save(repos, file = data_path("overview.RData"))
save(issues, file = data_path("issues.RData"))
