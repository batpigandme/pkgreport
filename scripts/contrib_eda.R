library(tidyverse)
library(lubridate)
load_all(here::here())
library(usethis)
library(purrr)


# pluck_chr helper --------------------------------------------------------

## from: https://github.com/r-lib/usethis/blob/master/R/utils.R
## mimimalist, type-specific purrr::pluck()'s
pluck_chr <- function(l, what) vapply(l, `[[`, character(1), what)

# get 2018 prs ------------------------------------------------------------
get_2018_pulls <- function(owner, repo,
                           from = "2018-01-01T00:00:00Z") {
  res <- gh::gh(
    glue::glue("/repos/{owner}/{repo}/issues"),
    owner = owner, repo = repo,
    since = as.POSIXct(from),
    state = "all",
    filter = "all",
    .limit = Inf
  )


  ## from: https://github.com/r-lib/usethis/blob/master/R/tidyverse.R#L333
  creation_time <- function(x) {
    as.POSIXct(pluck_chr(x, "created_at"))
  }

  ## extract html url from result
  html_url <- function(x) {
    pluck_chr(x, "html_url")
  }

  ## filters by whether or not is pull
  res <- res[grepl('pull', html_url(res))]

  tibble::tibble(
    owner = owner,
    repo = repo,
    contributor = unique(pluck_chr(res, c("user", "login")))
  )
}


# begin eda ---------------------------------------------------------------
orgs <- c("tidyverse", "r-lib", "r-dbi", "tidymodels")
org_repos <- orgs %>%
  map_df(github_repos) %>%
  filter(is_pkg) %>%
  select(-is_pkg)

repos <- org_repos

contributors <- map2_df(repos$owner, repos$repo, github_contributors)

contrib_pr_2018 <- map2_df(repos$owner, repos$repo, get_2018_pulls)

only_contribs <- contributors %>%
  select(contributor)

unique_contribs <- only_contribs %>%
  distinct()

contrib_json <- gh::gh(
  "GET /repos/tidyverse/ggplot2/contributors",
  owner = "tidyverse",
  repo = "ggplot2",
  .limit = Inf
)

thx <- use_tidy_thanks("tidyverse/ggplot2", from = "2018-01-01")


contrib_commits <- contributors %>%
  select(one_of(c("contributor", "commits")))

tot_contrib_commits <- contrib_commits %>%
  group_by(contributor) %>%
  summarise(tot_commits = sum(commits)) %>%
  arrange(desc(tot_commits))

count_commit_num <- tot_contrib_commits %>%
  select(tot_commits) %>%
  count(tot_commits)

ggplot(count_commit_num, aes(x = tot_commits, y = n)) +
  geom_line(alpha = 0.4) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 10, linetype = "dotted", colour = "blue") +
  geom_vline(xintercept = 23479, linetype = "dotted", colour = "blue") +
  scale_x_log10() +
  hrbrthemes::theme_ipsum_rc() +
  labs(title = "Count of contributors by number of commits",
       y = "count",
       x = "commits",
       caption = "source: GitHub 2018-12-1")

ggplot(tot_contrib_commits, aes(tot_commits)) +
  geom_freqpoly() +
  scale_x_log10()

write_csv(contributors, here::here("data", "contribs_by_repo.csv"))
write_csv(tot_contrib_commits, here::here("data", "tot_contrib_commits.csv"))
write_csv(count_commit_num, here::here("data", "count_commit_num.csv"))




res <- gh::gh(
  "/repos/tidyverse/ggplot2/issues",
  owner = "tidyverse", repo = "ggplot2",
  since = "2018-01-01T00:00:00Z",
  state = "all",
  filter = "all",
  .limit = Inf
)









dplyr_contribs_2018 <- get_2018_pulls(owner = "tidyverse", repo = "dplyr")

