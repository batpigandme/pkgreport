library(tidyverse)
library(lubridate)
load_all(here::here())
library(usethis)
library(purrr)
library(glue)

# pluck_chr helper --------------------------------------------------------

## from: https://github.com/r-lib/usethis/blob/master/R/utils.R
## mimimalist, type-specific purrr::pluck()'s
pluck_chr <- function(l, what) vapply(l, `[[`, character(1), what)


# indent from usethis utils -----------------------------------------------

indent <- function(x, first = "  ", indent = first) {
  x <- gsub("\n", paste0("\n", indent), x)
  paste0(first, x)
}

# ui todo from usethis ----------------------------------------------------

#' @rdname ui
#' @export
ui_todo <- function(x, .envir = parent.frame()) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)
  cat_bullet(x, crayon::red(clisymbols::symbol$bullet))
}


# ui codeblock from usethis -----------------------------------------------

#' @param copy If `TRUE`, the session is interactive, and the clipr package
#'   is installed, will copy the code block to the clipboard.
#' @rdname ui
#' @export
ui_code_block <- function(x, copy = interactive(), .envir = parent.frame()) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)

  block <- indent(x, "  ")
  block <- crayon::make_style("darkgrey")(block)
  cat_line(block)

  if (copy && clipr::clipr_available()) {
    x <- crayon::strip_style(x)
    clipr::write_clip(x)
    cat_line("  [Copied to clipboard]")
  }
}

# Cat wrappers from usethis --------------------------------------------------

cat_bullet <- function(x, bullet) {
  bullet <- paste0(bullet, " ")
  x <- indent(x, bullet, "  ")
  cat_line(x)
}

# All UI output must eventually go through cat_line() so that it
# can be quieted with 'usethis.quiet' when needed.
cat_line <- function(..., quiet = getOption("usethis.quiet", default = FALSE)) {
  if (quiet)
    return(invisible())

  lines <- paste0(..., "\n")
  cat(lines, sep = "")
}

# get 2018 prs ------------------------------------------------------------
get_2018_pulls <- function(owner, repo,
                           from = "2018-01-01T00:00:00Z") {
  res <- gh::gh(
    "/repos/:owner/:repo/issues",
    owner = owner, repo = repo,
    since = as.POSIXct(from),
    state = "all",
    filter = "all",
    .limit = Inf
  )

  if (identical(res[[1]], "")) {
    ui_line("No matching issues/PRs found.")
    return(invisible())
  }

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

# get 2018 contributors ---------------------------------------------------

contrib_pr_2018 <- map2_df(repos$owner, repos$repo, get_2018_pulls)
write_csv(contrib_pr_2018, here::here("data", "contrib_pr_2018.csv"))

prs_2018 <- sort(unique(contrib_pr_2018$contributor))
contrib_link <- glue("[&#x0040;{prs_2018}](https://github.com/{prs_2018})")
ui_todo("{length(prs_2018)} contributors identified")
ui_code_block(glue_collapse(contrib_link, sep = ", ", last = ", and "))

# contributor eda ---------------------------------------------------------

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




res <- gh::gh(
  "/repos/tidyverse/ggplot2/issues",
  owner = "tidyverse", repo = "ggplot2",
  since = "2018-01-01T00:00:00Z",
  state = "all",
  filter = "all",
  .limit = Inf
)









dplyr_contribs_2018 <- get_2018_pulls(owner = "tidyverse", repo = "dplyr")
write_csv(count_commit_num, here::here("data", "count_commit_num.csv"))
