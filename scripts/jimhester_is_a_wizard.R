suppressPackageStartupMessages(library(tidyverse))
issues <- gh::gh("/orgs/tidyverse/issues", labels = "tidy-dev-day :nerd_face:", filter = "all")

events <- gh::gh("/repos/:owner/:repo/issues/:number/timeline", owner = .x$repository$owner$login, repo = .x$repository$name, number = .x$number, .send_headers = c(Accept = "application/vnd.github.mockingbird-preview")) %>%
  keep(~ .x$event == "cross-referenced")

# testing with https://github.com/tidyverse/forcats/issues/142, which I know has a PR
referenced_numbers <- gh::gh("/repos/:owner/:repo/issues/:number/timeline", owner = "tidyverse", repo = "forcats", number = 142, .send_headers = c(Accept = "application/vnd.github.mockingbird-preview")) %>%
  keep(~ .x$event == "cross-referenced" && !is.null(.x$source$issue$pull_request)) %>% map_chr(c("source", "issue", "number"))

# get user from issue -----------------------------------------------------

get_user_from_issue <- function(issues_json) {
  pluck_chr(issues_json, c("user", "login"))
}

# get owner from issue ----------------------------------------------------

get_owner_from_issue <- function(issues_json) {
  pluck_chr(issues_json, c("repository", "owner", "login"))
}

# get repo from issue -----------------------------------------------------

get_repo_from_issue <- function(issues_json) {
  pluck_chr(issues_json, c("repository", "name"))
}

# get number from issue ---------------------------------------------------

get_number_from_issue <- function(issues_json) {
  pluck_num(issues_json, c("number"))
}


# issues_df ---------------------------------------------------------------
issues_df <- issues %>%
  tibble::tibble(
    owner = get_owner_from_issue(issues),
    repo = get_repo_from_issue(issues),
    issue_num = get_number_from_issue(issues)
    )







# events function ---------------------------------------------------------

gh_events <- function(.x) {
  events_json <- gh::gh("/repos/:owner/:repo/issues/:number/timeline", owner = .x$repository$owner$login, repo = .x$repository$name, number = .x$number, .send_headers = c(Accept = "application/vnd.github.mockingbird-preview")) %>%
    keep(~ .x$event == "cross-referenced")
}
gh_events2 <- function(.x) {
  gh::gh("/repos/:owner/:repo/issues/:number/timeline", owner = owner, repo = repo, number = issue_num, .send_headers = c(Accept = "application/vnd.github.mockingbird-preview")) %>%
    keep(~ .x$event == "cross-referenced")
}

gh_events(issues)

map(issues, gh::gh("/repos/:owner/:repo/issues/:number/timeline", owner = .x$repository$owner$login, repo = .x$repository$name, number = .x$number, .send_headers = c(Accept = "application/vnd.github.mockingbird-preview")) %>%
        keep(~ .x$event == "cross-referenced"))


# messy for loop fubar ----------------------------------------------------

for(i in 1:length(issues)) {print(issues[[i]][["user"]][["login"]])}



