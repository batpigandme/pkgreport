# TODO: better
github_repos <- function(username) {
  repos_json <- gh::gh("GET /users/:username/repos", username = username, .limit = Inf)

  # TODO: need better strategy for determining if package on CRAN
  tibble::tibble(
    owner = username,
    repo = repos_json %>% purrr::map_chr("name"),
    is_pkg = repo %>% stringr::str_detect("^[:alnum:]+$"),
    watchers = repos_json %>% purrr::map_int("watchers_count"),
    # stargazers = repos_json %>% purrr::map_int("stargazers_count"),
    # forks = repos_json %>% purrr::map_int("forks_count"),
  )
}

github_repo <- function(name) {
  pieces <- str_split(name, "/")[[1]]

  repos_json <- gh::gh("GET /repos/:owner/:repo",
    owner = pieces[1],
    repo = pieces[2]
  )

  tibble::tibble(
    owner = repos_json %>% pluck("owner", "login"),
    repo = repos_json %>% pluck("name"),
    watchers = repos_json %>% pluck("watchers_count"),
    # stargazers = repos_json %>% purrr::map_int("stargazers_count"),
    # forks = repos_json %>% purrr::map_int("forks_count"),
  )
}


github_issues <- function(username, repo) {
  issues_json <- gh::gh(
    "GET /repos/:owner/:repo/issues",
    owner = username,
    repo = repo,
    .limit = Inf
  )

  if (identical(issues_json[[1]], ""))
    return(tibble::tibble())

  tibble::tibble(
    owner = username,
    repo = repo,
    number = issues_json %>% map_int("number"),
    title = issues_json %>% map_chr("title"),
    comments = issues_json %>% map_int("comments"),
    pr = issues_json %>% map_lgl(has_name, "pull_request"),
    labels = issues_json %>% purrr::map("labels") %>% purrr::map(. %>% purrr::map_chr("name")),
  )
}

# https://developer.github.com/v3/reactions/#list-reactions-for-an-issue
github_reactions <- function(owner, repo, number) {
  reactions_json <- gh::gh(
    "GET /repos/:owner/:repo/issues/:number/reactions",
    owner = owner,
    repo = repo,
    number = number,
    .send_headers = c(
      Accept = "application/vnd.github.squirrel-girl-preview+json")
    )
  if (identical(reactions_json[[1]], "")) {
    return(tibble())
  }

  reactions_json %>%
    map_chr("content") %>%
    tibble(reaction = .) %>%
    count(reaction) %>%
    add_column(owner = owner, repo = repo, number = number, .before = 0)
}

github_rate_limit <- function() {
  rate_limit_json <- gh::gh("GET /rate_limit")
  rate <- rate_limit_json$rate
  rate$reset <- .POSIXct(rate$reset, tz = "")
  rate
}

# github contributors -----------------------------------------------------
github_contributors <- function(username, repo) {
  contrib_json <- gh::gh(
    "GET /repos/:owner/:repo/contributors",
    owner = username,
    repo = repo,
    .limit = Inf
  )

  if (identical(contrib_json[[1]], ""))
    return(tibble::tibble())

  tibble::tibble(
    owner = username,
    repo = repo,
    contributor = contrib_json %>% map_chr("login"),
    commits = contrib_json %>% map_int("contributions")
  )

}

