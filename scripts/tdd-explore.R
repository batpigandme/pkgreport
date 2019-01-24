library(tidyverse)
library(lubridate)
load_all(here::here())

#' @param org GitHub organization string
get_repo_ids <- function(org) {
  repos_json <- gh::gh("GET /orgs/:org/repos", org = org, .limit = Inf)
  tibble::tibble(
    owner = org,
    repo = repos_json %>% purrr::map_chr("name"),
    repo_id = repos_json %>% purrr::map_chr("id"),
    is_pkg = repo %>% stringr::str_detect("^[:alnum:]+$")
  )
}

# analysis stuff ----------------------------------------------------------
orgs <- c("tidyverse", "r-lib", "r-dbi", "tidymodels")

org_repo_ids <- orgs %>%
  map_df(get_repo_ids) %>%
  filter(is_pkg) %>%
  select(-is_pkg)

query <- "tidy-dev-day :nerd_face:"

get_tdd_labels <- function(repo_id) {
  gh::gh("GET /search/labels",
         repository_id = repo_id, q = "tidy-dev-day :nerd_face:",
         .send_headers = c(Accept = "application/vnd.github.symmetra-preview+json")
  )
}

get_tdd_issues <- function(repo_id) {
  gh::gh("GET /search/issues",
    repository_id = repo_id, q = "tidy-dev-day :nerd_face:", filter = "all",
    .send_headers = c(Accept = "application/vnd.github.symmetra-preview+json")
  )
}

get_tdd_issues("19438")

get_tdd_labels("19438")
