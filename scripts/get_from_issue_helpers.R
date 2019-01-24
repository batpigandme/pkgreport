source(here::here("scripts", "pluck_chr.R"))

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

