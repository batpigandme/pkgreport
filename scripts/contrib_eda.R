
contributors <- map2_df(repos$owner, repos$repo, github_contributors)

only_contribs <- contributors %>%
  select(contributor)

unique_contribs <- only_contribs %>%
  distinct()

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

write_csv(tot_contrib_commits, here::here("data", "tot_contrib_commits.csv"))
write_csv(count_commit_num, here::here("data", "count_commit_num.csv"))
