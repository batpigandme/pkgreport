releases <- releases %>%
  separate(version, c("major", "minor", "patch"), "\\.", fill = "right") %>%
  replace_na(list(patch = 0)) %>%
  mutate(release = case_when(
    minor == 0 & patch == 0 ~ "major",
    patch == 0 ~ "minor",
    TRUE ~ "patch"
  ))
