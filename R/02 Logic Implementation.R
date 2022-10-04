###################################################
## File Name: 02 Logic Implementation
## Date: 2nd October 2022
###################################################

playerNationality <- players_df$Nationality %>% table() %>% as.matrix()
playerNationality <- data.frame(Nation = rownames(playerNationality), Player = playerNationality) %>% as_tibble()

overallsAverages_df <- players_df %>%
  select(Nationality, Overall) %>%
  group_by(Nationality) %>%
  summarise("Overall Average" = mean(Overall)) %>%
  as_tibble() %>%
  arrange(desc(`Overall Average`))
