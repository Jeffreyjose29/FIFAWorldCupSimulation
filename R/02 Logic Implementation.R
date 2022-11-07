###################################################
## File Name: 02 Logic Implementation
## Date: 2nd October 2022
###################################################


# KEY VARIABLES
SIMULATION.NUMBER <- 500
PLAYER.WEIGHT <- 0.90 # % of score based on players
H2H.WEIGHT <- 0.10 # % of score based on previous head-to-head
STD <- 10 # "randomness" sampled from a normal distribution with standard deviation 
RANDAM.WEIGHT <- 0.50


playerNationality <- players_df$Nationality %>% table() %>% as.matrix()
playerNationality <- data.frame(Nation = rownames(playerNationality), Player = playerNationality) %>% as_tibble()

overallsAverages_df <- players_df %>%
  select(Nationality, Overall) %>%
  group_by(Nationality) %>%
  summarise("Overall Average" = mean(Overall)) %>%
  as_tibble() %>%
  arrange(desc(`Overall Average`))

# Joining the federation to the list of countries
colnames(federationsCountryCombo)[2] <- "Nationality"
overallsAverages_df <- merge(overallsAverages_df, federationsCountryCombo, by = "Nationality", all.x = TRUE, all.y = FALSE)

overallsAverages_df <- overallsAverages_df %>%
  select(Federation, Nationality, `Overall Average`)


winners <- data.frame(Country = c(), Averages = c(), Organization = c())
intFederations <- winners
countries.df <- data.frame(Organization = c('CONMEBOL', 
                                            'AFC', 
                                            'CONCACAF', 
                                            'OFC', 
                                            'CAF', 
                                            'UEFA'),
                           Spots = c(4, 5, 3, 0, 5, 13))

for(i in 1:length(countries.df$Organization)){
  sub <- overallsAverages_df %>% filter(Federation == countries.df$Organization[i])
  winners <- rbind(winners, head(sub, countries.df$Spots[i]))
  if(!(countries.df$Organization[i] %in% c('CAF', 'UEFA'))){
    intFederations <- rbind(intFederations, sub[countries.df$Spots[i] + 1, ])
  }
}
intFederations <- intFederations %>% arrange(desc(`Overall Average`))


final.teams <- winners %>% rbind(intFederations[1 : 2, ]) %>% rbind(c('Qatar', 0, 'AFC'))
final.teams$`Overall Average` <- final.teams$`Overall Average` %>% as.numeric()

final.teams <- na.omit(final.teams) 



matrix_df <- tibble::rownames_to_column(matrix_df, "Country")

match_up <- function(c1, c2){
  c1.average <- final.teams %>% filter(Nationality == c1) %>% select('Overall Average') %>% as.numeric()
  c2.average <- final.teams %>% filter(Nationality == c2) %>% select('Overall Average') %>% as.numeric()
  
  c1.h2h <- matrix_df %>% filter(Country == c1) %>% select(c2) %>% as.numeric()
  c2.h2h <- matrix_df %>% filter(Country == c2) %>% select(c1) %>% as.numeric()
  
  if(is.na(c1.h2h)) {
    c1.score <- PLAYER.WEIGHT * c1.average + (1 - PLAYER.WEIGHT) * rnorm(1, 0, STD)
    c2.score <- PLAYER.WEIGHT * c2.average + (1 - PLAYER.WEIGHT) * rnorm(1, 0, STD)
  } else{
    c1.score <- PLAYER.WEIGHT * c1.average + c1.h2h * H2H.WEIGHT * c1.average + RANDAM.WEIGHT * rnorm(1, 0, STD)
    c2.score <- PLAYER.WEIGHT * c2.average + c2.h2h * H2H.WEIGHT * c2.average + RANDAM.WEIGHT * rnorm(1, 0, STD)
  }
  
  ifelse(c1.score > c2.score, return(c1), return(c2))
}

match_up("Brazil", "Senegal")


# Initialising the groups
GROUP.A <- c("Qatar", "Ecuador", "Senegal", "Netherlands")
GROUP.B <- c("England", "United States", "Wales", "Iran")
GROUP.C <- c("Argentina", "Saudi Arabia", "Mexico", "Poland")
GROUP.D <- c("France", "Australia", "Denmark", "Tunisia")
GROUP.E <- c("Spain", "Costa Rica", "Germany", "Japan")
GROUP.F <- c("Belgium", "Canada", "Morocco", "Croatia")
GROUP.G <- c("Brazil", "Serbia", "Switzerland", "Cameroon")
GROUP.H <- c("Portugal", "Ghana", "Uruguay", "Korea Republic")


# Running simulation for each group
GROUP.A.DATASET <- data.frame(Group = "Group A",
                              Matchup = c(paste0(GROUP.A[1], "-", GROUP.A[2]),
                              paste0(GROUP.A[1], "-", GROUP.A[3]),
                              paste0(GROUP.A[1], "-", GROUP.A[4]),
                              paste0(GROUP.A[2], "-", GROUP.A[3]),
                              paste0(GROUP.A[2], "-", GROUP.A[4]),
                              paste0(GROUP.A[3], "-", GROUP.A[4])))

GROUP.B.DATASET <- data.frame(Group = "Group B",
                              Matchup = c(paste0(GROUP.B[1], "-", GROUP.B[2]),
                                          paste0(GROUP.B[1], "-", GROUP.B[3]),
                                          paste0(GROUP.B[1], "-", GROUP.B[4]),
                                          paste0(GROUP.B[2], "-", GROUP.B[3]),
                                          paste0(GROUP.B[2], "-", GROUP.B[4]),
                                          paste0(GROUP.B[3], "-", GROUP.B[4])))

GROUP.C.DATASET <- data.frame(Group = "Group C",
                              Matchup = c(paste0(GROUP.C[1], "-", GROUP.C[2]),
                                          paste0(GROUP.C[1], "-", GROUP.C[3]),
                                          paste0(GROUP.C[1], "-", GROUP.C[4]),
                                          paste0(GROUP.C[2], "-", GROUP.C[3]),
                                          paste0(GROUP.C[2], "-", GROUP.C[4]),
                                          paste0(GROUP.C[3], "-", GROUP.C[4])))

GROUP.D.DATASET <- data.frame(Group = "Group D",
                              Matchup = c(paste0(GROUP.D[1], "-", GROUP.D[2]),
                                          paste0(GROUP.D[1], "-", GROUP.D[3]),
                                          paste0(GROUP.D[1], "-", GROUP.D[4]),
                                          paste0(GROUP.D[2], "-", GROUP.D[3]),
                                          paste0(GROUP.D[2], "-", GROUP.D[4]),
                                          paste0(GROUP.D[3], "-", GROUP.D[4])))

GROUP.E.DATASET <- data.frame(Group = "Group E",
                              Matchup = c(paste0(GROUP.E[1], "-", GROUP.E[2]),
                                          paste0(GROUP.E[1], "-", GROUP.E[3]),
                                          paste0(GROUP.E[1], "-", GROUP.E[4]),
                                          paste0(GROUP.E[2], "-", GROUP.E[3]),
                                          paste0(GROUP.E[2], "-", GROUP.E[4]),
                                          paste0(GROUP.E[3], "-", GROUP.E[4])))

GROUP.F.DATASET <- data.frame(Group = "Group F",
                              Matchup = c(paste0(GROUP.F[1], "-", GROUP.F[2]),
                                          paste0(GROUP.F[1], "-", GROUP.F[3]),
                                          paste0(GROUP.F[1], "-", GROUP.F[4]),
                                          paste0(GROUP.F[2], "-", GROUP.F[3]),
                                          paste0(GROUP.F[2], "-", GROUP.F[4]),
                                          paste0(GROUP.F[3], "-", GROUP.F[4])))

GROUP.G.DATASET <- data.frame(Group = "Group G",
                              Matchup = c(paste0(GROUP.G[1], "-", GROUP.G[2]),
                                          paste0(GROUP.G[1], "-", GROUP.G[3]),
                                          paste0(GROUP.G[1], "-", GROUP.G[4]),
                                          paste0(GROUP.G[2], "-", GROUP.G[3]),
                                          paste0(GROUP.G[2], "-", GROUP.G[4]),
                                          paste0(GROUP.G[3], "-", GROUP.G[4])))

GROUP.H.DATASET <- data.frame(Group = "Group H",
                              Matchup = c(paste0(GROUP.H[1], "-", GROUP.H[2]),
                                          paste0(GROUP.H[1], "-", GROUP.H[3]),
                                          paste0(GROUP.H[1], "-", GROUP.H[4]),
                                          paste0(GROUP.H[2], "-", GROUP.H[3]),
                                          paste0(GROUP.H[2], "-", GROUP.H[4]),
                                          paste0(GROUP.H[3], "-", GROUP.H[4])))

GROUPS.DATASET <- rbind(GROUP.A.DATASET, GROUP.B.DATASET, GROUP.C.DATASET, GROUP.D.DATASET, GROUP.E.DATASET, GROUP.F.DATASET,
                        GROUP.G.DATASET, GROUP.H.DATASET)

GROUPS.DATASET$`Team 1` <- sub("\\-.*", "", GROUPS.DATASET$Matchup)
GROUPS.DATASET$`Team 2` <- sub('.*-', '', GROUPS.DATASET$Matchup)
GROUPS.DATASET$`Team 1 Wins` <- 0
GROUPS.DATASET$`Team 2 Wins` <- 0

for(i in 1:nrow(GROUPS.DATASET)){
  for(j in 1:SIMULATION.NUMBER){
    if(match_up(GROUPS.DATASET[i, 3], GROUPS.DATASET[i, 4]) == GROUPS.DATASET[i, 3]){
      GROUPS.DATASET[i, 5] <- GROUPS.DATASET[i, 5] + 1
    }else{
      GROUPS.DATASET[i, 6] <- GROUPS.DATASET[i, 6] + 1
    }
  }
}

GROUPS.DATASET$`Match Winner` <- if_else(GROUPS.DATASET$`Team 1 Wins` > GROUPS.DATASET$`Team 2 Wins`, GROUPS.DATASET$`Team 1`, GROUPS.DATASET$`Team 2`)

GROUP.SUMMARY <- GROUPS.DATASET %>%
  group_by(Group, `Match Winner`) %>%
  summarise(`Win Num` = n()) %>%
  arrange(Group, desc(`Win Num`)) %>%
  group_by(Group) %>%
  top_n(n = 2, wt = `Win Num`) %>%
  mutate(Ranking = c(1, 2))

GROUP.SUMMARY$`Group Ranking` <- c('1A', '2A', '1B', '2B', '1C', '2C', '1D', '2D', '1E', '2E', '1F', '2F', '1G', '2G', '1H', '2H')


# Knockouts

# 1A - 2B
# 1B - 2A
# 1C - 2D
# 1D - 2C
# 1E - 2F
# 1F - 2E
# 1G - 2H
# 1H - 2G


# Round of 16
ROUND.OF.16 <- data.frame(`GroupWinner` = c('1A', '1B', '1C', '1D', '1E', '1F', '1G', '1H'),
                          `GroupRunnerUp` = c('2B', '2A', '2D', '2C', '2F', '2E', '2H', '2G'),
                          `GroupWinnerName` = NA,
                          `RunnerUpName` = NA)


GROUP.WINNER <- ROUND.OF.16 %>%
  select(GroupWinner, GroupWinnerName) %>%
  rename(`Group Ranking` = GroupWinner) %>%
  mutate(GroupWinnerName = "Winner")

GROUP.RUNNER.UP <- ROUND.OF.16 %>%
  select(GroupRunnerUp, RunnerUpName) %>%
  rename(`Group Ranking` = GroupRunnerUp) %>%
  mutate(RunnerUpName = "Runner-Up")

GROUP.WINNER <- full_join(GROUP.SUMMARY, GROUP.WINNER)
GROUP.WINNER <- na.omit(GROUP.WINNER)
GROUP.WINNER$`R16 Opponent` <- c('2B', '2A', '2D', '2C', '2F', '2E', '2H', '2G')

GROUP.RUNNER.UP <- full_join(GROUP.SUMMARY, GROUP.RUNNER.UP)
GROUP.RUNNER.UP <- na.omit(GROUP.RUNNER.UP)
GROUP.RUNNER.UP <- GROUP.RUNNER.UP %>%
  select(`Group Ranking`, `Match Winner`) %>%
  rename(`R16 Opponent` = `Group Ranking`, `R16 Opponent Name` = `Match Winner`, `Group 1` = Group) 

ROUND.OF.16 <- full_join(GROUP.WINNER, GROUP.RUNNER.UP)
ROUND.OF.16$`Winner Games Won` <- 0
ROUND.OF.16$`Runner-Up Games Won` <- 0

write.csv(ROUND.OF.16, "R16.csv", row.names = FALSE)
ROUND.OF.16 <- read.csv("R16.csv", header = TRUE)

for(i in 1:nrow(ROUND.OF.16)){
  for(j in 1:SIMULATION.NUMBER){
    if(match_up(ROUND.OF.16[i, 2], ROUND.OF.16[i, 9]) == ROUND.OF.16[i, 2]){
      ROUND.OF.16[i, 10] <- ROUND.OF.16[i, 10] + 1
    }else{
      ROUND.OF.16[i, 11] <- ROUND.OF.16[i, 11] + 1
    }
  }
}

ROUND.OF.16$`Match Winner` <- if_else(ROUND.OF.16$Winner.Games.Won > ROUND.OF.16$Runner.Up.Games.Won, ROUND.OF.16$Match.Winner, ROUND.OF.16$R16.Opponent.Name)

# Quarter Finals
QUARTER.FINAL <- data.frame(`Team 1` = c(NA, NA, NA, NA), 
                            `Team 2` = c(NA, NA, NA, NA))

for(a in 1:nrow(ROUND.OF.16)){
  if(a == 1){
    QUARTER.FINAL[a, 1] <- ROUND.OF.16[a, 12]
  }else if(a == 2){
    QUARTER.FINAL[a, 1] <- ROUND.OF.16[a, 12]
  }else if(a == 3){
    QUARTER.FINAL[1, 2] <- ROUND.OF.16[a, 12]
  }else if(a == 4){
    QUARTER.FINAL[2, 2] <- ROUND.OF.16[a, 12]
  }else if(a == 5){
    QUARTER.FINAL[3, 1] <- ROUND.OF.16[a, 12]
  }else if(a == 6){
    QUARTER.FINAL[4, 1] <- ROUND.OF.16[a, 12]
  }else if(a == 7){
    QUARTER.FINAL[3, 2] <- ROUND.OF.16[a, 12]
  }else{
    QUARTER.FINAL[4, 2] <- ROUND.OF.16[a, 12]
  }
}

QUARTER.FINAL$`Team 1 Points` <- 0
QUARTER.FINAL$`Team 2 Points` <- 0

for(i in 1:nrow(QUARTER.FINAL)){
  for(j in 1:SIMULATION.NUMBER){
    if(match_up(QUARTER.FINAL[i, 1], QUARTER.FINAL[i, 2]) == QUARTER.FINAL[i, 1]){
      QUARTER.FINAL[i, 3] <- QUARTER.FINAL[i, 3] + 1
    }else{
      QUARTER.FINAL[i, 4] <- QUARTER.FINAL[i, 4] + 1
    }
  }
}

QUARTER.FINAL$`Match Winner` <- if_else(QUARTER.FINAL$`Team 1 Points`> QUARTER.FINAL$`Team 2 Points`, QUARTER.FINAL$Team.1, QUARTER.FINAL$Team.2)
