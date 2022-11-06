###################################################
## File Name: 02 Logic Implementation
## Date: 2nd October 2022
###################################################


# KEY VARIABLES
SIMULATION.NUMBER <- 5000
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
                           Spots = c(4, 4, 3, 0, 5, 13))

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
GROUP.A.DATASET <- as.data.frame(GROUP.A) %>%
  add_column(Points = NA)
GROUP.B.DATASET <- as.data.frame(GROUP.B) %>%
  add_column(Points = NA)
GROUP.C.DATASET <- as.data.frame(GROUP.C) %>%
  add_column(Points = NA)
GROUP.D.DATASET <- as.data.frame(GROUP.D) %>%
  add_column(Points = NA)
GROUP.E.DATASET <- as.data.frame(GROUP.E) %>%
  add_column(Points = NA)
GROUP.F.DATASET <- as.data.frame(GROUP.F) %>%
  add_column(Points = NA)
GROUP.G.DATASET <- as.data.frame(GROUP.G) %>%
  add_column(Points = NA)
GROUP.H.DATASET <- as.data.frame(GROUP.H) %>%
  add_column(Points = NA)

for(i in 1:SIMULATION.NUMBER){
  # SIMULATING GROUP A
  
}
