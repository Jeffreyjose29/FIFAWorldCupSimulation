# Package names
packages <- c("dplyr", "ggplot2", "rvest", "data.table", "qdapRegex", "stringr", "htmltab", "tidyr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Set Working Directory
setwd("C:/Users/jeffr/OneDrive/Desktop/Github Activities/FIFAWorldCupSimulation/Data")

# Step 1: Get each country and the federation they belong to
federationPage <- read_html("https://en.wikipedia.org/wiki/Oceania_Football_Confederation")
OFC <- federationPage %>%
  html_node(xpath = '/html/body/div[3]/div[3]/div[5]/div[1]/table[4]')  %>% html_table()
OFC$Federation <- "OFC"
colnames(OFC)[1] <- "Abbreviation"
OFC <- OFC %>%
  select(Abbreviation, Association, `IOC member`, Federation) %>%
  filter(!grepl(')', Abbreviation))

federationPage <- read_html("https://en.wikipedia.org/wiki/Asian_Football_Confederation")
AFC <- federationPage %>%
  html_node(xpath = '/html/body/div[3]/div[3]/div[5]/div[1]/table[5]')  %>% html_table()
AFC$Federation <- "AFC"
colnames(AFC)[1] <- "Abbreviation"
AFC <- AFC %>%
  select(Abbreviation, Name, IOCmember, Federation) %>%
  rename(`IOC member` = IOCmember, Association = Name) %>%
  filter(!grepl(')', Abbreviation))


federationPage <- read_html("https://en.wikipedia.org/wiki/CONCACAF")
CONCACAF <- federationPage %>%
  html_node(xpath = '/html/body/div[3]/div[3]/div[5]/div[1]/table[4]')  %>% html_table()
CONCACAF$Federation <- "CONCACAF"
colnames(CONCACAF)[1] <- "Abbreviation"
CONCACAF <- CONCACAF %>%
  select(Abbreviation, Association, `IOC  member`, Federation) %>%
  rename(`IOC member` = `IOC  member`) %>%
  filter(!grepl(')', Abbreviation))


federationPage <- read_html("https://en.wikipedia.org/wiki/Confederation_of_African_Football")
CAF <- federationPage %>%
  html_node(xpath = '/html/body/div[3]/div[3]/div[5]/div[1]/table[4]')  %>% html_table()
CAF$Federation <- "CAF"
colnames(CAF)[1] <- "Abbreviation"
CAF <- CAF %>%
  select(Abbreviation, Association, `IOC member`, Federation) %>%
  filter(!grepl(')', Abbreviation))



federationPage <- read_html("https://en.wikipedia.org/wiki/CONMEBOL")
CONMEBOL <- federationPage %>%
  html_node(xpath = '/html/body/div[3]/div[3]/div[5]/div[1]/table[5]')  %>% html_table()
CONMEBOL$Federation <- "CONMEBOL"
colnames(CONMEBOL)[1] <- "Abbreviation"
CONMEBOL <- CONMEBOL %>%
  select(Abbreviation, Association, `IOC member`, Federation) %>%
  filter(!grepl(')', Abbreviation))


federationPage <- read_html("https://en.wikipedia.org/wiki/UEFA")
UEFA <- federationPage %>%
  html_node(xpath = '/html/body/div[3]/div[3]/div[5]/div[1]/table[3]')  %>% html_table()
UEFA$Federation <- "UEFA"
colnames(UEFA)[1] <- "Abbreviation"
UEFA <- UEFA %>%
  select(Abbreviation, Association, IOCmember, Federation) %>%
  rename(`IOC member` = IOCmember) %>%
  filter(!grepl(')', Abbreviation))

# Bind all the federations together
federations <- do.call("rbind", list(OFC, AFC, CONCACAF, CAF, CONMEBOL, UEFA))
federations <- federations %>%
  filter(!grepl('members', Abbreviation))

AssociationsToFix <- federations[federations$Association %like% "]", ]
AssociationsToFix$Association <- rm_between(AssociationsToFix$Association, "[", "]")

federations <- rbind(federations, AssociationsToFix)
federations <- federations[!federations$Association %like% "]", ]

rm(OFC, AFC, CONCACAF, CAF, CONMEBOL, UEFA, federationPage, AssociationsToFix)

# Step 2: Extract Intl. Match Matrix
IntlMatches <- read.csv("intlMatches.csv", header = TRUE)
IntlShootouts <- read.csv("ShootOuts.csv", header = TRUE)

# Restrict to after 1930 when the first world cup was
IntlMatches <- IntlMatches %>%
  filter(date >= "1930-07-13")
IntlShootouts <- IntlShootouts %>%
  filter(date >= "1930-07-13")

IntlMatches <- left_join(IntlMatches, IntlShootouts, by = c("date" = "date", "home_team" = "home_team", "away_team" = "away_team"))

IntlMatches$winnerFinal <- if_else(IntlMatches$home_score > IntlMatches$away_score, IntlMatches$home_team, 
                              if_else(IntlMatches$away_score > IntlMatches$home_score, IntlMatches$away_team,
                                      if_else(is.na(IntlMatches$winner), "Draw", IntlMatches$winner)
                              )
)

IntlMatches$resultYielded <- if_else(IntlMatches$winnerFinal == "Draw", "Draw", "Result")
IntlMatches %>%
  group_by(resultYielded) %>%
  summarise(Count = n())


IntlMatches <- IntlMatches %>%
  filter(resultYielded == "Result")

IntlMatches$Team1 <- IntlMatches$home_team
IntlMatches$Team2 <- IntlMatches$away_team

IntlMatches <- transform(IntlMatches, Team1 = pmin(Team1, Team2), Team2 = pmax(Team1, Team2))

IntlMatches$MatchTeams <- paste(IntlMatches$Team1, '-', IntlMatches$Team2)

IntlMatches$winner <- NULL

IntlMatches$winnerByTeam1 <- if_else(IntlMatches$winnerFinal == IntlMatches$Team1, 1, 0)
IntlMatches$winnerByTeam2 <- if_else(IntlMatches$winnerFinal == IntlMatches$Team2, 1, 0)

AllMatchesResult <- IntlMatches %>%
  group_by(MatchTeams) %>%
  summarise(NumMatches = n(),
            Team1Winner = sum(winnerByTeam1),
            Team2Winner = sum(winnerByTeam2))

# Create decimal for wins
AllMatchesResult$Team1Winner <- round(AllMatchesResult$Team1Winner / AllMatchesResult$NumMatches, 2)
AllMatchesResult$Team2Winner <- round(AllMatchesResult$Team2Winner / AllMatchesResult$NumMatches, 2)
AllMatchesResult$Team1 <- sub("\\-.*", "", AllMatchesResult$MatchTeams)
AllMatchesResult$Team2 <- word(AllMatchesResult$MatchTeams, 2, sep = "-")

AllMatchesResult <- AllMatchesResult %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" ")))

AllMatchResultInverted <- AllMatchesResult
AllMatchResultInverted$Team1FirstLetter <- substr(AllMatchResultInverted$Team1, 1, 1)
AllMatchResultInverted$Team2FirstLetter <- substr(AllMatchResultInverted$Team2, 1, 1)
AllMatchResultInverted$Team2AfterTeam1 <- if_else(AllMatchResultInverted$Team2FirstLetter > AllMatchResultInverted$Team1FirstLetter, TRUE, FALSE)
AllMatchResultInverted <- AllMatchResultInverted %>%
  filter(Team2AfterTeam1 == "TRUE")
tmp <- AllMatchResultInverted[5]
AllMatchResultInverted[5] <- AllMatchResultInverted[6]
AllMatchResultInverted[6] <- tmp
# Score Swapping
tmp <- AllMatchResultInverted[3]
AllMatchResultInverted[3] <- AllMatchResultInverted[4]
AllMatchResultInverted[4] <- tmp
AllMatchResultInverted <- AllMatchResultInverted %>%
  select(MatchTeams, NumMatches, Team1Winner, Team2Winner, Team1, Team2)

# Rbind both inverted and all matches df
AllMatchesResult <- rbind(AllMatchesResult, AllMatchResultInverted)


# Number of distinct teams
distinct_teams <- n_distinct(AllMatchesResult$Team1)

#Attempt to create matrix
matrix_df <- as.data.frame(matrix(0, ncol = distinct_teams, nrow = distinct_teams))
teams <- unique(AllMatchesResult$Team1)

colnames(matrix_df) <- teams
rownames(matrix_df) <- teams

# Set all 0 to NA
matrix_df[matrix_df == 0] <- NA

for(i in 1:nrow(matrix_df)){
  currentTeam <- row.names(matrix_df)[i]
  
  filteredMatches <- AllMatchesResult %>%
    filter(Team1 == currentTeam)
  
  for(j in 1:nrow(filteredMatches)){
    Team2Column <- filteredMatches[j, 6]
    
    for(k in 1:ncol(matrix_df)){
      if(colnames(matrix_df)[k] == Team2Column){
        matrix_df[i, k] <- filteredMatches[j, 3]
      }
    }
  }
}

worldCupTeams <- c("Qatar", "Ecuador", "Senegal", "Netherlands",
                   "England", "Iran", "UnitedStates", "Wales",
                   "Argentina", "SaudiArabia", "Mexico", "Poland",
                   "France", "Australia", "Denmark", "Tunisia",
                   "Spain", "CostaRica", "Germany", "Japan",
                   "Belgium", "Canada", "Morocco", "Croatia",
                   "Brazil", "Serbia", "Switzerland", "Cameroon",
                   "Portugal", "Ghana", "Uruguay", "SouthKorea")

matrix_df <- subset(matrix_df, rownames(matrix_df) %in% c("Qatar", "Ecuador", "Senegal", "Netherlands",
                                  "England", "Iran", "UnitedStates", "Wales",
                                  "Argentina", "SaudiArabia", "Mexico", "Poland",
                                  "France", "Australia", "Denmark", "Tunisia",
                                  "Spain", "CostaRica", "Germany", "Japan",
                                  "Belgium", "Canada", "Morocco", "Croatia",
                                  "Brazil", "Serbia", "Switzerland", "Cameroon",
                                  "Portugal", "Ghana", "Uruguay", "SouthKorea")
)
matrix_df <- matrix_df[, worldCupTeams]
matrix_df <- matrix_df[, order(colnames(matrix_df))]


# Step 2: Extract Player Data
players_df <- data.frame()
for(i in 1:570){
  playersLink <- paste("https://www.fifaindex.com/players/fifa23_552/?page=", i, sep = "")
  htmlDriver <- read_html(playersLink)
  playersTableIndex <- html_nodes(htmlDriver, css = "table")
  allTables <- html_nodes(htmlDriver, css = "table")
  players_placeholder <- html_table(allTables)[[1]]
  
  players_df <- rbind(players_df, players_placeholder)
}

players_df <- players_df %>%
  drop_na(`OVR-POT`) %>%
  select(`OVR-POT`, Name, `Preferred Positions`, Age) %>%
  rename("Overall" = `OVR-POT`)

players_df$Overall <- substr(players_df$Overall, start = 1, stop = 2)

playingPositions <- c("GK", "SW", "RWB", "RB", "CB", "LWB", "LB", "CDM", "CM", "RM", "LM", "CAM", "CF", "RW", "LW", "ST")

players_df$GK <- ifelse(grepl("GK", players_df$`Preferred Positions`, fixed = TRUE) == TRUE, "GK", NA)
players_df$SW <- ifelse(grepl("SW", players_df$`Preferred Positions`, fixed = TRUE) == TRUE, "SW", NA)
players_df$RWB <- ifelse(grepl("RWB", players_df$`Preferred Positions`, fixed = TRUE) == TRUE, "RWB", NA)
players_df$RB <- ifelse(grepl("RB", players_df$`Preferred Positions`, fixed = TRUE) == TRUE, "RB", NA)
players_df$CB <- ifelse(grepl("CB", players_df$`Preferred Positions`, fixed = TRUE) == TRUE, "CB", NA)
players_df$LWB <- ifelse(grepl("LWB", players_df$`Preferred Positions`, fixed = TRUE) == TRUE, "LWB", NA)
players_df$LB <- ifelse(grepl("LB", players_df$`Preferred Positions`, fixed = TRUE) == TRUE, "LB", NA)
players_df$CDM <- ifelse(grepl("CDM", players_df$`Preferred Positions`, fixed = TRUE) == TRUE, "CDM", NA)
players_df$CM <- ifelse(grepl("CM", players_df$`Preferred Positions`, fixed = TRUE) == TRUE, "CM", NA)
players_df$RM <- ifelse(grepl("RM", players_df$`Preferred Positions`, fixed = TRUE) == TRUE, "RM", NA)
players_df$LM <- ifelse(grepl("LM", players_df$`Preferred Positions`, fixed = TRUE) == TRUE, "LM", NA)
players_df$CAM <- ifelse(grepl("CAM", players_df$`Preferred Positions`, fixed = TRUE) == TRUE, "CAM", NA)
players_df$CF <- ifelse(grepl("CF", players_df$`Preferred Positions`, fixed = TRUE) == TRUE, "CF", NA)
players_df$RW <- ifelse(grepl("RW", players_df$`Preferred Positions`, fixed = TRUE) == TRUE, "RW", NA)
players_df$LW <- ifelse(grepl("LW", players_df$`Preferred Positions`, fixed = TRUE) == TRUE, "LW", NA)
players_df$ST <- ifelse(grepl("ST", players_df$`Preferred Positions`, fixed = TRUE) == TRUE, "ST", NA)


players_df$Position <- paste(players_df$GK, ",",
                              players_df$SW, ",",
                              players_df$RWB, ",",
                              players_df$RB, ",",
                              players_df$CB, ",",
                              players_df$LWB, ",",
                              players_df$LB, ",",
                              players_df$CDM, ",",
                              players_df$CM, ",",
                              players_df$RM, ",",
                              players_df$LM, ",",
                              players_df$CAM, ",",
                              players_df$CF, ",",
                              players_df$RW, ",",
                              players_df$LW, ",",
                              players_df$ST, ",",
                             collapse = ' ',
                             na.skip = TRUE)


for (i in 1:length(players_df$`Preferred Positions`)) {
  string <- players_df$`Preferred Positions`[i]
  findWords <- intersect(unlist(strsplit(string," ")),playingPositions)
  if (!is.null(findWords)) {
    for (j in findWords) {
      players_df$`Preferred Positions`[i] <- gsub(j,paste0(j,","),string)
    }
  }
}




# Step 4: Write out the extracted datasets to a .csv file for back-up
write.csv(federations, "Federations.csv", row.names = FALSE)
