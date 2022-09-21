# Package names
packages <- c("dplyr", "ggplot2", "rvest", "htmltab", "httr", "XML")

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

rm(OFC, AFC, CONCACAF, CAF, CONMEBOL, UEFA, federationPage)

# Step 2: Extract Player Data
players_df <- data.frame()
for(i in 1:570){
  playersLink <- paste("https://www.fifaindex.com/players/fifa23_552/?page=", i, sep = "")
  htmlDriver <- read_html(playersLink)
  playersTableIndex <- html_nodes(htmlDriver, css = "table")
  
  players_placeholder <- html_table(allTables)[[1]]
  
  players_df <- rbind(players_df, players_placeholder)
}

playersLink <- "https://www.fifaindex.com/players/fifa23_552/"
driver <- read_html(playersLink)
allTables <- html_nodes(driver, css = "table")
players <- html_table(allTables)[[1]]


# Step 4: Write out the extracted datasets to a .csv file for back-up
write.csv(federations, "Federations.csv", row.names = FALSE)
