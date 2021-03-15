reg_season_stats <- read.csv("KaggleDataFiles/MRegularSeasonDetailedResults.csv", stringsAsFactors = FALSE)
tourney_stats <- read.csv("KaggleDataFiles/MNCAATourneyDetailedResults.csv", stringsAsFactors = FALSE)
teams <- read.csv("KaggleDataFiles/MTeams.csv", stringsAsFactors = FALSE)

#Alex use these
#reg_season_stats <- read.csv("MRegularSeasonDetailedResults.csv", stringsAsFactors = FALSE)
#tourney_stats <- read.csv("MNCAATourneyDetailedResults.csv", stringsAsFactors = FALSE)
#teams <- read.csv("MTeams.csv", stringsAsFactors = FALSE)


library(tidyverse)
##library(ggridges)
##library(knitr)
##library(magrittr)
##library(scales)

#Creating variable in reg season stats
reg_season_stats <- reg_season_stats %>%
  mutate(WPoss = WFGA + (WFTA * 0.475) + WTO - WOR,
         LPoss = LFGA + (LFTA * 0.475) + LTO - LOR,
         WIE = WScore + WFGM + WFTM - WFGA - WFTA + WDR + (0.5 * WOR) + WAst + WStl + (0.5 * WBlk) - WPF - WTO,
         LIE = LScore + LFGM + LFTM - LFGA - LFTA + LDR + (0.5 * LOR) + LAst + LStl + (0.5 * LBlk) - LPF - LTO,
         Wefgper = ((WFGM - WFGM3) + 1.5 * WFGM3)/ WFGA,
         Lefgper = ((LFGM - LFGM3) + 1.5 * LFGM3)/ LFGA,
         W3Pper = WFGM3 / WFGA3,
         L3Pper = LFGM3 / LFGA3,
         WOffRat = (WScore / WPoss) * 100,
         LOffRat = (LScore / LPoss) * 100,
         WSOS = WOffRat / LOffRat,
         LSOS = LOffRat / WOffRat,
         WTOPoss = WTO / WPoss,
         LTOPoss = LTO / LPoss,
         WORper = WOR / (WOR + LDR),
         LORper = LOR / (LOR + WDR),
         WFTR = WFTM / WFTA,
         LFTR = LFTM / LFTA)

#tournament version that we are not using
# tourney_stats <- tourney_stats %>%
#   mutate(WPoss = WFGA + (WFTA * 0.475) + WTO - WOR,
#          LPoss = LFGA + (LFTA * 0.475) + LTO - LOR,
#          WIE = WScore + WFGM + WFTM - WFGA - WFTA + WDR + (0.5 * WOR) + WAst + WStl + (0.5 * WBlk) - WPF - WTO,
#          LIE = LScore + LFGM + LFTM - LFGA - LFTA + LDR + (0.5 * LOR) + LAst + LStl + (0.5 * LBlk) - LPF - LTO,
#          Wefgper = ((WFGM - WFGM3) + 1.5 * WFGM3)/ WFGA,
#          Lefgper = ((LFGM - LFGM3) + 1.5 * LFGM3)/ LFGA,
#          W3Pper = percent(WFGM3 / WFGA3),
#          L3Pper = percent(LFGM3 / LFGA3),
#          WOffRat = (WScore / WPoss) * 100,
#          LOffRat = (LScore / LPoss) * 100,
#          WSOS = WOffRat / LOffRat,
#          LSOS = LOffRat / WOffRat,
#          WTOPoss = WTO / WPoss,
#          LTOPoss = LTO / LPoss,
#          WORper = percent(WOR / (WOR + LDR)),
#          LORper = percent(LOR / (LOR + WDR)),
#          WFTR = percent(WFTM / WFTA),
#          LFTR = percent(LFTM / LFTA))

#Function to split by WTeam and LTeam, bringing in name of school, creating summary stats based on Season and Team
reshape_detailed_results <- function(detailed_dataset) {
  
  season_team_stats_tot <- rbind(
    detailed_dataset %>%
      select(Season, DayNum, TeamID=WTeamID, IE=WIE, SOS=WSOS, EFGper=Wefgper, TOPoss=WTOPoss, ORper=WORper, FTR=WFTR) %>%
      mutate(Winner=1),
    
    detailed_dataset %>%
      select(Season, DayNum, TeamID=LTeamID, IE=LIE, SOS=LSOS, EFGper=Lefgper, TOPoss=LTOPoss, ORper=LORper, FTR=LFTR) %>%
      mutate(Winner=0)) %>%
    left_join(teams, by= "TeamID") %>%
    group_by(Season, TeamID, TeamName) %>%
    summarise(GP = n(),
              Wins = sum(Winner),
              'Win Percentage' = Wins/GP,
              IE = sum(IE),
              SOS = sum(SOS),
              EFGper = sum(EFGper),
              TOPoss = sum(TOPoss),
              ORper = sum(ORper),
              FTR = sum(FTR)
    ) %>% ungroup()
  
}

#Season Stats for Kaggle Data
season_team_stats_tot <- reshape_detailed_results(reg_season_stats)

#Function to Calculate averages
calculate_detailed_averages <- function(totals_dataframe) {
  
  
  averages <- totals_dataframe
  
  cols <- names(averages[,c(7:12)])
  
  for (eachcol in cols) {
    averages[,eachcol] <- round(averages[,eachcol] / averages$GP,2)
    
  }
  
  averages <- averages %>%
    rename(AvgIE = IE, AvgSOS = SOS, AvgEFGper = EFGper, AvgTOPoss = TOPoss, AvgORper = ORper, AvgFTR = FTR)
  
  return(averages)
  
}

#Kaggle Data Averages
season_team_stats_averages <- calculate_detailed_averages(season_team_stats_tot)




#####LOGISTIC REGRESSION

train <- tourney_stats %>% 
  select(Season, DayNum, WTeamID, LTeamID) %>% 
  mutate(team_id_diff = WTeamID - LTeamID,
         Team1 = if_else(team_id_diff < 0, WTeamID, LTeamID),
         Team2 = if_else(team_id_diff > 0, WTeamID, LTeamID),
         result = if_else(WTeamID == Team1, 1, 0)) %>% 
  select(Season, Team1, Team2, result)

train <- train %>% 
  left_join(season_team_stats_averages, by = c("Season", "Team1" = "TeamID")) %>%
  left_join(season_team_stats_averages, by = c("Season", "Team2" = "TeamID"))

fit <- glm(result ~ AvgIE.x + AvgSOS.x + AvgEFGper.x + AvgTOPoss.x + AvgORper.x + AvgFTR.x + AvgIE.y + AvgSOS.y + AvgEFGper.y + AvgTOPoss.y +AvgORper.y + AvgFTR.y,
           data = train, 
           family = "binomial")

test <- read.csv("SampleSubmissionStage2.csv", stringsAsFactors = FALSE) %>% 
  select(ID) %>% 
  separate(ID, sep = "_", into = c("Season", "Team1", "Team2"), convert = TRUE) %>%
  left_join(season_team_stats_averages, by = c("Season", "Team1" = "TeamID")) %>%
  left_join(season_team_stats_averages, by = c("Season", "Team2" = "TeamID")) 

test$Pred <- predict(fit, test, type = "response")

#write to file
submitR1 <- test %>% 
  select(Season, Team1, Team2, Pred) %>%
  unite("ID", Season, Team1, Team2, sep = "_") %>%
  write.csv("submitR1.csv", row.names = FALSE)





