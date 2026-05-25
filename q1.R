library(dplyr)
library(ggplot2)
library(knitr)

players <- read.csv("/Users/aka6208/Desktop/game assignment/players.csv")
sessions <- read.csv("/Users/aka6208/Desktop/game assignment/sessions.csv")

players$signup_date <- as.Date(players$signup_date)

players <- players %>%
mutate(
  experience_level = case_when(
    format(signup_date, "%Y") <"2023"~ "Veteran",
    format(signup_date, "%Y") == "2023"~ "Intermediate",
    format(signup_date, "%Y") > "2023"~ "New",
  )
)
merged_data <- sessions %>%
  left_join(players, by = "player_id")

summary_table <- merged_data %>%
  group_by(experience_level) %>%
  summarise(
    number_players = n_distinct(player_id),
    average_play_time = mean (play_time_minutes, na.rm =TRUE),
    average_score = mean (score, na.rm = TRUE)
  )
kable(summary_table)


ggplot(summary_table,
       aes(
         x = experience_level,
           y = average_play_time,
           fill = experience_level
         )) +
         geom_bar(stat = "identity") +
         labs(
           title = "Average Play Time by Experience Level",
           x = "Experience Level", 
           y = " Average Play Time"
         ) +
         theme_minimal()
setwd("/Users/aka6208/Desktop/game assignment")
getwd()
list.files()
list.files("/Users/aka6208/Desktop", pattern = "\\.R$")
         


library(dplyr)
library(ggplot2)
library(knitr)
# Read the game.csv file
# and save it into a dataframe called games
  games <- read.csv("games.csv") 
  
  # Take the sessions dataframe 
  # then join the games dataframe 
  # using the common column game_id 
  genre_data <- sessions %>%
    left_join (games, by = "game_id")
  
  #take the joined data
  #then group bu the data by genre 
  genre_summary <- genre_data %>%
  group_by (genre) %>%
    
    #create summary statistics for each genre 
    summarise(
      
      average_play_time = mean(play_time_minutes, na.rm = TRUE),
      average_score = mean(score, na.rm= TRUE),
      number_session = n(), 
      unique_players = n_distinct(player_id)
    )
  ggplot(
    genre_summary,
    aes(
      x=genre,
      y=unique_players,
      fill = genre
    )
  )+
    geom_bar(stat = "identity"
    )
  )
