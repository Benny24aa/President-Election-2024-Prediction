dataset <- "C:/Users/harle/OneDrive/Desktop/election analysis 2024/President-Election-2024-Prediction/2020 pres results.csv"
US_Results_2020 <- read.csv(dataset)

library(readxl)
library(dplyr)
library(fs)
library(purrr)
library(data.table)
library(tidyr)
library(janitor)
library(stringr)
library(readr)
library(writexl)
#### Swing States

US_Results_2020_Swing_States <- US_Results_2020 %>% 
  filter(state_abr == "AZ" | state_abr == "GA" | state_abr == "MI" | state_abr == "NV" | state_abr == "NC" | state_abr == "PA" | state_abr == "WI") %>% 
  select(-trump_win, -biden_win, -biden_pct, -trump_pct) %>% 
  mutate(turnout_total = trump_vote + biden_vote) %>% 
  mutate(turnout_total = turnout_total * 0.985) %>% #### Lower turnout predicted 
  rename(harris_vote = biden_vote) %>% 
  select(-trump_vote, -harris_vote)