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

US_Results_2020_Swing_States_Turnout_Total <- US_Results_2020 %>% 
  filter(state_abr == "AZ" | state_abr == "GA" | state_abr == "MI" | state_abr == "NV" | state_abr == "NC" | state_abr == "PA" | state_abr == "WI") %>% 
  select(-trump_win, -biden_win, -biden_pct, -trump_pct) %>% 
  mutate(turnout_total = trump_vote + biden_vote) %>% 
  mutate(turnout_total = turnout_total * 0.985) %>% #### Lower turnout predicted 
  rename(harris_vote = biden_vote) %>% 
  select(-trump_vote, -harris_vote)

US_Results_2020_Swing_State_Wisconsin <- US_Results_2020_Swing_States_Turnout_Total %>% 
  filter(state_abr == "WI") %>% 
  mutate(Harris_Vote = turnout_total*0.48, Trump_Vote = turnout_total * 0.475) %>% 
  mutate(Winner = "Harris") %>% 
  mutate(Difference = Harris_Vote - Trump_Vote)

US_Results_2020_Swing_State_AZ <- US_Results_2020_Swing_States_Turnout_Total %>% 
  filter(state_abr == "AZ") %>% 
  mutate(Harris_Vote = turnout_total*0.488, Trump_Vote = turnout_total * 0.493) %>% 
  mutate(Winner = "Trump")%>% 
  mutate(Difference = Harris_Vote - Trump_Vote)

US_Results_2020_Swing_State_GA <- US_Results_2020_Swing_States_Turnout_Total %>% 
  filter(state_abr == "GA") %>% 
  mutate(Harris_Vote = turnout_total*0.492, Trump_Vote = turnout_total * 0.501) %>% 
  mutate(Winner = "Trump")%>% 
  mutate(Difference = Harris_Vote - Trump_Vote)

US_Results_2020_Swing_State_MI <- US_Results_2020_Swing_States_Turnout_Total %>% 
  filter(state_abr == "MI") %>% 
  mutate(Harris_Vote = turnout_total*0.502, Trump_Vote = turnout_total * 0.482) %>% 
  mutate(Winner = "Harris")%>% 
  mutate(Difference = Harris_Vote - Trump_Vote)

US_Results_2020_Swing_State_NV <- US_Results_2020_Swing_States_Turnout_Total %>% 
  filter(state_abr == "NV") %>% 
  mutate(Harris_Vote = turnout_total*0.482, Trump_Vote = turnout_total * 0.483) %>% 
  mutate(Winner = "Trump")%>% 
  mutate(Difference = Harris_Vote - Trump_Vote)

US_Results_2020_Swing_State_NC <- US_Results_2020_Swing_States_Turnout_Total %>% 
  filter(state_abr == "NC") %>% 
  mutate(Harris_Vote = turnout_total*0.478, Trump_Vote = turnout_total * 0.503) %>% 
  mutate(Winner = "Trump")%>% 
  mutate(Difference = Harris_Vote - Trump_Vote)

US_Results_2020_Swing_State_PA <- US_Results_2020_Swing_States_Turnout_Total %>% 
  filter(state_abr == "PA") %>% 
  mutate(Harris_Vote = turnout_total*0.498, Trump_Vote = turnout_total * 0.503) %>% 
  mutate(Winner = "Trump")%>% 
  mutate(Difference = Harris_Vote - Trump_Vote)

Swing_States <- bind_rows(US_Results_2020_Swing_State_AZ, US_Results_2020_Swing_State_GA, US_Results_2020_Swing_State_MI, US_Results_2020_Swing_State_NC, US_Results_2020_Swing_State_NV, US_Results_2020_Swing_State_PA, US_Results_2020_Swing_State_Wisconsin)

rm(US_Results_2020_Swing_State_AZ, US_Results_2020_Swing_State_GA, US_Results_2020_Swing_State_MI, US_Results_2020_Swing_State_NC, US_Results_2020_Swing_State_NV, US_Results_2020_Swing_State_PA, US_Results_2020_Swing_State_Wisconsin)