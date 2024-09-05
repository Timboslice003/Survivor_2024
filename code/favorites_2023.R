library(tidyverse)

#Load in schedule and clean for home and away
schedule_loaded <- nflreadr::load_schedules(2023) %>%
  nflreadr::clean_homeaway()

#Grab teams, weeks, and odds
schedule <- schedule_loaded %>%
  filter(game_type == 'REG') %>%
  mutate(win = ifelse(team_score > opponent_score, 'Win', 'Loss')) %>%
  select(week, team, opponent, win, team_moneyline, opponent_moneyline)

#Functions for devigging
#----------------
#Vigged proportion function
vig_prop <- Vectorize(function(ml_1) {
  if (ml_1 >= 100) {
    return(100 / (ml_1 + 100))
  }
  else{
    return ((ml_1 * -1) / (ml_1 * -1 + 100))
  }
})

#Removing vig function
devigger <- Vectorize(function(ml_1, vig) {
  return(round(ml_1 / vig * 100, 2))
})
#----------------

#Get odds with vig and total vig
week_odds_vig <- schedule %>%
  mutate(
    team_prop_vig = vig_prop(team_moneyline),
    opp_prop_vig = vig_prop(opponent_moneyline),
    total_prop_vig = team_prop_vig + opp_prop_vig
  )

#Remove vig and keep relevant columns
week_odds_dv <- week_odds_vig %>%
  mutate(
    team_dv = devigger(team_prop_vig, total_prop_vig),
    opp_dv = devigger(opp_prop_vig, total_prop_vig)
  ) %>%
  select(week,
         team,
         opponent,
         win,
         team_wp = team_dv,
         opp_wp = opp_dv)

#Grab the team with the highest Win Probability for each week
favs <- week_odds_dv %>%
  group_by(week) %>%
  filter(team_wp == max(team_wp)) %>%
  ungroup() %>%
  select(week, team, team_wp, opponent, win)

#Split Data into two 9 row columns for data viz
#Weeks 1-9
first_9_df <- favs %>%
  filter(week < 10)

#Weeks 10-18. Need to change column names for merging
last_9_df <- favs %>%
  filter(week > 9) %>%
  select(
    week_9 = week,
    team_9 = team,
    team_wp_9 = team_wp,
    opponent_9 = opponent,
    win_9 = win
  )


#Join DFs and create Viz
first_9_df %>%
  cbind(last_9_df) %>%
  gt::gt() %>%
  gtExtras::gt_theme_espn() %>%
  gt::tab_header(title = '2023 NFL Weekly Favorites') %>%
  gt::tab_source_note(source_note = 'Graphic: @Timboslice003 | Win Probability: NFLreadR') %>%
  gt::tab_options(
    heading.title.font.size = 32,
    heading.align = 'center',
    source_notes.font.size = 18,
  ) %>%
  gt::cols_align(align = 'center') %>%
  gt::cols_label(
    week_9 = 'week',
    team_9 = 'team',
    team_wp_9 = 'Win Prob',
    team_wp = 'Win Prob',
    opponent_9 = 'opponent',
    win_9 = 'result',
    win = 'result'
  ) %>%
  nflplotR::gt_nfl_logos(columns = c('team', 'team_9', 'opponent', 'opponent_9')) %>%
  gtExtras::gt_add_divider(columns = win,
                           sides = 'right',
                           color = 'lightgrey') %>%
  gt::data_color(
    columns = contains('wp'),
    method = 'numeric',
    palette = c('purple', 'white', 'darkgreen'),
    domain = c(65, 92)
  ) %>%
  gt::data_color(
    columns = c('win', 'win_9'),
    colors = scales::col_factor(
      palette = c('purple', 'darkgreen'),
      domain = c('Win', 'Loss')
    )
  ) %>%
  gt::gtsave('favs_2023.png')
