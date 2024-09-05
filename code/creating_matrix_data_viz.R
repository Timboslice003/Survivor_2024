library(tidyverse)

#Read in 2024 schedule
raw_2024 <- nflreadr::load_schedules(2024)

#Grab away_team, home_team, and week
schedule_2024 <- raw_2024 %>%
  select(away_team, home_team, week)

#Read in odds
dk_odds_raw <- read.csv('dk_moneylines_9_4_2024.csv')

#Create a df with only one row per game
dk_per_game <- tibble(
  'away_team' = dk_odds_raw$team[seq(1, 544, 2)],
  'home_team' = dk_odds_raw$team[seq(2, 544, 2)],
  'away_ml' = dk_odds_raw$moneyline[seq(1, 544, 2)],
  'home_ml' = dk_odds_raw$moneyline[seq(2, 544, 2)]
)

#Get only abbreviation for teams
team_abbrevs <- dk_per_game %>%
  mutate(
    away_team = case_when(
      away_team == 'LA Chargers ' ~ 'LAC',
      away_team == 'LA Rams ' ~ 'LA',
      away_team == 'NY Jets ' ~ 'NYJ',
      away_team == 'NY Giants ' ~ 'NYG',
      TRUE ~ str_extract(away_team, "^[^ ]+")
    ),
    home_team = case_when(
      home_team == 'LA Chargers ' ~ 'LAC',
      home_team == 'LA Rams ' ~ 'LA',
      home_team == 'NY Jets ' ~ 'NYJ',
      home_team == 'NY Giants ' ~ 'NYG',
      TRUE ~ str_extract(home_team, "^[^ ]+")
    )
  )

#Join to get weeks and convert odds to numeric
weeks_odds <- team_abbrevs %>%
  left_join(schedule_2024) %>%
  mutate(home_ml = as.integer(str_replace_all(home_ml, "−", "-")),
         away_ml = as.integer(str_replace_all(away_ml, "−", "-")))

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
week_odds_vig <- weeks_odds %>%
  mutate(
    home_prop_vig = vig_prop(home_ml),
    away_prop_vig = vig_prop(away_ml),
    total_prop_vig = home_prop_vig + away_prop_vig
  )

#Remove vig and keep relevant columns
week_odds_dv <- week_odds_vig %>%
  mutate(
    home_dv = devigger(home_prop_vig, total_prop_vig),
    away_dv = devigger(away_prop_vig, total_prop_vig)
  ) %>%
  select(week,
         home_team,
         away_team,
         home_wp = home_dv,
         away_wp = away_dv)

#Lengthen data to have one row be week with team and win probability
long_data <- week_odds_dv %>%
  pivot_longer(
    cols = c(home_team, away_team),
    names_to = "team_type",
    values_to = "team"
  ) %>%
  pivot_longer(
    cols = c(home_wp, away_wp),
    names_to = "wp_type",
    values_to = "wp"
  ) %>%
  filter(str_sub(team_type, 1, 4) == str_sub(wp_type, 1, 4)) %>%
  select(week, team, wp)

#Widen Data to have each row be a team each colum the wp for each week
wide_data <- long_data %>%
  pivot_wider(names_from = week,
              values_from = wp,
              names_prefix = "week_") %>%
  arrange(team)

#Save data table for future use
write.csv(wide_data, 'survivor_matrix_9_4_24.csv', row.names = FALSE)

#Load Data if starting from here
#wide_data <- read.csv('survivor_matrix_9_4_24.csv')

#Create Graphic and save
wide_data %>%
  gt::gt() %>%
  gtExtras::gt_theme_espn() %>%
  nflplotR::gt_nfl_logos(columns = 'team') %>%
  gt::data_color(
    columns = -team,
    method = 'numeric',
    palette = c('purple', 'white', 'darkgreen'),
    domain = c(15, 85)
  ) %>%
  gt::tab_header(title = '2024 NFL Survivor Matrix', subtitle = 'Win Probability % By Team By Week') %>%
  gt::tab_source_note(source_note = 'Graphic: @Timboslice003 | Win Probability: Draftkings 9/4/2024') %>%
  gt::tab_options(
    heading.title.font.size = 32,
    heading.subtitle.font.size = 20,
    heading.align = 'center',
    source_notes.font.size = 18,
  ) %>%
  gt::cols_align(align = 'center') %>%
  gt::sub_missing(missing_text = 'BYE') %>%
  gt::tab_style_body(
    fn = function(x)
      is.na(x),
    style = gt::cell_fill(color = "black")
  ) %>%
  gt::gtsave('survivor_matrix_9_4_24.png',
             vwidth = 2800,
             expand = c(0, 10, 0, 0))
