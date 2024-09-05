library(lpSolve)
library(tidyverse)

#Load in csv
win_probs <- read.csv('survivor_matrix_9_4_24.csv')

# Convert tibble to a matrix (excluding team names)
# Change NAs to 0. Those are from the BYE weeks
prob_matrix <- win_probs %>%
  select(-team) %>%
  replace(is.na(.), 0) %>%
  as.matrix()

# Change from percentage to probability
prob_matrix <- prob_matrix / 100

# Number of teams (rows) and weeks (columns)
num_teams <- nrow(prob_matrix)
num_weeks <- ncol(prob_matrix)

# Create the objective function (flattened matrix)
objective <- as.vector(t(prob_matrix))

# Constraints:
# 1. Each team can be selected at most once
team_constraints <- matrix(0, nrow = num_teams, ncol = num_teams * num_weeks)
for (i in 1:num_teams) {
  team_constraints[i, ((i - 1) * num_weeks + 1):(i * num_weeks)] <- 1
}

# 2. Exactly one team must be selected per week
week_constraints <- matrix(0, nrow = num_weeks, ncol = num_teams * num_weeks)
for (j in 1:num_weeks) {
  week_constraints[j, seq(j, num_teams * num_weeks, by = num_weeks)] <- 1
}

# Combine all constraints
constraints <- rbind(team_constraints, week_constraints)

# Right-hand side of the constraints
rhs <- c(rep(1, num_teams), rep(1, num_weeks))

# Constraint direction (all constraints are equality)
constraint_dir <- rep("<=", length(rhs))

# Solve the linear program
lp_solution <- lp(
  direction = "max",
  objective.in = objective,
  const.mat = constraints,
  const.dir = constraint_dir,
  const.rhs = rhs,
  all.bin = TRUE
)

# Extract the solution
selection <- matrix(lp_solution$solution, nrow = num_teams, byrow = TRUE)
colnames(selection) <- colnames(win_probs)[-1]

# Solution matrix
prob_solution <- selection * prob_matrix %>%
  as_tibble()

# Add Team names back in
prob_solution$team <- win_probs$team

# Lengthen to have team, week, and win prob
long_solution <- prob_solution %>%
  select(team, everything()) %>%
  pivot_longer(cols = starts_with('week_'),
               names_to = 'week',
               values_to = 'wp') %>%
  mutate(week = as.numeric(gsub("week_", "", week))) %>%
  filter(wp != 0)

# Get schedules so we can add opponent to data viz
schedule_2024_loaded <- nflreadr::load_schedules(2024)

# Grab team, week, and opponent and join with solution
schedule_2024 <- nflreadr::clean_homeaway(schedule_2024_loaded) %>%
  select(team, week, opponent) %>%
  left_join(long_solution) %>%
  filter(!is.na(wp)) %>%
  select(week, team, wp, opponent) %>%
  mutate(wp = wp * 100)

#Split Data into two 9 row columns for data viz
#Weeks 1-9
first_9_df <- schedule_2024 %>%
  filter(week < 10)

#Weeks 10-18. Need to change column names for merging
last_9_df <- schedule_2024 %>%
  filter(week > 9) %>%
  select(
    week_9 = week,
    team_9 = team,
    wp_9 = wp,
    opponent_9 = opponent
  )

#Join DFs and create Viz
first_9_df %>%
  cbind(last_9_df) %>%
  gt::gt() %>%
  gtExtras::gt_theme_espn() %>%
  gt::tab_header(title = '2024 NFL Survivor Optimization') %>%
  gt::tab_source_note(source_note = 'Graphic: @Timboslice003 | Win Probability: Draftkings 9/4/2024') %>%
  gt::tab_options(
    heading.title.font.size = 32,
    heading.align = 'center',
    source_notes.font.size = 18,
  ) %>%
  gt::cols_align(align = 'center') %>%
  gt::cols_label(
    week_9 = 'week',
    team_9 = 'team',
    wp_9 = 'Win Prob',
    wp = 'Win Prob',
    opponent_9 = 'opponent',
  ) %>%
  nflplotR::gt_nfl_logos(columns = starts_with('team')) %>%
  nflplotR::gt_nfl_logos(columns = starts_with('opp')) %>%
  gtExtras::gt_add_divider(columns = opponent,
                           sides = 'right',
                           color = 'lightgrey') %>%
  gt::data_color(
    columns = starts_with('wp'),
    method = 'numeric',
    palette = c('purple', 'white', 'darkgreen'),
    domain = c(55, 85)
  ) %>%
  gt::gtsave('optimized_9_4_24.png')
