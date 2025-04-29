library(tidyverse)
library(janitor)
library(sportyR)
setwd("~/Downloads/nba-movement-data/data")

# read in data
tracking <- read_csv("~/Downloads/nba-movement-data/data/csv/0021500660.csv")
events <- read_csv("~/Downloads/nba-movement-data/data/events/0021500660.csv") |> 
  clean_names()
shots_fixed <- read_csv("~/Downloads/nba-movement-data/data/shots/shots_fixed.csv") |> 
  filter(GAME_ID == "0021500660") |> clean_names()

unique(events$eventmsgtype)
unique(events$eventmsgactiontype)

events_shots <- events |> 
  filter(eventmsgtype == 1 | eventmsgtype == 2)

events_shots <- events_shots |> 
  separate(pctimestring, into = c("minutes", "seconds", "millis"), sep = ":", convert = TRUE) |> 
  mutate(game_clock = minutes * 60 + seconds) |> 
  select(-minutes, -seconds, -millis)

# get tracking data for all shots
tracking_shots <- tracking |> 
  filter(team_id == -1) |> 
  inner_join(events_shots |> select(quarter = period, game_clock), by = "quarter") |> 
  filter(abs(game_clock.x - game_clock.y) <= 0.5)
  #filter(event_id %in% events_shots$eventnum)

length(unique(tracking_shots$event_id))

# tracking_shots <- tracking_shots |> 
#   mutate(timeleft = sprintf("%02d:%02d", game_clock.y %/% 60, game_clock.y %% 60))

clean_shots <- tracking_shots |> 
  group_by(game_clock.y, quarter) |> 
  summarise(x_loc = first(x_loc), y_loc = first(y_loc)) |> 
  ungroup()

# shots with court overlayed
geom_basketball(league = "nba", display_range = "full", x_trans = 50, y_trans = 25) +
    geom_point(data = clean_shots, aes(x_loc, y_loc))

# shots with basket location overlayed
clean_shots |> 
  #mutate(x_loc = ifelse(x_loc > 50, 100 - x_loc, x_loc)) |> 
  ggplot(aes(x_loc, y_loc)) +
  geom_point() +
  geom_vline(xintercept = 94-5.5, color = "red") +
  geom_vline(xintercept = 0+5.5, color = "red")

# get all shots from github repo
shots <- read_csv("shots/shots.csv") |> clean_names() |> 
  filter(game_id == 0021500660)

shots |> 
  ggplot(aes(loc_x, loc_y)) +
  geom_point()

shots_fixed |> 
  ggplot(aes(loc_x, loc_y)) +
  geom_point()

length(unique(shots_fixed$game_event_id))

# start_frames <- tracking |>
#   filter(team_id == -1) |> 
#   inner_join(shots_fixed |> select(game_clock = shot_time, quarter)) |>
#   #filter(team_id == -1) |>
#   distinct() |>
#   filter(event_id %in% events_shots$eventnum)
# 
# start_frames <- tracking |>
#   filter(team_id == -1) |> 
#   inner_join(shots_fixed |> select(game_clock = shot_time, quarter)) |>
#   filter(event_id %in% events_shots$eventnum) |>
#   distinct(x_loc, y_loc, radius, .keep_all = TRUE)

# get all start frames for shots
start_frames <- tracking |>
  filter(team_id == -1) |> 
  inner_join(shots_fixed |> select(game_clock = shot_time, quarter)) |>
  filter(event_id %in% events_shots$eventnum) |>
  group_by(event_id) |>
  slice(1) |>
  ungroup() |> 
  distinct(x_loc, y_loc, radius, .keep_all = TRUE)

# standardize tracking coordinates so all plays from right to left
# calculate acceleration, velocity in all directions
tracking_ball <- tracking |> 
  filter(team_id == -1) |> 
  mutate(y_loc = ifelse(x_loc > 47, 50 - y_loc, y_loc),
                x_loc = ifelse(x_loc > 47, 94 - x_loc, x_loc)) |> 
  mutate(
    vx = c(NA, diff(x_loc)),
    vy = c(NA, diff(y_loc)),
    vz = c(NA, diff(radius)),
    ax = c(NA, diff(vx)),
    ay = c(NA, diff(vy)),
    az = c(NA, diff(vz)),
    acceleration = sqrt(ax^2 + ay^2 + az^2)
  )

# find end frames for each shot
# end frames as when ball goes below 10 ft
end_frames <- tracking_ball |> 
  mutate(
    above_rim = radius > 10,
    went_below = ifelse(lag(above_rim) == TRUE & above_rim == FALSE, TRUE, FALSE)
  ) |> 
  filter(went_below) |> 
  group_by(event_id) |> 
  summarise(end_frame = first(game_clock)) |> 
  filter(event_id %in% start_frames$event_id)

# end_frames <- tracking_ball |> 
#   mutate(
#     above_rim = radius > 10,
#     went_below = lag(above_rim, default = FALSE) & !above_rim  
#   ) |> 
#   left_join(start_frames |> select(start_frame = game_clock, event_id), by = "event_id") |>
#   filter(game_clock > start_frame) |> 
#   filter(went_below) |> 
#   group_by(event_id) |> 
#   summarise(end_frame = first(game_clock)) |> 
#   filter(event_id %in% start_frames$event_id)

# combine start and end frames, filter for when start frames come before end frames
start_and_end_frames <- start_frames |> 
  select(start_frame = game_clock, event_id) |> 
  left_join(end_frames) |> 
  select(event_id, start_frame, end_frame) |> 
  distinct(event_id, .keep_all = TRUE) |> 
  filter(!is.na(end_frame)) |> 
  filter(start_frame > end_frame)

# frame plot to sanity check start and end frames
frame_plot <- function(eventid){
  start_frame <- start_and_end_frames |> filter(event_id == eventid) |> pull(start_frame)
  end_frame <- start_and_end_frames |> filter(event_id == eventid) |> pull(end_frame)
  tracking_ball |> 
    filter(event_id == eventid) |> 
    ggplot(aes(-game_clock, radius)) +
    geom_point() +
    geom_vline(xintercept = -start_frame, color = "red") +
    geom_vline(xintercept = -end_frame, color = "red")
    #geom_vline(xintercept = -614.15, color = "blue")
}
frame_plot(start_and_end_frames$event_id[1])

# get all ball tracking within the start and end frames of each shot
shot_sequences <- tracking_ball |> 
  inner_join(start_and_end_frames, by = "event_id") |>
  filter(game_clock <= start_frame & game_clock >= end_frame)

# Get R² values for each event
parabola_fits <- shot_sequences |> 
  filter(vx < 0) |> 
  group_by(event_id) |> 
  nest() |> 
  mutate(
    model = map(data, ~ lm(radius ~ poly(x_loc, 2, raw = TRUE), data = .x)),
    model_info = map(model, glance)
  ) |> 
  unnest(model_info) |> 
  select(event_id, r.squared)

# Filter out "bad" fits
good_parabolas <- parabola_fits |> 
  filter(r.squared > 0.9) |> 
  pull(event_id)

shot_sequences <- shot_sequences |> 
  filter(event_id %in% good_parabolas) |> 
  filter(vx < 0)

shot_sequences <- shot_sequences |> 
  group_by(event_id) |> 
  mutate(vx = c(NA, diff(x_loc)),
         vy = c(NA, diff(y_loc)),
         vz = radius - lag(radius),
         ax = c(NA, diff(vx)),
         ay = c(NA, diff(vy)),
         az = c(NA, diff(vz)),
         acceleration = sqrt(ax^2 + ay^2 + az^2))

shot_peaks <- shot_sequences |> 
  group_by(event_id) |> 
  slice_max(radius) |> 
  select(event_id, shot_peak = game_clock)

shot_sequences <- shot_sequences |> 
  left_join(shot_peaks) |> 
  group_by(event_id) |> 
  filter(!(radius > lag(radius) & game_clock < shot_peak)) |> 
  ungroup()

unique(shot_sequences$event_id)

# sanity check ball x and ball z for each shot
x_and_z_plot <- function(eventid){
  shot_sequences |> 
    filter(event_id == eventid) |> 
    filter(vx < 0) |> 
    ggplot(aes(x_loc, radius)) +
    geom_point()
}
x_and_z_plot(351)
# concerning <- c(43, 325, 365)

# calculate mu zero as seen in the paper
library(broom)
beta_hats <- shot_sequences |> 
  mutate(x2 = x_loc^2,
         y2 = y_loc^2,
         xy = x_loc * y_loc) |> 
  group_by(event_id) |> 
  summarise(
    model = list(lm(radius ~ x_loc + y_loc + x2 + y2 + xy, data = cur_data())),
    .groups = "drop"
  ) |> 
  mutate(beta_hat = map(model, coef)) |>  # Extract coefficients
  select(event_id, beta_hat) |> 
  unnest_wider(beta_hat, names_sep = "")  # Expand the coefficients into separate rows

colnames(beta_hats) <- c("event_id", "beta0", "beta1", "beta2", "beta3", "beta4", "beta5")

beta_hat_matrix <- beta_hats |> 
  select(-event_id) |> 
  as.matrix()

mu_zero <- colMeans(beta_hat_matrix, na.rm = TRUE)
lambda_zero <- solve(var(beta_hat_matrix))

# get shooter location for each shot
shooter_location_shot <- shots_fixed |> 
  select(event_id = game_event_id, player_id, quarter, game_clock = shot_time) |> 
  filter(event_id %in% shot_sequences$event_id) |> 
  inner_join(tracking) |> 
  mutate(y_loc = ifelse(x_loc > 47, 50 - y_loc, y_loc),
         x_loc = ifelse(x_loc > 47, 94 - x_loc, x_loc))

shooter_location_shot |> 
  ggplot(aes(x_loc, y_loc)) +
  geom_point() +
  geom_vline(xintercept = 5.5, color = "red")
  
shot_sequences <- shot_sequences |> 
  filter(event_id %in% shooter_location_shot$event_id)

# location of basket: x_loc 5.5, y_loc 25
shot_ids <- unique(shooter_location_shot$event_id)
basket_locations <- data.frame(
  event_id = rep(shot_ids,2)
) |> 
  mutate(x_loc = 5.5, y_loc = 25)

shooter_locations <- bind_rows(shooter_location_shot, shooter_location_shot) |> 
  select(event_id, x_loc, y_loc)

pseudo_x <- bind_rows(shooter_locations, basket_locations)

# Function to compute mu1 for a single event_id
compute_mu1_lambda1 <- function(df, mu0, lambda0) {
  # create the x matrix
  X1 <- cbind(
    1,                # Column of ones
    df$x_loc,         # x
    df$y_loc,         # y
    df$x_loc^2,       # x^2
    df$y_loc^2,       # y^2
    df$x_loc * df$y_loc # xy
  )
  # create the z matrix
  Z1 <- matrix(c(7, 7, 10, 10), nrow = 4, ncol = 1)
  mu1 <- solve(t(X1) %*% X1 + lambda0) %*% (lambda0 %*% mu0 + t(X1) %*% Z1)
  lambda1 <- t(X1) %*% X1 + lambda0
  return(list(mu1 = mu1, lambda1 = lambda1))
}

# calculate mu1 and lambda1 for every shot
mu_one <- pseudo_x |> 
  group_by(event_id) |> 
  summarise(
    beta0 = compute_mu1_lambda1(cur_data(), mu_zero, lambda_zero)$mu1[1, 1],
    beta1 = compute_mu1_lambda1(cur_data(), mu_zero, lambda_zero)$mu1[2, 1],
    beta2 = compute_mu1_lambda1(cur_data(), mu_zero, lambda_zero)$mu1[3, 1],
    beta3 = compute_mu1_lambda1(cur_data(), mu_zero, lambda_zero)$mu1[4, 1],
    beta4 = compute_mu1_lambda1(cur_data(), mu_zero, lambda_zero)$mu1[5, 1],
    beta5 = compute_mu1_lambda1(cur_data(), mu_zero, lambda_zero)$mu1[6, 1]
  )

lambda_one <- pseudo_x |> 
  group_by(event_id) |> 
  summarise(lambda1 = list(compute_mu1_lambda1(cur_data(), mu_zero, lambda_zero)$lambda1))

# calculate mu2 for every shot
compute_mu2_lambda2 <- function(df, mu1, lambda1) {
  # create the x matrix
  X2 <- cbind(
    1,                # Column of ones
    df$x_loc,         # x
    df$y_loc,         # y
    df$x_loc^2,       # x^2
    df$y_loc^2,       # y^2
    df$x_loc * df$y_loc # xy
  )
  # create the z matrix
  Z2 <- matrix(df$radius, ncol = 1)
  eventid <- df$event_id[1]
  #eventid <- unique(df$event_id)
  # print(df$event_id)
  mu_one <- as.matrix(mu1 |> filter(event_id == eventid) |> select(-event_id))
  mu_one <- mu_one[1, ]
  lambda_one <- lambda1 |> filter(event_id == eventid) |> pull(lambda1)
  lambda_one <- as.matrix(lambda_one[[1]])
  # print(mu_one)
  # print(lambda_one)
  # print(paste("lambda_one dimensions:", paste(dim(lambda_one), collapse = " x ")))
  # print(paste("mu_one dimensions:", paste(dim(mu_one), collapse = " x ")))
  mu2 <- solve(t(X2) %*% X2 + lambda_one) %*% (lambda_one %*% mu_one + t(X2) %*% Z2)
  lambda2 <- t(X2) %*% X2 + lambda_one
  return(list(mu2 = mu2, lambda2 = lambda2))
}
mu_two <- shot_sequences |> 
  group_by(event_id) |> 
  summarise(
    beta0 = compute_mu2_lambda2(cur_data() |> mutate(event_id = unique(event_id)), mu_one, lambda_one)$mu2[1, 1],
    beta1 = compute_mu2_lambda2(cur_data() |> mutate(event_id = unique(event_id)), mu_one, lambda_one)$mu2[2, 1],
    beta2 = compute_mu2_lambda2(cur_data() |> mutate(event_id = unique(event_id)), mu_one, lambda_one)$mu2[3, 1],
    beta3 = compute_mu2_lambda2(cur_data() |> mutate(event_id = unique(event_id)), mu_one, lambda_one)$mu2[4, 1],
    beta4 = compute_mu2_lambda2(cur_data() |> mutate(event_id = unique(event_id)), mu_one, lambda_one)$mu2[5, 1],
    beta5 = compute_mu2_lambda2(cur_data() |> mutate(event_id = unique(event_id)), mu_one, lambda_one)$mu2[6, 1]
  )

lambda_two <- shot_sequences |> 
  group_by(event_id) |> 
  summarise(lambda2 = list(compute_mu2_lambda2(cur_data() |> 
                                                 mutate(event_id = unique(event_id)),
                                               mu_one, lambda_one)$lambda2))

# get end locations for each shot
end_locations <- end_frames |>
  filter(event_id %in% shot_ids) |>
  inner_join(shooter_location_shot |> select(event_id, quarter, player_id)) |>
  mutate(player_id = -1) |>
  rename(game_clock = end_frame) |>
  inner_join(tracking) |> 
  mutate(y_loc = ifelse(x_loc > 47, 50 - y_loc, y_loc),
         x_loc = ifelse(x_loc > 47, 94 - x_loc, x_loc))

end_locations <- shots_and_betas |> 
  group_by(event_id) |> 
  slice_min(game_clock)

# combine shot tracking and beta estimates
shots_and_betas <- shot_sequences |> 
  left_join(mu_two) |> 
  mutate(
    pred_z = beta0 + 
      beta1 * x_loc + 
      beta2 * y_loc + 
      beta3 * (x_loc^2) + 
      beta4 * (y_loc^2) + 
      beta5 * (x_loc * y_loc)
  )
library(patchwork)

# sanity check ball x and predicted ball z
x_and_predz_plot <- function(eventid){
  # pred_plot <- shots_and_betas |> 
  #   filter(event_id == eventid) |> 
  #   ggplot(aes(x_loc, pred_z)) +
  #   geom_point() +
  #   geom_path()
  #   labs(title = "predicted")
  actual_plot <- shots_and_betas |> 
    filter(event_id == eventid) |> 
    ggplot() +
    geom_point(aes(x = x_loc, y = radius)) +
    geom_path(aes(x = x_loc, y = pred_z))
  actual_plot
}
x_and_predz_plot(3)

# join shot sequences with quadratic regression
shots_and_mu_zero <- shot_sequences |> 
  left_join(beta_hats) |> 
  mutate(
    pred_z = beta0 + 
      beta1 * x_loc + 
      beta2 * y_loc + 
      beta3 * (x_loc^2) + 
      beta4 * (y_loc^2) + 
      beta5 * (x_loc * y_loc)
  )
# sanity check mu zero vs actual z
x_and_mu0_plot <- function(eventid){
  actual_plot <- shots_and_mu_zero |> 
    filter(event_id == eventid) |> 
    ggplot() +
    geom_point(aes(x = x_loc, y = radius)) +
    geom_path(aes(x = x_loc, y = pred_z))
  actual_plot
}
x_and_mu0_plot(3)

# shots_and_betamean <- shot_sequences |> 
#   mutate(beta0 = mu_zero["beta0"],
#          beta1 = mu_zero["beta1"],
#          beta2 = mu_zero["beta2"],
#          beta3 = mu_zero["beta3"],
#          beta4 = mu_zero["beta4"],
#          beta5 = mu_zero["beta5"],
#          pred_z = beta0 + 
#            beta1 * x_loc + 
#            beta2 * y_loc + 
#            beta3 * (x_loc^2) + 
#            beta4 * (y_loc^2) + 
#            beta5 * (x_loc * y_loc))
# x_and_betameans <- function(eventid){
#   actual_plot <- shots_and_betamean |> 
#     filter(event_id == eventid) |> 
#     ggplot() +
#     geom_point(aes(x = x_loc, y = radius)) +
#     geom_path(aes(x = x_loc, y = pred_z))
#   actual_plot
# }
# x_and_betameans(3)

unique(shots_and_betas$event_id)

# calculate shot depth and left-right distance
calculations <- end_locations |> 
  left_join(shooter_location_shot |> select(shooter_x = x_loc, shooter_y = y_loc,
                                            event_id, quarter)) |> 
  select(event_id, x_loc, y_loc, radius, shooter_x, shooter_y) |> 
  mutate(
    # Vector from shooter to hoop
    d_x = 5.5 - shooter_x,
    d_y = 25 - shooter_y,
    d_norm = sqrt(d_x^2 + d_y^2),
    d_hat_x = d_x / d_norm,
    d_hat_y = d_y / d_norm,
    
    # Rim front
    front_x = 5.5 - 0.75 * d_hat_x,
    front_y = 25 - 0.75 * d_hat_y,
    
    # Tangent line to rim
    tangent_slope = -d_hat_x / d_hat_y,
    tangent_intercept = front_y - tangent_slope * front_x,
    
    # Shot depth (distance along shot direction)
    shot_depth = (tangent_slope * x_loc - y_loc + tangent_intercept) / sqrt(tangent_slope^2 + 1),
    
    # Left-right accuracy (distance perpendicular to shot direction)
    shot_line_slope = d_y / d_x,
    shot_line_intercept = shooter_y - shot_line_slope * shooter_x,
    left_right_accuracy = (shot_line_slope * x_loc - y_loc + shot_line_intercept) / sqrt(shot_line_slope^2 + 1)
  )


# calculations <- end_locations |> 
#   left_join(shooter_location_shot |> select(shooter_x = x_loc, shooter_y = y_loc,
#                                             event_id, quarter)) |> 
#   select(event_id, x_loc, y_loc, radius, shooter_x, shooter_y) |> 
#   mutate(
#     slope = (25 - shooter_y) / (5.5 - shooter_x), # Calculate slope of line from shooter to rim
#     intercept = shooter_y - slope * shooter_x,   # y-intercept of the line
#     left_right = abs(slope * x_loc - y_loc + intercept) / sqrt(slope^2 + 1) # Point-line distance formula
#   ) |> 
#   mutate(
#     # Direction vector from shooter to hoop
#     d_x = 5.5 - shooter_x,
#     d_y = 25 - shooter_y,
#     d_norm = sqrt(d_x^2 + d_y^2),
#     # Unit vector components
#     d_hat_x = d_x / d_norm,
#     d_hat_y = d_y / d_norm,
#     # Adjusted front of the hoop
#     front_x = 5.5 - .75 * d_hat_x,
#     front_y = 25 - .75 * d_hat_y,
#     # Tangent line slope
#     tangent_slope = -d_hat_x / d_hat_y,
#     # Tangent line intercept
#     tangent_intercept = front_y - tangent_slope * front_x,
#     # Shot depth
#     shot_depth = (tangent_slope * x_loc - y_loc + tangent_intercept) / sqrt(tangent_slope^2 + 1)
#   )

# y = mx + b
# mx - 1y + b = 0 so A = m, B = -1, C = b

depth_left_right <- calculations |> 
  select(event_id, left_right_accuracy, shot_depth)

mean(depth_left_right$left_right_accuracy)
mean(depth_left_right$shot_depth)

# calculate entry angle
entry_angles <- shot_sequences |> 
  group_by(event_id) |> 
  slice_tail(n = 5) |> 
  mutate(entry_angle_radians = atan(abs(vz) / sqrt(vx^2 + vy^2)),
         entry_angle_degrees = entry_angle_radians * (180 / pi)) |> 
  ungroup() |> 
  select(event_id, entry_angle_degrees)

mean(entry_angles$entry_angle_degrees, na.rm = TRUE)

entry_angle_summary <- entry_angles |> 
  group_by(event_id) |> 
  summarise(entry_angle = mean(entry_angle_degrees, na.rm = TRUE)) |> 
  ungroup()

all_metrics <- depth_left_right |> 
  left_join(entry_angle_summary) |> 
  left_join(shots_fixed |> select(made_shot = shot_made_flag, event_id = game_event_id))

all_metrics |> 
  ggplot(aes(entry_angle)) +
  geom_histogram()

all_metrics |> 
  ggplot(aes(left_right_accuracy * 12, shot_depth * 12, color = as.factor(made_shot))) +
  geom_point() +
  xlim(-50, 50) +
  ylim(-50, 50)


# solve for when the z of ball crosses 10 feet
# Function to solve for y given x
solve_y_given_x <- function(x, beta0, beta1, beta2, beta3, beta4, beta5, Z_target = 10) {
  # Quadratic form: A * y^2 + B * y + C = 0
  A <- beta4
  B <- beta2 + beta5 * x
  C <- beta0 + beta1 * x + beta3 * x^2 - Z_target
  
  # Compute discriminant
  D <- B^2 - 4 * A * C
  
  if (D < 0) {
    return(numeric(0))  # No real solutions
  }
  
  # Compute solutions for y
  y1 <- (-B + sqrt(D)) / (2 * A)
  y2 <- (-B - sqrt(D)) / (2 * A)
  
  # Return solutions that satisfy the constraint 0 ≤ y ≤ 50
  valid_y <- c(y1, y2)[c(y1, y2) >= 0 & c(y1, y2) <= 50]
  
  return(valid_y)
}

# Define valid x values within [0, 47]
x_vals <- seq(0, 47, by = 0.1)  # Adjust step size if needed

# Function to compute solutions for each row in mu_two
compute_solutions <- function(row) {
  results <- map2_df(x_vals, map(x_vals, ~ solve_y_given_x(.x, row$beta0, row$beta1, row$beta2, 
                                                           row$beta3, row$beta4, row$beta5)), 
                     ~ tibble(event_id = row$event_id, x = .x, y = .y))
  
  return(results)
}

# Apply function to all rows in mu_two
solution_df <- mu_two %>%
  rowwise() %>%
  do(compute_solutions(.)) %>%
  ungroup()

solution_df |> 
  group_by(event_id) |> 
  summarise(solutions = n()) |> View()

solution_locations <- solution_df |> 
  mutate(dist = abs(x - 5.5) + abs(y - 25)) |>  # Compute distance metric
  group_by(event_id) |> 
  slice_min(dist, with_ties = FALSE) |>  # Select row with the smallest distance
  ungroup() |> 
  rename(x_end = x, y_end = y)

# solution_calculations <- solution_locations |> 
#   rename(x_loc = x_end, y_loc = y_end) |> 
#   mutate(radius = 10) |> 
#   left_join(shooter_location_shot |> select(shooter_x = x_loc, shooter_y = y_loc,
#                                             event_id, quarter)) |> 
#   select(event_id, x_loc, y_loc, radius, shooter_x, shooter_y) |> 
#   mutate(
#     slope = (25 - shooter_y) / (5.5 - shooter_x), # Calculate slope of line from shooter to rim
#     intercept = shooter_y - slope * shooter_x,   # y-intercept of the line
#     left_right = slope * x_loc - y_loc + intercept / sqrt(slope^2 + 1) # Point-line distance formula
#   ) |> 
#   mutate(
#     # Direction vector from shooter to hoop
#     d_x = 5.5 - shooter_x,
#     d_y = 25 - shooter_y,
#     d_norm = sqrt(d_x^2 + d_y^2),
#     # Unit vector components
#     d_hat_x = d_x / d_norm,
#     d_hat_y = d_y / d_norm,
#     # Adjusted front of the hoop
#     front_x = 5.5 - .75 * d_hat_x,
#     front_y = 25 - .75 * d_hat_y,
#     # Tangent line slope
#     tangent_slope = -d_hat_x / d_hat_y,
#     # Tangent line intercept
#     tangent_intercept = front_y - tangent_slope * front_x,
#     # Shot depth
#     shot_depth = (tangent_slope * x_loc - y_loc + tangent_intercept) / sqrt(tangent_slope^2 + 1)
#   )

solution_calculations <- solution_locations |> 
  rename(x_loc = x_end, y_loc = y_end) |> 
  mutate(radius = 10) |> 
  left_join(shooter_location_shot |> 
              select(shooter_x = x_loc, shooter_y = y_loc, event_id, quarter)) |> 
  select(event_id, x_loc, y_loc, radius, shooter_x, shooter_y) |> 
  mutate(
    # Direction vector from shooter to hoop
    d_x = 5.5 - shooter_x,
    d_y = 25 - shooter_y,
    d_norm = sqrt(d_x^2 + d_y^2),
    
    # Unit direction vector components
    d_hat_x = d_x / d_norm,
    d_hat_y = d_y / d_norm,
    
    # Front of the rim (adjusted)
    front_x = 5.5 - 0.75 * d_hat_x,
    front_y = 25 - 0.75 * d_hat_y,
    
    # Tangent to rim at entry point
    tangent_slope = -d_hat_x / d_hat_y,
    tangent_intercept = front_y - tangent_slope * front_x,
    
    # Shot depth: how far "into the rim" the shot goes
    shot_depth = (tangent_slope * x_loc - y_loc + tangent_intercept) / sqrt(tangent_slope^2 + 1),
    
    # Shot line from shooter to rim (for left-right accuracy)
    shot_line_slope = d_y / d_x,
    shot_line_intercept = shooter_y - shot_line_slope * shooter_x,
    
    # Left-right accuracy: lateral deviation from the shot line
    left_right_accuracy = (shot_line_slope * x_loc - y_loc + shot_line_intercept) / sqrt(shot_line_slope^2 + 1)
  )


solution_depth_left_right <- solution_calculations |> 
  select(event_id, left_right_accuracy, shot_depth)

mean(solution_depth_left_right$left_right_accuracy)
mean(solution_depth_left_right$shot_depth)

solution_depth_left_right |> 
  left_join(shots_fixed |> select(made_shot = shot_made_flag, event_id = game_event_id)) |> 
  ggplot(aes(left_right_accuracy * 12, shot_depth * 12, color = as.factor(made_shot))) +
  geom_point() +
  xlim(-50, 50) +
  ylim(-50, 50)

shot_sequences <- shot_sequences |> 
  group_by(event_id) |> 
  mutate(time = max(game_clock) - game_clock) # Normalize time so it starts at 0

# smooth x and y position based on time
# Fit a linear regression model for each event_id
x_regression_results <- shot_sequences %>%
  group_by(event_id) %>%
  summarise(model = list(lm(x_loc ~ time, data = cur_data())),
            .groups = "drop") %>%
  mutate(tidy_model = map(model, tidy)) %>%
  unnest(tidy_model) %>%
  select(event_id, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate, names_prefix = "") %>%
  rename(b_x = `(Intercept)`, a_x = time) |> 
  select(event_id, a_x, b_x)

y_regression_results <- shot_sequences %>%
  group_by(event_id) %>%
  summarise(model = list(lm(y_loc ~ time, data = cur_data())),
            .groups = "drop") %>%
  mutate(tidy_model = map(model, tidy)) %>%
  unnest(tidy_model) %>%
  select(event_id, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate, names_prefix = "") %>%
  rename(b_y = `(Intercept)`, a_y = time) |> 
  select(event_id, a_y, b_y)

xy_smoothing <- x_regression_results |> 
  inner_join(y_regression_results) |> 
  mutate(x_dir = a_x / sqrt(a_x^2 + a_y^2), 
         y_dir = a_y / sqrt(a_x^2 + a_y^2))

smooth_and_beta <- xy_smoothing |> 
  left_join(mu_two)
  
# Function to solve for x
solve_x <- function(beta0, beta1, beta2, beta3, beta4, beta5, a_x, a_y, b_x, b_y) {
    
    if (a_x == 0) return(NA)  # Avoid division by zero
    
    slope <- a_y / a_x
    
    # Quadratic equation coefficients
    A <- beta3 + beta4 * slope^2
    B <- beta1 + beta2 * slope + beta5 * slope + 2 * beta4 * slope * (-slope * b_x + b_y) + beta5 * (-slope * b_x + b_y)
    C <- beta0 + beta2 * (-slope * b_x + b_y) + beta4 * (-slope * b_x + b_y)^2 + beta5 * b_x * (-slope * b_x + b_y) - 10
    
    discriminant <- B^2 - 4 * A * C
    
    if (discriminant < 0) {
      return(NA)  # No real solutions
    } else if (discriminant == 0) {
      return(-B / (2 * A))  # One real solution
    } else {
      x1 <- (-B + sqrt(discriminant)) / (2 * A)
      x2 <- (-B - sqrt(discriminant)) / (2 * A)
      return(c(x1, x2))  # Two real solutions
    }
  }

# Apply to each row of the dataframe
new_solution_df <- smooth_and_beta %>%
  rowwise() %>%
  mutate(x_solution = list(solve_x(beta0, beta1, beta2, beta3, beta4, beta5, a_x, a_y, b_x, b_y))) %>%
  unnest(x_solution) |> 
  mutate(y_solution = a_y / a_x * (x_solution - b_x) + b_y)
  
  
# start_locs <- shooter_location_shot |> 
#   select(event_id, x_start = x_loc, y_start = y_loc)
# 
# filtered_locs <- solution_df %>%
#   inner_join(start_locs, by = "event_id") %>%
#   inner_join(xy_smoothing, by = "event_id") %>%
#   mutate(
#     t_x = (x - x_start) / x_dir,  # Solve for t using x equation
#     t_y = (y - y_start) / y_dir   # Solve for t using y equation
#   ) |> 
#   filter(abs(t_x - t_y) < 0.1 & t_x >= 0)  # Ensure consistency and positive t

# gradient of z: z = b0 + b1x + b2y + b3x^2 + b4y^2 + b5xy
# dz/dx = b1 + 2 * b3x + b5y
# dz/dy = b2 + 2 * b4y + b5x

# recalculate entry angle using smoothed x and y locations
solution_entry_angles <- solution_locations |> 
  select(-dist) |> 
  left_join(xy_smoothing |> select(event_id, ends_with("_dir"))) |> 
  mutate(z = 10) |> 
  left_join(mu_two) |> 
  mutate(dz_dx = beta1 + 2 * beta3 * x_end + beta5 * y_end,
         dz_dy = beta2 + 2 * beta4 * y_end + beta5 * x_end,
         dot_product = dz_dx * x_dir + dz_dy * y_dir,
         entry_angle_radians = abs(atan(dot_product / sqrt(dz_dx^2 + dz_dy^2))),
         entry_angle_degrees = entry_angle_radians * (180 / pi))

mean(solution_entry_angles$entry_angle_degrees)
