library(gganimate)
library(sportyR)
nba_court <- geom_basketball(league = "nba")
play_animation <- function(eventid){
  event_tracking <- tracking |> 
    filter(event_id == eventid) |> 
    mutate(color = case_when(
      team_id == 1610612756 ~ "#1D1160",
      team_id == -1 ~ "#F88158",
      team_id == 1610612737 ~ "#C8102E"
    ))
  nba_court <- geom_basketball(league = "nba", display_range = "full", x_trans = 50, y_trans = 25)
  nba_court + 
    geom_point(data = event_tracking, 
               aes(x_loc,  y_loc, color = color, 
                   size = ifelse(radius == 0, 3, radius))) +
    scale_color_identity() +
    transition_time(-event_tracking$game_clock) +
    guides(legend = "none")
  # event_tracking |> 
  #   ggplot(aes(x_loc, y_loc, color = color, size = ifelse(team_id == -1, radius, 3))) +
  #   geom_point(size = 4) +
  #   scale_color_identity() +
  #   transition_time(game_clock)
}
play_animation(154)
animate(play_animation(3), fps = 5)
unique(tracking$team_id)
