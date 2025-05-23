library(dplyr)
library(ggplot2)

df <- play_off_box_scores_2010_2024

#Q1. Which players have had the greatest impact in a playoff run?

player_impact_summary <- df |>
  group_by(season_year, teamSlug, personName) |>
  summarize(
    total_points = sum(points, na.rm = TRUE),
    total_assists = sum(assists, na.rm = TRUE),
    total_rebounds = sum(reboundsTotal, na.rm = TRUE),
    total_plus_minus = sum(plusMinusPoints, na.rm = TRUE),
    games_played = n(),
    .groups = "drop"
  ) |>

  mutate(
    impact_score = total_points + total_assists * 1.5 + total_rebounds * 1.2 + total_plus_minus * 1.5
  )

ranked_players <- player_impact_summary |>
  group_by(season_year, teamSlug) |>
  arrange(desc(impact_score)) |>
  mutate(team_rank = row_number()) |>
  ungroup()

top_3_per_team <- ranked_players |>
  filter(team_rank <= 3)

print(top_3_per_team)

top_player_2024 <- ranked_players |>
  filter(season_year == 2024, team_rank == 1)

ggplot(top_player_2024, aes(x = reorder(teamSlug, impact_score), y = impact_score, fill = personName)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top Playoff Performer per Team (2024)", x = "Team", y = "Impact Score") +
  theme_minimal()

#After taking a look at the data, it appears as though I'll be able to do a decent amount with this information. There are a whopping 33 columns, all of which contain playoff data I can use. There are 31,185 columns of player performances in the playoffs over a course of 15 years. Some of the flaws include lack of scoring by quarter, or with a certain amount of time remaining in the game. Because of this, I can't really look at clutch statistics for various players over the end of the game, which is something that I wanted to do and is integral to playoff success. The other limits is that is is only 15 years worth of data. If there were more, we could compare eras of NBA playoff play. As for terms I needed to find out, there were things like teamId and gameId that I needed to look more into, but they made sense after my initial research. 

#Q2: Which NBA playoff players are the most efficient scorers (2010–2024)?

#Here, I wanted to figure out which players basically hit their shots in the playoffs. I made another formula again, and when I saw my results, the only thig I think I'd need to toggle or change is the minutes played variable, because then it's going to look at regular rotation players that play all the time in the playoffs, instead of players who might get in here and there. 

library(dplyr)
library(ggplot2)

df <- play_off_box_scores_2010_2024

min_minutes_played <- 10000

efficiency_summary <- df |>
  filter(minutes >= min_minutes_played) |>
  group_by(season_year, teamSlug, personName) |>
  summarize(
    total_points = sum(points, na.rm = TRUE),
    fieldGoalsMade = sum(fieldGoalsMade, na.rm = TRUE),
    fieldGoalsAttempted = sum(fieldGoalsAttempted, na.rm = TRUE),
    threePointersMade = sum(threePointersMade, na.rm = TRUE),
    threePointersAttempted = sum(threePointersAttempted, na.rm = TRUE),
    freeThrowsMade = sum(freeThrowsMade, na.rm = TRUE),
    freeThrowsAttempted = sum(freeThrowsAttempted, na.rm = TRUE),
    games_played = n(),
    .groups = "drop"
  ) |>
  mutate(

    fg_percentage = fieldGoalsMade / fieldGoalsAttempted,
    threeP_percentage = threePointersMade / threePointersAttempted,
    ft_percentage = freeThrowsMade / freeThrowsAttempted,

    efficiency_score = fg_percentage * 0.4 + threeP_percentage * 0.3 + ft_percentage * 0.3
  )

ranked_efficiency_players <- efficiency_summary |>
  group_by(season_year, teamSlug) |>
  arrange(desc(efficiency_score)) |>
  mutate(team_rank = row_number()) |>
  ungroup()

top_efficiency_per_team <- ranked_efficiency_players |>
  filter(team_rank <= 3)

top_efficiency_2024 <- ranked_efficiency_players |>
  filter(season_year == 2024, team_rank == 1)

#Q3. Which teams rely most heavily on a single player during the playoffs?

library(dplyr)
library(ggplot2)

df <- play_off_box_scores_2010_2024 |>
  mutate(minutes = as.numeric(minutes))

player_team_stats <- df |>
  group_by(season_year, teamSlug, personName) |>
  summarize(
    total_points = sum(points, na.rm = TRUE),
    total_assists = sum(assists, na.rm = TRUE),
    total_minutes = sum(minutes, na.rm = TRUE),
    .groups = "drop"
  )

team_totals <- player_team_stats |>
  group_by(season_year, teamSlug) |>
  summarize(
    team_points = sum(total_points, na.rm = TRUE),
    team_assists = sum(total_assists, na.rm = TRUE),
    team_minutes = sum(total_minutes, na.rm = TRUE),
    .groups = "drop"
  )

player_share <- player_team_stats |>
  inner_join(team_totals, by = c("season_year", "teamSlug")) |>
  mutate(
    point_share = total_points / team_points,
    assist_share = total_assists / team_assists,
    minutes_share = total_minutes / team_minutes,
    reliance_score = point_share * 0.5 + assist_share * 0.3 + minutes_share * 0.2
  )

most_reliant_players <- player_share |>
  group_by(season_year, teamSlug) |>
  arrange(desc(reliance_score)) |>
  slice(1) |>
  ungroup()

top_reliant_teams <- most_reliant_players |>
  arrange(desc(reliance_score)) |>
  select(season_year, teamSlug, personName, point_share, assist_share, minutes_share, reliance_score)

print(head(top_reliant_teams, 10))

#4. What are the most well-rounded (all-around) playoff single-game performances from 2010–2024?

library(dplyr)
library(ggplot2)

df <- play_off_box_scores_2010_2024 |>
  mutate(minutes = as.numeric(minutes))  

well_rounded_games <- df |>
  mutate(
    well_rounded_score = points +
      reboundsTotal +
      assists +
      1.5 * steals +
      1.5 * blocks -
      turnovers
  ) |>
  select(season_year, game_date, teamSlug, personName, minutes, points, reboundsTotal,
         assists, steals, blocks, turnovers, well_rounded_score)

well_rounded_filtered <- well_rounded_games |>
  filter(minutes >= 20) 

top_well_rounded <- well_rounded_filtered |>
  arrange(desc(well_rounded_score)) |>
  head(20)

print(top_well_rounded)

#5. Which players are defensive anchors in the playoffs?

library(dplyr)
library(ggplot2)

df <- play_off_box_scores_2010_2024 |>
  mutate(minutes = as.numeric(minutes)) 

defensive_summary <- df |>
  group_by(personName, season_year) |>
  summarize(
    total_minutes = sum(minutes, na.rm = TRUE),
    total_def_rebounds = sum(reboundsDefensive, na.rm = TRUE),
    total_steals = sum(steals, na.rm = TRUE),
    total_blocks = sum(blocks, na.rm = TRUE),
    total_plus_minus = sum(plusMinusPoints, na.rm = TRUE),
    games_played = n(),
    .groups = "drop"
  ) |>
  filter(total_minutes >= 100) |>
  
  mutate(
    defensive_impact_score = total_blocks * 2 +
      total_steals * 1.5 +
      total_def_rebounds * 1.2 +
      total_plus_minus
  )

top_defenders <- defensive_summary |>
  arrange(desc(defensive_impact_score)) |>
  head(20)

print(top_defenders)