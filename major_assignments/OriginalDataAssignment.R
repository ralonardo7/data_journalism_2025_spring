library(dplyr)
library(ggplot2)

df <- play_off_box_scores_2010_2024

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







library(dplyr)
library(ggplot2)

df <- play_off_box_scores_2010_2024

df <- df |>
  mutate(
    minutes = as.numeric(minutes),
    missed_fg = fieldGoalsAttempted - fieldGoalsMade,
    missed_ft = freeThrowsAttempted - freeThrowsMade
  ) |>
  filter(!is.na(minutes) & minutes > 0)

efficiency_df <- df |>
  mutate(
    efficiency = ((points + reboundsTotal + assists + steals + blocks) -
                    (turnovers + missed_fg + missed_ft)) / minutes * 36
  )

player_efficiency_summary <- efficiency_df |>
  group_by(season_year, personName) |>
  summarise(
    avg_efficiency = mean(efficiency, na.rm = TRUE),
    games_played = n(),
    avg_minutes = mean(minutes, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(games_played >= 15) 

efficiency_ranked <- player_efficiency_summary |>
  group_by(season_year) |>
  arrange(desc(avg_efficiency)) |>
  mutate(efficiency_rank = row_number()) |>
  ungroup()

top10_all_time <- efficiency_ranked |>
  arrange(desc(avg_efficiency)) |>
  slice_head(n = 10)

print(top10_all_time)

top10_2024 <- efficiency_ranked |>
  filter(season_year == 2024) |>
  slice_max(avg_efficiency, n = 10)

ggplot(top10_2024, aes(x = reorder(personName, avg_efficiency), y = avg_efficiency, fill = personName)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top 10 Most Efficient Playoff Players (2024)",
    x = "Player",
    y = "Avg Efficiency (per 36 min)"
  ) +
  theme_minimal()






library(dplyr)
library(ggplot2)

nba_playoffs <- play_off_box_scores_2010_2024 |>
  filter(game_date >= as.Date("2010-04-15"))  

player_efficiency <- nba_playoffs |>
  group_by(personId, personName) |>
  summarise(
    games_played = n(),
    total_minutes = sum(minutes, na.rm = TRUE),
    total_FGM = sum(fieldGoalsMade, na.rm = TRUE),
    total_FGA = sum(fieldGoalsAttempted, na.rm = TRUE),
    total_3PM = sum(threePointersMade, na.rm = TRUE),
    total_3PA = sum(threePointersAttempted, na.rm = TRUE),
    total_FTM = sum(freeThrowsMade, na.rm = TRUE),
    total_FTA = sum(freeThrowsAttempted, na.rm = TRUE)
  ) |>
  ungroup()

player_efficiency <- player_efficiency |>
  filter(total_minutes >= 1000, total_FGA > 150, total_FTA > 100, total_3PA > 100)

player_efficiency <- player_efficiency |>
  mutate(
    FG_Percentage = total_FGM / total_FGA,
    TP_Percentage = total_3PM / total_3PA,
    FT_Percentage = total_FTM / total_FTA,

    Scoring_Efficiency_Index = (FG_Percentage * 0.4) + 
      (TP_Percentage * 0.3) + 
      (FT_Percentage * 0.3)
  )

top_scorers <- player_efficiency |>
  arrange(desc(Scoring_Efficiency_Index)) |>
  slice_head(n = 20)