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

#After taking a look at the data, it appears as though I'll be able to do a decent amount with this information. There are a whopping 33 columns, all of which contain playoff data I can use. There are 31,185 columns of player performances in the playoffs over a course of 15 years. Some of the flaws include lack of scoring by quarter, or with a certain amount of time remaining in the game. Because of this, I can't really look at clutch statistics for various players over the end of the game, which is something that I wanted to do and is integral to playoff success. The other limits is that is is only 15 years worth of data. If there were more, we could compare eras of NBA playoff play. As for terms I needed to find out, there were things like teamId and gameId that I needed to look more into, but they made sense after my initial research. 


