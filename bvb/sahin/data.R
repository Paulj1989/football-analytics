
# coach comparison data

buli <- 
  worldfootballR::fb_match_results(
    country = "GER", gender = "M", 
    season_end_year = c(2023, 2025), tier = "1st"
  )

buli_urls <- 
  buli |> 
  janitor::clean_names() |> 
  dplyr::filter(home == "Dortmund" | away == "Dortmund") |>
  dplyr::mutate(wk = as.numeric(wk)) |>
  dplyr::filter(wk <= 12) |> 
  dplyr::pull(match_url)

 bvb_summary_raw <- 
  worldfootballR::fb_advanced_match_stats(
    buli_urls, stat_type = "summary", team_or_player = "team",
    time_pause = 6
  )

bvb_summary_stats <-
  bvb_summary_raw |> 
  janitor::clean_names(replace = c("xG_Expected" = "xg", "Sh" = "shots")) |>  
  dplyr::mutate(
    season = lubridate::year(match_date), 
    for_against = dplyr::case_when(
      team == "Dortmund" ~ "for", 
      team != "Dortmund" ~ "against"
    )
  ) |> 
  dplyr::select(season, match_date, matchweek, for_against, gls, xg, npxg, shots) |> 
  tidyr::pivot_wider(
    names_from = for_against, 
    values_from = c(gls, xg, npxg, shots), 
    names_glue = "{.value}_{for_against}"
  ) |> 
  dplyr::mutate(round = dplyr::row_number(match_date), .by = season) |> 
  dplyr::select(
    season, round, 
    dplyr::starts_with("gls"), 
    dplyr::starts_with("xg"),
    dplyr::starts_with("npxg"),
    dplyr::starts_with("shots")
  ) |> 
  dplyr::arrange(season, round)

bvb_possession_raw <-
  worldfootballR::fb_advanced_match_stats(
    buli_urls, stat_type = "possession", team_or_player = "team",
    time_pause = 6
  )

bvb_possession_stats <-
  bvb_possession_raw |> 
  janitor::clean_names(
    replace = c(
      "Att Pen" = "pen",
      "Touches_Touches" = "touches"
    )
  ) |> 
  dplyr::mutate(
    season = lubridate::year(match_date), 
    for_against = dplyr::case_when(
      team == "Dortmund" ~ "for", 
      team != "Dortmund" ~ "against"
    )
  ) |> 
  dplyr::select(season, match_date, matchweek, for_against, touches, pen_touches) |> 
  tidyr::pivot_wider(
    names_from = for_against, 
    values_from = c(touches, pen_touches), 
    names_glue = "{.value}_{for_against}"
  ) |> 
  dplyr::mutate(round = dplyr::row_number(match_date), .by = season) |> 
  dplyr::select(season, round, dplyr::contains("touches")) |> 
  dplyr::arrange(season, round)

bvb_stats <- 
  bvb_summary_stats |> 
  dplyr::full_join(bvb_possession_stats)

readr::write_csv(bvb_stats, here::here("bvb", "sahin", "data", "bvb_stats.csv"))

# home vs away data

buli_results_raw <- 
  worldfootballR::fb_match_results(
    country = "GER", gender = "M", season_end_year = 2025, tier = "1st"
)


home_away_raw <- 
  worldfootballR::fb_season_team_stats(
    country = "GER", gender = "M", season_end_year = 2025, 
    tier="1st", stat_type = "league_table_home_away"
  )

home_away_splits <-
  home_away_raw |> 
  janitor::clean_names(
    replace = c("xG" = "xg", "xgD" = "xgd", "xgA" = "xga")
  ) |> 
  dplyr::select(squad, dplyr::starts_with(c("mp", "xg"))) |>
  dplyr::select(!dplyr::contains("per_90")) |> 
  dplyr::mutate(
    dplyr::across(dplyr::contains(c("home", "away")), ~ as.numeric(.x))
  ) |> 
  dplyr::mutate(
    across(dplyr::contains("xg") & dplyr::contains("home"), ~ .x/mp_home),
    across(dplyr::contains("xg") & dplyr::contains("away"), ~ .x/mp_away),
  )

home_away_splits |> 
  dplyr::mutate(
    abs_diff = xgd_home - xgd_away,
    .by = squad
  ) |> 
  dplyr::mutate(
    mean_diff = mean(abs_diff)
  ) |> 
  dplyr::summarise(
    pct_diff = abs_diff/mean_diff,
    .by = squad
  ) |> 
  dplyr::arrange(desc(pct_diff))
  

home_away_splits |> dplyr::select(squad, xgd_home, xgd_away)
