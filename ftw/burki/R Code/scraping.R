## scrape function

buli_age_utility_info <- function(team_name, team_num, season, total_minutes) {
  ## get age + minutes played
  session <- bow(glue::glue("https://www.transfermarkt.com/{team_name}/leistungsdaten/verein/{team_num}/plus/1?reldata=L1%26{season}/"))
  
  # grab name from photo element instead
  result_name <- scrape(session) %>% 
    html_nodes("#yw1 .bilderrahmen-fixed") %>% 
    html_attr("title") 
  
  # grab age
  result_age <- scrape(session) %>% 
    html_nodes(".posrela+ .zentriert") %>% 
    html_text()
  
  # grab minutes played in league
  result_mins <- scrape(session) %>% 
    html_nodes("td.rechts") %>% 
    html_text()
  
  ## get length
  session <- bow(glue::glue("https://www.transfermarkt.com/{team_name}/kader/verein/{team_num}/saison_id/{season}/plus/1"))
  
  result_name2 <- scrape(session) %>% 
    html_nodes("#yw1 .bilderrahmen-fixed") %>% 
    html_attr("title") 
  
  result_bday <- scrape(session) %>% 
    html_nodes(".posrela+ .zentriert") %>% 
    html_text()
  
  result_joinedteam <- scrape(session) %>% 
    html_nodes("td:nth-child(7)") %>% 
    html_text()
  
  result_leaveteam <- scrape(session) %>% 
    html_nodes("td:nth-child(9)") %>% 
    html_text()
  
  # place each vector into list
  resultados <- list(result_name, result_age, result_mins)
  
  col_name <- c("name", "age", "minutes")
  
  results_comb <- resultados %>% 
    reduce(cbind) %>% 
    as_tibble() %>% 
    set_names(col_name)
  
  ## join + bday
  resultados2 <- list(result_name2, result_bday, 
                      result_joinedteam, result_leaveteam)
  
  col_name2 <- c("name", "bday", "join", "leave")
  
  results_comb2 <- resultados2 %>% 
    reduce(cbind) %>% 
    as_tibble() %>% 
    set_names(col_name2)
  
  ## combine BOTH
  results_comb <- results_comb %>% 
    left_join(results_comb2) 
  
  # fix "strings" into proper formats, calculate % of minutes appeared
  all_team_minutes <- results_comb %>% 
    mutate(age = as.numeric(age),
           minutes = minutes %>% 
             str_replace("\\.", "") %>% 
             str_replace("'", "") %>% 
             as.numeric(),
           min_perc = (minutes / total_minutes) %>% round(digits = 3),
           bday = str_replace_all(bday, "\\(.*\\)", "") %>% mdy(),
           join = join %>% mdy(),
           join_age = interval(bday, join) / years(1),
           age_now = interval(bday, Sys.Date()) / years(1)) %>% 
    filter(!is.na(minutes)) %>% 
    separate(name, into = c("first_name", "last_name"), 
             sep = " ", fill = "left") %>%
    mutate(
      fname = if_else(!is.na(first_name), 
                      str_extract(first_name, "[A-Z]{1}"),
                      ""),
      player = case_when(
        !is.na(first_name) ~ glue("{fname}. {last_name}"),
        TRUE ~ last_name),
      team_name = team_name,
      team_name = case_when(
        team_name == "fc-bayern-munchen" ~ "Bayern München",
        team_name == "rasenballsport-leipzig" ~ "RB Leipzig", 
        team_name == "fc-schalke-04" ~ "Schalke 04",
        team_name == "hertha-bsc" ~ "Hertha Berlin",
        team_name == "1-fc-union-berlin" ~ "Union Berlin",
        team_name == "fortuna-dusseldorf" ~ "Fortuna Düsseldorf",
        team_name == "bayer-04-leverkusen" ~ "Bayer Leverkusen",
        team_name == "sv-werder-bremen" ~ "Werder Bremen",
        team_name == "borussia-monchengladbach" ~ "Borussia Mönchengladbach",
        team_name == "tsg-1899-hoffenheim" ~ "Hoffenheim",
        TRUE ~ str_replace_all(team_name, "-", " ") %>% stringr::str_to_title()),
      season = season) %>% 
    arrange(desc(min_perc)) 
  
  return(all_team_minutes)
}
```

```{r}
# ALL TEAMS AT ONCE, WILL TAKE A WHILE:
age_utility_df_ALL <- pmap(list(team_links_df$team_name, 
                                team_links_df$team_num,
                                team_links_df$season,
                                2160),
                           .f = buli_age_utility_info)
buli_age_utility_df <- age_utility_df_ALL %>% 
  reduce(rbind)
## save
saveRDS(buli_age_utility_df, file = glue("{here::here()}/data/buli_age_utility_df_MD24_1920.RDS"))
```