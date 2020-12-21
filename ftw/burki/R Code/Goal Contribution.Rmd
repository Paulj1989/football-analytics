# pkgs
pacman::p_load(tidyverse, polite, scales, ggimage, ggforce,
               rvest, glue, extrafont, ggrepel, magick)
loadfonts()

## add_logo function from Thomas Mock
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}

## webscrape soccerway

url <- "https://us.soccerway.com/national/england/premier-league/20182019/regular-season/r48730/"

session <- bow(url)

team_links <- scrape(session) %>% 
  html_nodes("#page_competition_1_block_competition_tables_7_block_competition_league_table_1_table .large-link a") %>% 
  html_attr("href")

team_links_df <- team_links %>% 
  enframe(name = NULL) %>% 
  separate(value, c(NA, NA, NA, "team_name", "team_num"), sep = "/") %>% 
  mutate(link = glue("https://us.soccerway.com/teams/england/{team_name}/{team_num}/squad/"))

# for each team link:

player_name_info <- function(session) {
  
  player_name_info <- scrape(session) %>% 
    html_nodes("#page_team_1_block_team_squad_3-table .name.large-link") %>% 
    html_text()
}

num_goals_info <- function(session) {
  
  num_goals_info <- scrape(session) %>% 
    html_nodes(".goals") %>% 
    html_text()
  
  num_goals_info_clean <- num_goals_info[-1]
}

num_assists_info <- function(session) {
  
  num_assists_info <- scrape(session) %>% 
    html_nodes(".assists") %>% 
    html_text()
  
  num_assists_info_clean <- num_assists_info[-1]
}

# BIG FUNCTION
premier_stats_info <- function(link) {
  
  session <- bow(link)
  
  player_name <- player_name_info(session = session)
  
  num_goals <- num_goals_info(session = session)
  
  num_assists <- num_assists_info(session = session)
  
  resultados <- list(player_name, num_goals, num_assists)
  col_names <- c("name", "goals", "assists") 
  
  premier_stats <- resultados %>% 
    reduce(cbind) %>% 
    as_tibble() %>% 
    set_names(col_names)
  
}

# ALL 18 TEAMS AT ONCE, WILL TAKE A WHILE:
goal_contribution_df_ALL <- map2(.x = team_links_df$link, 
                                 .y = team_links_df$team_name,
                                 ~ premier_stats_info(link = .x) %>% mutate(team = .y))

goal_contribution_df <- goal_contribution_df_ALL %>% 
  reduce(rbind)

#### -----------------------------------------------------------
## ALTERNATIVE: break scraping into groups of 3-5 teams, then combine:
goal_contribution_df1 <- map2(.x = team_links_df$link[1:3], 
                              .y = team_links_df$team_name[1:3],
                              ~ premier_stats_info(link = .x) %>% mutate(team = .y))

goal_contribution_df2 <- map2(.x = team_links_df$link[4:8], 
                              .y = team_links_df$team_name[4:8],
                              ~ premier_stats_info(link = .x) %>% mutate(team = .y))

goal_contribution_df3 <- map2(.x = team_links_df$link[9:13], 
                              .y = team_links_df$team_name[9:13],
                              ~ premier_stats_info(link = .x) %>% mutate(team = .y))

goal_contribution_df4 <- map2(.x = team_links_df$link[14:18], 
                              .y = team_links_df$team_name[14:18],
                              ~ premier_stats_info(link = .x) %>% mutate(team = .y))

a1 <- goal_contribution_df1 %>% reduce(rbind)
a2 <- goal_contribution_df2 %>% reduce(rbind)
a3 <- goal_contribution_df3 %>% reduce(rbind)
a4 <- goal_contribution_df4 %>% reduce(rbind)

resultados_grande <- list(a1, a2, a3, a4)

goal_contribution_df <- resultados_grande %>% 
  reduce(rbind)
#### -----------------------------------------------------------

## clean
goal_contribution_clean_df <- goal_contribution_df %>% 
  mutate_at(.vars = c("goals", "assists"), 
            ~str_replace(., "-", "0") %>% as.numeric) %>% 
  mutate(team = team %>% str_replace_all(., "-", " ") %>% str_to_title) %>% 
  group_by(team) %>% 
  mutate(total_goals = sum(goals),
         total_assists = sum(assists),
         goal_contrib = goals/total_goals,
         ## assists/total_goals because looking at perspective of contribution to goals.
         ## Will be an underestimation as not all goals have assists.
         assist_contrib = assists/total_goals) %>% 
  ungroup()

## save
saveRDS(goal_contribution_clean_df, 
        file = glue("{here::here()}/data/epl_goal_contrib_clean_df.RDS"))
goal_contribution_clean_df <- readRDS(file = glue("{here::here()}/data/epl_goal_contrib_clean_df.RDS"))

## Description text
desc_hazard <- "Hazard FC: With 16 goals and 15 assists Eden Hazard has been involved in the most goals for a team this season."
desc_vardymurray <- "Scoring 37.5% and 37.1% of their team's goals, Jamie Vardy and Glen Murray have proven to be talismans for their team yet again!"
desc_fraser <- "Another fantastic season from Ryan Fraser with 7 goals and 14 assists (one behind league-leader Hazard)"

## PLOT!
goal_contribution_clean_df %>% 
  ggplot(aes(assist_contrib, goal_contrib)) +
  geom_point(data = goal_contribution_clean_df %>%
               filter(goal_contrib < 0.25 | assist_contrib < 0.15),
             color = "grey20", size = 4, alpha = 0.2) +
  geom_point(data = goal_contribution_clean_df %>%
               filter(goal_contrib > 0.25 | assist_contrib > 0.15),
             color = "red", size = 4) +
  geom_hline(yintercept = 0.25, color = "grey20", alpha = 0.4) +
  geom_vline(xintercept = 0.15, color = "grey20", alpha = 0.4) +
  geom_text_repel(data = goal_contribution_clean_df %>%
                    filter(goal_contrib > 0.25 | assist_contrib > 0.15, 
                           !name %in% c("E. Hazard", "R. Fraser", "J. Vardy", "G. Murray")),
                  aes(label = name, family = "Roboto Condensed", fontface = "bold"), 
                  seed = 15, size = 5, 
                  min.segment.length = 0, segment.color = "red",
                  point.padding = 0.5) +
  geom_mark_circle(aes(filter = name == "E. Hazard", label = "Eden Hazard",
                       description = desc_hazard), 
                   label.family = "Roboto Condensed", label.fontsize = c(14, 10)) +
  geom_mark_hull(aes(filter = name %in% c("G. Murray", "J. Vardy"), label = "Vardy & Murray",
                     description = desc_vardymurray),
                 label.buffer = unit(20, "mm"), label.fontsize = c(14, 10),
                 label.family = "Roboto Condensed") +
  geom_mark_circle(aes(filter = name == "R. Fraser", label = "Ryan Fraser",
                       description = desc_fraser),
                   label.buffer = unit(9.8, "mm"), label.fontsize = c(14, 10),
                   label.family = "Roboto Condensed") +
  scale_x_continuous(labels = percent_format(accuracy = 1), 
                     breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3),
                     limits = c(0, 0.3)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
                     limits = c(0, 0.5)) +
  labs(title = "Goal Contribution Matrix: Premier League (2018-2019 Season)", #Hazard FC: Eden Hazard has been involved in the most goals for a team this season.
       subtitle = "Team Goal Involvement as Percentage of Total Club Goals* and/or Assists.",
       caption = glue("
                      * Own goals not counted in total
                      Data: soccerway.com
                      By: @R_by_Ryo"),
       x = "Percentage of Club Goals Assisted",
       y = "Percentage of Club Goals Scored") +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor.x = element_blank()) -> goal_contribution_matrix

goal_contribution_matrix

ggsave(plot = goal_contribution_matrix, 
       "../Premier League 2018-2019/output/goal_contribution_matrix_plot_epl.png",
       height = 9, width = 11)

plot_logo <- add_logo(
  plot_path = "../Premier League 2018-2019/output/goal_contribution_matrix_plot_epl.png",
  logo_path = "https://upload.wikimedia.org/wikipedia/en/f/f2/Premier_League_Logo.svg",
  logo_position = "top right",
  logo_scale = 8)

plot_logo

image_write(image = plot_logo, 
            "../Premier League 2018-2019/output/goal_contribution_matrix_plot_logo.png")

