pacman::p_load(dplyr, formattable, tidyr)

# load data
akanji_year <- readr::read_csv("akanji_passing.csv")
akanji_buli <- readr::read_csv("akanji_buli.csv")

# change pass completion to percentages
akanji_year$`Pass Completion` <- percent(akanji_year$`Pass Completion`, digits = 1)
akanji_buli$`Pass Completion` <- percent(akanji_buli$`Pass Completion`, digits = 1)

# define custom function for cell tiles
custom_color_tile <- function(...) {
  formatter("span",
    style = function(x) {
      style(
        display = "block",
        padding = "0 4px",
        `color` = "white",
        `border-radius` = "4px",
        `background-color` = csscolor(gradient(
          as.numeric(x),
          ...
        ))
      )
    }
  )
}

# set table styles and format
t1 <- formattable(akanji_year,
  align = c("l", rep("r", NCOL(df) - 1)),
  list(
    `Season` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
    `Pass Completion` = custom_color_tile("#B1CBEB", "#3E7DCC"),
    `Progressive Passes` = custom_color_tile("#B1CBEB", "#3E7DCC"),
    `Progressive Distance (Yards)` = custom_color_tile("#B1CBEB", "#3E7DCC"),
    `Passes into Final 1/3` = custom_color_tile("#B1CBEB", "#3E7DCC")
  )
)

t2 <- formattable(akanji_buli,
  align = c("l", rep("r", NCOL(df) - 1)),
  list(
    `Player` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
    `Pass Completion` = custom_color_tile("#B1CBEB", "#3E7DCC"),
    `Progressive Passes` = custom_color_tile("#B1CBEB", "#3E7DCC"),
    `Progressive Distance (Yards)` = custom_color_tile("#B1CBEB", "#3E7DCC"),
    `Passes into Final 1/3` = custom_color_tile("#B1CBEB", "#3E7DCC")
  )
)

# print to HTML
format_table(t1)
format_table(t2)
