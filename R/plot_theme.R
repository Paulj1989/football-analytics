
theme_ftw <-
  function(base_family = "Inter", base_size = 15,
           grid_y = TRUE, grid_x = FALSE) {
    # Use theme_minimal as basis for theme
    ggplot2::theme_minimal(
      base_size = base_size,
      base_family = base_family
    ) %+replace%
      ggplot2::theme(
        # plot elements
        plot.background = element_rect(fill = "white", color = "white"),
        plot.margin = margin(25, 75, 25, 45),
        # text elements
        plot.title = ggtext::element_markdown(
          size = rel(1.6),
          hjust = 0,
          margin = margin(b = 15)
        ),
        plot.subtitle = ggtext::element_markdown(
          colour = "gray40", size = rel(1),
          hjust = 0, lineheight = 1.4,
          margin = margin(b = 15)
        ),
        plot.caption = element_text(
          colour = "gray60", size = rel(0.9),
          hjust = 0, margin = margin(t = 25)
        ),
        strip.text = element_text(
          size = rel(1.2),
          margin = margin(t = 10, b = 20)
        ),
        plot.title.position = "plot", plot.caption.position = "plot",
        # axis elements
        axis.title.y = element_text(
          colour = "gray20", size = rel(1.2),
          angle = 90, vjust = 5
        ),
        axis.text = element_text(size = rel(1)),
        # legend elements
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = rel(1)),
        legend.key.width = unit(2, "cm"),
        # panel elements
        panel.grid.major.x =
          if (grid_x) {
            ggplot2::element_line(
              linewidth = 0.6,
              colour = "grey80",
              linetype = "dotted"
            )
          } else {
            ggplot2::element_blank()
          },
        panel.grid.major.y =
          if (grid_y) {
            ggplot2::element_line(
              linewidth = 0.6,
              colour = "grey80",
              linetype = "dotted"
            )
          } else {
            ggplot2::element_blank()
          },
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.spacing.x = unit(3.5, "lines"),
        complete = TRUE
      )
  }
