houstonkemp_colours <- c("#008698", "#232C31",  "#b5a991", "#c94b20", "#696A6D", "#A79344",
                         "#006472", "#4e4f51",  "#963817", "#928262", "#7d6e33",
                         "#28e5ff", "#a4a5a7",  "#e88e6f", "#d2cbbc", "#cfc189")
houstonkemp_teal <- c("#008698", "#28e5ff")

houstonkemp_theme <- function (base_size = 24, base_family = "") {
  half_line <- base_size / 2
  theme(
    line = element_line(
      colour = "#232C31",
      size = 0.5,
      linetype = 1,
      lineend = "butt"
    ),
    rect = element_rect(
      fill = "white",
      colour = "white",
      size = 0.5,
      linetype = 1
    ),
    text = element_text(
      family = base_family,
      face = "plain",
      colour = "#232C31",
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = margin(),
      debug = FALSE
    ),
    axis.line = element_line(),
    axis.line.x = element_line(colour = "#bfbfbf", linetype = 1, size = 0.75),
    axis.line.y = element_blank(),
    axis.text = element_text(size = rel(0.8),
                             colour = houstonkemp_colours[2]),
    axis.text.x = element_text(margin = margin(t = 0.8 *half_line / 2), hjust = 0.5, angle = 90),
    axis.text.y = element_text(margin = margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.ticks = element_line(colour = "#bfbfbf", size = 0.75),
    axis.ticks.length = unit(half_line / 2, "pt"),
    axis.title.x = element_text(margin = margin(
      t = 0.8 * half_line, b = 0.8 * half_line / 2)),
    axis.title.y = element_text(angle = 90, margin = margin(r = 0.8 * half_line, l = 0.8 * half_line / 2)),
    legend.background = element_rect(fill = "transparent",
                                     colour = NA),
    legend.spacing = unit(0.2, "cm"),
    legend.key = element_rect(fill = "transparent",
                              colour = NA),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "bottom",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    panel.background = element_rect(fill = "transparent",
                                    colour = NA),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(colour = "#bfbfbf", linetype = 1, size = 0.75),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,
    strip.background = element_rect(fill = houstonkemp_colours[2], colour = NA),
    strip.text = element_text(colour = "white", size = rel(0.8)),
    strip.text.x = element_text(margin = margin(t = half_line,
                                                b = half_line)),
    strip.text.y = element_text(
      angle = -90,
      margin = margin(l = half_line, r = half_line)
    ),
    strip.switch.pad.grid = unit(0.1,
                                 "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),
    plot.background = element_rect(colour = NA, fill = "transparent"),
    plot.title = element_text(size = rel(1.2), margin = margin(b = half_line *
                                                                 1.2)),
    plot.margin = margin(base_size, base_size,
                         base_size, base_size),
    complete = TRUE
  )
}

scale_fill_manual_houstonkemp <- function(name = "") {
  structure(list(scale_fill_manual(values = houstonkemp_colours, name = name)))
}

scale_color_discrete_houstonkemp <- function(name = "") {
  structure(list(scale_color_manual(values = houstonkemp_colours, name = name)))
}

scale_color_continuous_houstonkemp <- function(name = "", guide = T) {
  structure(list(scale_color_gradientn(colours = houstonkemp_teal, name = name, guide = guide)))
}
