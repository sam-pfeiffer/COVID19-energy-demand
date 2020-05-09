# Creation date: 11 May 2020
# Client: Internal
# Project: LinkedIn post
# Description: Change in the load profile
# Author: Sam Pfeiffer

library(tidyverse)
library(lubridate)
library(magrittr)
library(scales)
library(extrafont)
library(wesanderson)
library(shiny)

# Load data
data <- readRDS(file = 'plot_data.rds')

# HK theme
function (base_size = 24, base_family = "") {
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

ui = fluidPage(
  titlePanel("Average weekday demand profile"),
  sidebarLayout(
    sidebarPanel(
      selectInput("period",
                  "Select period:",
                  choices = c("23 March",
                              "30 March",
                              "6 April",
                              "13 April",
                              "20 April",
                              "27 April",
                              "4 May"),
                  selected = "23 March"
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server = function(input, output) {
  
  output$plot = renderPlot({
    plot.data <- data[data$period %in% input$period, ]
    
    ggplot(data = plot.data) +
      geom_rect(aes(NULL,
                    NULL,
                    xmin = AM_x_min,
                    xmax = AM_x_max,
                    fill = AM_colour_variable),
                alpha = 0.6,
                ymin = 6,
                ymax = 10) +
      geom_rect(aes(NULL,
                    NULL,
                    xmin = PM_x_min,
                    xmax = PM_x_max,
                    fill = PM_colour_variable),
                alpha = 0.6,
                ymin = 6,
                ymax = 10) +
      geom_line(aes(x = daily_idx,
                    y = demand/1000,
                    col = type),
                size = 2) +
      geom_text(aes(x = AM,
                    y = 10.5,
                    label = AM_text),
                size = 6,
                colour = "#232C31",
                family = "Century Gothic") +
      geom_text(aes(x = PM,
                    y = 10.5,
                    label = PM_text),
                size = 6,
                colour = "#232C31",
                family = "Century Gothic") + 
      geom_text(aes(x = 5,
                    y = 10.5,
                    label = "Actual"),
                size = 6,
                colour = "#232C31",
                family = "Century Gothic") +   
      geom_text(aes(x = AM_predict,
                    y = 5.5,
                    label = predicted_AM_time),
                size = 6,
                colour = "#008698",
                family = "Century Gothic") +
      geom_text(aes(x = PM_predict,
                    y = 5.5,
                    label = predicted_PM_time),
                size = 6,
                colour = "#008698",
                family = "Century Gothic") +
      geom_text(aes(x = 5,
                    y = 5.5,
                    label = "Predicted"),
                size = 6,
                colour = "#008698",
                family = "Century Gothic") +
      houstonkemp_theme(base_size = 22,
                        base_family = "Century Gothic") +      
      scale_color_manual(name = "",
                         values = c("#232C31",
                                    "#008698"),
                         breaks = c("average_actual",
                                    "average_predicted"),
                         labels = c("Actual demand",
                                    "Predicted demand")) +
      scale_fill_manual(name = "",
                        values = c(wes_palette("Royal2")[3],
                                   wes_palette("Royal2")[4],
                                   "white"),
                        breaks = c("earlier",
                                   "later"),
                        labels = c("Earlier peak",
                                   "Later peak")) +  
      labs(title = "",
           x = "",
           y = "Average demand (GW)") +
      scale_y_continuous(labels = comma_format(accuracy = 1, big.mark = ","),
                         breaks = c(seq(5,10,1)),
                         expand = expand_scale(mult = c(0, 0))) +
      scale_x_continuous(breaks = c(seq(12,36,12)),
                         labels = c("6AM",
                                    "Noon",
                                    "6PM")) +
      coord_cartesian(ylim = c(4.75,11.25)) +
      guides(col = guide_legend(nrow = 2),
             fill = guide_legend(nrow = 2)) +
      theme(#text = element_text(family = "Century Gothic"),
            axis.text.x = element_text(size = 18,
                                       angle = 0,
                                       vjust = 0.5,
                                       hjust = 0.5),
            axis.ticks.x = element_blank())#,
            # axis.text.y = element_text(size = 18),
            # legend.text=element_text(size = 20))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

