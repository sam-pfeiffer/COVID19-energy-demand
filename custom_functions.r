#### Compounded addition ####
`%+%` <- function(x, y){
  eval.parent(substitute(x <- x + y))
}

#### Compounded substraction ####
`%-%` <- function(x, y){
  eval.parent(substitute(x <- x - y))
}

#### Compounded multiplication ####
`%**%` <- function(x, y){
  eval.parent(substitute(x <- x * y))
}

#### Compounded division ####
`%//%` <- function(x, y){
  eval.parent(substitute(x <- x / y))
}

#### Financial years ####
# Function for returning the financial year of a date variable
financial_years <- function(dates, timezone = "UTC") {
  require(lubridate)
  if (sum(class(dates) == "Date") > 0) {
    # Create vector of years
    fy.tmp <- seq(as.Date("1900-07-01", "%Y-%m-%d", tz = timezone), length = (200), by = "year")
    # Find the intervals/financial years of the dates
    fin_year <- (1901:2100)[findInterval(dates, fy.tmp)]
    return(fin_year)
  } else if (sum(class(dates) == "POSIXct") > 0) {
    # Create vector of years
    fy.tmp <- seq(as.Date("1900-07-01", "%Y-%m-%d", tz = timezone), length = (200), by = "year")
    # Find the intervals/financial years of the dates
    fin_year <- (1901:2100)[findInterval(as.Date(dates, tz = timezone), fy.tmp)]
    return(fin_year)
    # See if the vector passed is a character vector
  } else if (sum(class(dates) == "character") > 0) {
    # Test parsing to ymd
    if (!is.na(suppressWarnings(ymd(dates[1])))) {
      fy.tmp <- seq(as.Date("1900-07-01", "%Y-%m-%d", tz = timezone), length = (200), by = "year")
      # Find the intervals/financial years of the dates
      fin_year <- (1901:2100)[findInterval(as.Date(ymd(dates, tz = timezone)), fy.tmp)]
      return(fin_year)
      # Test parsing to ymd hms
    } else if (!is.na(suppressWarnings(ymd_hms(dates[1])))) {
      fy.tmp <- seq(as.Date("1900-07-01", "%Y-%m-%d", tz = timezone), length = (200), by = "year")
      # Find the intervals/financial years of the dates
      fin_year <- (1901:2100)[findInterval(as.Date(ymd_hms(dates, tz = timezone)), fy.tmp)]
      return(fin_year)
    } else {
      warning("Input is not a recognised date format.")
    }
  } else {
    warning("Input is not a recognised date format.")
  }
}


#### Evaluate the proportion based on a grouped variable
proportion <- function(x, na_rm = TRUE) {
  if (sum(x, na.rm = na_rm) == 0) {
    return(rep(NA_real_, length(x)))
  } else {
    return(x / sum(x, na.rm = na_rm))
  }
}

#### Convert NaN to NA in vectors
nan_to_na <- function(x, na_type = NULL) {
  if (!is.null(na_type)) {
    if (is.na(na_type)) {
      x[is.nan(x)] <- na_type
    } else {
      stop("Argument na_type should be an NA value.")
    }
  } else {
    x[is.nan(x)] <- NA
  }
  
  return(x)
}


#### Factors of a number
factors <- function(x, include_original = FALSE) {
  if (include_original) {
    return(c((1:(x / 2))[(x %% 1:(x / 2)) == 0], x))
  } else {
    return((1:(x / 2))[(x %% 1:(x / 2)) == 0])
  }
}


#### Create year, month, days from a data column ####
year_month_day <- function(data, date, year_name = "year", month_name = "month",
                           day_name = "day") {
  require(lubridate)
  data[[year_name]] <- year(data[[deparse(substitute(date))]])
  data[[month_name]] <- month(data[[deparse(substitute(date))]])
  data[[day_name]] <- day(data[[deparse(substitute(date))]])
  return(data)
}

#### Create year and quarter from a data column ####
year_quarter <- function(data, date, year_name = "year", quarter_name = "quarter") {
  require(lubridate)
  data[[year_name]] <- year(data[[deparse(substitute(date))]])
  data[[quarter_name]] <- quarter(data[[deparse(substitute(date))]])
  return(data)
}

#### Convert excel number to a date ####
excel_date <- function(date) {
  x <- as.Date(as.numeric(date), origin = "1899-12-30")
  return(x)
}

#### Convert excel number to a datetime ####
excel_datetime <- function(date) {
  require(lubridate)
  x <- as_datetime(x = date * (60 * 60 * 24), origin = "1899-12-30", tz = "UTC") 
  return(x)
}

#### Create a png geom in ggplot from an image or an image path ####
geom_png <- function(data, x, y, image_path, width, height, path_for_each = FALSE) {
  # Load Grid and PNG packages to annotate
  require(grid)
  require(png)
  
  # If there is only one type of image, annotate with static widths and heights
  if (!path_for_each) {
    img <- readPNG(source = image_path)
    g <- rasterGrob(img, interpolate = T)
    mapply(function(xx, yy) annotation_custom(g, xmin = xx - (0.5 * width), xmax =  xx + (0.5 * width),
                                              ymin = yy - (0.5 * height), ymax = yy + (0.5 * height)),
           data[[deparse(substitute(x))]], data[[deparse(substitute(y))]])
    
    # If there are different image paths, annotate by row
  } else {
    mapply(function(xx, yy, ww, hh, pathpath) annotation_custom(rasterGrob(readPNG(source = pathpath), interpolate = T),
                                                                xmin = xx - (0.5 * ww), xmax =  xx + (0.5 * ww),
                                                                ymin = yy - (0.5 * hh), ymax = yy + (0.5 * hh)),
           data[[deparse(substitute(x))]],
           data[[deparse(substitute(y))]],
           data[[deparse(substitute(width))]],
           data[[deparse(substitute(height))]],
           data[[deparse(substitute(image_path))]])
    
  }
}

#### ggplot geom to add axis arrows
geom_axis <- function(colour = "#232C31", linesize = 1.3,
                      xintercept = 0, yintercept = 0) {
  return(geom_segment(data = data_frame(x1 = c(yintercept, Inf), x2 = c(yintercept, -Inf),
                                        y1 = c(Inf, xintercept), y2 = c(-Inf, xintercept)),
                      aes(x = x1, xend = x2,
                          y = y1, yend = y2),
                      col = colour, size = linesize,
                      arrow = arrow(ends = "both", type = "closed",
                                    length = unit(0.4, "cm")),
                      inherit.aes = FALSE))
}

#### View the corners of a matrix or dataframe
glance <- function(x, views = c("left", "right"), rows = 6, cols = 6, dim_view = TRUE) {
  if (dim_view) {
    cat(paste("There are", dim(x)[1], "rows and", dim(x)[2], "columns."))
    cat("\n")
  }
  if ("left" %in% views) {
    cat("\n")
    cat("Top left view")
    cat("\n")
    print(x[1:min(rows, nrow(x)), 1:min(cols, ncol(x))])
    cat("\n")
    cat("Bottom left view")
    cat("\n")
    print(x[max(1, (nrow(x) - rows)):nrow(x), 1:min(cols, ncol(x))])
    cat("\n")
  }
  
  if ("right" %in% views) {
    cat("\n")
    cat("Top right view")
    cat("\n")
    print(x[1:min(rows, nrow(x)), max(1, (ncol(x) - (cols - 1))):ncol(x)])
    cat("\n")
    cat("Bottom right view")
    cat("\n")
    print(x[max(1, (nrow(x) - rows)):nrow(x), max(1 , (ncol(x) - (cols - 1))):ncol(x)])
    cat("\n")
  }
}

#### Tic Tac Toe
tic_tac_toe <- function() {
  
  cat("MMP\"\"MM\"\"YMM db     MMP\"\"MM\"\"YMM         MMP\"\"MM\"\"YMM    \n")
  cat("P'   MM   `7        P'   MM   `7         P'   MM   `7              \n")
  cat("     MM    `7MM  ,p6\"bo  MM   ,6\"Yb.  ,p6\"bo  MM  ,pW\"Wq.   .gP\"Ya \n")
  cat("     MM      MM 6M'  OO  MM  8)   MM 6M'  OO  MM 6W'   `Wb ,M'   Yb\n")
  cat("     MM      MM 8M       MM   ,pm9MM 8M       MM 8M     M8 8M\"\"\"\"\"\"\n")
  cat("     MM      MM YM.    , MM  8M   MM YM.    , MM YA.   ,A9 YM.    ,\n")
  cat("   .JMML.  .JMML.YMbmd'.JMML.`Moo9^Yo.YMbmd'.JMML.`Ybmd9'   `Mbmmd'\n")
  cat("\n\n\n")
  
  # Setup TTT environment
  ttt <- new.env()
  assign("state", 1:9, envir = ttt)
  names(ttt$state) <- 1:9
  assign("player", "X", envir = ttt)
  assign("win", FALSE, envir = ttt)
  assign("played_cells", c(), envir = ttt)
  assign("moves", 0, envir = ttt)
  
  # Function to print the board
  print_board <- function() {
    for (i in 1:3) {
      cat(paste(" ", ttt$state[1 + ((i - 1) * 3)], " | ", ttt$state[2 + ((i - 1) * 3)], " | ", ttt$state[3 + ((i - 1) * 3)], "\n"))
      if (i < 3) {
        cat("_________________\n")
      }
    }
  }
  
  # Function to detect whether user input is legal
  accept_input <- function() {
    legal_move <- FALSE
    while (legal_move == FALSE) {
      n <- readline(prompt = paste("Player", ttt$player, "to move: "))
      # Detect whether the cell has already been played
      if (n %in% ttt$played_cells) {
        cat("Cell has already been played.")
        # Detect whether cell is from 1 to 9
      } else if (!n %in% 1:9) {
        cat("Pick a cell from 1 to 9")
        # Accept the move and assign the move to the board
      } else {
        ttt$state[n] <- ttt$player
        ttt$played_cells <- c(ttt$played_cells, n)
        print_board()
        legal_move <- TRUE
      }
    }
    # Check whether the move was a winning move
    check_win()
    # If not a winning move, switch player
    switch_player()
  }
  
  # Switch the player
  switch_player <- function() {
    if (ttt$player == "X") {
      ttt$player <- "O"
    } else {
      ttt$player <- "X"
    }
  }
  
  # Check whether anythe board has been won
  check_win <- function() {
    if ((ttt$state[1] == ttt$state[2]) & (ttt$state[2] == ttt$state[3])) {
      ttt$win <- TRUE
    } else if ((ttt$state[4] == ttt$state[5]) & (ttt$state[5] == ttt$state[6])) {
      ttt$win <- TRUE
    } else if ((ttt$state[4] == ttt$state[5]) & (ttt$state[5] == ttt$state[6])) {
      ttt$win <- TRUE
    } else if ((ttt$state[7] == ttt$state[8]) & (ttt$state[8] == ttt$state[9])) {
      ttt$win <- TRUE
    } else if ((ttt$state[1] == ttt$state[5]) & (ttt$state[5] == ttt$state[9])) {
      ttt$win <- TRUE
    } else if ((ttt$state[7] == ttt$state[5]) & (ttt$state[5] == ttt$state[3])) {
      ttt$win <- TRUE
    } else if ((ttt$state[1] == ttt$state[4]) & (ttt$state[4] == ttt$state[7])) {
      ttt$win <- TRUE
    } else if ((ttt$state[2] == ttt$state[5]) & (ttt$state[5] == ttt$state[8])) {
      ttt$win <- TRUE
    } else if ((ttt$state[3] == ttt$state[6]) & (ttt$state[6] == ttt$state[9])) {
      ttt$win <- TRUE
    }
  }
  
  print_board()
  # Only allow 9 total moves in a game
  # Repeat game mechanism until win or stalemate
  while ((ttt$win == FALSE) & ttt$moves < 9) {
    accept_input()
    ttt$moves %+% 1
  }
  # Switch the player to get the correct winner
  switch_player()
  
  # Declare winner or stalemate
  if (ttt$win) {
    cat(paste("Player", ttt$player, "wins!"))
  } else {
    cat("Stalemate.")
  }
}

#### Left-right string interpolation
`%str%` <- function(string, replacement) {
  .Deprecated(new = "Check out `%fmt%` instead!")
  if (lengths(regmatches(string, gregexpr("%s", string))) != length(replacement)) {
    stop("Number of %s and replacement should be the same.")
  }
  
  if ("%s" %in% replacement) {
    stop("Replacement vector cannot contain %s.")
  }
  
  str_out <- string
  
  for (i in seq_along(replacement)) {
    str_out <- sub(pattern = "%s", replacement = replacement[i], x = str_out)
  }
  
  return(str_out)
}

#### Left-right string interpolation with formatting options of sprintf/python
# Think pythonic string.format()
`%fmt%` <- function(x, y) {
  if (!is.list(y) & !is.atomic(y)) {
    stop("Right hand side must be a list or vector object.")
  }
  
  if (is.atomic(y)) {
    y <- as.list(y)
  }
  
  do.call(sprintf, append(list(x), y))
}

# "I rate my %s %.4f out of %i." %fmt% list("job", 9.99898898898, 10)

#### Vector string replacement
# This function operates like gsub, but uses a vector of patterns and replacements as inputs
vecsub <- function(x, pattern, replacement, ignore_case = FALSE,
                   perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  # Ensure pattern and replacement are the same length
  if (length(pattern) != length(replacement)) {
    stop("Pattern and replacement should be the same lengths.")
  }
  
  str_out <- x
  
  # Loop over vectors
  for (i in seq_along(pattern)) {
    str_out <- gsub(pattern = pattern[i], replacement = replacement[i],
                    x = str_out, ignore.case = ignore_case, perl = perl,
                    fixed = fixed, useBytes = useBytes)
  }
  
  return(str_out)
}


#### Code for page break
geom_pagebreak <- function(title = "") {
  require(ggplot2)
  require(extrafont)
  return(ggplot() +
           geom_text(data = data.frame(x = c(0), y = c(0), lablab = title),
                     aes(x = x, y = y, label = lablab),
                     family = "Century Gothic", size = 28,
                     col = houstonkemp_colours[1]) +
           # annotate(geom = "rect", xmin = -Inf, xmax = Inf,
           #          ymin = -Inf, ymax = -0.9, fill = houstonkemp_colours[1]) +
           # annotate(geom = "rect", xmin = -Inf, xmax = Inf,
           #          ymin = 0.8, ymax = Inf, fill = houstonkemp_colours[2]) +
           coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
           # geom_png(data = data.frame(x = c(-0.75), y = c(0.945)),
           #          x = x, y = y, image_path = "//HKDC01/Shared Folders/Company/ggplot_theme/HoustonKemp Logo Strip Reversed (ID 25697).png",
           #          width = 0.8, height = 0.2) +
           # geom_text(data = data.frame(x = c(-0.85), y = c(-0.995), lablab = c("HoustonKemp.com")),
           #           aes(x = x, y = y, label = lablab),
           #           family = "Century Gothic", size = 10,
           #           col = "#FFFFFF") +
           theme_void())
}

#### ggplot speedometer and POSL combo
gg_speedo <- function(speed, posl, max_speed = 100, min_speed = 0,
                      font_size = 10) {
  require(ggplot2)
  require(extrafont)
  
  speed_breaks <- seq(from = min_speed, to = (max_speed * 1.25), by = ((max_speed * 1.25) - min_speed) / 5)
  
  polygon_maker <- function(start, stop, minimum_speed = min_speed,
                            maximum_speed = ((max_speed * 1.8) - ((1.8 / 1.25) * min_speed)),
                            inner_rad = 0.5, outer_rad = 1) {
    start_point <- pi * (1 - ((start - minimum_speed) / (maximum_speed)))
    end_point <- pi * (1 - ((stop - minimum_speed) / (maximum_speed)))
    interpolated <- seq(start_point, end_point, length = 100)
    x_points <- c(inner_rad * cos(interpolated), rev(outer_rad * cos(interpolated)))
    y_points <- c(inner_rad * sin(interpolated), rev(outer_rad * sin(interpolated)))
    
    return(data.frame(x_points, y_points))
  }
  
  return(ggplot() +
           geom_polygon(data = polygon_maker(speed_breaks[1], speed_breaks[2]),
                        aes(x = x_points, y = y_points),
                        fill = "#F21A00") +
           geom_polygon(data = polygon_maker(speed_breaks[2], speed_breaks[3]),
                        aes(x = x_points, y = y_points),
                        fill = "#E1AF00") +
           geom_polygon(data = polygon_maker(speed_breaks[3], speed_breaks[4]),
                        aes(x = x_points, y = y_points),
                        fill = "#EBCC2A") +
           geom_polygon(data = polygon_maker(speed_breaks[4], speed_breaks[5]),
                        aes(x = x_points, y = y_points),
                        fill = "#78B7C5") +
           geom_polygon(data = polygon_maker(speed_breaks[5], speed_breaks[6]),
                        aes(x = x_points, y = y_points),
                        fill = "#3B9AB2") +
           geom_polygon(data = polygon_maker(start = speed - 1, stop = speed + 1, inner_rad = 0.25, outer_rad = 1),
                        aes(x = x_points, y = y_points),
                        fill = houstonkemp_colours[2]) +
           geom_polygon(data = polygon_maker(start = 0, stop = 100, minimum_speed = 0, maximum_speed = 100,
                                             inner_rad = 0.9, outer_rad = 1),
                        aes(x = x_points - 2.2, y = y_points),
                        fill = houstonkemp_colours[1]) +
           geom_polygon(data = polygon_maker(start = 0, stop = min(posl, 100),
                                             minimum_speed = 0, maximum_speed = 100,
                                             inner_rad = 0, outer_rad = 0.9),
                        aes(x = x_points - 2.2, y = y_points),
                        fill = houstonkemp_colours[4]) +
           geom_polygon(data = data_frame(xx = c(-3.1, -3.1, -2.8, -2.8),
                                          yy = c(0, 0.075, 0.075, 0)),
                        aes(x = xx, y = yy),
                        fill = houstonkemp_colours[1]) +
           geom_polygon(data = data_frame(xx = c(-1.3, -1.3, -1.6, -1.6),
                                          yy = c(0, 0.075, 0.075, 0)),
                        aes(x = xx, y = yy),
                        fill = houstonkemp_colours[1]) +
           geom_polygon(data = data_frame(xx = c(-2.1625, -2.1625, -2.2375, -2.2375),
                                          yy = c(0.9, 0.75, 0.75, 0.9)),
                        aes(x = xx, y = yy),
                        fill = houstonkemp_colours[1]) +
           geom_text(data = data.frame(xx = 0, yy = 0.05, lablab = paste0(round(speed), "km/h")),
                     aes(x = xx, y = yy, label = lablab),
                     size = font_size, family = "Century Gothic", vjust = 0,
                     col = houstonkemp_colours[2]) +
           geom_text(data = data.frame(xx = -2.2, yy = 0.45, lablab = paste0(round(posl), "%")),
                     aes(x = xx, y = yy, label = lablab),
                     size = font_size * 2, family = "Century Gothic", vjust = 0.5,
                     col = houstonkemp_colours[2]) +
           geom_text(data = data.frame(xx = c(-0.2, -2.2), yy = c(-0.175, -0.175), lablab = c("Average speed", "Average POSL")),
                     aes(x = xx, y = yy, label = lablab),
                     size = font_size + 1.5, family = "Century Gothic", vjust = 0,
                     col = houstonkemp_colours[2]) +
           coord_fixed() +
           theme_void())
}

#### ggplot speedometer and POSL combo
gg_posl <- function(posl,
                    font_size = 10) {
  require(ggplot2)
  require(extrafont)
  
  polygon_maker <- function(start, stop, minimum_speed = 0,
                            maximum_speed = 100,
                            inner_rad = 0.5, outer_rad = 1) {
    start_point <- pi * (1 - ((start - minimum_speed) / (maximum_speed)))
    end_point <- pi * (1 - ((stop - minimum_speed) / (maximum_speed)))
    interpolated <- seq(start_point, end_point, length = 100)
    x_points <- c(inner_rad * cos(interpolated), rev(outer_rad * cos(interpolated)))
    y_points <- c(inner_rad * sin(interpolated), rev(outer_rad * sin(interpolated)))
    
    return(data.frame(x_points, y_points))
  }
  
  return(ggplot() +
           geom_polygon(data = polygon_maker(start = 0, stop = 100, minimum_speed = 0, maximum_speed = 100,
                                             inner_rad = 0.9, outer_rad = 1),
                        aes(x = x_points, y = y_points),
                        fill = houstonkemp_colours[1]) +
           geom_polygon(data = polygon_maker(start = 0, stop = posl,
                                             minimum_speed = 0, maximum_speed = 100,
                                             inner_rad = 0, outer_rad = 0.9),
                        aes(x = x_points, y = y_points),
                        fill = houstonkemp_colours[4]) +
           geom_polygon(data = data_frame(xx = c(-3.1 + 2.2, -3.1 + 2.2, -2.8 + 2.2, -2.8 + 2.2),
                                          yy = c(0, 0.075, 0.075, 0)),
                        aes(x = xx, y = yy),
                        fill = houstonkemp_colours[1]) +
           geom_polygon(data = data_frame(xx = c(-1.3 + 2.2, -1.3 + 2.2, -1.6 + 2.2, -1.6 + 2.2),
                                          yy = c(0, 0.075, 0.075, 0)),
                        aes(x = xx, y = yy),
                        fill = houstonkemp_colours[1]) +
           geom_polygon(data = data_frame(xx = c(-2.1625 + 2.2, -2.1625 + 2.2, -2.2375 + 2.2, -2.2375 + 2.2),
                                          yy = c(0.9, 0.75, 0.75, 0.9)),
                        aes(x = xx, y = yy),
                        fill = houstonkemp_colours[1]) +
           geom_text(data = data.frame(xx = 0, yy = 0.45, lablab = paste0(round(posl), "%")),
                     aes(x = xx, y = yy, label = lablab),
                     size = font_size * 2, family = "Century Gothic", vjust = 0.5,
                     col = houstonkemp_colours[2]) +
           geom_text(data = data.frame(xx = c(0), yy = c(-0.175), lablab = c("Average POSL")),
                     aes(x = xx, y = yy, label = lablab),
                     size = font_size + 1.5, family = "Century Gothic", vjust = 0,
                     col = houstonkemp_colours[2]) +
           coord_fixed() +
           theme_void())
}

#### BadBadNotGood
ahhhhhhhhhhhhhh <- function() {
  do.call("q", list("no", "no", "no"))
}

#### I'm sick of typing length(unique(x))
len_uni <- function(x) {
  return(length(unique(x)))
}

#### Count the number of NA in an object
sum_na <- function(x) {
  return(sum(is.na(x)))
}

#### Show the subset of NA in an object
which_na <- function(x) {
  return(which(is.na(x)))
}

#### Opposite of %in%
`%not in%` <- function(x, y) {
  return(!x %in% y)
}

#### Also opposite of %in%
`%nin%` <- function(x, y) {
  return(!x %in% y)
}

#### Also also opposite of %in%
`%ni%` <- function(x, y) {
  return(!x %in% y)
}


#### Convert text on clipboard from backslash to forwardslash
back_to_forward <- function() {
  return(gsub("\\\\", "/", readClipboard()))
}

#### Convert text on clipboard from backslash to forwardslash and copy to clipboard
back_to_forward_clip <- function() {
  writeClipboard(charToRaw(paste0(gsub("\\\\", "/", readClipboard()), ' ')))
  return("Copied to clipboard!")
}

#### Dumps R code to make a unique vector of the clipboard
clip_2_distinct <- function() {
  x <- unique(unlist(strsplit(readClipboard(), split = "\t")))
  return(dump("x", stdout()))
}

#### Convert hexadecimal colours to RGB
hex_to_rgb <- function(hex_colours) {
  
  stripped <- gsub(pattern = "#", replacement = "", x = hex_colours)
  
  # Test character lengther
  if (!all(nchar(stripped) == 6)) {
    bad_colours <- which(nchar(stripped) != 6)
    warning(paste("The following colours do not have six digits.\n", paste(bad_colours, collapse = ", ")))
    stop()
  }
  
  # Test all parse to numeric
  if (any(is.na(strtoi(paste0("0x", stripped))))) {
    bad_colours <- which(is.na(strtoi(paste0("0x", stripped))))
    warning(paste("The following colours do not parse from hex to decimal.\n", paste(bad_colours, collapse = ", ")))
    stop()
  }
  
  
  
  return(data.frame(hex = hex_colours,
                    red = strtoi(paste0("0x", substr(stripped, 1, 2))),
                    green = strtoi(paste0("0x", substr(stripped, 3, 4))),
                    blue = strtoi(paste0("0x", substr(stripped, 5, 6))),
                    stringsAsFactors = FALSE))
  
}

#### Convert RGB colours to hex
rgb_to_hex <- function(red, green, blue) {
  
  require(stringr)
  
  return(paste0("#", str_pad(string = as.hexmode(red), width = 2,
                             pad = "0", side = "left"),
                str_pad(string = as.hexmode(green), width = 2,
                        pad = "0", side = "left"),
                str_pad(string = as.hexmode(blue), width = 2,
                        pad = "0", side = "left")))
  
}

#### Convert a string in the clipboard to hex values
clipboard_2_hex <- function() {
  
  require(stringr)
  
  cb_value <- readClipboard()
  regex_values <- as.numeric(unlist(regmatches(cb_value, gregexpr(pattern = "?[0-9]+", cb_value))))
  if ((length(regex_values) != 3) | (!all(regex_values <= 255)) | (!all(regex_values >= 0))) {
    stop(paste0('"', cb_value, '"', " did not match to three valid values."))
  }
  return(paste0("#", str_pad(string = as.hexmode(regex_values[1]), width = 2,
                             pad = "0", side = "left"),
                str_pad(string = as.hexmode(regex_values[2]), width = 2,
                        pad = "0", side = "left"),
                str_pad(string = as.hexmode(regex_values[3]), width = 2,
                        pad = "0", side = "left")))
}

#### gg waterfall - builds a barebone ggplot waterfall chart
ggwaterfall <- function(data, x, y, bar_type, planted_flag,
                        start_y_min = 0, end_y_min = 0,
                        planted_y_min = 0, fill_variable,
                        line_colour = "#696A6D",
                        line_type = 2, line_size = 1.4) {
  require(dplyr)
  require(ggplot2)
  
  # Create a blank data frame of NA for the rectangle drawing
  rect_data <- data_frame(y_min = rep(NA, nrow(data)),
                          y_max = rep(NA, nrow(data)),
                          x_pos = rep(NA, nrow(data)))
  
  # Add factor levels if required
  if (!is.factor(data[[substitute(x)]])) {
    data[[substitute(x)]] <- as.factor(data[[substitute(x)]])
  }
  
  ### Arrange data to factor levels
  # Create a new variable quote to arrange by
  arrange_variable <- enquo(x)
  
  # Arrange by factor levels
  data <- data %>% arrange(!!arrange_variable)
  
  # Create the data to draw the rectangles
  for (i in 1:nrow(data)) {
    if (i == 1) {
      # The first bar is planted by default
      rect_data$y_min[i] <- start_y_min
    } else if (i == nrow(data)) {
      # The last bar is planted by default
      rect_data$y_min[i] <- end_y_min
    } else if (data[[substitute(bar_type)]][i] %in% planted_flag) {
      # Plant any specified bars
      rect_data$y_min[i] <- planted_y_min
    } else {
      # Every other bar starts at the previous bar's level
      rect_data$y_min[i] <- data[[substitute(y)]][(i - 1)]
    }
  }
  
  # Fill in the remaining data
  rect_data$y_max <- data[[substitute(y)]]
  rect_data$x_pos <- data[[substitute(x)]]
  rect_data$fill_var <- data[[substitute(fill_variable)]]
  
  # Create a data frame of line segment data
  line_data <- rect_data
  line_data$x_pos_end <- c(line_data$x_pos[2:nrow(line_data)], NA)
  line_data <- line_data[1:(nrow(line_data) - 1), ]
  
  # Return the barebones plot
  return(ggplot() +
           geom_rect(data = rect_data,
                     aes(xmin = as.numeric(x_pos) - 0.4,
                         xmax = as.numeric(x_pos) + 0.4,
                         ymin = y_min, ymax = y_max,
                         fill = fill_var)) +
           geom_segment(data = line_data,
                        aes(x = as.numeric(x_pos),
                            xend = as.numeric(x_pos_end),
                            y = y_max, yend = y_max),
                        col = line_colour, size = line_size,
                        linetype = line_type) +
           scale_x_continuous(breaks = as.numeric(rect_data$x_pos),
                              labels = levels(rect_data$x_pos)))
  
}

### An example of using ggwaterfall
### Use the factor levels of the x variable to choose the order of the x variable
# data_test <- data.frame(yy = c(50, 100, 150, 200, 200, 250, 250),
#                     xx = factor(c("a", "b", "c", "d", "e", "f", "g")),
#                     bar_bar = c("ye", "na", "na", "na", "ye", "na", "ye"))
#
# ggwaterfall(data = data_test, x = xx, y = yy, bar_type = bar_bar, planted_flag = "ye",
#             fill_variable = bar_bar)


#### geom_interpolated ribbon - draws a geom_ribbon that goes around corners a bit better
# Generally, a ribbon drawn around intersecting lines doens't draw very prettily
# Using linear interpolation around data, you can jazz it up
geom_interpolated_ribbon <- function(data, x, ymin, ymax,
                                     interpolation = 20,
                                     fill_colour = NULL, fill_alpha = NULL,
                                     restrict_to_minimum = FALSE) {
  
  require(dplyr)
  require(ggplot2)
  
  if (is.null(fill_colour)) {
    fill_colour <- "#008698"
  }
  if (is.null(fill_alpha)) {
    fill_alpha <- 1
  }
  
  # Create a new variable name to arrange by
  arrange_variable <- enquo(x)
  
  # Arrange by the x variable
  data <- data %>% arrange(!!arrange_variable)
  
  # Intialise an interpolated data_frame
  inter_data <- data_frame()
  
  # Loop over observations to interpolate the ymin variable
  for (i in 1:(nrow(data) - 1)) {
    inter_data <- bind_rows(inter_data,
                            as_data_frame(approx(x = data[[substitute(x)]][i:(i + 1)],
                                                 y = data[[substitute(ymin)]][i:(i + 1)],
                                                 n = interpolation)))
  }
  
  # Rename the y variable to ymin
  inter_data <- rename(inter_data, ymin = y)
  
  
  # Initialise the data for the interpolated ymax
  inter_y_max <- data_frame()
  
  # Loop over observations to interpolate the ymax
  for (i in 1:(nrow(data) - 1)) {
    inter_y_max <- bind_rows(inter_y_max,
                             as_data_frame(approx(x = data[[substitute(x)]][i:(i + 1)],
                                                  y = data[[substitute(ymax)]][i:(i + 1)],
                                                  n = interpolation)))
  }
  
  # Bind to bigger data
  inter_data$ymax <- inter_y_max$y
  
  # If the ribbon is restricted to the minimum, it will not swap the
  # min and max variables
  if (!restrict_to_minimum) {
    # Store to a variable
    ribbon <- geom_ribbon(data = inter_data,
                          aes(x = x, ymin = ymin, ymax = ymax),
                          fill = fill_colour, alpha = fill_alpha)
  } else {
    # Store to a variable
    ribbon <- geom_ribbon(data = inter_data,
                          aes(x = x, ymin = pmin(ymin, ymax), ymax = ymax),
                          fill = fill_colour, alpha = fill_alpha)
  }
  
  
  # Return the geom_ribbon
  return(ribbon)
  
  
}
### Test case using geom_interpolated ribbon
# library(dplyr)
# library(ggplot2)
# data_test <- data_frame(xx = c(1:10),
#                         yy1 = c(1:5, 7:11),
#                         yy2 = c(rep(1, 5), rep(10, 5)))
#
# # When you want to restrict the area to drawn only when the ymax
#     # variable is greater than the ymin variable, some weird things happen
# ggplot() +
#     geom_ribbon(data = data_test,
#                 aes(x = xx, ymin = pmin(yy1, yy2), ymax = yy2)) +
#     geom_line(data = data_test,
#               aes(x = xx, y = yy1),
#               col = "#c94b20", size = 1.4) +
#     geom_line(data = data_test,
#               aes(x = xx, y = yy2),
#               col = "#b5a991", size = 1.4)

# # Example 1: Where the ribbon will fill the area between the series
# ggplot() +
#     geom_interpolated_ribbon(data = data_test,
#                              x = xx, ymin = yy1,
#                              ymax = yy2,
#                              restrict_to_minimum = FALSE) +
#     geom_line(data = data_test,
#               aes(x = xx, y = yy1),
#               col = "#c94b20", size = 1.4) +
#     geom_line(data = data_test,
#               aes(x = xx, y = yy2),
#               col = "#b5a991", size = 1.4)

# # Example 2: Where the ribbon will draw only when the ymax variable
#    # is greater than the ymin variable
#    # Very pretty!
# ggplot() +
#     geom_interpolated_ribbon(data = data_test,
#                              x = xx, ymin = yy1,
#                              ymax = yy2,
#                              restrict_to_minimum = TRUE) +
#     geom_line(data = data_test,
#               aes(x = xx, y = yy1),
#               col = "#c94b20", size = 1.4) +
#     geom_line(data = data_test,
#               aes(x = xx, y = yy2),
#               col = "#b5a991", size = 1.4)


#### Infix operator to swap the values of variables
# ie, value of x assigned to y and value of y assigned to x
`%swap%` <- function(x, y) {
  temp_x <- x
  temp_y <- y
  
  eval.parent(substitute(x <- temp_y))
  eval.parent(substitute(y <- temp_x))
}

#### Gives the numerical index or range of excel column (or general base 26 indices)
# Returns one value if one column reference given
# Returns a vector if two column references given
excel_column <- function(column, ...) {
  
  collector <- list(...)
  
  # Generic function of outputting column index
  base_26_calc <- function(xx) {
    
    xx <- tolower(xx)
    do_the_splits <- strsplit(x = xx, split = "")[[1]]
    numbers_and_letters <- match(do_the_splits, letters)
    
    this_column_is <- sum(numbers_and_letters * (26 ^ rev((1:length(numbers_and_letters)) - 1)))
    
    return(this_column_is)
  }
  
  # Return one column number if only one argument
  if (length(collector) == 0) {
    
    return(base_26_calc(column))
    
    # Return a range if two columns given
  } else if (length(collector) == 1) {
    
    return(base_26_calc(column):base_26_calc(collector))
    
  } else {
    
    stop("A maximum of two column references can be given.")
    
  }
  
}


### Plotting polygons with holes in them in ggplot
# Stolen from the internet
# https://qiita.com/kohske/items/9272e29a75d32416ff5e
GeomHolygon <- ggplot2::ggproto("GeomHolygon",
                                ggplot2::GeomPolygon,
                                extra_params = c("na.rm", "rule"),
                                draw_panel = function(data, scales, coordinates, rule) {
                                  n <- nrow(data)
                                  if (n == 1)
                                    return(zeroGrob())
                                  
                                  munched <- coord_munch(coordinates, data, scales)
                                  munched <- munched[order(munched$group), ]
                                  
                                  first_idx <- !duplicated(munched$group)
                                  first_rows <- munched[first_idx, ]
                                  
                                  ggplot2:::ggname(
                                    "geom_holygon",
                                    pathGrob(munched$x, munched$y, default.units = "native",
                                             id = munched$group, rule = rule,
                                             gp = gpar(col = first_rows$colour,
                                                       fill = alpha(first_rows$fill, first_rows$alpha),
                                                       lwd = first_rows$size * .pt,
                                                       lty = first_rows$linetype)))
                                }
)

geom_holygon <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, rule = "winding", ...) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomHolygon,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm , rule = rule, ...))
  
}

### Binary string concatenation
`%&%` <- function(x, y) {
  return(paste0(x, y))
}
# c("Dogs", "Cats") %&% " are all " %&% c("girls", "boys")

#### SQL-like like wildcard
`%like%` <- function(x, y) {
  return(grepl(y, x))
}

#### SQL-like not-like wildcard
`%notlike%` <- function(x, y) {
  return(!grepl(y, x))
}

#### Digital sum function - Just for N Twort - sorry Stuart I mistakenly called this 'binary' sum when it is definitely 'digital'
digital_sum <- function(x) {
  temp <- x
  
  while (temp >= 10) {
    digits <- ceiling(log10(temp + 1))
    temp <- sum((temp %% (10 ^ (1:digits))) %/% (10 ^ (0:(digits - 1))))
  }
  
  return(temp)
}

#### Dice simulator
dice_roller <- function(sides = 6, times = 1, dice = 1) {
  if (dice > 1) {
    return(lapply(X = 1:dice, FUN = function(x) unlist(lapply(1:times, FUN = function(x) which(rmultinom(1, 1, rep((1 / sides), sides)) == 1)))))
  } else {
    return(unlist(lapply(1:times, FUN = function(x) which(rmultinom(1, 1, rep((1 / sides), sides)) == 1))))
  }
}

#### Convert phrase on clipboard to poem split across selected number of lines
poetry_in_motion <- function(lines) {
  
  x <- unlist(strsplit(readClipboard(), " "))
  
  if (length(x) < lines) {
    stop('Number of lines cannot exceed number of words')
  }
  
  if (lines <= 0) {
    stop('Number of lines must be greater than zero')
  }
  
  sample <- sample(2:length(x) - 1, lines - 1)
  
  y <- ifelse(1:length(x) %in% sample, "\n", " ")
  
  z <- paste(paste0(x, y), collapse = "")
  
  cat(z)
  
}

#### Convert date time data to calendar date
dttm_to_date <- function(dttm) {
  require(lubridate)
  return(ymd(paste(year(dttm), month(dttm), day(dttm), sep = "-")))
}

#### Translate calendar dates to the date of the first of the month
date_to_first_of_month <- function(date) {
  require(lubridate)
  return(ymd(paste(year(date), month(date), "01", sep = "-")))
}

#### Nothing to see here ####
the_ad_scale <- function(celebrity = "Adam Driver", ca = -8, bb = 10, rater="Anon") {
  source("//HKDC01/Shared Folders/Company/ggplot_theme/houstonkemp_theme.r")
  require(dplyr)
  require(magrittr)
  require(readr)
  require(ggplot2)
  if(ca < -10 | ca > 10) {
    warning("Error: conventional attractiveness must be between -10 and 10")
    return()
  }
  if(bb < -10 | bb > 10) {
    return("Error: bb factor must be between -10 and 10")
  }
  if(celebrity == "Adam Driver" & ca > 0) {
    return("Error: Adam Driver is not conventionally attractive")
  }
  if(celebrity == "Adam Driver" & bb < 10) {
    return("Error: Adam Driver is totes bangable")
  }
  current_ratings <- read_csv("//HKDC01/Shared Folders/Company/ReferenceFormatter/The AD scale/ratings.csv")
  current_ratings <- rbind(current_ratings, list(celebrity, ca, bb, rater, as.POSIXct(Sys.time())))
  write_csv(current_ratings, path = "//HKDC01/Shared Folders/Company/ReferenceFormatter/The AD scale/ratings.csv")
  current_ratings %>%
    ggplot(aes(x = ca, y = bb)) +
    geom_point(colour = houstonkemp_teal[1], size = 2) +
    geom_text(aes(label=celebrity), colour = "black",hjust=0, vjust=0) +
    coord_cartesian(xlim = c(-10, 10),
                    ylim = c(-10, 10)) +
    scale_y_continuous(minor_breaks = seq(-10, 10, 1),
                       breaks = c(-10, 0, 10)) +
    scale_x_continuous(minor_breaks = seq(-10, 10, 1),
                       breaks = c(-10, 0, 10)) +
    theme(panel.background = element_rect(fill = NA),
          panel.grid.minor = element_line(colour = "grey80"),
          panel.grid.major = element_line(colour = "black"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
}

view_the_ad_scale <- function(celebrity_filter = "All", rater_filter = "All") {
  source("//HKDC01/Shared Folders/Company/ggplot_theme/houstonkemp_theme.r")
  require(dplyr)
  require(magrittr)
  require(readr)
  require(ggplot2)
  current_ratings <- read_csv("//HKDC01/Shared Folders/Company/ReferenceFormatter/The AD scale/ratings.csv")
  if (celebrity_filter != "All") {
    current_ratings %<>% filter(celebrity == celebrity_filter)
  }
  if (rater_filter != "All") {
    current_ratings %<>% filter(rater == rater_filter)
  }
  current_ratings %>%
    ggplot(aes(x = ca, y = bb)) +
    geom_point(colour = houstonkemp_teal[1], size = 2) +
    geom_text(aes(label=celebrity), colour = "black",hjust=0, vjust=0) +
    coord_cartesian(xlim = c(-10, 10),
                    ylim = c(-10, 10)) +
    scale_y_continuous(minor_breaks = seq(-10, 10, 1),
                       breaks = c(-10, 0, 10)) +
    scale_x_continuous(minor_breaks = seq(-10, 10, 1),
                       breaks = c(-10, 0, 10)) +
    theme(panel.background = element_rect(fill = NA),
          panel.grid.minor = element_line(colour = "grey80"),
          panel.grid.major = element_line(colour = "black"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
}

#### Large number syntax divider ####
# To write large numbers into R in a readable manner
as_num <- function(...) {
  collector <- list(...)
  return(as.numeric(gsub(x = collector, pattern = "(?!\\.)[[:punct:]]",
                         replacement = "", perl = TRUE)))
}
# as_num("1,222,314", "1_234_244.23")

#### Replace NA in a vector to a numeric
no_na <- function(x, replace = 0) {
  x[is.na(x)] <- replace
  return(x)
}

#### Clear (delete) console history --------------------------------------------
# Useful if a password is in the history, for example
clear_history <- function() {
  write("", file=".blank")
  loadhistory(".blank")
  unlink(".blank")
}

#### Find the closest match in another vector
# Returns the element from y that is closest in least absolute terms in x
closest <- function(x, y) {
  
  # Function that returns for one element in x
  return_closest <- function (x, y) {
    return(y[which.min(x = abs(x - y))])
  }
  
  # Apply over all of x
  vapply(X = x, FUN = return_closest, FUN.VALUE = as.numeric(length(x)), y)
}

#### Adds little tick marks for pasting code into 8x8
copy_for_64 <- function() {
  x <- readClipboard()
  y <- trimws(x)
  y <- paste0("`",y,"`")
  z <- regexpr("^\\s+", x)
  x <- paste0(strrep(" ",ifelse(attr(z, "match.length") == -1, 0, attr(z, "match.length"))), y)
  x <- gsub("^\\s*``$", "", x)
  writeClipboard(x)
}

#### Turns a string into a wavey boi
phrase_wave <- function(string, amplitude = 5, interval = 1 / 2,
                        orientation = "horizontal") {
  
  if (orientation == "vertical") {
    # Split the string into characters
    string_characters <- unlist(strsplit(x = string, ""))
    
    # Find the amplitude of the leading white space
    spaces <- amplitude * sin(1:length(string_characters) / length(string_characters) / interval * 2 * pi)
    
    # Convert to Integer spaces
    spaces <- round(spaces - min(spaces))
    
    # Create a vector of white spaces
    space_vector <- vapply(spaces, function(x) return(paste(rep(" ", x), collapse = "")), FUN.VALUE = character(1))
    
    # Paste string and whitespace together
    wave <- paste0(space_vector, string_characters, collapse = "\n")
    
    # Write to clipboard
    writeClipboard(str = wave)        
    
  } else if (orientation == "horizontal") {
    # Split the string into characters
    string_characters <- unlist(strsplit(x = string, ""))
    
    # Find the amplitude of the leading white space
    spaces <- amplitude * sin(1:length(string_characters) / length(string_characters) / interval * 2 * pi - (pi / 2))
    
    # Convert to Integer spaces
    spaces <- round(spaces - min(spaces) + 0.5)
    
    # Create a matrix of spaces
    space_matrix <- matrix(data = rep(" ", max(spaces) * length(string_characters)),
                           nrow = max(spaces))
    
    for (i in 1:length(string_characters)) {
      space_matrix[spaces[i], i] <- string_characters[i]
    }
    
    wave <- ""
    
    for (i in 1:nrow(space_matrix)) {
      wave <- paste0(c(wave, paste(space_matrix[i, ], collapse = "  "), "\n"), collapse = "")
    }
    
    # Write to clipboard
    writeClipboard(str = wave)                
  }
  
}




#### Ability to use mapshot when storing files on a network drive
mapshot_network <- function(
  x, # Leaflet map
  url = NULL, # url if wanting to save the html
  file = NULL, # file name if wanting to save a png
  remove_url = TRUE, # Only used if url is NULL
  remove_controls = c("zoomControl", "layersControl", "homeButton"),
  ...
) {
  tmp_dir <- tempdir()
  if (!is.null(file)) {
    filename <- paste0(tmp_dir, "/", gsub("(.*)/([^/]*$)", "\\2", file))
    if (!is.null(url)) {
      urlname <- paste0(tmp_dir, "/", gsub("(.*)/([^/]*$)", "\\2", url))
      mapview::mapshot(x = x, url = urlname, file = filename,
                       remove_url = remove_url,
                       remove_controls = remove_controls, ... = ...)
    } else {
      mapview::mapshot(x = x, url = url, file = filename,
                       remove_url = remove_url,
                       remove_controls = remove_controls, ... = ...)
    }
  } else if(!is.null(url)) {
    urlname <- paste0(tmp_dir, "/", gsub("(.*)/([^/]*$)", "\\2", url))
    mapview::mapshot(x = x, url = urlname, file = file,
                     remove_url = remove_url,
                     remove_controls = remove_controls, ... = ...)
  } else {
    mapview::mapshot(x = x, url = url, file = file,
                     remove_url = remove_url,
                     remove_controls = remove_controls, ... = ...)
  }
  
  if (!is.null(file)) {
    file.rename(filename, file)
    cat(paste0("Successfully created ", file, "\n"))
  }
  if (!is.null(url)) {
    file.rename(urlname, url)
    cat(paste0("Successfully created ", url))
  }
}

# Function to create regex to gsub replacement before a symbol
replace_before <- function(string, symbol, replacement,
                           ignore.case = FALSE, perl = FALSE,
                           fixed = FALSE, useBytes = FALSE) {
  # Create the regex pattern 
  regex_pattern <- paste0(".*", symbol)
  
  # replace string before the pattern
  gsub(pattern = regex_pattern, replacement = replacement,
       x = string, ignore.case = ignore.case, perl = perl,
       fixed = fixed, useBytes = useBytes)
}

# Function to create regex to gsub replacement after a symbol
replace_after <- function(string, symbol, replacement,
                          ignore.case = FALSE, perl = FALSE,
                          fixed = FALSE, useBytes = FALSE) {
  # Create the regex pattern 
  regex_pattern <- paste0(symbol, ".*")
  
  # replace string before the pattern
  gsub(pattern = regex_pattern, replacement = replacement,
       x = string, ignore.case = ignore.case, perl = perl,
       fixed = fixed, useBytes = useBytes)
}

case_wheeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeen <- function(...) {
  case_when(... = ...)
}

# Ensure that y-axis has a tick label above (below) the max (min) y data value
# Usage:
#   plot <- ggplot(mtcars) + geom_point(aes(x = mpg, y = drat))
#   plot + expandy(plot)
expandy <-  function(plot, type = c("max", "min"), percent = FALSE) {
  
  max_y <- max(layer_data(plot)$y, na.rm = TRUE)
  min_log <- floor(log10(max_y))
  
  min_y <- min(layer_data(plot)$y, na.rm = TRUE)
  max_log <- ceiling(log10(min_y))
  
  expand_limits(
    y = na.omit(c(
      if ("min" %in% type) {
        if (percent) {
          0
        } else {
          floor(min_y / 10^max_log) * 10^max_log
        }
      } else { NA },
      if ("max" %in% type) {
        if (percent) {
          1
        } else {
          ceiling(max_y / 10^min_log) * 10^min_log
        }
      } else { NA }
    ))
  )
}
