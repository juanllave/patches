library(updateme)
library(tidyverse)
library(markdown)
library(magick)
library(gganimate)

setwd('~/Desktop')

# Loop to generate and store 3 random colors in separate values
for (i in 1:3) {
  color_name <- paste0("rc", i)
  assign(color_name, rgb(runif(1), runif(1), runif(1)))
}

# Print the colors to preview them
scales::show_col(c(rc1, rc2, rc3), ncol = 3)

# Create a vector with the colors to pick one at random later
rcolors <- c(rc1, rc2, rc3)

# Function to pick a random color from rcolors
pick_random_color <- function() {
  sample(rcolors, 1)
}

# Create a tibble using one random color picked at the start
# Replacements allows colors to be repeated
flag_colors <- tibble(
  inner1 = rep(sample(rcolors, 1, replace = TRUE), 54), 
  mid = rep(sample(rcolors, 2, replace = TRUE), 27),
  outer = rep(sample(rcolors, 2, replace = TRUE), 27), 
  x = rep(1:6, each = 9), 
  y = rep(1:9, times = 6),
  inner2 = rep(sample(rcolors, 4, replace = TRUE), times = 14)[1:54], 
  inner3 = rep(sample(rcolors, 4, replace = TRUE), times = 14)[1:54], 
  picker4 = sample(rep(c(1, 2), times = 27)),
  inner4 = if_else(picker4 == 1, outer, sample(rcolors, 1, replace = TRUE)), 
  picker5 = rep(c(sample(rcolors, 1, replace = TRUE), sample(rcolors, 1, replace = TRUE), "a", "a"), times = 14)[1:54] |>
    matrix(nrow = 9, ncol = 6, byrow = TRUE) |>
    as.vector(),
  inner5 = if_else(picker5 == "a", outer, sample(rcolors, 1, replace = TRUE))
)

# Create a function to pick one of the inner at random
pick_random_inner <- function() {
  sample(c('inner1', 'inner2', 'inner3', 'inner4', 'inner5'), 1)
}

# Randomly pick one of the inner columns
random_inner <- pick_random_inner()

# Create a function for the patch plot
patch_plot <- function(data, inner, cons = 2.15, radius = 0.25){
  c1 = 0.25*(sqrt(5) - 1)*radius
  c2 = 0.25*(sqrt(5) + 1)*radius
  s1 = 0.25*(sqrt(10 + 2*sqrt(5)))*radius
  s2 = 0.25*(sqrt(10 - 2*sqrt(5)))*radius
  
  ggplot(data = data, aes(x = x, y = y, color = mid)) +
    geom_point(aes(color = outer), size = 16*cons, shape = 15) +
    geom_point(aes(y = y + radius), size = 5*cons) +
    geom_point(aes(x = x + s1, y = y + c1), size = 5*cons) +
    geom_point(aes(x = x + s2, y = y - c2), size = 5*cons) +
    geom_point(aes(x = x - s2, y = y - c2), size = 5*cons) +
    geom_point(aes(x = x - s1, y = y + c1), size = 5*cons) +
    geom_point(aes(color = .data[[inner]]), size = 4*cons) + 
    scale_color_identity() + 
    coord_fixed() + 
    xlim(c(0.5, 6.5)) +
    ylim(c(0.5, 9.5)) +
    theme_void()
}

# Generate and save 10 plots
plot_files <- vector("list", 10)
for (i in 1:10) {
    p <- patch_plot(flag_colors, inner = inner1)
  plot_file <- paste0("pp", i, ".png")
  ggsave(plot_file, p, width = 6, height = 9)
  plot_files[[i]] <- plot_file
}
  
# # Use magick to combine the plots into an animated GIF
# images <- image_read(plot_files)
# animation <- image_animate(images, fps = 1)  # 1 frame per second
# 
# # Save the animated GIF
# image_write(animation, 'animated_plots.gif')