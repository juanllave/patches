# This is the final version (for now).
# In this iteration, the mid and outer are created in each iteration of the plot, based on three initially generated colors.

library(tidyverse)
library(magick)
# library(scales)

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

# Another function was needed to prevent colors repeating for both repetitions of mid and outer
pick_two_different_colors <- function() {
  colors <- sample(rcolors, 2, replace = FALSE)
}

# Create a function to pick one of the inner columns at random
pick_random_inner <- function() {
  sample(c('inner1', 'inner2', 'inner3', 'inner4', 'inner5'), 1)
}

# Create a tibble using one random color picked at the start
create_flag_colors <- function() {
  tibble(
    inner1 = rep(pick_random_color(), 54), 
    mid = rep(pick_two_different_colors(), 27),
    outer = rep(pick_two_different_colors(), 27),
    x = rep(1:6, each = 9), 
    y = rep(1:9, times = 6),
    inner2 = rep(pick_random_color(), 54), 
    inner3 = rep(pick_random_color(), 54), 
    picker4 = sample(rep(c(1, 2), times = 27)),
    inner4 = if_else(picker4 == 1, outer, pick_random_color()), 
    picker5 = rep(c(pick_random_color(), pick_random_color(), 'a', 'a'), times = 14)[1:54] |>
      matrix(nrow = 9, ncol = 6, byrow = TRUE) |>
      as.vector(),
    inner5 = if_else(picker5 == 'a', outer, pick_random_color())
  )
}

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
plot_files <- vector('list', 10)
for (i in 1:10) {
  flag_colors <- create_flag_colors()  # Create flag colors for each plot
  p <- patch_plot(flag_colors, inner = pick_random_inner())
  plot_file <- paste0('ri', i, '.png')
  ggsave(plot_file, p, width = 6, height = 9)
  plot_files[[i]] <- plot_file
}

# Save as vector to ensure magick works
plot_files <- as_vector(plot_files)

# Use magick to combine the plots into an animated GIF
images <- image_read(plot_files)
animation <- image_animate(images, fps = 2.5)  # 5 frames per second

# Save the animated GIF
image_write(animation, 'ri.gif')
