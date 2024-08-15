# Getting even closer.
# This iteration randomizes the color selection in the tibble, but again from only a three-color pool.
# The end result are plots that can have all inner, mid, and outer in the same color. Outer consistency is maintained.

library(updateme)
library(tidyverse)

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

# Another funciton was needed to prevent colors repeating for both repetitions of mid and outer
pick_two_different_colors <- function() {
  colors <- sample(rcolors, 2, replace = FALSE)
}

# Create a tibble using one random color picked at the start
flag_colors <- tibble(
  inner1 = rep(pick_random_color(), 54), 
  # Innitially this was a solution I used, but given a pool of only three colors, the risk of getting the same twice was very high.
  # mid = rep(pick_random_color(), 27) %>% rep(each = 2),
  # outer = rep(pick_random_color(), 27) %>% rep(each = 2), 
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
    geom_point(aes(color = {{inner}}), size = 4*cons) + 
    scale_color_identity() + 
    coord_fixed() + 
    xlim(c(0.5, 6.5)) +
    ylim(c(0.5, 9.5)) +
    theme_void()
}

# Create and save 5 plots, one for each of the inners. 
i1 = patch_plot(data = flag_colors, inner = inner1)
i2 = patch_plot(data = flag_colors, inner = inner2)
i3 = patch_plot(data = flag_colors, inner = inner3)
i4 = patch_plot(data = flag_colors, inner = inner4)
i5 = patch_plot(data = flag_colors, inner = inner5)

ggsave(filename = 'i1.png', plot = i1, width = 7, height = 10)
ggsave(filename = 'i2.png', plot = i2, width = 7, height = 10)
ggsave(filename = 'i3.png', plot = i3, width = 7, height = 10)
ggsave(filename = 'i4.png', plot = i4, width = 7, height = 10)
ggsave(filename = 'i5.png', plot = i5, width = 7, height = 10)
