# In this version I am getting closer to where I want to be, but not there yet.
# Instead of creating a pool of three colors, I created a variable to store colors().
# From this variable, colors will be chosen at random during the creation of the tibble.
# This allows for a wider variety of colors to be used the inners whilst maintaining mid and outter consistency.

library(updateme)
library(tidyverse)

setwd('~/Desktop')

# Create a variable to store colors
rcolors <- colors()

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
