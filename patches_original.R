# RYAN MCSHANE'S SCRIPT - UNCHANGED
## Some times, when play with scripts created by others, I copy-paste them without changes if only to see how everything works.
## Some comments will be mine, noted by 'JL' in the beggining. The rest are Ryan's original comments.

library(tidyverse)

# JL - Select the colors to be used for this exercise.
lblue = "#B3DDF2"
white = "#FFFFFF"
red = "#FF0000"
scales::show_col(c(lblue, white, red), ncol = 3)

# JL - Create a tibble with the data to create the plot.
# JL - Five distinct inners are created for the center piece of each daisy in the plot.
flag_colors = tibble(
  inner1 = rep(red, 54), 
  mid = rep(c(lblue, white), 27),
  outer = rep(c(white, lblue), 27), 
  x = rep(1:6, each = 9), 
  y = rep(1:9, times = 6),
  # 2 - 4 not shown JL -  I do show this.
  inner2 = rep(c(red, lblue, white, red), times = 14)[1:54], 
  inner3 = rep(c(white, red, red, lblue), times = 14)[1:54], 
  picker4 = sample(rep(c(1, 2), times = 27)),
  inner4 = if_else(picker4 == 1, outer, red), 
  # I learned mapping was "up and down"
  # "cleverly" used matrix byrow parameter to swap to "left to right"
  picker5 = rep(c(red, red, "a", "a"), times = 14)[1:54] |>
    matrix(nrow = 9, ncol = 6, byrow = TRUE) |>
    as.vector(),
  inner5 = if_else(picker5 == "a", outer, red)
)

# JL - The next step is to create a function to create the plot with any established inner in the tibble.
# JL - Not being a mathematician, I will not pretend to understand how Regular Pentagons work.
patch_plot = function(data, inner, cons = 2.15, radius = 0.25){
  c1 = 0.25*(sqrt(5) - 1)*radius
  c2 = 0.25*(sqrt(5) + 1)*radius
  s1 = 0.25*(sqrt(10 + 2*sqrt(5)))*radius
  s2 = 0.25*(sqrt(10 - 2*sqrt(5)))*radius
  
  ggplot(data = {{data}}, aes(x = x, y = y, color = mid)) +
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

# JL - Lastly, two plots are created with different inners.
# JL - I added inners 2-4 to see the differences.
amy = patch_plot(data = flag_colors, inner = inner1)
ryan = patch_plot(data = flag_colors, inner = inner5)
i2 = patch_plot(data = flag_colors, inner = inner2)
i3 = patch_plot(data = flag_colors, inner = inner3)
i4 = patch_plot(data = flag_colors, inner = inner4)

ggsave(filename = "amy.png", plot = amy, width = 7, height = 10)
ggsave(filename = "ryan.png", plot = ryan, width = 7, height = 10)
ggsave(filename = "i2.png", plot = i2, width = 7, height = 10)
ggsave(filename = "i3.png", plot = i3, width = 7, height = 10)
ggsave(filename = "i4.png", plot = i4, width = 7, height = 10)
