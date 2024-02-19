# Load necessary libraries, install them if they are not installed
# install.packages("tidyverse", "ggpattern", dependencies = TRUE)
library(tidyverse)
library(ggpattern)

squares <- read_csv("squares.csv")

# Randomize squares and repeat based on quantity
set.seed(42) # For reproducibility
squares <- squares[rep(row.names(squares), squares$quantity),]

# Shuffle the expanded dataset
squares <- squares[sample(nrow(squares)),]

# Let's first find out a suitable grid size
total_squares <- nrow(squares)
grid_height <- round(sqrt(total_squares / (60 / 50)))
grid_width <- ceiling(total_squares / grid_height)
square_size <- 9 # cm

# Create a dataframe for plotting
plot_data <- data.frame(
  x = rep(1:grid_width, each = grid_height)[1:total_squares],
  y = rep(1:grid_height, times = grid_width)[1:total_squares],
  color = squares$forecolor,
  pattern_color = squares$backcolor,
  pattern = squares$pattern
)

plot_colors <- unique(c(squares$forecolor, squares$backcolor)) %>%
  discard(is.na) %>%
  set_names() %>%
  map(~as.character(.x))

# Adjusting the ggplot code for specific pattern directions and appearances
ggplot(plot_data, aes(x = x, y = y)) +
  # Add helper lines every 5 squares rowwise and columnwise
  geom_tile_pattern(
    aes(fill = color, pattern = pattern, pattern_fill = pattern_color),
    pattern_spacing = 0.02, pattern_size = 0,
    color = 'black', linewidth = 0.1,
    pattern_angle = 90, # Adjust angle for 'crosshatch' pattern
  ) +
  # Use unique HEX colors from your data
  scale_fill_manual(values = plot_colors, guide = NULL) +
  scale_pattern_fill_manual(values = plot_colors, guide = FALSE) +
  scale_pattern_manual(values = c("none" = "none", "checkered" = "crosshatch", "lines" = "stripe"), guide = NULL) +
  theme_void() +
  labs(title = paste("Irregular Throw Pattern of size", grid_height * square_size, 'x', grid_width * square_size, "cm"),
       subtitle = paste("Each square represents a ", square_size, "x", square_size, "cm puff square")) +
  coord_fixed() # Equal aspect ratio

ggsave("arrangement.png", width = grid_width, height = grid_height + 1, units = "cm")