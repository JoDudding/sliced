library(tidyverse)

#-------------------------------------------------------------------------------
# hexagon
#-------------------------------------------------------------------------------

hex_r <- 1
hex_R <- hex_r * sqrt(3) / 2

y <- c(
  1.5 * hex_r,
  2.0 * hex_r,
  1.5 * hex_r,
  0.5 * hex_r,
  0.0 * hex_r,
  0.5 * hex_r
  )

x <- c(
  2 * hex_R,
  hex_R,
  0,
  0,
  hex_R,
  2 * hex_R
)

hexagon <- as.data.frame(cbind(x, y)) %>%
  mutate(
    val = 1,
    id = 1
  )

#-------------------------------------------------------------------------------
#' repeat hexagons
#-------------------------------------------------------------------------------

repeats <- tibble(
     hex_x = rep(-1:200, times = 6),
     hex_y = rep(-1:4, each = 202),
     id = 1,
     code = 1:(6*202)
  ) %>%
  filter(
    hex_x %% 2 == hex_y %% 2
  ) %>%
  glimpse()

repeats_hex <- repeats %>%
 left_join(hexagon, by = 'id') %>%
 mutate(
    x = x + hex_x * hex_R,
    y = y + hex_y * hex_r * 1.5
  ) %>%
  group_by(code) %>%
  mutate(
    mid_x = (max(x) - min(x)) / 2 + min(x),
    mid_y = (max(y) - min(y)) / 2 + min(y)
  ) %>%
  select(-id) %>% 
  ungroup()

#-------------------------------------------------------------------------------
#' plot
#-------------------------------------------------------------------------------

ggplot(repeats_hex) +
  annotate("rect", xmin = -1, xmax = 35 * 4 + 1, ymin = -1, ymax = 2 * 4 + 1,
    fill = '#5F187F') +
  geom_polygon(aes(x = x, y = y, group = code),
    colour = "#ebe3ef", size = 0.3, fill = NA
  ) +
  scale_x_continuous(expand = c(0, 0)) +  
  scale_y_continuous(expand = c(0, 0)) +  
  coord_fixed(xlim = c(0, 35*4), ylim = c(0, 2*4)) +
  theme_void()

#-------------------------------------------------------------------------------
#' save
#-------------------------------------------------------------------------------

ggsave("sliced/banner.png", device = "png", units = "cm", 
  width = 35, height = 2)

#-------------------------------------------------------------------------------
