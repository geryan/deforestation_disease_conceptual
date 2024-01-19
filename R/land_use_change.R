library(ggplot2)
library(dplyr)
library(tidyr)

decrease <- function(x, y){
  ifelse(y < 0, x + y, x)
}

stop0 <- function(x){
  ifelse(x > 0, x, 0)
}


land_use <- tibble(
  year = 1:120
) %>% 
  mutate(
    urban = year/200,
    periurban = 0.10,
    rural = 0.25,
    edge = 0.05,
    forest = 0.60,
    total = urban + periurban + rural + edge + forest
  ) %>%
  mutate(
    forest = forest - (total - 1),
    edge = decrease(edge, forest),
    rural = decrease(rural, edge),
    periurban = decrease(periurban, rural),
    forest = stop0(forest),
    edge = stop0(edge),
    rural = stop0(rural),
    periurban = stop0(periurban),
    total = urban + periurban + rural + edge + forest
  ) %>%
  mutate(rural = round(rural, digits = 3)) %>% # floating point bullshit
  select(-total) %>%
  #print(n = 201) 
  pivot_longer(
    cols = -year,
    names_to = "land_use",
    values_to = "proportion"
  ) %>%
  mutate(
    land_use = factor(
      land_use,
      levels = c("forest", "edge", "rural", "periurban", "urban")
    )
  )


land_use %>% 
  ggplot +
  geom_area(
    aes(
      x = year,
      y = proportion,
      fill = land_use
    )
  ) +
  #scale_fill_viridis_d(begin = 0.75, end = 0)
  scale_fill_manual(values = c("green4", "springgreen2", "lightgoldenrod1", "darkorchid3", "darkorchid4"))