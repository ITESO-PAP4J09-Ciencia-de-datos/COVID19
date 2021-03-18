# Script de prueba Kevin

# Base R plot
plot(cars)

# ggplot

library(tidyverse)

cars %>% 
  ggplot(aes(x = speed, y = dist)) +
  geom_point()
