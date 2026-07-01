library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)



bigfoot <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-09-13/bigfoot.csv')

unique(bigfoot$state)
nrow(bigfoot) # Hay 5021 avistamientos de bigfoot en el dataset
# Hawaii es el único estado que nunca ha tenido un avistamiento de Bigfoot


# VARIABLES

#En que estados se encontraron bigfoot (mayor a menor)
bigfoot_states <- bigfoot %>% 
  count(state) %>% 
  arrange(desc(state))

#En que county de washington tiene la mayor cantidad de avistamientos
bigfoot_washington_counties <- bigfoot %>% 
  filter(state == "Washington") %>%
  count(county) %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 10)

# Top 10 estados con la mayor cantidad de avistamientos 
bigfoot_states_limpio <- bigfoot_states %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 10)


# En que temporada se encontraron bigfoot (mayor a menor)
bigfoot_season <- bigfoot %>% 
  count(season) %>% 
  arrange(desc(season))

# En que año los avistamientos fueron más prevalentes
bigfoot_dates <- bigfoot %>%
  mutate(year = substr(date, 1, 4)) %>%
  count(year) %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 10)



# ¿En qué estado se encuentra la mayor cantidad de avistamientos de Bigfoot?
bigfoot_states_limpio %>%
  mutate(state = reorder(state, n)) %>%
  ggplot(aes(x = state, y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(
    low = "lightgreen",
    high = "darkgreen"
  ) +
  labs(
    title = "Top 10 estados con la mayor cantidad de avistamientos",
    x = "Nombre del estado",
    y = "Número de avistamientos"
  ) +
  theme(legend.position = "none")

# Gráfica que muestra donde se ubica todos los avistamientos de bigfoot
ggplot(data = bigfoot, aes(x = longitude, y = latitude)) +
  geom_point(size = .5, alpha = 0.3, na.rm = TRUE) +
  labs(
    title = "Donde se encuentran los avistamientos de Bigfoot",
    x = "Longitud",
    y = "Latitud",
  ) +
  theme_minimal()

# Washington Counties
bigfoot_washington_counties %>%
  mutate(county = reorder(county, n)) %>%
  ggplot(aes(x = county, y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(
    low = "lightgreen",
    high = "darkgreen"
  ) +
  labs(
    title = "Top 10 estados con la mayor cantidad de avistamientos",
    x = "Nombre del estado",
    y = "Número de avistamientos"
  ) +
  theme(legend.position = "none")

# Años de Bigfoot
bigfoot_dates %>%
  mutate(date = reorder(year, n)) %>%
  ggplot(aes(x = year, y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(
    low = "lightgreen",
    high = "darkgreen"
  ) +
  labs(
    title = "Años de Bigfoot",
    x = "Año",
    y = "Número de avistamientos"
  ) +
  theme(legend.position = "none")





# SEASON

# Gráfica
bigfoot_season %>%
  mutate(season = reorder(season, n)) %>%
  ggplot(aes(x = season, y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(
    low = "lightgreen",
    high = "darkgreen"
  ) +
  labs(
    title = "Bigfoot seasons",
    x = "Season",
    y = "Número de avistamientos"
  ) +
  theme(legend.position = "none")

  