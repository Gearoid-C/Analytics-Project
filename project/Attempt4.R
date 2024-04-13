# Libraries 

install.packages("tidyverse")
install.packages("plotly")
install.packages("ggplot2")
library (tidyverse)
library (ggplot2)
library (plotly)

# Importing Datasets

unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
unicef_metadata <- read_csv("unicef_metadata.csv")

# Joining Datasets

data_join <- full_join(unicef_indicator_1, unicef_indicator_2)
data_join_1 <- full_join(unicef_indicator_2, unicef_metadata)
data_join_2 <- full_join(unicef_indicator_1, unicef_metadata)

# World Map

map_world <- map_data("world")
map_data_join <- full_join(data_join, map_world, by = c("country" = "region"))
ggplot(map_data_join) +
  aes(x = long, y = lat, group = group, fill = as.numeric(obs_value)) +
  geom_polygon() +
  scale_fill_gradient(name = "%", low = "green", high = "red") +
  xlab("Longitude") +
  ylab("Latitude")  +
  ggtitle("Percentage of population with no handwashing facilities at home")

# Time Series

timeseries_plot_1 <- data_join_1 %>%
  ggplot() +
  aes(time_period, obs_value_1, color = country) +
  geom_line() +
  labs(x = "Time Period", y = "Observation Value") +
  ggtitle("Mother-to-child HIV transmission rate - Rate per 100 of Population") +
  theme(legend.position = "none")

timeseries_plot_1
# Scatter Plot

filtered_data <- unicef_metadata %>%
  filter(year >= 2013 & year <= 2021)

ggplot(filtered_data, aes(x = `GDP per capita (constant 2015 US$)`, y = `Life expectancy at birth, total (years)`, label = country)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ year) +
  scale_x_continuous(
    limits = c(0, 60000),
    breaks = c(20000, 50000),
    labels = scales::unit_format(unit = "K", scale = .001)
  ) +
  labs(
    x = "GDP per Capita (constant 2015 US$)", 
    y = "Life Expectancy",
    title = "Changes in Life Expectancy Against GDP from 2013 to 2021"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Comic Sans MS", color = "blue", size = 12),
    plot.title = element_text(hjust = 0.5) # Center the title
  )

# Bar Chart

data_join %>%
  filter(country %in% c("Mali", "Bolivia", "Indonesia", "Serbia"), time_period == 2014) %>%
  group_by(country) %>%
  summarise(m_obs = mean(obs_value, na.rm = TRUE)) %>%
  ggplot() +
  aes(reorder(country, m_obs), m_obs, fill = country) +
  geom_col() +
  labs(x = "Country", y = "Average of Population with no handwashing facilities", title = "Handwashing facilities in countries in different continents") + # moved ggtitle into labs()
  theme(legend.position = "none", 
        panel.background = element_blank(),
        plot.title = element_text(color = "purple", size = 15))

