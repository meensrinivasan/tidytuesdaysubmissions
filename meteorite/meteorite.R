library(tidyverse)
library(ggthemes)
library(scales)

meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv") %>%
  filter(!is.na(year)) %>%
  filter(!is.na(lat)) %>%
  filter(!is.na(long)) %>% 
  filter(!is.na(mass)) %>%
  mutate(year_cat = case_when(year < 1700 ~ 1,
                              year >= 1700 & year < 1800 ~ 2,
                              year >= 1800 & year < 1850 ~ 3,
                              year >= 1850 & year < 1900 ~ 4,
                              year >= 1900 & year < 1950 ~ 5,
                              year >= 1950 ~ 6))
ggplot() + 
  borders("world", colour = "#232323", fill = "#232323") +
  theme_map(base_family = "Merriweather") +
  coord_map(projection = "mercator", orientation = c(90, 0, 0)) +
  geom_point(data = subset(meteorites, year_cat == 1), color = "#fcef02",
             aes(x = long, y = lat, size = mass), alpha = .7) +
  geom_point(data = subset(meteorites, year_cat == 2), color = "#fcdf02",
             aes(x = long, y = lat, size = mass), alpha = .7) +
  geom_point(data = subset(meteorites, year_cat == 3), color = "#fcd602",
             aes(x = long, y = lat, size = mass), alpha = .7) +
  geom_point(data = subset(meteorites, year_cat == 4), color = "#fcc202",
             aes(x = long, y = lat, size = mass), alpha = .7) +
  geom_point(data = subset(meteorites, year_cat == 5), color = "#fc9402",
             aes(x = long, y = lat, size = mass), alpha = .7) +
  geom_point(data = subset(meteorites, year_cat == 6), color = "#fc7202",
             aes(x = long, y = lat, size = mass), alpha = .7) +
  scale_size_continuous(name = "Mass", range = c(1, 20), labels = comma_format()) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(color = "white", hjust = 0.5, size = 20,
                              face = "bold"),
    plot.subtitle = element_text(color = "white", hjust = 0.5, size = 15, face = "italic"),
    plot.caption = element_text(color = "white", size = 6),
    plot.background = element_rect(fill = "#1D1B1C"),
    panel.background = element_rect(fill = "#1D1B1C", color = "#1D1B1C"),
    plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  labs(title = "Every recorded meterorite strike on Earth since 860 AD mapped",
    subtitle = "Life goal: To see a meteor shower :) ",
    caption = "Lighter points indicate older strikes and darker points indicate newer strikes\nData: NASA, Code: @srini_meen") +
  annotate("text", x = -170, y = -60 , color = "#f1f1f1", hjust = 0,
           fontface = "italic", size = 3, family = "Merriweather",
           label = "A meteorite is a solid piece of debris\n from a comet, asteroid, or meteoroid,\n that originates in outer space and \nsurvives its passage through the atmosphere\n to reach the surface of a planet") 


ggsave("meteormap.png", width = 15, height = 8)
