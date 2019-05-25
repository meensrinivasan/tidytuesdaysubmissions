extrafont::loadfonts(device = "win")

library(tidyverse)
library(janitor)
library(countrycode)
library(ggrepel)
library(ggsci)
library(ggbeeswarm)
library(patchwork)
library(scales)
devtools::install_github("thomasp85/patchwork")

codes <- codelist %>%
  select(iso3c, country.name.en, region, continent) %>%
  janitor::clean_names() %>%
  filter(!is.na(continent)) %>%
  filter(!is.na(region))


coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv") %>%
  janitor::clean_names() %>%
  filter(!is.na(mismanaged_plastic_waste_tonnes)) %>%
  left_join(codes, by = c("code" = "iso3c")) %>%
  select(-country_name_en) %>%
  filter(!is.na(continent)) %>%
  mutate(mmwpercap = mismanaged_plastic_waste_tonnes/total_population_gapminder)

p1 <- coast_vs_waste %>%
  filter(coastal_population > 30000000) %>%
ggplot(aes(coastal_population, mismanaged_plastic_waste_tonnes, color = continent, label = entity)) +
  geom_point() +
  scale_x_log10(labels = scales::comma_format()) +
  scale_color_nejm(name = "Continent") +
  scale_fill_nejm(name = "Continent") +
  geom_label_repel(aes(fill = continent), colour = "white", fontface = "bold", show.legend = F) +
  labs(x = "Coastal population (log)",
       y = "Mismanaged plastic waste (tonnes)",
       title = "Coastal population > 30 million") +
  theme(text = element_text(family = "Papyrus"),
        axis.line = element_line(size = 1),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#f2f5f7"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        plot.caption = element_text( size = 12),
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.1, 0.8), 
        legend.justification = c(0.1, 0.8), 
        legend.background = element_rect(colour = NA, fill = "white"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"))

p1

p2 <- ggplot(coast_vs_waste, aes(coastal_population, mismanaged_plastic_waste_tonnes, color = continent)) +
  geom_point(size = 3) +
  scale_x_log10(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_color_nejm(name = "Continent") +
  scale_fill_nejm(name = "Continent") +
  labs(x = "Coastal population (log)",
       y = "Mismanaged plastic waste (tonnes)",
       title = "All countries") +
  theme(text = element_text(family = "Papyrus"),
        axis.line = element_line(size = 1),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#f2f5f7"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        plot.caption = element_text(size = 12),
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = c(0.1, 0.8), 
        legend.justification = c(0.1, 0.8), 
        legend.background = element_rect(colour = NA, fill = "white"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"))
p2

p3 <- coast_vs_waste %>%
  filter(!is.na(mmwpercap)) %>%
  ggplot() +
  geom_quasirandom(aes(x = continent, y = mmwpercap, color = continent), size = 2.5) +
  coord_flip() +
  scale_color_nejm() +
  geom_label_repel(data=subset(coast_vs_waste, mmwpercap > 0.030),
                   aes(continent,mmwpercap,label=entity, fill = continent), colour = "white", fontface = "bold")+
  labs(x = " ",
       y = "Mismanaged plastic waste (tonnes)per capita",
       title = "Could per capital estimates could be misleading?") +
  theme(text = element_text(family = "Papyrus"),
        axis.line = element_line(size = 1),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#f2f5f7"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        plot.caption = element_text(size = 12),
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.position = "none")
p3                 

( p2 / p3 ) | p1
ggsave("facet.png", width = 18, height = 9)

waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv") %>%
  janitor::clean_names() %>%
  filter(!is.na(per_capita_plastic_waste_kilograms_per_person_per_day)) %>%
  left_join(codes, by = c("code" = "iso3c")) %>%
  select(-country_name_en) %>%
  filter(!is.na(continent)) 

ggplot(waste_vs_gdp, mapping = aes(x = gdp_per_capita_ppp_constant_2011_international_constant_2011_international, y = per_capita_plastic_waste_kilograms_per_person_per_day, size = total_population_gapminder, color = continent)) +
  geom_point() +
  scale_size_continuous(name = "Population", range = c(2, 12), labels = scales::comma_format()) +
  scale_x_log10(labels = comma_format()) +
  scale_fill_nejm(name = "Continent")+
  scale_color_nejm(name = "Continent") +
  geom_label_repel(data=subset(waste_vs_gdp, per_capita_plastic_waste_kilograms_per_person_per_day > 0.5),
                   aes(gdp_per_capita_ppp_constant_2011_international_constant_2011_international,
                       per_capita_plastic_waste_kilograms_per_person_per_day,label=entity, color = continent), 
                    size = 5, show.legend = F) +
  labs(x = "GDP per capita",
       y = "Per capital plastic waste (kg)",
       title = "Looking like Caribbean countries have a higher per capita plastic waste",
       subtitle = "Surprising...What could be the reason? Tourism?",
       caption = "Source: Our World in Data \n Code: @srini_meen") +
  theme(text = element_text(family = "Papyrus"),
        panel.background = element_rect(fill = "#f9feff"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        plot.caption = element_text(size = 12),
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"))

ggsave("plasticpercap.png", width = 15, height = 8)
