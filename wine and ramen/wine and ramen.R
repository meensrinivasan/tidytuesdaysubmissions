# Trying out ridgeplot and boxplots with observations plotted with ggbeeswarm
# Code inspiration: @allison_horst

extrafont::loadfonts(device = "win")
library(tidyverse)
library(ggridges)
library(paletteer)
library("LaCroixColoR")
library(ggbeeswarm)

pal_1 <- lacroix_palette("Berry", n = 20, type = "continuous")

wine_ratings <- readr::read_csv("winemag-data-130k-v2.csv") %>%
  select(country, points, price, variety, winery)

wine_ratings2 <- wine_ratings %>%
  group_by(country) %>%
  tally() %>%
  arrange(-n) %>%
  head(20) %>%
  inner_join(wine_ratings)


ggplot(wine_ratings2, aes(x = points, y = country)) +
  geom_density_ridges(scale = 2,
                      aes(fill = country),
                      size = 0.3,
                      color = "black") +
  scale_fill_manual(values = pal_1) +
  theme_light() +
  theme(text = element_text(family = "Lobster"),
        legend.position = "NA",
        panel.grid = element_blank(),
        plot.subtitle = element_text(color = "black", hjust = 0.5),
        plot.caption = element_text(color = "black"),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
  ) +
  labs(x = "WineEnthusiast Points (0-100)\n",
       y = "Country\n",
       title = "Wine ratings by country",
       subtitle = "Totally understand its hard to distinguish between wines...",
       caption =
         "Data: Kaggle\n For 20 countries with most observations in dataset")

ggsave("wine.png", width = 15, height = 8)


ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

ramen_ratings2 <- ramen_ratings %>%
  group_by(country) %>%
  tally() %>%
  arrange(-n) %>%
  head(20) %>%
  inner_join(ramen_ratings) 

ramen_ratings3 <- ramen_ratings2 %>%
  group_by(country) %>%
  summarize(
    medians = median(stars, na.rm = TRUE)
  ) %>%
  inner_join(ramen_ratings2) 


pal_2 <- lacroix_palette("Tangerine", n = 20, type = "continuous")


ggplot(ramen_ratings3, aes(x = reorder(country, medians), y = stars)) +
  geom_quasirandom(aes(color = country), alpha = 0.5, size = 2) +
  geom_boxplot(size = 0.2,
               aes(fill = country),
               alpha = 0.8,
               outlier.alpha = 0) +
  coord_flip() +
  theme_light() +
  theme(text = element_text(family = "Lobster"),
        legend.position = "NA",
        panel.grid = element_blank(),
        plot.subtitle = element_text(color = "black", hjust = 0.5),
        plot.caption = element_text(color = "black"),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
  ) + 
  scale_color_manual(values = pal_2) +
  scale_fill_manual(values = pal_2) +
  labs(x = "Country",
       y = "Rating scale (0-5)",
       title = "Ramen ratings by country",
       subtitle = "Malaysia rates their ramen very highly. I have no doubts!",
       caption =
         "Data: The Ramen Rater \n Data for 20 countries with most observations (n) in dataset")

ggsave("ramen.png", width = 15, height = 8)

