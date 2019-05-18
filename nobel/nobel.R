## Special thanks to https://www.r-bloggers.com/bitcoin-world-map-bubbles/ and 
## @WireMonkey https://twitter.com/WireMonkey/status/1128761148541677568 
## for some of the code inspiration

extrafont::loadfonts(device = "win")
library(tidyverse)
library(countrycode)
library(ggplot2)
library(ggalt)
library(ggthemes)
library(lubridate)
library(viridis)
library(ggpubr)


nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv") %>%
  mutate_all(., tolower) %>%
  mutate(prize_year = as.integer(prize_year)) %>%
  mutate(birth_country = ifelse(grepl('\\(', birth_country), str_extract(birth_country, "(?<=\\().*?(?=\\))"), birth_country),
         death_country = ifelse(grepl('\\(', death_country), str_extract(death_country, "(?<=\\().*?(?=\\))"), death_country)) %>%
  mutate(birth_country = case_when(birth_country == "scotland" ~ "united kingdom",
                                   birth_country == "northern ireland" ~ "united kingdom",
                                   grepl("czech", birth_country) ~ "czechia",
                                   birth_country == "east_germany" ~ "germany",
                                   TRUE ~ birth_country),
         death_country = case_when(death_country == "scotland" ~ "united kingdom",
                                   death_country == "northern ireland" ~ "united kingdom",
                                   grepl("czech", death_country) ~ "czechia",
                                   death_country == "east_germany" ~ "germany",
                                   TRUE ~ death_country)) %>%
  select(prize_year, category, birth_date, birth_country, gender, organization_name, organization_country, death_country)


nobel_winners_cntry <- nobel_winners %>%
  count(birth_country) %>%
  filter(!is.na(birth_country)) %>%
  mutate(iso3 = countrycode(birth_country, "country.name", "iso3c"))

codes <- codelist %>%
  select(iso3c, country.name.en, region, continent) %>%
  janitor::clean_names() %>%
  filter(!is.na(continent)) %>%
  filter(!is.na(region)) %>%
  rename(iso3 = iso3c) %>%
  left_join(CoordinateCleaner::countryref %>% select(iso3, capital.lon, capital.lat)) %>%
  distinct() %>%
  filter(!is.na(capital.lon)) %>%
  filter(!is.na(capital.lat)) 

 nobel_winners_cntry_coord <- nobel_winners_cntry %>%
  left_join(codes)
 
 
 world <- map_data("world")
 world <- world[world$region != "Antarctica", ]
 
 names_dif <- anti_join(nobel_winners_cntry_coord, world, by = c("country_name_en" = "region"))

 nobel_winners_cntry_coord2 <- nobel_winners_cntry_coord %>%
   mutate(country_name_en = recode(country_name_en, 
                                   "United Kingdom" = "UK",
                                   "United States" = "USA",
                                   "Bosnia & Herzegovina" = "Bosnia and Herzegovina",
                                   "Czechia" = "Czech Republic",
                                   "Trinidad & Tobago" = "Trinidad",
                                   "Myanmar (Burma)" = "Myanmar",
                                   "St. Lucia" = "Saint Lucia"))
 
ggplot() +
   geom_cartogram(
     data = world, map = world,
     aes(x = long, y = lat, map_id = region),
     color = "#113c7a", fill = "#113c7a", size = 0.125
   ) +
   geom_point(
     data = nobel_winners_cntry_coord, aes(capital.lon, capital.lat, size = n), fill = "#ffe923",
     shape = 21, alpha = 0.8, stroke = 0.25, color = "#ffe923"
   ) +
   coord_proj("+proj=robin") +
   scale_size_area(name = "Number of Nobel Laureates", breaks = c(10, 50, 100, 200), max_size = 30, labels = scales::comma) +
   labs(
     x = NULL, y = NULL,
     title = "Nobel Winners by country",
     subtitle = "Size of bubble indicates number of Nobel lauretes",
     caption = "Source: Kaggle"
   ) +
   theme_map(base_family = "Britannic Bold") +
   theme(plot.title = element_text(hjust = 0.5, size = 25)) +
   theme(plot.subtitle = element_text(hjust = 0.5, size = 15)) +
   theme(plot.caption = element_text(size = 15)) +
   theme(legend.position = "bottom") +
   theme(legend.title = element_text(size = 18)) +
   theme(legend.text = element_text(size = 18)) 

 ggsave("map.png", width = 15, height = 8)
 
 
 nobel_winners_age <- nobel_winners %>%
   mutate(age = prize_year - year(birth_date)) %>%
   mutate(category = str_to_title(category)) %>%
   filter(!is.na(gender)) %>%
   mutate(gender = str_to_title(gender))
 
 ggplot(data = nobel_winners_age)+
   geom_point(aes(prize_year, age, color = gender))+
   geom_smooth(aes(prize_year, age)) +
   facet_wrap(~category, scale = "free_y")+
   theme_minimal(base_family = "Britannic Bold")+
   labs(
     x = "Year of prize", y = "Age of winner (years)",
     title = "Age of Nobel winners at the time of being awarded",
     subtitle = "Recent winners of Chemistry and Physics seem to be older",
     caption = "Source: Kaggle"
   ) +
   scale_color_manual(name = "Gender", values = c("red", "darkblue")) +
   theme(strip.text = element_text(face = "bold", hjust = 0, size = 15)) +
   theme(plot.title = element_text( hjust = 0.5, size = 25)) +
   theme(plot.subtitle = element_text( hjust = 0.5, size = 15)) +
   theme(plot.caption = element_text(size = 15)) +
   theme(legend.title = element_text(size = 15)) +
   theme(legend.text = element_text(size = 15)) +
   theme(axis.title = element_text(size = 15)) +
   theme(axis.text = element_text(size = 12))
 
 
 ggsave("age.png", width = 15, height = 8)
   
 
 nobel_winners_usa <- nobel_winners %>%
   filter(birth_country == "united states of america") %>%
   filter(organization_country == "united states of america") %>%
   filter(!is.na(organization_name)) %>%
   group_by(category, organization_name) %>%
   summarise(wins = n()) %>%
   mutate(organization_name = recode(organization_name, 
                                     "massachusetts institute of technology (mit)" = "MIT",
                                     "massachusetts institute of technology (mit), center for cancer research" = "MIT",
                                     "california institute of technology (caltech)" = "Caltech",
                                     "university of texas southwestern medical center at dallas" = "UT Southwestern",
                                     "research division of infectious diseases, children's medical center" = "children's medical center",
                                     "rockefeller institute for medical research" = "rockefeller institute",
                                     "university of california school of medicine" = "University Of California Med",
                                     "johns hopkins university school of medicine" = "Johns Hopkins University")) %>%
   mutate(organization_name = str_to_title(organization_name)) %>%
   mutate(organization_name = recode(organization_name, 
                                     "Mit" = "MIT"))
   
                                        
                                    
 
 nobel_winners_usa_economics <- nobel_winners_usa %>%
   filter(category == "economics") %>%
   mutate(organization_name = fct_reorder(organization_name, wins)) %>%
   filter(wins > 1)
 
 nobel_winners_usa_medicine <- nobel_winners_usa %>%
   filter(category == "medicine") %>%
   mutate(organization_name = fct_reorder(organization_name, wins)) %>%
   filter(wins > 1)
   
 nobel_winners_usa_physics <- nobel_winners_usa %>%
   filter(category == "physics") %>%
   mutate(organization_name = fct_reorder(organization_name, wins)) %>%
   filter(wins > 1)
 
 nobel_winners_usa_chemistry <- nobel_winners_usa %>%
   filter(category == "chemistry") %>%
   mutate(organization_name = fct_reorder(organization_name, wins)) %>%
   filter(wins > 1)
 
 
e <- ggplot(nobel_winners_usa_economics, mapping = aes(x = as.factor(organization_name), y = wins, fill = organization_name)) +
   geom_col() +
   coord_flip() +
   scale_fill_viridis_d(option = "magma") +
   theme_minimal(base_family = "Britannic Bold") +
   labs(
     y = "Number of Nobel Prizes", x = "",
     title = "Economics") +
   theme(legend.position = "none") +
   theme(plot.title = element_text(size = 25)) +
   theme(axis.title = element_text(size = 15)) +
   theme(axis.text = element_text(size = 12))
 
 
 
m <- ggplot(nobel_winners_usa_medicine, mapping = aes(x = organization_name, y = wins,
                                                  fill = organization_name)) +
   geom_col() +
   coord_flip() +
   scale_fill_viridis_d(option = "magma") +
   theme_minimal(base_family = "Britannic Bold") +
   labs(
     y = "Number of Nobel Prizes", x = "",
     title = "Medicine") +
   theme(legend.position = "none") +
   theme(plot.title = element_text(size = 25)) +
   theme(axis.title = element_text(size = 15)) +
   theme(axis.text = element_text(size = 12))
 
 
p <- ggplot(nobel_winners_usa_physics, mapping = aes(x = organization_name, y = wins,
                                                  fill = organization_name)) +
   geom_col() +
   coord_flip() +
   scale_fill_viridis_d(option = "magma") +
   theme_minimal(base_family = "Britannic Bold") +
   labs(
     y = "Number of Nobel Prizes", x = "",
     title = "Physics") +
   theme(legend.position = "none") +
   theme(plot.title = element_text(size = 25)) +
   theme(axis.title = element_text(size = 15)) +
   theme(axis.text = element_text(size = 12))
 
c <- ggplot(nobel_winners_usa_chemistry, mapping = aes(x = organization_name, y = wins,
                                                 fill = organization_name)) +
   geom_col() +
   coord_flip() +
   scale_fill_viridis_d(option = "magma") +
   theme_minimal(base_family = "Britannic Bold") +
   labs(
     y = "Number of Nobel Prizes", x = "",
     title = "Chemistry") +
   theme(legend.position = "none") +
   theme(plot.title = element_text(size = 25)) +
   theme(axis.title = element_text(size = 15)) +
   theme(axis.text = element_text(size = 12))

ggarrange(m, e, p, c, ncol=2, nrow=2)

ggsave("uni.png", width = 15, height = 7)
