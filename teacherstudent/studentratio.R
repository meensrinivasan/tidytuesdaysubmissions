extrafont::loadfonts(device = "win")

library(tidyverse)
library(countrycode)
library(rworldmap)
library(scico)
library(ggsci)
library(scales)
library(ggthemes)

## Loading the data
student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv") %>%
  select(-flag_codes, -flags) %>%
  select(-edulit_ind) %>%
  group_by(indicator, country_code) %>%
  summarize(median_ratio = median(student_ratio, na.rm = TRUE)) 

## Getting a dataframe of countries
rworldmap_countries<-getMap(resolution = "coarse", projection = NA)
rworldmap_countries_list<- rworldmap_countries@data #saved in a dataframe

# Using the robinson projection that would enable showing the entire world at once. 
# Also Antarctica is not needed in this map
map_world <- broom::tidy(spTransform(getMap(), CRS("+proj=robin"))) %>% 
  filter(id != "Antarctica")

# Just need to check what are the countries to avoid discrepancies between this and other dfs
map_world_countries <- map_world %>%
  select(id)

# codes from countrycode package
codes <- codelist %>%
  select(iso3c, country.name.en, region, continent) %>%
  janitor::clean_names() %>%
  filter(!is.na(continent)) %>%
  filter(!is.na(region)) 

# Joining the country codes to the ratio data
student_ratio2 <- student_ratio %>%
  left_join(., codes, by = c("country_code" = "iso3c")) %>%
  filter(!is.na(continent)) 

# Checking if there is any discrepancy in names
names_dif <- anti_join(student_ratio2, map_world_countries, by = c("country_name_en" = "id")) 

# Yes, there is. Correcting that. 
student_ratio3 <- student_ratio2 %>%
  mutate(country_name_en = recode(country_name_en, 
                                  "Antigua & Barbuda" = "Antigua and Barbuda",
                                  "Bahamas" = "The Bahamas",
                                  "Bosnia & Herzegovina" = "Bosnia and Herzegovina",
                                  "Côte d'Ivoire" = "Ivory Coast",
                                  "Congo - Kinshasa"  = "Democratic Republic of the Congo",
                                  "Congo - Brazzaville" = "Republic of the Congo",     
                                  "Czechia" = "Czech Republic",
                                  "Micronesia (Federated States of)" = "Federated States of Micronesia",
                                  "Hong Kong SAR China" = "Hong Kong S.A.R.", 
                                  "St. Kitts & Nevis"=  "Saint Kitts and Nevis",                   
                                  "St. Lucia"= "Saint Lucia", 
                                  "Myanmar (Burma)" = "Myanmar",  
                                  "Macau SAR China" = "Macau S.A.R",
                                  "Serbia" = "Republic of Serbia",
                                  "French Southern Territories" = "French Southern and Antarctic Lands", 
                                  "São Tomé & Príncipe" = "Sao Tome and Principe",
                                  "Turks & Caicos Islands"= "Turks and Caicos Islands",
                                  "Tanzania" =  "United Republic of Tanzania",
                                  "United States" =  "United States of America",                             
                                  "Vatican City"= "Vatican",            
                                  "St. Vincent & Grenadines"= "Saint Vincent and the Grenadines"))             
 
# Checking if the discrepancies still exist                                 
names_dif <- anti_join(student_ratio3, map_world_countries, by = c("country_name_en" = "id"))                                 

# Filtering df for Primary Education
student_ratio3_prim <- student_ratio3 %>%
  filter(indicator == "Primary Education")


# Map
ggplot() +
  geom_map(data = map_world, map = map_world,
           aes(x = long, y = lat, map_id = id),
           fill = "grey50") + 
  geom_map(data = student_ratio3_prim, map = map_world,
           aes(fill = median_ratio, map_id = country_name_en)) + 
  theme_minimal(base_family = "Comfortaa", base_size = 14) +
  scale_fill_scico(palette = "lajolla", na.value = "grey50") +
  labs(title = "Student to teacher ratios in Primary Education by country",
    subtitle = "Is S:T ratio and GDP per capita related?",
    caption = "Source: UNESCO Institute of Statistics",
    fill = "Student to teacher ratio") + 
  theme_map(base_family = "Segoe Script", base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
                     plot.caption = element_text(size = 12),
                     plot.subtitle = element_text(hjust = 0.5, size = 14),
                     legend.title = element_text(size = 15, face = "bold"),
                     legend.text = element_text(size = 13),
                     legend.position = "bottom")

ggsave("Map.png", width = 15, height = 8)

# Downloading gdp data
gdpercap <- read_csv("API_NY.GDP.PCAP.CD_DS2_en_csv_v2_10576699\\API_NY.GDP.PCAP.CD_DS2_en_csv_v2_10576699.csv",
                     skip = 4) %>%
  janitor::clean_names() %>%
  select(country_name, country_code, x2017) %>%
  rename("gdpercap_17" = "x2017") %>%
  mutate(country_name = recode(country_name, "United States" = "United States of America"))

# Downloading population data
pop <- readxl::read_excel("TotalPopSex-20190510084722.xlsx", sheet = "Data", skip = 1) %>%
  janitor::clean_names() %>%
  select(-sex, -note, -x1951) %>%
  rename("pop_2017_000" = "x2017")

# Combining into one dataframe
pop_gdp <- gdpercap %>%
  left_join(., pop, by = c("country_name" = "location")) %>%
  left_join(., student_ratio3, by = c("country_name" = "country_name_en")) %>%
  filter(!is.na(continent)) %>%
  select(-iso_3166_1_numeric_code, -country_code.y) %>%
  filter(!is.na(median_ratio)) %>%
  drop_na()


# Relationship between gdp and ts ratio
pop_gdp %>%
  filter(indicator == "Primary Education") %>%
ggplot(pop_gdp, mapping = aes(x = gdpercap_17, y = median_ratio, size = pop_2017_000, color = continent)) +
  geom_point() +
  scale_size_continuous(name = "Population", range = c(2, 12), labels = comma_format()) +
  scale_x_log10(labels = comma_format()) +
  scale_color_startrek(name = "Continent") +
  labs(x = "GDP per capita",
       y = "Median teacher student ratio",
       title = "As GDP increases, teacher to student ratio decreases",
       subtitle = "Makes sense, countries with lower GDP have fewer teachers",
       caption = "Source: UNESCO Institute of Statistics \n Code: @srini_meen") +
  theme_minimal(base_family = "Segoe Script", base_size = 15) +
  theme(plot.background = element_rect(fill = "#f9feff"),
               plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
               plot.subtitle = element_text(hjust = 0.5, size = 14),
               plot.caption = element_text( size = 12),
               axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(size = 14, face = "bold"))

ggsave("gdp.png", width = 15, height = 8)

countries <- c("Brazil", "India", "China", "Rwanda", "United States of America")

pop_gdp$indicator <- fct_relevel(pop_gdp$indicator, "Tertiary Education", "Post-Secondary Non-Tertiary Education", 
                                 "Upper Secondary Education", "Secondary Education", "Lower Secondary Education",
                                 "Primary Education", "Pre-Primary Education")

indicator_needed <- c("Tertiary Education", 
                                 "Upper Secondary Education", "Secondary Education", "Lower Secondary Education",
                                 "Primary Education", "Pre-Primary Education")

# Specific countries bar plot
pop_gdp %>%
  filter(country_name %in% countries) %>%
  filter(indicator %in% indicator_needed) %>%
  mutate(country_name = recode(country_name, "United States of America" = "USA")) %>%
  ggplot(pop_gdp, mapping = aes(x = country_name, y = median_ratio, fill = country_name)) +
  geom_col() +
  facet_wrap(~ indicator, scales = "free_y")+
  coord_flip() +
  labs(x = " ",
       y = "Median teacher student ratio",
       title = "China catching up with USA in teacher student ratio",
       subtitle = "Amazing that China is investing heavily in teachers.",
       caption = "Source: UNESCO Institute of Statistics \n Code: @srini_meen") +
  scale_fill_startrek() +
  theme_minimal(base_family = "Segoe Script", base_size = 15) +
theme(plot.background = element_rect(fill = "#f9feff"),
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 14),
      plot.caption = element_text( size = 12),
      axis.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(size = 14, face = "bold"),
      legend.position = "none",
      strip.text = element_text(face = "bold"))

ggsave("countries.png", width = 15, height = 8)


# Primary education by region
pop_gdp %>%
  filter(indicator == "Primary Education") %>%
  mutate(region = fct_reorder(region, median_ratio)) %>%
  ggplot(pop_gdp, mapping = aes(x = region, y = median_ratio, fill = continent)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_startrek() +
  labs(x = " ",
       y = "Median teacher student ratio",
       title = "Teacher student ratio in Primary Education by region",
       subtitle = "Investment in primary education is super important in Africa and Southern Asia",
       caption = "Source: UNESCO Institute of Statistics \n Code: @srini_meen") +
  theme_minimal(base_family = "Segoe Script", base_size = 15) +
  theme(plot.background = element_rect(fill = "#f9feff"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        plot.caption = element_text( size = 12),
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = "none",
        strip.text = element_text(face = "bold"))

ggsave("box.png", width = 15, height = 8)

