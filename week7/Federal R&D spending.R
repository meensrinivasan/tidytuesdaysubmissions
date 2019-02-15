library(tidyverse)
library(ghibli)
library(gganimate)

# Load packages
fed_rd <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")
energy_spend <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/energy_spending.csv")
climate_spend <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/climate_spending.csv")

# Theme
my_theme <- theme(strip.background = element_rect(fill = "white"),
      strip.text = element_text(hjust = 0, color = "black", size = 15, face = "bold"),
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 15, face = "bold"),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

# Total Federal R&D Spending by department
fed_rd_1 <- fed_rd %>%
  mutate(pc_rd_gdp = (rd_budget/gdp)*100,
         rd_budget = rd_budget/10^9)%>%
  filter(department %in% c("DOD", "NIH", "NASA", "HHS"))

# Percent of GDP plot  
ggplot(fed_rd_1, aes(year, pc_rd_gdp, col = department, fill = department))+
  geom_line()+
  geom_point()+
  geom_area(alpha = 0.2)+
  scale_fill_manual(values = rev(ghibli_palette("LaputaMedium")))+
  scale_color_manual(values = rev(ghibli_palette("LaputaMedium")))+
  facet_wrap(~ department, scales = "free_y")+
  theme_light()+
  labs(x = "",
       y = "R&D budget as a percent of GDP",
       title = "R&D budget as a percent of total GDP from 1976-2017",
       subtitle = "R&D budget as a % of GDP seems to be falling",
       caption= "Source: American Association for the Advancement of Science")+
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(hjust = 0, color = "black", size = 12))+
  my_theme


# Actual budget plot
ggplot(fed_rd_1, aes(year, rd_budget, col = department, fill = department))+
  geom_line()+
  geom_point()+
  geom_area(alpha = 0.2)+
  scale_fill_manual(values = rev(ghibli_palette("MononokeMedium")))+
  scale_color_manual(values = rev(ghibli_palette("MononokeMedium")))+
  facet_wrap(~ department, scales = "free_y")+
  scale_y_continuous(labels = scales::dollar_format())+
  theme_light()+
  labs(x = "",
       y = "R&D budget (billions)",
       title = "R&D budget (billions) 1976-2017",
       subtitle = "R&D budget has remained steady in recent years",
       caption= "Source: American Association for the Advancement of Science")+
  theme(legend.position = "none")+
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(hjust = 0, color = "black", size = 12))+
  my_theme

# Energy Departments data
energy_spending_1 <- energy_spend %>%
  filter(department %in% c("Fossil Energy", "Energy Efficiency and Renew Energy", 
                           "Nuclear Energy", "Office of Science R&D", "Atomic Energy Defense",
                           "Basic Energy Sciences*")) %>%
  mutate(energy_spending = energy_spending/ 10^6)


ggplot(energy_spending_1, aes(year, energy_spending, col = department))+
  geom_line(size = 1)+
  geom_point(size = 2)+
  scale_y_continuous(labels = scales::dollar_format())+
  scale_color_paletteer_d(name = "Department", LaCroixColoR, PeachPear)+
  labs(title = "Energy Departments Data",
       subtitle = "Atomic Energy Defence and Office of Sciences spend over two fold more than other organisations",
       caption = "Source: American Association for the Advancement of Science",
       x = "",
       y = "R&D Spending (Millions)")+
  theme_light()+
  my_theme


# GCC Research Program Spending
climate_spend_1 <- climate_spend %>%
  mutate(gcc_spending = gcc_spending / 10^6)

p <- ggplot(climate_spend_1, aes(year, gcc_spending, group = department, col = department))+
  geom_line(size = 1.5)+
  geom_segment(aes(xend = 2017, yend = gcc_spending), linetype = 2, color = "black")+
  geom_point(size = 2)+
  geom_text(aes(x = 2018, label = department), hjust = 0)+
  scale_color_paletteer_d(LaCroixColoR, Pamplemousse)+
  scale_y_continuous(labels = scales::dollar_format())+
  labs(title = "Global Climate Change Research Spending Program",
       subtitle = "NASA GCC Spending is three fold other organisations!",
       caption = "Source: American Association for the Advancement of Science",
       x = "",
       y = "R&D Spending (Millions)")+
  theme_light()+
  my_theme+
  theme(plot.margin = margin(5.5, 80, 5.5, 5.5))+
  theme(legend.position = "none")+
  transition_reveal(year)+
  coord_cartesian(clip = 'off') 

p
