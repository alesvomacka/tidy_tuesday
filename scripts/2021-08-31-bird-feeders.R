# Packages ----------------------------------------------------------------

library(tidyverse)

# Data --------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2021-08-31')
birds <- tuesdata$bird_baths

# Biodiversity plot -------------------------------------------------------

bird_diversity <- birds %>% 
  filter(bird_count != 0) %>%
  select(-urban_rural) %>% 
  distinct() %>% 
  count(survey_year, bioregions) %>%
  filter(!is.na(survey_year)) %>% 
  pivot_wider(names_from = survey_year, names_prefix = "year", values_from = n) %>% 
  mutate(bioregions = fct_reorder(bioregions, year2015)) %>%  
  ggplot(aes(xend = year2015, x = year2014, y = bioregions, yend = bioregions)) +
  geom_segment(size = 2, color = "grey60", alpha = 0.5) +
  geom_point(aes(x = year2015, color = "2015"), size = 3) +
  geom_point(aes(x = year2014, color = "2014"), size = 3) +
  scale_color_manual(values = c("#6F69AC", "#FD6F96")) +
  scale_x_continuous(limits = c(0, 120),
                     breaks = seq(25,100,25)) +
  labs(x = "Number of Species",
       y = element_blank(),
       color = element_blank(),
       title = "Australia's Bird Biodiversity",
       subtitle = "Number of bird species recorded",
       caption = "Data source: Avian Assemblages at Bird Baths: A Comparison of Urban and Rural Bird Baths in Australia (Cleary et al, 2016)") + 
  theme(plot.background = element_rect(fill = "#FFEBA1"),
        panel.background = element_rect(fill = "#FFEBA1"),
        axis.line = element_line(color = "white"),
        axis.ticks = element_line(color = "white"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = c(0.9,0.13),
        legend.background = element_rect(fill = "#FFE194", color = "#FACE7F"),
        legend.key = element_rect(fill = "#FFE194"),
        legend.text = element_text(size = 12),
        plot.title.position = "plot",
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 15, face = "italic"),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot") +
  guides(color = guide_legend(reverse = TRUE))

# Export plot -------------------------------------------------------------

ggsave(filename = "figures/2021-08-31-bird-feeders.png", plot = bird_diversity,
       dpi = 300)
