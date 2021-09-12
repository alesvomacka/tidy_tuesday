# Packages ----------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(scales)
library(patchwork)
library(tidytuesdayR)

# Data --------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2021-07-13')
scooby   <- tuesdata$scoobydoo

scooby_palette <- c('fred' = "#76A2CA", "velma" = "#CD7E05", "scooby" = "#966A00", "shaggy" = "#B2BB1B", "daphne" = "#7C68AE")
theme_set(theme_light())

# Real or Not Plot --------------------------------------------------------

monster_real <- scooby %>% 
  select(season, monster_real) %>% 
  mutate(monster_real = na_if(monster_real, "NULL"),
         monster_real = as.logical(monster_real)) %>%
  filter(!is.na(monster_real)) %>% 
  group_by(season) %>%
  summarise(number_real  = sum(monster_real, na.rm = TRUE),
            number_fake  = n() - sum(monster_real, na.rm = TRUE),
            number_total = n()) %>%
  mutate(season = fct_relevel(season,
                              "1", "2", "3", "4", "Movie", "Special", "Crossover"))

percent_real <- nrow(subset(scooby, monster_real == "TRUE")) / nrow(subset(scooby, monster_real == "TRUE" | monster_real == "FALSE"))
percent_fake <- 1 - percent_real
percent_real <- percent(percent_real)
percent_fake <- percent(percent_fake)

scooby_plot <- 
monster_real %>% 
  ggplot(aes(x = seq_along(season), y = number_total)) +
  geom_rect(aes(xmin = seq_along(season) - 0.2, xmax = seq_along(season) + 0.2,
                ymin = number_real, ymax = number_total, fill = "fake"),
            show.legend = FALSE) +
  geom_rect(aes(xmin = seq_along(season) - 0.2, xmax = seq_along(season) + 0.2,
                ymin = number_total, ymax = number_total + number_real, fill = "real"),
            show.legend = FALSE) +
  geom_line(color  = "grey30") +
  geom_point(color = "grey30") +
  annotate(geom = "richtext", label = paste0("Out of all the monsters the gang encountered, <br><b>",
                                             percent_real,
                                             " were <span style = 'color:#B2BB1B;'>real</span> </b>and <b>",
                                             percent_fake, " were <span style = 'color:#7C68AE;'>fake</span></b>."),
           x = 1.7, y = 270, fill = NA, label.color = NA, hjust = 0, size = 7) +
  annotate(geom = "richtext", label = "The line represents the total number of monsters per season",
           x = 1.7, y = 240, fill = NA, label.color = NA, hjust = 0, size = 4) +
  scale_x_continuous(breaks = seq(1,7,1), labels = levels(monster_real$season)) +
  scale_fill_manual(values = c(scooby_palette[[5]], scooby_palette[[4]])) +
  labs(x = "Season",
       y = element_blank()) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "#f2e5c4"),
        plot.background  = element_rect(fill = "#f2e5c4"))

# Export plot -------------------------------------------------------------

ggsave(scooby_plot, filename = "2021-07-13-scooby.png", device = "png", path = "figures/", dpi = 300,
       width = 8)
