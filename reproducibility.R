library(readxl)
library(tidyverse)
library(janitor)
library(here)

# Import and clean data
fish_length <- read_excel(here("./data/Fundulus_notatus.xls"), skip = 4) |> 
  janitor::clean_names()  |> 
  filter(population != "HD",
         population != "RS") |> # Not sure what the population from Curtis Creek is abbreviated as, therefore not removed
  filter(age == "1") |> 
  dplyr::select(standard_length_mm, habitat, age, population)

# Calculate mean, sd and se
fish_summary <- fish_length |> 
  group_by(habitat) |> 
  summarise(n = n(), 
            mean_length = mean(standard_length_mm),
            sd_length = sd(standard_length_mm),
            se_length = sd_length / sqrt(n)) 

# Make figure
figure <- ggplot(data = fish_summary, aes(x = habitat, y = mean_length)) +
  geom_bar(stat = "identity", fill = "light grey") + 
  geom_errorbar(aes(ymin = mean_length - se_length, 
                    ymax = mean_length + se_length), 
                width = 0.2, color = "black") +
  labs(x = "Habitat",
       y = "Standard Length (mm)") +
  theme_bw() +
  coord_cartesian(ylim = c(35, 42)) + # Questionable to break the axis?
  scale_y_continuous(breaks = seq(35, 42, by = 1), 
                     labels = paste0(seq(35, 42, by = 1)), 
                     limits = c(0, 50))

figure

# Statistical analysis?
glm <- lm(standard_length_mm ~ habitat, data = fish_length)
anova(glm)
summary(glm)
