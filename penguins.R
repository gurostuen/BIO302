library(palmerpenguins)
library(tidyverse)
penguins

p <- penguins |> 
  drop_na(sex) |> 
  group_by(species, sex) |> 
  summarise(
    bill_length_mean = mean(bill_length_mm),
    .groups = "drop")

penguins |> filter(sex == "male", species == "Gentoo")

penguins



