library(palmerpenguins)
library(tidyverse)
penguins

p <- penguins |> 
  drop_na(sex) |> 
  group_by(species, sex) |> 
  drop_na(bill_length_mm) 

p


