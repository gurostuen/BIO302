library(palmerpenguins)
library(tidyverse)
penguins

p <- penguins |> 
  drop_na(sex) 


p