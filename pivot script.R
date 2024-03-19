library(tidyverse)
library(here)

wide <- read_csv(here("data", "beachbugs_wide.csv"))

long <- read.csv(here("data", "beachbugs_long.csv"))


beach_long <- wide %>%
  pivot_longer(names_to = "site", 
               values_to = "buglevels", 
               `Bondi Beach`:`Tamarama Beach`)

beach_wide <- long %>%
  pivot_wider(names_from = site, 
              values_from = buglevels)


frames_wide <- read_csv(here("data", "frames_wide.csv"))


frames_long <- frames_wide %>%
  pivot_longer(names_to = c("size", "item"), 
               values_to = "response", 
               large_item1:small_item7, 
               names_sep = "_")

