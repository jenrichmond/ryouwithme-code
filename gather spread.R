library(tidyverse)
library(here)

wide_beaches <- read_csv(here("data", "beachbugs_wide.csv"))

long_beaches <- read_csv(here("data", "beachbugs_long.csv"))

#gather wants to know 1. key= name of the column you are creating, 2. value = name of column with DV data in it, 3. range of columns to gather 

newlongbeaches <- wide_beaches %>% gather(key = site, value = beachbugs, `Bondi Beach`:`Tamarama Beach`)


newwidebeaches <- newlongbeaches %>% spread(key= site, value= beachbugs)


# lets try Dani's data

frames <- read_csv(here("data", "frames_ex2.csv")) %>%
  select(- n_obs) 

frameswide <- frames %>% unite(sample_item, sample_size:test_item) %>%
  spread(key=sample_item, value = response)
