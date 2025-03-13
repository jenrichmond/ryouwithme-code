# 3 ways to read the Sydney beaches data into RStudio

library(tidyverse)
library(here)
library(ryouwithme)

## read_csv

csv_beaches <- read_csv(here("data", "sydneybeaches.csv"))

## raw URL

url_beaches <- read_csv("https://raw.githubusercontent.com/rladiessydney/RYouWithMe/master/sydneybeaches.csv")


## RYouWithMe package

rywm_beaches <- sydneybeaches

bakers <- bakers_wide
