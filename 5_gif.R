library(readr)
library(ggplot)
library(magick)
library(beepr)

raintemp <- read_csv(here("data", "rain_temp_beachbugs.csv"))

raintemp %>%
  na.omit() %>%
  filter(beachbugs > 500) %>%
  ggplot(aes(x = rain_mm, y = beachbugs, color = temp_airport)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  scale_colour_distiller(name = "Temp (C)", palette = "RdYlBu") +
  labs(title = "Mean enterococci bacteria levels at Eastern Suburbs \nbeaches as a function of rainfall and temperature", 
       subtitle = "only day > 500", 
       caption = "data from https://www.environment.nsw.gov.au/beachapp/report_enterococci.aspx", 
       x = "Rainfall (mm)", 
       y = "Mean enterococci levels")

ggsave("beaches.png")


beachplot <- image_read("beaches.png")
wizgif <- image_read("ladywiz.gif")



frames <- image_composite(beachplot, wizgif, offset = "+600+200")

animation <- image_animate(frames, fps = 10)

image_write(animation, "beachwiz.gif")

beep()