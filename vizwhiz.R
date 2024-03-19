#load packages ---------

library(tidyverse)
library(here)
library(ggbeeswarm)
library(beepr)
library(RColorBrewer)

# set your favourite plot theme ------------
theme_set(theme_classic())

#read in cleanbeaches_new data--------

plotbeaches <- read_csv(here("data", "cleanbeaches_new.csv"))

# plot buglevels  by year-------

plotbeaches %>%
  ggplot(aes(x = year, y = beachbugs)) +
  geom_point() 

# summarising how many obs per year
plotbeaches %>%
  group_by(year) %>%
  summarise(obs = n())

# replotting bug levels by year using jitter/quasirandom
plotbeaches %>%
  ggplot(aes(x = year, y = beachbugs)) +
  geom_jitter() 

ggsave(here("output", "bugsbyyear_jitter.png"))

# coerce year to be factor rather than integer
plotbeaches$year <- as.factor(plotbeaches$year)

# plot bug levels by site--------
plotbeaches %>% 
  na.omit() %>%
  ggplot(aes(x = site, y = beachbugs, color = year)) +
  geom_jitter() +
  coord_flip()

ggsave(here("output", "bugsbybeach_coloryear.png"))

# glimpse to check it did the right thing
glimpse(plotbeaches)

# facet wrap-----------

plotbeaches %>%
  na.omit() %>%
  ggplot(aes(x = year, y= beachbugs, colour = site)) +
  geom_jitter() +
  facet_wrap(~ site) 

ggsave(here("output", "bugsbybeach_facetsite_all.png"))

#combine filter and ggplot-----------

plotbeaches %>%
  na.omit() %>%
  filter(beachbugs < 1000) %>%
  ggplot(aes(x = year, y= beachbugs, colour = site)) +
  geom_jitter() +
  facet_wrap(~site)

ggsave(here("output", "bugsbybeach_facetsite1000.png"))

plotbeaches %>%
  na.omit() %>%
  filter(beachbugs < 1000) %>%
  filter(site %in% c("Coogee Beach", "Bondi Beach")) %>%
  ggplot(aes(x = year, y= beachbugs, colour = site)) +
  geom_point() +
  facet_wrap(~ site) 

#how to get ggplots out of R -----------
  
  ggsave(here("output", "coogeebondi.png"))

# boxes and violins----------

plotbeaches %>%
  na.omit() %>%
  ggplot(aes(x = site, y=logbeachbugs)) +
  geom_boxplot() +
  coord_flip()

ggsave(here("output", "log_boxplot.png"))

plotbeaches %>%
  na.omit() %>% 
  ggplot(aes(x = year, y=logbeachbugs)) +
  geom_violin()

ggsave(here("output", "log_violin.png"))

#filtered for buggier than average for that site = true
  
plotbeaches %>%
    na.omit() %>% 
  filter(buggier_site == "TRUE") %>%
  ggplot(aes(x = year, y=logbeachbugs, color = year, fill = year)) +
    geom_violin() +
  facet_wrap(~ site)

ggsave(here("output", "log_violin_buggier.png"))

#histogram----------

hist(plotbeaches$beachbugs)

plotbeaches %>%
  na.omit() %>%
  filter(site == "Clovelly Beach", 
         year == "2018", 
         logbeachbugs > 0) %>%
  ggplot(aes(x = beachbugs)) +
  geom_histogram(binwidth = 10)

ggsave(here("output", "clovelly_histogram.png"))

# combination plot---------------

plotbeaches %>%
  na.omit() %>%
  filter(buggier_site == "TRUE") %>%
  ggplot(aes(x = site, y = logbeachbugs)) +
  geom_boxplot() +
  geom_point(aes(color = year)) +
  coord_flip()

ggsave(here("output", "boxcolour_year.png"))

plotbeaches %>%
  na.omit() %>%
  filter(buggier_site == "TRUE") %>%
  ggplot(aes(x = year, y =logbeachbugs)) +
  geom_violin() +
  geom_quasirandom(aes(colour = council))

ggsave(here("output", "boxcolour_council.png"))

# bar and column plots ----------

# use geom_bar for frequency/count data
plotbeaches %>%
  na.omit() %>%
  ggplot(aes(x = year)) +
  geom_bar() +
  facet_wrap(~ site)

ggsave(here("output", "bar_year.png"))

# use geom_col for plotting asummary stat (the default is sum)
plotbeaches %>%
  na.omit() %>%
  ggplot(aes(x = year, y = beachbugs)) +
  geom_col()

#checking what the geom_col is plotting
plotbeaches %>%
  na.omit() %>%
  group_by(year) %>%
  summarise(totalbugs = sum(beachbugs))

plotbeaches %>%
  na.omit() %>%
  group_by(year, site) %>%
  summarise(meanbugs = mean(beachbugs)) %>%
  ggplot(aes(x = year, y= meanbugs)) +
  geom_col() +
  facet_wrap(~site)
  
  ggsave(here("output", "col_meanbugs.png"))
  
#what about error bars---------
  
  plotbeaches %>%
    na.omit() %>%
    group_by(site) %>%
    summarise(mean = mean(beachbugs),
              sd = sd(beachbugs), 
              n = n(), 
              stderr = sd/sqrt(n)) %>%
    ggplot(aes(x = site, y = mean)) +
    geom_col() +
    coord_flip() +
    geom_errorbar(aes(x = site, ymin = mean-stderr, ymax = mean+stderr))
  
  ggsave(here("output", "col_meanbugs_error.png"))

# correlation/scatter plots ------------
  
  raintemp <- read_csv(here("data", "rain_temp_beachbugs.csv"))
  
  raintemp %>%
    na.omit() %>%
    filter(beachbugs > 500) %>%
    ggplot(aes(x = rain_mm, y = beachbugs, color = temp_airport)) +
    geom_point() +
    geom_smooth() 
  
  ggsave(here("output", "scatter_rain_temp.png"))

# how do I change x-------------
  
  #get rid of the grey and gridlines
  
  raintemp %>%
    na.omit() %>%
    filter(beachbugs > 500) %>%
    ggplot(aes(x = rain_mm, y = beachbugs, color = temp_airport)) +
    geom_point() +
    geom_smooth() +
    theme_classic()
  
  ggsave(here("output", "scatter_classic.png"))
  
  
  # the color so it refelects cool is blue and hot is red
  
  raintemp %>%
    na.omit() %>%
    filter(beachbugs > 500) %>%
    ggplot(aes(x = rain_mm, y = beachbugs, color = temp_airport)) +
    geom_point() +
    geom_smooth() +
    theme_classic() +
    scale_colour_gradient(low = "blue", high = "red")
  
  ggsave(here("output", "scatter_colour1.png"))
  
 #use a palette for setting colors
  
  display.brewer.all()
  
  raintemp %>%
    na.omit() %>%
    filter(beachbugs > 500) %>%
    ggplot(aes(x = rain_mm, y = beachbugs, color = temp_airport)) +
    geom_point() +
    geom_smooth() +
    theme_classic() +
    scale_colour_distiller(palette = "RdYlBu")
  
  ggsave(here("output", "scatter_colour2.png"))
  
 # add titles and change axis labels -----------
  
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
    
    
    ggsave("beaches_labs.png")
  
knitr::spin(here("scripts", "vizwhiz.R"), precious = TRUE)   
  