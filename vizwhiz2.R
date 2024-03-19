
#other geoms to illustrate variability
#boxplots----------
plotbeaches %>%
  na.omit() %>%
  filter(buggier_site == "TRUE") %>%
  ggplot(aes(x = site, y= logbeachbugs)) +
  geom_boxplot() +
  coord_flip()

#violin plots----------
plotbeaches %>%
  na.omit() %>%
  filter(buggier_site == "TRUE") %>%
  ggplot(aes(x = year, y= logbeachbugs, color = site)) +
  geom_violin()  +
  facet_wrap(~ site)

#histogram ---------------
#https://ggplot2.tidyverse.org/reference/geom_histogram.html
plotbeaches %>%
  na.omit() %>% 
  filter(logbeachbugs > 0, 
         site == "Clovelly Beach", 
         year == "2018") %>%
  ggplot(aes(x = beachbugs)) +
  geom_histogram(binwidth= 5) 

#layering points on boxplot/violins----------

plotbeaches %>%
  na.omit() %>%
  filter(buggier_site == "TRUE") %>%
  ggplot(aes(x = site, y= logbeachbugs)) +
  geom_boxplot() +
  geom_point(aes(color= year)) +
  coord_flip()

plotbeaches %>%
  na.omit() %>%
  filter(site == "Clovelly Beach") %>%
  ggplot(aes(x = year, y= logbeachbugs)) +
  geom_violin()  +
  geom_quasirandom(aes(colour = buggier_site))

#viz whiz 3bars and columns-------------- 

#bar for when the height of the bar is a count

plotbeaches %>%
  ggplot(aes(x=year)) +
  geom_bar() 

#differentiating with facet

plotbeaches %>%
  ggplot(aes(x=year)) +
  geom_bar() +
  facet_wrap(~ site)


#differentiating with color

plotbeaches %>%
  ggplot(aes(x=year, fill= site)) +
  geom_bar() 


#columns for when the height of the bar is a value you calculated

plotbeaches %>%
  group_by(site) %>%
  summarise(meanbugs = mean(beachbugs, na.rm = TRUE)) %>%
  ggplot(aes(x=site, y= meanbugs)) +
  geom_col() +
  coord_flip()

library(lubridate)
plotbeaches$date <- dmy(plotbeaches$date)
glimpse(plotbeaches)

#lines 
plotbeaches %>%
  filter(year == "2018", site == "Coogee Beach") %>%
  ggplot(aes(x= date, y= beachbugs)) +
  geom_line() 

#lines 
plotbeaches %>%
  group_by(month) %>%
 summarise(meanbugs = mean(beachbugs, na.rm= TRUE)) %>%
  ggplot(aes(x= month, y= meanbugs)) +
  geom_line(group = month) 
  