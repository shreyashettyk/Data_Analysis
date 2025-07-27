#manipulate the data :: video lecture 13

library(tidyverse)

glimpse(msleep)


msleep %>% rename("conserv" = "conservation")

glimpse(msleep)


#change variable type

class(msleep$vore)

msleep$vore <- as.factor(msleep$vore)

class(msleep$vore)

glimpse(msleep)


msleep %>%  mutate(vore = as.character(vore)) %>% glimpse()

names(msleep)

msleep %>% select(2:5,awake,starts_with("sleep"),contains("wt")) %>% names()


unique(msleep$order)


msleep %>% filter(order == "Carnivora" | order == "Primates",sleep_total > 8) %>% 
  select(name,order,sleep_total) %>% 
  arrange(sleep_total)

msleep %>% filter(order == "Carnivora" | order == "Primates",sleep_total > 8) %>% 
  select(name,order,sleep_total) %>% 
  arrange(-sleep_total)


msleep %>% filter(order == "Carnivora" | order == "Primates",sleep_total > 8) %>% 
  select(name,order,sleep_total) %>% 
  arrange(order)



msleep %>% mutate(brain_wt = brainwt * 1000) %>% View()


size_of_brain <- msleep %>% select(name,brainwt) %>% 
  drop_na(brainwt) %>% 
  mutate(brain_size = if_else(brainwt > 0.01,"big","small"))

size_of_brain


size_of_brain %>%  mutate(brain_size = recode(brain_size,"big" = 1,"small" = 0)) %>% View()



#reshape the data from wide to long and long to wide
#install.packages("gapminder")
library(gapminder)

View(gapminder)

#asp er the gapfinder data the wide format would be to have each year as a column and have a certain attribute as lifeexpectancy plugged in


#long format

data <- select (gapminder,country,year,lifeExp) 

#wide format -> convert from long format to wide format

wide_data <- data %>% pivot_wider(names_from = year,values_from = lifeExp)

View(wide_data)

#long format -> convert from wide format to long format

names(wide_data[,2:13])


long_data <- wide_data %>% pivot_longer(2:13,names_to = 'yr',values_to = "lifeExp")

View(long_data)


#describeand summarise the data

library(tidyverse)

glimpse(msleep)


attach(msleep)

min(awake)
max(awake)
range(awake)
IQR(awake)
summary(msleep)


msleep %>% select(sleep_total,brainwt) %>% summary()



#for each category of vore compute the min,max,difference and average sleep_total

msleep %>% 
  drop_na(vore) %>% 
  group_by(vore) %>% 
  summarise(Lower = min(sleep_total),
            Average = mean(sleep_total),
            Upper = max(sleep_total),
            Difference = max(sleep_total) - min(sleep_total)) %>% 
  arrange(Average)


#creating contigency tables


library(MASS)

glimpse(Cars93)

attach(Cars93)

table(Origin)
table(AirBags,Origin)


addmargins(table(AirBags,Origin))
addmargins(table(AirBags,Origin),1)
addmargins(table(AirBags,Origin),2)


prop.table(table(AirBags,Origin))
prop.table(table(AirBags,Origin))*100
prop.table(table(AirBags,Origin),1)*100
prop.table(table(AirBags,Origin),2)*100
round(prop.table(table(AirBags,Origin),1)*100)


dim(Cars93 %>% filter(Origin == "USA" & AirBags == "Driver & Passenger"))

Cars93 %>% group_by(Origin,AirBags) %>% summarise(number = n()) %>% pivot_wider(names_from = Origin,values_from = number) %>% 
  View()


#acrossfunction ::video lecture 24

library(tidyverse)

glimpse(mpg)


mpg %>% mutate(across(where(is.character),as.factor)) %>% 
  glimpse()

mpg %>% group_by(manufacturer) %>% summarise(across(where(is.numeric) & contains('y'),mean))




#gtExtras
install.packages("gtExtras")
install.packages("svglite")
library(tidyverse)
library(RColorBrewer)
library(gtExtras)
library(gapminder)


plot <- gapminder %>% rename(Country = country) %>% 
  filter(continent == 'Asia') %>% 
  group_by(Country) %>% 
  summarise(`GDP pc` = round(mean(gdpPercap)),
            `Pop size` = round(mean(pop)),
            `Life Expectancy` = list(lifeExp)) %>% 
  arrange(-`GDP pc`) %>% 
  head(10) %>% 
  gt() %>% 
  gt_plt_dist(`Life Expectancy`) %>% tab_header(title = "the gdp and pop sixe of asia") %>% 
  cols_align(align = 'left')

plot

plot %>% gt_theme_538()

plot<-plot %>%  gt_theme_pff()

plot %>% gt_highlight_rows(rows = Country %in% c("Japan","Oman"), fill = "lightpink")

plot %>% gt_plt_bar_pct(`GDP pc`,fill="steel blue",height = 15,width = 20)

?gt






require(dplyr)

my_db <- src_sqlite("my_db.sqlite3")
