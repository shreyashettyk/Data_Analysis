#Notes from lecture -https://www.youtube.com/watch?v=9kYUGMg_14s&list=PLtL57Fdbwb_Chn-dNR0qBjH3esKS2MXY3

#import data and install R packages :: lecture video 3

my_data  <- read.csv("friends.csv")


head(my_data)
tail(my_data)
View(my_data)

my_data[1,3]
my_data[,3]
my_data$Gender

install.packages("tidyverse")
require("tidyverse")

my_data %>% select(Name,Age,Height)

my_data %>% select(Name,Age,Height) %>% filter(Age > 25 & Height > 165)

 
my_data %>% select(Name,Age,Height) %>% filter(Age > 25 & Height > 165) %>% arrange(Height)


#import data from excel into R studio lecture :: video 4
library(readxl)
friends <- read_excel("friends.xlsx",
                      sheet = "position", 
                      range = "C4:G15",
                      na = "**")
View(friends)

?read_excel

#manipulate data using tidyverse : select,filter and mutate :: video lecture 5


require(tidyverse)

data() #gives list of built in datasets

View(starwars)

#compare the body mass index of males and females among humans
#convert height in meters and compute bmi = (mass/height^2)
#get the average bmi among men and women


starwars %>%
  select(gender,mass,height,species) %>% 
  filter(species == 'Human') %>% 
  na.omit() %>% 
  mutate(height = height/100) %>% 
  mutate(bmi = mass/height^2) %>% 
  group_by(gender) %>% 
  summarise(Avg_bmi = mean(bmi))


#data types in R programming  :: video lecture 6

data()
View(iris)
View(starwars)


 
data <- starwars %>%
  select(name,sex,height,mass) %>% na.omit()

head(data)


str(data)


data$sex <- as.factor(data$sex)

levels(data$sex)

data$sex <- factor(data$sex,levels = c("male","female","hermaphroditic","none"))



data$height

data$old <- data$height > 160

class(data$old)
str(data)


#Rename variables and reorder columns :: video lecture 7


View(starwars)

data <- starwars %>% 
  select(name,mass,height,hair_color) %>% rename(weight = mass)

View(data)


#recoding data :: vidoe lecture 8

require(tidyverse)

sw <- starwars %>%  select(name,height,mass,gender) %>% 
  rename(weight = mass) %>% na.omit() %>% mutate (height = height/100) %>% filter(gender == 'masculine' | gender == 'feminine')

sw <- starwars %>%  select(name,height,mass,gender) %>% 
  rename(weight = mass) %>% na.omit() %>% mutate (height = height/100) %>% filter(gender %in%  c("masculine","feminine"))

View(sw)



sw <- starwars %>%  select(name,height,mass,gender) %>% 
  rename(weight = mass) %>% na.omit() %>% mutate (height = height/100) %>% filter(gender %in%  c("masculine","feminine")) %>%
  mutate(gender = recode(gender,masculine='m',feminine = 'f'))


sw <- starwars %>%  select(name,height,mass,gender) %>% 
  rename(weight = mass) %>% na.omit() %>% mutate (height = height/100) %>% filter(gender %in%  c("masculine","feminine")) %>%
  mutate(gender = recode(gender,masculine='m',feminine = 'f')) %>% 
  mutate(size = height > 1 & weight > 75)


sw <- starwars %>%  select(name,height,mass,gender) %>% 
  rename(weight = mass) %>% na.omit() %>% mutate (height = height/100) %>% filter(gender %in%  c("masculine","feminine")) %>%
  mutate(gender = recode(gender,masculine='m',feminine = 'f')) %>% 
  mutate(size = height > 1 & weight > 75,size = if_else(size == TRUE,"BIG","SMALL"))

sw


#top10 filtering tips in R :: video lecture 9

View(msleep)


#select name and sleep total and filter on sleep total > 18

my_data <- msleep %>% 
  select(name,sleep_total)%>%
  filter(sleep_total > 18)

my_data
#select name and sleep total and filter on sleep total < 18
my_data <- msleep %>% 
  select(name,sleep_total)%>%
  filter(! sleep_total > 18)
my_data


#apply filter on order and bodywt

my_data <- msleep %>% 
  select(name,order,bodywt,sleep_total) %>% 
  filter(order == "Primates",bodywt > 20)
my_data


my_data <- msleep %>% 
  select(name,order,bodywt,sleep_total) %>% 
  filter(order == "Primates") %>% 
  filter(bodywt > 20)

my_data

my_data <- msleep %>% 
  select(name,order,bodywt,sleep_total) %>% 
  filter(order == "Primates" | bodywt > 20)

my_data


#seelct only cow,dog and goat

my_data <- msleep %>% filter(name %in% c("Cow","Dog","Goat")) %>% select (name,sleep_total)
my_data

#select all rows where the sleeep total is between 16 and 18

my_data <- msleep %>%  filter(between(sleep_total,16,18)) %>% select(name,sleep_total)
my_data

#select  all the rows where sleep total is around 17 with tol +/- 0.5

my_data <- msleep %>%  filter(near(sleep_total,17,tol=0.5)) %>%  select (name,sleep_total)
my_data


#select all rows where the conservation column is na

my_data <- msleep %>% select(name,conservation,sleep_total) %>% filter(is.na(conservation))
my_data

#select all rows where conservation is not na
my_data <- msleep %>% select(name,conservation,sleep_total) %>% filter(! is.na(conservation))
my_data



##using function and objects in R :: video lecture 10

my_age <- 12

sum(my_age,10)

View(cars)

sum(cars$speed)

plot(cars)


hist(cars$speed)

attach(cars)


hist(speed)

hist(dist)



?attach


summary(cars)

summary(speed)

class(cars)
class(speed)
length(speed)
unique(speed)

head(cars)

tail(cars)


cars[2:5,1]

cars[2:10,]

help(attach)

median(speed)

median(dist)
new_data <- c(2,3,4,5,6,NA,9) 
median(new_data)
median(new_data,na.rm = TRUE)

str(cars)

class(cars)


#Explore your data :: video lecture 11

require(tidyverse)

data()

dim(starwars) #imp

str(starwars)

glimpse(starwars) #imp


View(starwars)

head(starwars)

View(head(starwars$films))

starwars$name

attach(starwars)

names(starwars) #imp


length(starwars)

class(hair_color)
length(hair_color)
unique(hair_color)


table(hair_color)

sort(table(hair_color))

sort(table(hair_color),decreasing = TRUE)

sort(hair_color)

sort(table(hair_color))


View(sort(table(hair_color),decreasing = TRUE))

barplot(sort(table(hair_color),decreasing = TRUE))
barplot()


starwars %>% select(hair_color) %>% 
  count(hair_color) %>% 
  arrange(desc(n)) %>% 
  View()


is.na(hair_color)

starwars[is.na(hair_color),]

View(starwars[is.na(hair_color),])

boxplot(height)

hist(height)

#clean the data :: video lecture 12


#get a preview of data
View(starwars)

glimpse(starwars)

class(starwars$gender)

unique(starwars$gender)

names(starwars)

#change to factor type
starwars$gender = as.factor(starwars$gender)

class(starwars$gender)

levels(starwars$gender)

starwars$gender = factor((starwars$gender),levels = c("masculine","feminine"))

levels(starwars$gender)

#get selected column names and use ends_with function
starwars %>% select(name,height,ends_with("color")) %>% 
  names()

starwars %>%  filter(hair_color %in% c("blond","brown"), height < 180) 


summary(starwars %>%  filter(hair_color %in% c("blond","brown"), height < 180) %>%
  select(height))


#finding and replacing NA values

starwars[is.na(starwars),]
starwars[is.na(height),]

mean(starwars$height,na.rm=TRUE)


starwars %>%  select(name,gender,hair_color,height) %>%  na.omit()

starwars %>%  select(name,gender,hair_color,height) %>% filter(complete.cases(.))

starwars %>%  select(name,gender,hair_color,height) %>% filter(!complete.cases(.))

starwars %>%  select(name,gender,hair_color,height) %>% filter(!complete.cases(.)) %>% drop_na(height)

starwars %>%  select(name,gender,hair_color,height) %>% filter(!complete.cases(.)) %>% mutate(hair_color = replace_na(hair_color,"none"))

#dealing with duplicates

names <- c("peter","john","andrew","peter")
age <- c(22,33,44,22)

friends <- data.frame(names,age)
friends


friends %>% distinct()

unique(friends)

duplicated(friends)
friends[duplicated(friends),]


friends[!duplicated(friends),]

unique(friends) %>%  View()




