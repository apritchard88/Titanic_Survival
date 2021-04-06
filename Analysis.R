library(tidyverse)
library(readr)
library(ggplot2)

### First step is to load the data
test_data <- read_csv("data/test.csv")
train_data <- read_csv("data/train.csv")

### Check for missing data
summary(train_data)
na_count <- sapply(train_data, function(y) sum(is.na(y)))

# Age has a lot of NAs. Will be important predictor so need to infer missing values
# Cabin has mostly NAs. Could be useful, best bet is probably to recode it as had/did not have cabin number
# Embarked has only 2 NAs. Not obvious how to infer these. Embarked may be useful (ie did
# everyone getting on at Southampton go to a safer part of the ship for instance?)

### Mark the samples and combine for inferring missing data
test_data$type <- "test"
test_data$Survived <- NA   # needed so columns match in both sets
train_data$type <- "train"

full_data <- rbind(test_data, train_data)

# I can infer the missing ages by take the mean value by title
# Names include titles (Mr, Miss, Mrs, Master etc) which are likely to be related to age
# It is the best information I have to go on for inferring missing ages
get_title <- function(name){
  str_split(str_split(name, ", ")[[1]][2], "\\.")[[1]][1]
}

# apply the function to every row to add the title as a new colum
full_data <- full_data %>% rowwise() %>% # need rowwise() or first value is repeated
  mutate(title = get_title(Name))

# group by title and find the mean age 
age_by_title <- full_data %>% filter(!is.na(Age)) %>%
  select(title, Age) %>%
  group_by(title) %>%
  summarise(mean_age = mean(Age))

# join the summarised data to the main table 
full_data <- left_join(full_data, age_by_title, by = c("title","title"))

print(sum(is.na(full_data$mean_age)))

# now I see that every person has a mean age. I need to coalesce the age columns so
# any blanks are filled in
full_data$Age <- ifelse(is.na(full_data$Age), full_data$mean_age, full_data$Age)

full_data$Has_Cabin <- ifelse(is.na(full_data$Cabin), 0, 1)

# there are now 2 remaining NA values. They are in 'Embarked':
full_data %>% filter(is.na(Embarked))

# they are 2 first class passengers who paid £80 fare. Is there an easy way to infer these
# missing 'embarked' values?
first_class_embarked <- full_data %>% 
  filter(Pclass==1) %>% 
  group_by(Embarked) %>% 
  summarise(n=n())

first_class_embarked

# this shows that almost no first class passengers embarked at Q, so it is unlikely the
# missing values are NA. I will just infer S for the NAs as it is most popular for first
# class embarking
full_data$Embarked <- ifelse(is.na(full_data$Embarked), "S", full_data$Embarked)
na_count <- sapply(train_data, function(y) sum(is.na(y)))

# the only remaining NA is for the Fare for one passenger
full_data %>% filter(is.na(Fare))
third_class_mean_fare <- full_data %>% filter(!is.na(Fare)) %>%
  group_by(Pclass) %>%
  summarise(mean_fare = mean(Fare)) %>%
  filter(Pclass==3)

full_data$Fare <- ifelse(is.na(full_data$Fare), third_class_mean_fare$mean_fare, full_data$Fare)

# at this point all the NAs in the data have been filled
# write it out and then can do the analysis in a new file
write_csv(full_data, "Data/Cleansed_Data.csv", append = FALSE, col_names = TRUE)
