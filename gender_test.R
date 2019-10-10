library(dplyr)
library(stringr)
getwd() 
setwd("/Users/carminadietrich/Desktop/Project/Test")

# load, subset and clean data
df <- load(file = "/Users/carminadietrich/Desktop/Project/newspaper-data/bloomberg_to_may_2019.RDa")
df <- bloomberg_df

#clean author
df$author <- gsub(",$", "", df$author)
df$author <- gsub(".([a-z]+)\\.", "", df$author) 
df$author <- gsub("^[A-Z]\\.", "", df$author)
df$author <- gsub(".(,|and|&).", NA, df$author)
df <- filter(df, !is.na(author))

#extract firstnames
df$firstname <- word(df$author,1)

#unique first names 
n_distinct(df$firstname)


#load names 
names <- read.csv(file = "gender_refine-csv.csv")
colnames(names)[1] <- "firstname"
df2 <- left_join(df, names[, 1:2], by = "firstname")

#df2 %>% filter(is.na(gender)) %>% count(firstname) %>% top_n(firstname, w = 10)

#df2 %>% filter(gender == 3) %>% count(firstname)

df_clean <- df2 %>% filter(!is.na(gender), gender != 3) %>% 
  mutate(gender = ifelse(gender == 1, "male", "female")) %>% 
  select(-c(firstname, author))


