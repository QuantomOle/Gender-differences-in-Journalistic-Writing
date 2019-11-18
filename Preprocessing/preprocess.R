#getwd()
#setwd("~/Hertie/second_sem/text_data/Gender-differences-in-Journalistic-Writing/")

#load libraries
library(tidyverse)
#library(here)
library(lubridate)

#import all the data
path <- "C:/Users/JOSHUA/Documents/Hertie/second_sem/text_data/newspaper-data/us/"
#path <- "/Users/carminadietrich/Desktop/Project/newspaper-data-seperated/American/"
files <- list.files(path=path, pattern="*.(RDa)")

for(file in files){
    load(paste(path,file,sep=""))
}


#create lists of english and german datasets
en <- list(abcnews_df, bbcnews_df, bloomberg_df, breitbart_df, buzzfeed_df, cnbc_df, dailykos_df, foxnews_df, guardian_df, huffingtonpost_df, infowars_df, motherjones_df, newsweek_df, newyorker_df, nytimes_df, politico_df, thehill_df, thinkprogress_df, townhall_df, usatoday_df, vice_df, washingtonpost_df, wsj_df, yahoous_df) #removed infowars for lack of column 'topic_tags'

#de <- list(abendblatt_df, bild_df, faz_df, focus_df, freitag_df, gmx_df, handelsblatt_df, jungefreiheit_df, spiegel_df, stuttgarter_df, tagesspiegel_df, tonline_df, webde_df, welt_df, yahoode_df, zeit_df)


#creat function that cleans the datasets
clean_data <- function(data) {
  if("section" %in% colnames(data)) {
    data$section = data$section
  }
  else{
    data$section = NA
  }
  
  data <- select(data, outlet, datetime, headline, description, author, section, domain, text)
  data$author <- gsub(",$", "", data$author) #delete the last comma in the author names
  data$author <- gsub(".([a-z]+)\\.", "", data$author) #delete titles in author names
  data$author <- gsub("^[A-Z]\\.", NA, data$author) #delete abbreviation of first name
  data$author <- gsub(".*(,|and|&).*", NA, data$author) #replace articles containing two authors with NA's
  data <- filter(data, !is.na(author)) #delete NA's
  
  utf8 <- function(x) {
  x %>% gsub('[^\x20-\x7E]', '', .) %>% 
    gsub("[^[:alnum:][:blank:]?&/\\-\\.,]", "", .) %>% 
    gsub("U00..", "", .)}
  
  data <- apply(data, 2, utf8)
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data$datetime <- parse_date_time(data$datetime, orders = c("ymd HMS"))
  
  #create new column firstname
  data$firstname <- word(data$author,1)
  
  
  return(data)
}



#load names datafile
gender_df <- read.csv("C:/Users/JOSHUA/Documents/Hertie/second_sem/text_data/newspaper-data/us/gender_refine-csv.csv")
#rename first column
colnames(gender_df)[1] <- "firstname"

assign_gender <-function(data) {
  data <- left_join(data, gender_df[, 1:2], by = "firstname")
data<- data %>% filter(!is.na(gender), gender != 3) %>% 
  mutate(gender = ifelse(gender == 1, "male", "female")) %>% 
  select(-c(firstname, author))
}

#apply the function 
en_list <- lapply(en, clean_data)

#join the csv file to the dataframes in english list
en_list2 <- lapply(en_list, assign_gender)


#save list
save(en_list2, file = "en_list_clean.RData")

