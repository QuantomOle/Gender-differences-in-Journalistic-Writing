#getwd()
setwd("~/Hertie/second_sem/text_data/Gender-differences-in-Journalistic-Writing/")

#load libraries
library(tidyverse)
#library(here)

#import all the data
path <- "C:/Users/JOSHUA/Documents/Hertie/second_sem/text_data/newspaper-data/"
files <- list.files(path=path, pattern="*.RDa")

for(file in files){
  load(paste(path,file,sep=""))
}


#create lists of english and german datasets
en <- list(abcnews_df, bbcnews_df, bloomberg_df, breitbart_df, buzzfeed_df, cnbc_df, dailykos_df, foxnews_df, guardian_df, huffingtonpost_df, motherjones_df, newsweek_df, newyorker_df, nytimes_df, politico_df, thehill_df, thinkprogress_df, townhall_df, usatoday_df, vice_df, washingtonpost_df, wsj_df, yahoous_df) #removed infowars for lack of column 'topic_tags'

de <- list(abendblatt_df, bild_df, faz_df, focus_df, freitag_df, gmx_df, handelsblatt_df, jungefreiheit_df, spiegel_df, stuttgarter_df, tagesspiegel_df, tonline_df, webde_df, welt_df, yahoode_df, zeit_df)


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
  data$author <- gsub(".*(,|and|&).*", NA, data$author) #replace articles containing two authors with NA's
  data <- filter(data, !is.na(author)) #delete NA's
  
  
  return(data)
}

#apply the function 
en_list <- lapply(en, clean_data) 
de_list <- lapply(de, clean_data) #doesn't work yet

#
bbc_short <- (en_list[[2]])
bbc_short %>% count(author)
unique(bbc_short$author)

  
#test code
fruits <- c("The apple, two apple", "Abc pears", "three bananas", "Espn.com News Services", "Dr. Snow and College")
str_remove(fruits, "(^The|Abc|Espn.com|News).*")
str_remove(fruits, ",$|")
