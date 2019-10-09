#getwd()
setwd("~/Hertie/second_sem/text_data/Gender-differences-in-Journalistic-Writing/")

#load libraries
library(tidyverse)
library(here)

#import all the data
path <- "C:/Users/JOSHUA/Documents/Hertie/second_sem/text_data/newspaper-data/"
files <- list.files(path=path, pattern="*.RDa")

for(file in files){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
  gsub(" ","",substr(file, 1, perpos-1)), 
  load(paste(path,file,sep="")))
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
  
  data <- filter(data, !is.na(author))
  data$tag <- data$topic_tags
  data <- select(data, outlet, datetime, headline, description, author, section, domain, tag, text)
  
  return(data)
}

#apply the function 
en_list <- lapply(en[[1]], clean_data) 
de_list <- lapply(de, clean_data)

for(i in en) {
  print(i)
  x<- clean_data(i)
  print
}


x <- clean_data(abcnews_df)
  


