getwd()
setwd("~/Hertie/second_sem/text_data/Gender-differences-in-Journalistic-Writing/")

#load libraries
library(tidyverse)


#import all the data
path <- "C:/Users/JOSHUA/Documents/Hertie/second_sem/text_data/newspaper-data/"
files <- list.files(path=path, pattern="*.RDa")

for(file in files){
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
  gsub(" ","",substr(file, 1, perpos-1)), 
  load(paste(path,file,sep="")))
}


#clean
en <- list(abcnews_df, bbcnews_df, bloomberg_df, breitbart_df, buzzfeed_df, cnbc_df, dailykos_df, foxnews_df, guardian_df, huffingtonpost_df, infowars_df, motherjones_df, newsweek_df, newyorker_df, nytimes_df, politico_df, thehill_df, thinkprogress_df, townhall_df, usatoday_df, vice_df, washingtonpost_df, wsj_df, yahoous_df)

de <- list(abendblatt_df, bild_df, faz_df, focus_df, freitag_df, gmx_df, handelsblatt_df, jungefreiheit_df, spiegel_df, stuttgarter_df, tagesspiegel_df, tonline_df, webde_df, welt_df, yahoode_df, zeit_df)

# New

