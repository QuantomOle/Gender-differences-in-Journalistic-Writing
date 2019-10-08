getwd()
setwd("~/Hertie/second_sem/text_data/Gender-differences-in-Journalistic-Writing/newspaper-data")

#load libraries
library(tidyverse)


#import data
path <- "C:/Users/JOSHUA/Documents/Hertie/second_sem/text_data/Gender-differences-in-Journalistic-Writing/newspaper-data/"
files <- list.files(path=path, pattern="*.RDa")
for(file in files)
{
perpos <- which(strsplit(file, "")[[1]]==".")
assign(
gsub(" ","",substr(file, 1, perpos-1)), 
load(paste(path,file,sep="")))
}
