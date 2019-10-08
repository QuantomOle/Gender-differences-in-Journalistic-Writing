getwd()
setwd("~/Hertie/second_sem/text_data/Gender-differences-in-Journalistic-Writing/newspaper-data")

#load libraries
library(tidyverse)


#import all the data
path <- "C:/Users/JOSHUA/Documents/Hertie/second_sem/text_data/Gender-differences-in-Journalistic-Writing/"
files <- list.files(path=path, pattern="*.RDa")
for(file in files) {
 load(file) 
}

#clean



