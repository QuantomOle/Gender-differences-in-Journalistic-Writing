
### Sentiment Analysis: Guardian dataset ###

#Libraries
library(stringr)
library(tidyverse)
library(quanteda)
library(parallel)
library(doSNOW)
library(SentimentAnalysis)

############################ IMPORTANT ############################
### Directly jump to analysis section if df_SA.RDS is available ###
###################################################################

# Information on dictionnaries used Dictionary

#GIDictionary
#GIDictionary with opinionated words from the Harvard-IV 
#dictionary asused in the General Inquirer software

#DictionaryHE
#Dictionary with opinionated words from Henry's Financial dictionary

#DictionaryLM
#Dictionary with opinionated words from Loughran-McDonald Finan-cial dictionary

# QDAP
# polarity words from QDAP package


# Load preprocessing functions provided by http://www.lexicoder.com/
source(file = "LSDprep_jan2018.R")

#import data
guardian <- read.csv("guardian.csv", stringsAsFactors = FALSE)
df <- guardian %>% dplyr::select(outlet, domain, text, gender)
remove(guardian)

## Sentiment Analysis specific preprocessing recommended by http://www.lexicoder.com/ ##

# Replace negations
df$text <- LSDprep_contr(df$text)

# Remove word that should not be included
df$text <- LSDprep_dict_punct(df$text)

# Insert spaces around punctuation
df$text <- LSDprep_punctspace(df$text)

# create clear negations
df$text <- LSDprep_negation(df$text)

# destroy sentiment words in different contexts
df$text <- LSDprep_dict(df$text)

# Save pre-processed data frame
saveRDS(df,file="df_SA.RDS")


## Sentiment Analysis ##

df <- readRDS("df_SA.RDS")

#Increase memory
memory.limit(size = 57000)
#Parallel computing
c <- makeCluster(10, "SOCK")
registerDoSNOW(c)

#Quanteda implementation
toks_news <- tokens(df$text, remove_punct = TRUE)

toks_news_lsd <- tokens_lookup(toks_news, dictionary =  data_dictionary_LSD2015[1:2])
head(toks_news_lsd, 2)

#Using Sentiment Analysis package

sentiment <- analyzeSentiment(df$text)

# use smaller subsets

sentiment1 <- analyzeSentiment(df$text[1:5000])
sentiment2 <- analyzeSentiment(df$text[5001:10000])
sentiment3 <- analyzeSentiment(df$text[10001:15726])

#Merge back together
sentiment <- rbind(sentiment1, sentiment2, sentiment3) 

#Save output as RDS
saveRDS(sentiment, file = "sentiment.RDS")