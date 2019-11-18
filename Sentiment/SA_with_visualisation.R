
### Sentiment Analysis: Guardian dataset ###
#Author: Iris van der Lugt


#Libraries
library(stringr)
library(tidyverse)
library(quanteda)
library(parallel)
library(doSNOW)
library(SentimentAnalysis)
library(ggplot2)


############################ IMPORTANT ############################
### Directly jump to analysis section if df_SA.RDS is available ###
###################################################################

# Information on dictionnaries used Dictionary

#GIDictionary
#GIDictionary with opinionated words from the Harvard-IV 
#dictionary as used in the General Inquirer software

#DictionaryHE
#Dictionary with opinionated words from Henry's Financial dictionary

#DictionaryLM
#Dictionary with opinionated words from Loughran-McDonald Financial dictionary

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

#If the vector is too large, use smaller subsets
sentiment1 <- analyzeSentiment(df$text[1:5000])
sentiment2 <- analyzeSentiment(df$text[5001:10000])
sentiment3 <- analyzeSentiment(df$text[10001:15726])

#Merge back together
sentiment <- rbind(sentiment1, sentiment2, sentiment3) 

#Save output as RDS
saveRDS(sentiment, file = "sentiment.RDS")


## Visualisation ##

#Merge sentiment output with df
df$ID <- seq.int(nrow(df))
sentiment$ID <- seq.int(nrow(sentiment))
df_SA <- merge(df, sentiment, by = "ID")

#Arrange IDs by sentiment score per dictionary, and get plots
#LM
df_SA <- df_SA %>% arrange(SentimentLM)
df_SA$ID <- seq.int(nrow(df_SA))

ggplot(df_SA, aes(x = ID, y = SentimentLM)) +
  geom_col(aes(fill = gender),) +
  ggtitle("Sentiment scores for the LM dictionary") +
  labs(y = "LM Sentiment scores", x = "Article ID")

ggplot(subset(df_SA, domain %in% c("World")), aes(x = ID, y = SentimentLM)) +
  geom_col(aes(fill = gender),)

#Subset of articles with domain "World"
df_SA_World <- subset(df_SA,df_SA$domain == "World")
df_SA_World <- df_SA_World %>% arrange(SentimentLM)
df_SA_World$ID <- seq.int(nrow(df_SA_World))

ggplot(df_SA_World, aes(x = ID, y = SentimentLM)) +
  geom_col(aes(fill = gender),)+
  ggtitle("Sentiment scores for the LM dictionary, domain 'World'") +
  labs(y = "LM Sentiment scores", x = "Article ID")


## Additional plots ##

#Plots for remaining dictionaries and histograms

#QDAP
df_SA <- df_SA %>% arrange(SentimentQDAP)
df_SA$ID <- seq.int(nrow(df_SA))

ggplot(df_SA, aes(x = ID, y = SentimentQDAP)) +
  geom_col(aes(fill = gender),)

ggplot(subset(df_SA, domain %in% c("World")), aes(x = ID, y = SentimentQDAP)) +
  geom_col(aes(fill = gender),)

#HE
df_SA <- df_SA %>% arrange(SentimentHE)
df_SA$ID <- seq.int(nrow(df_SA))

ggplot(df_SA, aes(x = ID, y = SentimentHE)) +
  geom_col(aes(fill = gender),)

ggplot(subset(df_SA, domain %in% c("World")), aes(x = ID, y = SentimentHE)) +
  geom_col(aes(fill = gender),)

#GI
df_SA <- df_SA %>% arrange(SentimentGI)
df_SA$ID <- seq.int(nrow(df_SA))

ggplot(df_SA, aes(x = ID, y = SentimentGI)) +
  geom_col(aes(fill = gender),)

ggplot(subset(df_SA, domain %in% c("World")), aes(x = ID, y = SentimentGI)) +
  geom_col(aes(fill = gender),)

#Histograms
ggplot(df_SA, aes(x=SentimentGI, color=gender)) +
  geom_histogram(binwidth = 0.01, fill="white", 
                 alpha = 0.5, position = "identity")

ggplot(subset(df_SA, domain %in% c("Science")), aes(x=SentimentQDAP, color=gender)) +
  geom_histogram(bins = 50, fill="white", 
                 alpha = 0.5, position = "identity")