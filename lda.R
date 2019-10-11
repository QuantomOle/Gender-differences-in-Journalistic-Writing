#LDA models
library(topicmodels)
library(quanteda)
#getwd()
#setwd("/Users/carminadietrich/Desktop/Project/newspaper-data-seperated/American")
#load  dataset
load("en_list_clean.RData")
guardian_df <- en_list2[[9]]
#removed list
#rm(en_list2)
#write.csv(guardian_df, "guardian.csv")
#save(guardian_df, "guardian.RData")
test <- read.csv("guardian.csv", stringsAsFactors = FALSE)

#divide female and male
f<-subset(guardian_df, guardian_df$gender == "female")
m<-subset(guardian_df, guardian_df$gender == "male")

#create corpus for females (fcor), dfm for female chose 20, because if a word appears in less than 20 documents it is too rare to be informative
fcor <- corpus(f$text)

#preprocess the corpus
preprocess <- function(corpus) {
  corpus <- dfm(corpus, remove=stopwords("english"), verbose=TRUE,
     remove_punct=TRUE, remove_numbers=TRUE)
  
  corpus <- dfm_trim(corpus, min_docfreq = 20)
}

#fdfm <- dfm(fcor, remove=stopwords("english"), verbose=TRUE,
 #           remove_punct=TRUE, remove_numbers=TRUE)
#cfdfm <- dfm_trim(fdfm, min_docfreq = 20)

cfdfm <- preprocess(fcor)

#same for males 
mcor <- corpus(m$text)
#mdfm <- dfm(mcor, remove=stopwords("english"), verbose=TRUE,
 #           remove_punct=TRUE, remove_numbers=TRUE)
#cmdfm <- dfm_trim(mdfm, min_docfreq = 20)

cmdfm <- preprocess(mcor)

#general analysis for Females, estimate K for 30 topics
K <- 30
flda <- LDA(cfdfm, k = K, method = "Gibbs", 
            control = list(verbose=25L, seed = 123, burnin = 100, iter = 500))

#get the top `n` terms from the topic model for each document
fterms <- get_terms(flda, 15)
fterms[,1]
#predict the top `k` topic for each document.
ftopics <- get_topics(flda, 1)
head(ftopics)
paste(fterms[,1], collapse=",")
sample(f$text[ftopics==1], 1)
#!!!!!!!to do: general anlysis for males!!!!!!

#Specific analysis for females, looking at sports
#create a genral female corpus
fsport <- subset(f, f$domain == 'Sport')

fsport_cor <- corpus(fsport$text)

fsport_dfm <- preprocess(fsport_cor)

K <- 5
flda <- LDA(fsport_dfm, k = K, method = "Gibbs", 
            control = list(verbose=25L, seed = 123, burnin = 100, iter = 500))

ftermssport <- get_terms(flda, 15)
fterms[,1]






