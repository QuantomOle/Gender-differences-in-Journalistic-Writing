#Topic Modelling

library(topicmodels)
library(quanteda)
library(tidyverse)
library(RColorBrewer)
library(LDAvis) #LDA visualizations

#setwd("/Users/carminadietrich/Desktop/Project/newspaper-data-seperated/American")


#load the data
load("en_list_clean.RData")

# create guardian dataset
guardian_df <- en_list2[[9]]

#remove the list
#rm(en_list2)

#save(guardian_df, "guardian.RData")
#test <- read.csv("guardian.csv", stringsAsFactors = FALSE)

#divide female and male
fguard <-subset(guardian_df, guardian_df$gender == "female") #5619
mguard <-subset(guardian_df, guardian_df$gender == "male") #10096

# create corpus for female, male
fcorpus <- corpus(fguard$text)
mcorpus <- corpus(mguard$text)

# Function -- preprocess the corpus: remove stopwords; remove sparse terms `trim`
preprocess <- function(corpus) {
  corpus <- dfm(corpus, remove=stopwords("english"), verbose=TRUE,
                ngrams = 1L,
                stem = F,
     remove_punct=TRUE, remove_numbers=TRUE, remove_symbols = TRUE)
  
  corpus <- dfm_trim(corpus, min_termfreq = 10, min_docfreq = 20)
}

#Function: bigrams
preprocess_bi <- function(corpus) {
  corpus <- dfm(corpus, remove=stopwords("english"), verbose=TRUE,
                ngrams = 2L,
                stem = F,
     remove_punct=TRUE, remove_numbers=TRUE, remove_symbols = TRUE)
  
  corpus <- dfm_trim(corpus, min_termfreq = 10, min_docfreq = 20)
}

# min_count = remove words used less than x
# min_docfreq = remove words used in less than x docs

# create dfms: female, male
fdfm <- preprocess(fcorpus)
mdfm <- preprocess(mcorpus)

fdfm_bi <- preprocess_bi(fcorpus)
mdfm_bi <- preprocess_bi(mcorpus)

## Exploration
# Top 20 features
topfeatures(fdfm, n = 20) #nothing much to infer 
topfeatures(mdfm, 20)

topfeatures(fdfm_bi, 20) #nothing to infer
topfeatures(mdfm_bi, 20) #nothing to infer

#Dendograms: get an idea of how the words cluster
numWords <- 50

word_Dfm <- dfm_sort(dfm_tfidf(fdfm, scheme_tf = "prop")) 
word_Dfm <- t(word_Dfm)[1:numWords, ] #keep top numWords
wordDist <- dist(word_Dfm)
wordCluster <- hclust(wordDist)
plot(wordCluster, xlab = "", main = "TF-IDF Frequency Weighting: First 50 Words(Female)")

word_Dfm <- dfm_sort(dfm_tfidf(mdfm, scheme_tf = "prop")) 
word_Dfm <- t(word_Dfm)[1:numWords, ] #keep top numWords
wordDist <- dist(word_Dfm)
wordCluster <- hclust(wordDist)
plot(wordCluster, xlab = "", main = "TF-IDF Frequency Weighting: First 50 Words(Male)")

#####################################Latent Dirichlet Allocation (LDA)################################################################################################################################
# Function to convert our lda model results to a json 
# object, required by LDAVis: courtesy of Ryan Wesslen
topicmodels_json_ldavis <- function(fitted, dfm, dtm){
  # Required packages
  library(topicmodels)
  library(dplyr)
  library(stringi)
  library(quanteda)
  library(LDAvis)
  library(tm)
  
  # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  
  doc_length <- ntoken(dfm[rownames(dtm)])
  
  temp_frequency <- as.matrix(dtm)
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency),
                            stringsAsFactors = F)
  rm(temp_frequency)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}

####################Female###############################
K <- 20

# convert to a format that we can run the topic model with
fdtm <- convert(fdfm, to="topicmodels")

#estimate LDA with K topics
flda <- LDA(fdtm, k = K, method = "Gibbs", 
            control = list(verbose=25L, seed = 123, burnin = 100, iter = 500))

#create Json for Shiny-based interactive visualization called LDAvis
json <- topicmodels_json_ldavis(flda,fdfm,fdtm)

#extract topic order
new.order <- RJSONIO::fromJSON(json)$topic.order

# visualize LDA results
serVis(json, out.dir = 'femaleLDA', open.browser = T)

# get the top `n` terms
fterms <- get_terms(flda, 15)
#topics reordered
fterms <- fterms[,new.order]
colnames(fterms) <- paste("Topic",1:K)
fterms

# function to create dataframe of document-topic matrix
dataframe.dtm <- function(lda_model) {
  #extract the document-topic probability matrix
  post_list <- posterior(lda_model)
  
  #present document-topic probability matrix as a data frame
  dtm <- data.frame(post_list$topics)
  dtm <- dtm[,new.order]
  #change column names
  colnames(dtm) <- paste("Topic",1:K)
  
  return(dtm)
}

fprobtopics <- dataframe.dtm(flda)

#extract the top `k` topic for each document.
topics <- get_topics(flda, 1)
head(topics)


########################Male###########################
# convert to a format that we can run the topic model with
mdtm <- convert(mdfm, to="topicmodels")

#estimate LDA with K topics
mlda <- LDA(mdtm, k = K, method = "Gibbs", 
            control = list(verbose=25L, seed = 123, burnin = 100, iter = 500))

#create Json for Shiny-based interactive visualization called LDAvis
json <- topicmodels_json_ldavis(mlda,mdfm,mdtm)

#extract topic order
new.order <- RJSONIO::fromJSON(json)$topic.order

# visualize LDA results
serVis(json, out.dir = 'maleLDA', open.browser = T)

# get the top `n` terms
mterms <- get_terms(mlda, 15)
#topics reordered
mterms <- mterms[,new.order]
colnames(mterms) <- paste("Topic",1:K)
mterms

mprobtopics <- dataframe.dtm(mlda)


###################################################################################################
#######################################ENVIRONMENT################################################
#divide male and female: environment
fguard_env <- subset(fguard, fguard$domain == "Environment") #135
mguard_env <- subset(mguard, mguard$domain == "Environment") #304

# create corpus for female, male: environment
fcorpus_env <- corpus(fguard_env$text)
mcorpus_env <- corpus(mguard_env$text)



###################################################################################################
########################################SOCIETY##################################################
# divide male and female: society
fguard_soc <- subset(fguard, fguard$domain == "Society")
mguard_soc <- subset(mguard, mguard$domain == "Society")

# create corpus for female, male: society
fcorpus_soc <- corpus(fguard_soc$text)
mcorpus_soc <- corpus(mguard_soc$text)





############################################################################################################################################POLITICS##################################################
# divide male and female: politics
fguard_pol <- subset(fguard, fguard$domain == "Politics")
mguard_pol <- subset(mguard, mguard$domain == "Politics")

# create corpus for female, male: politics
fcorpus_pol <- corpus(fguard_pol$text)
mcorpus_pol <- corpus(mguard_pol$text)





