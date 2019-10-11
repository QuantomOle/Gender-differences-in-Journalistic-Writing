################################################
### Baseline: Latent Semantic Analysis (LSA) ###
################################################

# Libraries
library(topicmodels)
library(quanteda)

#load data

df <- readRDS("guardian.RDS")

#divide female and male
f<-subset(df, df$gender == "female")
m<-subset(df, df$gender == "male")

#create corpus for females (fcor), dfm for female
fcor <- corpus(f$text)
fdfm <- dfm(fcor, remove=stopwords("english"), verbose=TRUE,
            remove_punct=TRUE, remove_numbers=TRUE)
cfdfm <- dfm_trim(fdfm, min_docfreq = 20)

#create corpus for males 
mcor <- corpus(m$text)
mdfm <- dfm(mcor, remove=stopwords("english"), verbose=TRUE,
            remove_punct=TRUE, remove_numbers=TRUE)
cmdfm <- dfm_trim(mdfm, min_docfreq = 20)

# Create LSA model for female
lsa_f <- textmodel_lsa(cfdfm, nd = 30)

head(lsa_f$docs)
lsa_f$matrix_low_rank[,1:5]

head(lsa_f$features)
