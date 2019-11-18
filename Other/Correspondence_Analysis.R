# Correspondence Anlysis

### Key problem: how to select the number of dimensions that we want? ###


library(tidyverse)
library(quanteda)

#import data
guardian <- read.csv("guardian.csv", stringsAsFactors = FALSE)
df <- guardian %>% dplyr::select(outlet, domain, text, gender)
remove(guardian)

# Select subset

df2 <- head(df,10)


#function to generate dfm
makeDFM <- function(df) {
  corpus <- corpus(df, text_field = "text")
  doctm <- dfm(corpus, remove=stopwords("english"), verbose=TRUE,
               ngrams = 1L,
               stem = TRUE,
               remove_punct=TRUE, 
               remove_numbers=TRUE,
               remove_symbols = TRUE)
  
  doctm <- dfm_trim(doctm, max_docfreq = 0.9, docfreq_type = "prop")
  
  return(doctm)
}

dfm <- makeDFM(df2)


tmod_ca <- textmodel_ca(dfm) #,nd=100 to determine how many dimensions to keep

#plot results
textplot_scale1d(tmod_ca)

# get first dimension for the 10 texts
coef(tmod_ca, doc_dim = 1:9)$coef_document

#Dimension into dataframe
df_test <- data.frame (coef(tmod_ca, doc_dim = 1:9)$coef_document) 
# How do I get more than 9 dimensions?


dat_ca <- data.frame(dim1 = coef(tmod_ca, doc_dim = 1)$coef_document, 
                     dim2 = coef(tmod_ca, doc_dim = 2)$coef_document)
head(dat_ca)

#Plot dimensions
plot(1, xlim = c(-2, 2), ylim = c(-2, 2), type = 'n', xlab = 'Dimension 1', ylab = 'Dimension 2')
grid()
text(dat_ca$dim1, dat_ca$dim2, labels = rownames(dat_ca), cex = 0.8, col = rgb(0, 0, 0, 0.7))
