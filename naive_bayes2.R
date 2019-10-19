################################################
### Naive Bayes Optimisation                 ###
################################################

# Main update: downs-sampling excluded; experiments with pre-processing


# Install libraries
install.packages("caret")
install.packages("dplyr")
install.packages("readr")
install.packages("quanteda")
install.packages("text2vec")
install.packages("glmnet")
install.packages("e1071")
install.packages("SparseM1")
install.packages("LiblineaR")
install.packages("tictoc")
install.packages("purrr")

# Start the Flow

rm(list = ls())

# Libraries
library(purrr)
library(caret)
library(dplyr)
library(readr)
library(quanteda)
library(text2vec)
library(glmnet)
library(e1071)
library(SparseM)
library(LiblineaR)
library (tictoc)

#load data

df <- readRDS("guardian.RDS")

# Split into training and test

df$gender <- as.factor(df$gender)

set.seed(555) 
index <- createDataPartition(df$gender, p = 0.7, 
                             list = FALSE)
train <- df[index, ]
test  <- df[-index, ]

# Assign labels

labels_train <- train$gender
labels_test <- test$gender


## preprocessing

stop_words <- stopwords("english")

#train data
tok.balanced <- tokens(train$headline, what="word",
                       remove_symbols = TRUE,
                       remove_punct = TRUE,
                       remove_numbers = TRUE,
                       remove_url= TRUE,
                       remove_hyphens = FALSE,
                       verbose = TRUE,
                       remove_twitter = TRUE,
                       include_docvars = TRUE,
                       ngrams = 1L)

# Creating a document feature matrix
train_dfm <- dfm(tok.balanced,
                 tolower= TRUE,
                 remove=stop_words,
                 verbose=TRUE,
                 include_docvars = TRUE)

train_dfm <- dfm_tfidf(train_dfm)

#train_dfm <- dfm_trim(train_dfm, min_termfreq = 10, min_docfreq = 20)

# test data
tok.balanced <- tokens(test$headline, what="word",
                       remove_symbols = TRUE,
                       remove_punct = TRUE,
                       remove_numbers = TRUE,
                       remove_url= TRUE,
                       remove_hyphens = FALSE,
                       verbose = TRUE,
                       remove_twitter = TRUE,
                       include_docvars = TRUE,
                       ngrams = 1L)

# Creating a document feature matrix
test_dfm <- dfm(tok.balanced,
               tolower= TRUE,
                remove=stop_words,
                verbose=TRUE,
                include_docvars = TRUE)

test_dfm <- dfm_tfidf(test_dfm)

#test_dfm <- dfm_trim(test_dfm, min_termfreq = 10, min_docfreq = 20)


# To fit a "binary multinomial" model, first convert the dfm to a binary matrix using dfm_weight(x, scheme = "boolean")


## Naive Bayes Model ##
tic()
nb_model <- textmodel_nb(train_dfm, train$gender, prior = "docfreq")
toc()
## Assessing Naive Bayes: Performance on test set ##

dfmat_matched <- dfm_match(test_dfm, features = featnames(train_dfm))
predict_test<- predict(nb_model, newdata = dfmat_matched)

table2 <- table(labels_test, predict_test)
confusionMatrix(table2, mode="everything")


confusionMatrix(table2, mode="everything")


### Baseline ###

# Ridge Regression Model
tic()
ridge <- cv.glmnet(x=train_dfm, y=train$gender,
                   alpha=0, nfolds=4, family="binomial")
toc()

# Evaluation

dfmat_matched <- dfm_match(test_dfm, features = featnames(train_dfm))
predict_test <- predict(ridge, newx=dfmat_matched, type="class")

table2 <- table(labels_test, predict_test)
confusionMatrix(table2, mode="everything")