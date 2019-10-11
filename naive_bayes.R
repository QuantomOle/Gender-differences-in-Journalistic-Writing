################################################
### Baseline classifier: Naive Bayes         ###
################################################

# Libraries
library(caret)
library(dplyr)
library(readr)
library(quanteda)

#load data

df <- readRDS("guardian.RDS")

# look at distribution of gender (unbalanced: double the number of male authors)
class_distribution <- df %>% group_by(gender) %>% summarize(class_count=n())
print(head(class_distribution))

# To remedy the unbalanced sample, I make use of downsampling

set.seed(123)
df$gender <- as.factor(df$gender)
df_down <- downSample(x = df[, -ncol(df)],
                         y = df$gender)
table(df_down$Class)  

names(df_down)[names(df_down) == "Class"] <- "gender"

# Split into training and test

set.seed(555) 
index <- createDataPartition(df_down$gender, p = 0.7, 
                               list = FALSE)
train <- df[index, ]
test  <- df[-index, ]

labels_train <- train$gender
labels_test <- test$gender

## preprocessing

#train data
tok.balanced <- tokens(train$text, what="word",
                       remove_symbols = TRUE,
                       remove_punct = TRUE,
                       remove_numbers = TRUE,
                       remove_url= TRUE,
                       remove_hyphens = FALSE,
                       verbose = TRUE,
                       remove_twitter = TRUE,
                       include_docvars = TRUE)

# Creating a document feature matrix
train_dfm <- dfm(tok.balanced,
              tolower= TRUE,
              remove=stop_words,
              verbose=TRUE,
              include_docvars = TRUE)

# test data
tok.balanced <- tokens(test$text, what="word",
                       remove_symbols = TRUE,
                       remove_punct = TRUE,
                       remove_numbers = TRUE,
                       remove_url= TRUE,
                       remove_hyphens = FALSE,
                       verbose = TRUE,
                       remove_twitter = TRUE,
                       include_docvars = TRUE)

# Creating a document feature matrix
test_dfm <- dfm(tok.balanced,
              tolower= TRUE,
              remove=stop_words,
              verbose=TRUE,
              include_docvars = TRUE)

# Naive Bayes Model

nb_model <- textmodel_nb(train_dfm, train$gender)
summary(nb_model)

#dfmat_matched <- dfm_match(test_dfm, features = featnames(train_dfm))

predict_train <- predict(nb_model, newdata = train_dfm)

table1 <- table(labels_train, predict_train)

# Assessment of Model

confusionMatrix(table1, mode="everything")

# Performance on test set

dfmat_matched <- dfm_match(test_dfm, features = featnames(train_dfm))

predict_test<- predict(nb_model, newdata = dfmat_matched)

table2 <- table(labels_test, predict_test)

confusionMatrix(table2, mode="everything")
