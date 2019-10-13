################################################
### Baseline classifier: Logistic regression ###
################################################

# Libraries
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

# Create a smaller subset for developing the models: Randomizing the rows

#set.seed(125)
#df_down <- df_down[sample(row.names (df_down)), ]

#df_down <- df_down[1:500,]

# Split into training and test

set.seed(555) 
index <- createDataPartition(df_down$gender, p = 0.7, 
                             list = FALSE)
train <- df_down[index, ]
test  <- df_down[-index, ]

labels_train <- train$gender
labels_test <- test$gender

## preprocessing

stop_words <- stopwords("english")

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

# Baseline: Regression Model
tic()
ridge <- glmnet(x=train_dfm, y=train$gender,
                   alpha=0, family="binomial")
toc()

lasso <- cv.glmnet(x=train_dfm, y=train$gender,
                   alpha=1, nfolds=10, family="binomial")

elastic_net <- cv.glmnet(x=train_dfm, y=train$gender,
                      alpha=0.5, nfolds=10, family="binomial")

# Evaluation

dfmat_matched <- dfm_match(test_dfm, features = featnames(train_dfm))
predict_test <- predict(ridge, newx=dfmat_matched, type="class")

table2 <- table(labels_test, predict_test)
confusionMatrix(table2, mode="everything")


## Naive Bayes Model
tic()
nb_model <- textmodel_nb(train_dfm, train$gender)
toc()
summary(nb_model)

## Assessing Naive Bayes: Performance on test set ##

dfmat_matched <- dfm_match(test_dfm, features = featnames(train_dfm))

predict_test<- predict(nb_model, newdata = dfmat_matched)

table2 <- table(labels_test, predict_test)

confusionMatrix(table2, mode="everything")

## SVM Model

# gridsearch

cv <- trainControl(
  method = "cv", 
  number = 5, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary)

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))

set.seed(323)
svm_model_grid <- train(y=labels_train, 
                   x=train_dfm,
             method = "svmLinear",
             tuneGrid = grid,
             trControl = cv)

plot(svm_model_grid)

tic()
svm_model_2 <- train(y=labels_train, 
                        x=train_dfm,
                        method = "svmLinear",
                     cost=0.25)
toc()

tic()
svm_model <- svm(train_dfm, labels_train, kernel = "linear", cost=0.25) 
toc()

# Evaluation
dfmat_matched <- dfm_match(test_dfm, features = featnames(train_dfm))
predict_test<- predict(svm_model_2, newdata = dfmat_matched)
table2 <- table(labels_test, predict_test)
confusionMatrix(table2, mode="everything")
