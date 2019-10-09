
library(dplyr)
library(stringr)

# load, subset and clean data
df <- load(file = "us/guardian_to_may_2019.RDa")
df <- guardian_df
rm(guardian_df)

df <- df %>% filter(!is.na(author))

length(unique(df$author))

df_test <- head(df)
df_test$firstname <- word(df_test$author,1)

df_test <- df_test %>% filter(!(firstname=="GrrlScientist")) # manually taking out strange names (we need to find a better
                                                              # way to do this)

# Test gender prediction

library(gender)

df_test$year <- 1990

results <- gender_df(df_test, name_col = "firstname", year_col = "year",
                     method = "ssa")
results
