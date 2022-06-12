# ==============================================================================
# INSTALL DEPENDENCIES
# ==============================================================================
list.of.packages <- c("stringr", "dplyr","data.table","ROSE","rpart")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(stringr)
library(dplyr)
library(data.table)
library(ROSE)
library(rpart)
# ==============================================================================
# IMPORT DATASET
# ==============================================================================

# Set the path where the file lives
#getwd()

# Import the .csv file
#setwd(path)
#bean <- read.csv("llbean_full.csv")

# Remove unused variables
#rm(path)

#source("data_import.R")

# ==============================================================================
# DEFINE CLEANING OPERATIONS
# ==============================================================================
source("function_clean_corpus.R")
# Contains clean_corpus(feature, stop_words, comb_words), which takes 3 arguments:
#   feature: a column of variable within a dataset
#   stop_words: a list of stop words
#   comb_words: a list of words or phrases to be combined into a single word
# Returns a corpus object

# ==============================================================================
# CLASSIFICATION MODEL WORKFLOW
# ==============================================================================
# Workflow:
#   Combine review_title & review_text into one Variable
#   Create Corpus & dtm of ALL Bean Reviews
#   Join dtm with rating
#   Perform Negative and Positive Filters (1/2-Star & 5 Star)
#   Build & Test Model
#   Apply Model

# ==============================================================================
# COMBINE REVIEW_TITLE & REVIEW_TEXT
# ==============================================================================
# This will improve the classification quality, as titles use more polarizing verbiage
LLB$review <- paste(LLB$reviewTitle,LLB$reviewText)

# ==============================================================================
# CREATE CORPUS & DTM OF ALL BEAN REVIEWS
# ==============================================================================
# Define Stop Words:
stopwords <- c("will", "shall", "can", "also", "bean", "llbean", "ll", "ve", "s")

# Call clean_corpus():
corpus <- clean_corp(LLB$review,stop_words = stopwords)

# Create the Document Term Matrix to serve as the Independent Variable
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.98)

# ==============================================================================
# JOIN THE DTM WITH RATING
# ==============================================================================

rating_dtm <- cbind(LLB$rating,as.matrix(dtm))
rating_df <- data.frame(rating_dtm)
names(rating_df)[names(rating_df) == "V1"] <- "rating"
rm(dtm,rating_dtm,corpus)

# ==============================================================================
# FILTER THE DATAFRAME TO CREATE SUPERVISED TRAINING/TESTING DATA
# ==============================================================================
# Rather than manually classify each model as positive or negative...
#   Assumption:   5 Star Reviews = Positive
#                 1/2 Star Reviews = Negative

# Define the negative review dataframe
mod.df.neg <- rating_df %>%
  filter(rating == 1 | rating == 2)

# Define the positive review dataframe
mod.df.pos <- rating_df %>%
  filter(rating == 5)

# Select a random sampling of positive reviews, such that nPositive = nNegative
#sampleIndexes <- sample(nrow(mod.df.pos), nrow(mod.df.neg))
#mod.df.pos <- mod.df.pos[sampleIndexes,]

# Combine the dataframes into one and factor
mod.df <- rbind(mod.df.neg,mod.df.pos)
mod.df$rating <- as.factor(mod.df$rating)
levels(mod.df$rating) <- c("negative","negative","positive")

# Remove the discrete dataframes
rm(mod.df.neg,mod.df.pos)

# Split into Test and Train Data
trainIndexes <- sample(nrow(mod.df),nrow(mod.df)*0.80)
data.train = mod.df[trainIndexes,]
data.test = mod.df[-trainIndexes,]

# ==============================================================================
# REGRESSION MODEL DEVELOPMENT AND TEST
# ==============================================================================
# True Positive: We correctly predicted Positive for a Positive Review
# True Negative: We correctly predicted Negative for a Negative Review
# False Positive: We incorrectly predicted Positive for a Negative Review
# False Negative: We incorrectly predicted Negative for a Positive Review
#       False Positives are worse for us - we want to identify improvements.
#
# If correctly identifying negatives is most important, we should focus on
#       having a model with higher sensitivity
#
# Prediction Bias:
#     Average of Prediction ~= Average of Observations

# Create the model
mod.glm = glm(rating~ ., family = "binomial", data =data.train, maxit = 100);
pred.glm = as.factor(as.numeric(predict(mod.glm, data.test, type="response")>.40))
roc.curve(data.test$rating, pred.glm)

# Re-assign levels so both match-up
levels(pred.glm) <- c("negative","positive")
levels(data.test$rating) <- c("negative","positive")

# Look at Confusion Tables:

print("Logistic Regression Classifier Results")
print(table(data.test$rating,pred.glm,dnn=c("Observed","Predicted")))
print(paste("The accuracy of the Regression Model was", 100*round(mean(ifelse(data.test$rating == pred.glm, 1, 0)),4),"%"))
print(paste("The sensitivity of the Regression Model was", 
            100*round(sum(ifelse(data.test$rating == pred.glm & data.test$rating=="positive", 1, 0))/
              (sum(ifelse(data.test$rating == pred.glm & data.test$rating=="positive", 1, 0))+
                 sum(ifelse(data.test$rating == "positive" & pred.glm=="negative", 1, 0))),4),"%"))
print("====================================================================")

# ==============================================================================
# DECISION TREE DEVELOPMENT AND TEST
# ==============================================================================
mod.tree <- rpart(rating~.,data=data.train,method="class", control=(cp=0.01))
pred.tree = predict(mod.tree, data.test, type="class")

# Re-assign levels so both match-up
levels(pred.tree) <- c("negative","positive")
levels(data.test$rating) <- c("negative","positive")

# Look at Confusion Tables:

print("Pruned Decision Tree Classifier Results")
print(table(data.test$rating,pred.tree,dnn=c("Observed","Predicted")))
print(paste("The accuracy of the Decision Tree Model was", 100*round(mean(ifelse(data.test$rating == pred.tree, 1, 0)),4),"%"))
print(paste("The sensitivity of the Regression Model was", 
            100*round(sum(ifelse(data.test$rating == pred.tree & data.test$rating=="positive", 1, 0))/
                        (sum(ifelse(data.test$rating == pred.tree & data.test$rating=="positive", 1, 0))+
                           sum(ifelse(data.test$rating == "positive" & pred.tree=="negative", 1, 0))),4),"%"))
print("====================================================================")

# ==============================================================================
# REGRESSION MODEL DEPLOYMENT
# ==============================================================================

pred.bean = as.factor(as.numeric(predict(mod.glm, rating_df, type="response")>.50))
levels(pred.bean) <- c("negative","positive")

LLB <- cbind(LLB,pred.bean)
names(LLB)[names(LLB) == "pred.bean"] <- "bi_rating"

setDT(LLB)
LLB[rating == 1 | rating == 2, bi_rating := "negative"]
LLB[rating == 5, bi_rating := "positive"]
LLB[bi_rating=="negative", n_rating := 1]
LLB[bi_rating=="positive", n_rating := 5]

LLB <- data.frame(LLB)

# ==============================================================================
# HIGHEST RATED ITEMS
# ==============================================================================

LLB %>%
  group_by(itemGroup) %>%
  summarise(Avg_Rating= mean(rating,na.rm=TRUE),Bi_Rating=mean(n_rating,na.rm=TRUE),nReviews = n()) %>%
  filter(nReviews > 1000) %>%
  arrange(-Bi_Rating)

LLB %>%
  group_by(itemGroup) %>%
  summarise(Avg_Rating= mean(rating,na.rm=TRUE),Bi_Rating=mean(n_rating,na.rm=TRUE),Diff_Rating=mean(n_rating,na.rm=TRUE)-mean(rating,na.rm=TRUE),nReviews = n()) %>%
  filter(nReviews > 1000) %>%
  arrange(Diff_Rating)

LLB %>%
  group_by(itemGroup) %>%
  summarise(nReviews = n()) %>%
  filter(nReviews > 1000) %>%
  arrange(-nReviews)

# ==============================================================================
# CLEAN OUT ENVIRONMENT VARIABLES
# ==============================================================================
write.csv(LLB, "LLB.csv",fileEncoding = "UTF-8")
rm(data.test,data.train,mod.df,mod.glm,rating_df,pred.bean,pred.glm,
   stopwords,trainIndexes,mod.tree,cp,pred.tree,LLB)


