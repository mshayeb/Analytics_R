library(tm)
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(RTextTools)

trials <- read.csv("clinical_trial.csv", stringsAsFactors = F, fileEncoding="latin1")
# How many characters are there in the longest abstract?
max(nchar(trials$abstract))

# How many search results provided no abstract?
sum(nchar(trials$abstract) == 0)

# What is the shortest title of any article?
trials$title[which.min(nchar(trials$title))]


# Create Corpus
corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))

# Convert to lower case
corpusTitle <- tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract <- tm_map(corpusAbstract, content_transformer(tolower))

# Remove punctuation
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

# Remove Stop words
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))

# Stem the words
corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)


# Create matrix
dtmTitle= DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)
dtmTitle
dtmAbstract
# Filter out sparse terms by keeping only terms that appear in at least 5%
# or more of the documents
dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)
dtmTitle
dtmAbstract
# Convert dtmTitle and dtmAbstract to data frames
dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))


frequencyAbstract <- colSums(dtmAbstract)
which.max(frequencyAbstract)

## Building a model
colnames(dtmTitle) <- paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) <- paste0("A", colnames(dtmAbstract))
colnames(dtmTitle)

# Combine the two dataframes
dtm <- cbind(dtmTitle, dtmAbstract)
# Add the Vandal variable
dtm$trial <- trials$trial
str(dtm)

set.seed(144)
trialsSplit <- sample.split(dtm$trial, SplitRatio=0.7)
train <- subset(dtm, trialsSplit==T)
test<- subset(dtm, trialsSplit==F)
table(train$trial)


library(rpart)
library(rpart.plot)
library(caTools)
trialCART = rpart(trial~., data=train, method="class")
prp(trialCART)

pred <- predict(trialCART, newdata=test, type = 'class')
table(test$trial, test[,2]>0.5)
