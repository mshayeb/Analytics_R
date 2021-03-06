library(tm)
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(RTextTools)

wiki = read.csv("wiki.csv")
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)
corpusAdded <- Corpus(VectorSource(wiki$Added))
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded <- tm_map(corpusAdded, stemDocument)
dtmAdded <- DocumentTermMatrix(corpusAdded)

# Filter out sparse terms by keeping only terms that appear in 0.3% or more of the revisions. 
# Call the new matrix sparseAdded. How many terms appear in sparseAdded?
dtmAddedN = removeSparseTerms(dtmAdded, 0.997)
dtmAddedN

# Convert sparseAdded to a data frame called wordsAdded
wordsAdded = as.data.frame(as.matrix(dtmAddedN))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

# Creating wordsRemoved dataset
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(dtmRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
# How many words are in the wordsRemoved data frame?
str(wordsRemoved)


# Combine the two dataframes (using cbind) into a data frame called wikiWords
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal
table(wikiWords$Vandal)

# Creating training and test sets
set.seed(123)
spl = sample.split(wikiWords$Vandal, 0.7)
train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)
table(test$Vandal)

wikiCART = rpart(Vandal~., data=train, method="class")
pred = predict(wikiCART, newdata=test, method="class")
# What is the accuracy of the model on the test set, using a threshold of 0.5?
pred.prob = pred[,2]
table(test$Vandal, pred.prob >= 0.5)

# Plot the CART tree. How many word stems does the CART model use?
prp(wikiCART)

#####################################################################################
#####################################################################################
##     PROBLEM 2 ####################################################################
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
# Counting of links added in revisions
table(wikiWords2$HTTP)

# Creating new training and test sets
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
wikiCART2 = rpart(Vandal~., data=wikiTrain2, method="class")
pred2 = predict(wikiCART2, newdata=wikiTest2, method="class")
pred.prob2 = pred2[,2]
table(wikiTest2$Vandal, pred.prob2 >= 0.5)

dtmRemoved = DocumentTermMatrix(corpusRemoved)
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
# What is the average number of words added?
summary(wikiWords2$NumWordsAdded)
mean(wikiWords2$NumWordsAdded)

# Creating new train and test sets
wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)
wikiCART3 = rpart(Vandal~., data=wikiTrain3, method="class")
pred3 = predict(wikiCART3, newdata=wikiTest3, method="class")
pred.prob3 = pred3[,2]
table(wikiTest3$Vandal, pred.prob3 >= 0.5)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
# Build a CART model using all the training data. What is the accuracy of the model on the test set?
wikiTrain3 = subset(wikiWords3, spl==TRUE)
wikiTest3 = subset(wikiWords3, spl==FALSE)
wikiCART3 = rpart(Vandal~., data=wikiTrain3, method="class")
pred3 = predict(wikiCART3, newdata=wikiTest3, method="class")
pred.prob3 = pred3[,2]
table(wikiTest3$Vandal, pred.prob3 >= 0.5)

prp(wikiCART3)
















