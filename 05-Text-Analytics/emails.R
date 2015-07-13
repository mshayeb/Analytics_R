library(tm)
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(RTextTools)



emails<-read.csv("emails.csv",stringsAsFactors=F)
str(emails)
summary(emails)
table(emails$spam)
head(emails$text)
nchar(emails[which.max(nchar(emails[,1])),1])
which.min(nchar(emails[,1]))


#### Create corpus

corpus = Corpus(VectorSource(emails$text))

corpus[[1]]


# Pre-process data
corpus <- tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)

# Look at first email
corpus[[1]]

# Create matrix

dtm = DocumentTermMatrix(corpus)
dtm

# Remove sparse terms
spdtm = removeSparseTerms(dtm, 0.95)

# Create data frame
emailsSparse = as.data.frame(as.matrix(spdtm))###
str(emailsSparse)

# Make all variable names R-friendly

colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))
emailsSparse$spam<-emails$spam
head(emailsSparse)
stemSums<-colSums(emailsSparse[emailsSparse$spam == 1,])
table(stemSums>=1000)

sort(colSums(subset(emailsSparse, spam == 1)))

emailsSparse$spam = as.factor(emailsSparse$spam)

set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)
str(train)

#Log
spamLog<-glm(spam~.,data=train,family="binomial")
summary(spamLog)




###training
#Predict Log
SpamPredLog<-predict(spamLog,type="response")
table(SpamPredLog<0.00001)
table(SpamPredLog>0.99999)
table(SpamPredLog<0.00001,SpamPredLog>0.99999)
table(SpamPredLog >= 0.00001 & SpamPredLog <= 0.99999)

table(train$spam,SpamPredLog>=.5)
summary(SpamPredLog)

#auc
library(ROCR)

predROCR = prediction(SpamPredLog, train$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values


#CART
spamCART<-rpart(spam~.,data=train,method="class")
prp(spamCART)

#Predict CART
SpamPredCart<-predict(spamCART,newdata=train)[,2]
head(SpamPredCart)
table(train$spam,SpamPredCart>=.5)
(2885+894)/nrow(train)


predROCR = prediction(SpamPredCart, train$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values


#randomForest
library(randomForest)
set.seed(123)
spamRF<-randomForest(spam~., data=train)
prp(spamRF)
plot(spamRF)

#Predict RF
SpamPredRF<-predict(spamRF,newdata=train,type="prob")[,2]
head(SpamPredCart)
table(train$spam,SpamPredRF>=.5)
(3046+958)/nrow(train)



predROCR = prediction(SpamPredRF, train$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values






###TESTING

#Predict Log
SpamPredLog<-predict(spamLog,newdata=test,type="response")
table(SpamPredLog<0.00001)
table(SpamPredLog>0.99999)
table(SpamPredLog<0.00001,SpamPredLog>0.99999)
table(SpamPredLog >= 0.00001 & SpamPredLog <= 0.99999)

table(test$spam,SpamPredLog>=.5)
(1257+376)/nrow(test)

#auc
library(ROCR)

predROCR = prediction(SpamPredLog, test$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values


#CART
spamCART<-rpart(spam~.,data=train,method="class")
prp(spamCART)

#Predict CART
SpamPredCart<-predict(spamCART,newdata=test)[,2]
head(SpamPredCart)
table(test$spam,SpamPredCart>=.5)
(1228+386)/nrow(test)


predROCR = prediction(SpamPredCart, test$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values


#randomForest
library(randomForest)
set.seed(123)
spamRF<-randomForest(spam~., data=train)
prp(spamRF)
plot(spamRF)

#Predict RF
SpamPredRF<-predict(spamRF,newdata=test,type="prob")[,2]
head(SpamPredCart)
table(test$spam,SpamPredRF>=.5)
(1290+386)/nrow(test)


predROCR = prediction(SpamPredRF, test$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values


#New section
#wordCount = rowSums(as.matrix(spdtm))
wordCount = rowSums(as.matrix(dtm))
hist(wordCount,breaks=100,xlim=c(0,2000))
hist(log(wordCount))

emailsSparse$logWordCount<-log(wordCount)
boxplot(logWordCount~spam, data=emailsSparse)

spl #done earlier
train2 = subset(emailsSparse, spl == TRUE)
test2 = subset(emailsSparse, spl == FALSE)

#CART
spam2CART<-rpart(spam~.,data=train2,method="class")
prp(spam2CART)

predSpam2Cart<-predict(spam2CART,newdata=test2)[,2]
table(test2$spam,predSpam2Cart>=.5)
(1214+384)/nrow(test2)

predROCR = prediction(predSpam2Cart, test2$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values


#randomForest
set.seed(123)
spam2RF<-randomForest(spam~.,data=train2)[,2]
SpamPred2RF<-predict(spam2RF,newdata=test2,type="prob")[,2]
table(test2$spam,SpamPred2RF>=.5)
(1296+383)/nrow(test2)

spredROCR = prediction(SpamPred2RF, test2$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values



###n-grams
install.packages("RTextTools")
library(RTextTools)
dtm2gram = create_matrix(as.character(corpus), ngramLength=2)
dtm2gram

spdtm2gram<-removeSparseTerms(dtm2gram, 0.95)


emailsSparse2gram<-as.data.frame(as.matrix(spdtm2gram))
colnames(emailsSparse2gram) = make.names(colnames(emailsSparse2gram))
emailsCombined = cbind(emailsSparse, emailsSparse2gram)

spl #done earlier
trainCombined = subset(emailsCombined, spl == TRUE)
testCombined = subset(emailsCombined, spl == FALSE)

spamCARTcombined<-rpart(spam~.,data=trainCombined,method="class")
prp(spamCARTcombined,varlen=0)
spamCartComboPred<-predict(spamCARTcombined,newdata=testCombined)[,2]
table(testCombined$spam,spamCartComboPred>=.5)
(1233+374)/nrow(testCombined)

predROCR = prediction(spamCartComboPred, testCombined$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values



##forest

set.seed(123)
spamRFcombined<-randomForest(spam~.,data=trainCombined)[,2]
SpamPred2RF<-predict(spamRFcombined,newdata=testCombined,type="prob")[,2]
table(testCombined$spam,SpamPred2RF>=.5)
(1296+383)/nrow(testCombined)



spredROCR = prediction(SpamPred2RF, testCombined$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

#AUC
performance(predROCR, "auc")@y.values