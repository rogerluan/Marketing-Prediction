#changes the maxprint param to display more lines
#options(max.print=999999)

#reads the .csv in an object named "bank"
bank <- read.csv("~/Downloads/bank/bank-full.csv", header = TRUE, sep = ";", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

#installs and imports "reshape2" package
install.packages("reshape2")
library(reshape2)

#function to replace the job column with 12 brand new specific job columns containing bool values
bank<-cbind(bank, model.matrix( ~ 0 + job, bank))

#removes the original job column
bank<-bank[,-2]

#replaces non-numeric attributes with numeric values
bank <- within(bank, marital <- factor(marital, labels = c(1,2,3)))
bank <- within(bank, education <- factor(education, labels = c(1,2,3,-1)))
bank <- within(bank, default <- factor(default, labels = c(0,1)))
bank <- within(bank, housing <- factor(housing, labels = c(0,1)))
bank <- within(bank, loan <- factor(loan, labels = c(0,1)))
bank <- within(bank, contact <- factor(contact, labels = c(1,0,-1)))
bank <- within(bank, month <- factor(month, labels = c(4,8,12,2,1,7,6,3,5,11,10,9)))
bank <- within(bank, poutcome <- factor(poutcome, labels = c(0,2,1,-1)))
bank <- within(bank, y <- factor(y, labels = c(0,1)))

#prints the attributes (used just to debug - uncomment to use)
#bank$age
#bank$marital
#bank$education
#bank$default
#bank$balance
#bank$housing
#bank$loan
#bank$contact
#bank$day
#bank$month
#bank$duration
#bank$campaign
#bank$pdays
#bank$previous
#bank$poutcome
#bank$y

#saves the resulting dataframe into a new .csv, and then open it again.
#this is done to solve a particular bug in the conversion process
write.csv(bank, file = "bank-normalized.csv")
bank <- read.csv("~/Downloads/bank-normalized.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

#creates the training and testing datasets
training <- bank[-c(1:250, 1001:1250, 2001:2250, 3001:3250, 4001:4250, 5001:5250, 6001:6250, 7001:7250, 8001:8250, 9001:9250, 10001:10250, 11001:11250, 12001:12250, 13001:13250, 14001:14250, 15001:15250, 16001:16250, 17001:17250, 18001:18250, 19001:19250, 20001:20250, 21001:21250, 22001:22250, 23001:23250, 24001:24250, 25001:25250, 26001:26250, 27001:27250, 28001:28250, 29001:29250, 30001:30250, 31001:3250, 31001:3250, 32001:32250, 33001:33250, 34001:34250, 35001:35250, 36001:36250, 37001:37250, 38001:38250, 39001:39250, 40001:40250, 41001:41250, 42001:42250, 43001:43250, 44001:44250, 45001:45211),]
test <- bank[c(1:250, 1001:1250, 2001:2250, 3001:3250, 4001:4250, 5001:5250, 6001:6250, 7001:7250, 8001:8250, 9001:9250, 10001:10250, 11001:11250, 12001:12250, 13001:13250, 14001:14250, 15001:15250, 16001:16250, 17001:17250, 18001:18250, 19001:19250, 20001:20250, 21001:21250, 22001:22250, 23001:23250, 24001:24250, 25001:25250, 26001:26250, 27001:27250, 28001:28250, 29001:29250, 30001:30250, 31001:3250, 31001:3250, 32001:32250, 33001:33250, 34001:34250, 35001:35250, 36001:36250, 37001:37250, 38001:38250, 39001:39250, 40001:40250, 41001:41250, 42001:42250, 43001:43250, 44001:44250, 45001:45211),]

#installs and imports "FNN" package
install.packages("FNN")
library(FNN)

#creates training and testing classes
trainClass <- training[,16]
testingClass <- test[,16]

#removes classification columns from datasets
training <- training[,-16]
test <- test[,-16]

#result for k=1
k1results <- knn(training, test, trainClass, k=1)
plot(k1results)

#result for k=3
k3results <- knn(training, test, trainClass, k=3)
plot(k3results)

#result for k=5
k5results <- knn(training, test, trainClass, k=5)
plot(k5results)

#result for k=11
k11results <- knn(training, test, trainClass, k=11)
plot(k11results)

#calculates percentage of "yes" answers of result dataset
negativeAnswer <- sum(bank$y == 0)
positiveAnswer <- sum(bank$y == 1)
bankSum <- bankNegativeAnswer + bankPositiveAnswer
bankSum
acceptancePercentage <- positiveAnswer/sum
acceptancePercentage <- round(acceptancePercentage, digits = 4)
acceptancePercentageString <- paste(acceptancePercentage*100, "%", sep="", collapse = NULL)
acceptancePercentageString

#calculates percentage of "yes" answers of the results after applying KNN algorithm
negativeAnswer <- sum(k3results == 0)
positiveAnswer <- sum(k3results == 1)
sum <- negativeAnswer + positiveAnswer
sum
acceptancePercentage <- positiveAnswer/sum
acceptancePercentage <- round(acceptancePercentage, digits = 4)
acceptancePercentageString <- paste(acceptancePercentage*100, "%", sep="", collapse = NULL)
acceptancePercentageString
