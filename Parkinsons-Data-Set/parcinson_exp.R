parkinson_data <- read.csv(file="data\\parkinsons.data", header=TRUE, sep=",")

names(parkinson_data)
summary(parkinson_data)

parkinson_s <- scale(parkinson_data[,2:24])

pc<-princomp(parkinson_s)
summary(pc)

parkinson_data$status[parkinson_data$status == 0] <- 2

pairs(parkinson_data[,2:24], col=c("red", "blue")[parkinson_data[,18]])

#Logistic regression

#Reimport dataset
parkinson_data <- read.csv(file="data\\parkinsons.data", header=TRUE, sep=",")

summary(parkinson_data)

glm.fit = glm(status~.-name, data=parkinson_data, family = "binomial")
summary(glm.fit)

glm.probs=predict(glm.fit,type="response") 
glm.probs[1:24]

#set a threshold
glm.pred=ifelse(glm.probs>0.1,1,0)

names(glm.pred)
summary(glm.pred)

table(glm.pred, parkinson_data$status)
mean(glm.pred == parkinson_data$status)

# Make training and test set

parkinson_data <- read.csv(file="data\\parkinsons.data", header=TRUE, sep=",")
parkinson_data$name=NULL
parkinson_data$status[parkinson_data$status == 0] <- 2

splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/5))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}


splits <- splitdf(parkinson_data, seed=808)


training <- splits$trainset
testing <- splits$testset

glm.fit=glm(status~., data=parkinson_data,family="binomial", subset=1:20)

glm.probs=predict(glm.fit,newdata=testing,type="response") 

glm.pred=ifelse(glm.probs>0.1,1,0)

table(glm.pred, parkinson_data$status)
mean(glm.pred == parkinson_data$status)

#Fit smaller model
glm.fit=glm(status~ DFA + RPDE + status+ HNR+ PPE + D2 + NHR + spread1 + spread2, data=parkinson_data,family='binomial', subset=training)
glm.probs=predict(glm.fit,newdata=testing,type="response") 

glm.pred=ifelse(glm.probs>0.1,1,0)

table(glm.pred, parkinson_data$status)
mean(glm.pred == parkinson_data$status)

## Linear Discriminant Analysis
install.packages('MASS')
require(MASS)

lda.fit=lda(status~.-name,data=parkinson_data, subset=training)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit,testing)
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,testing$status)
mean(lda.pred$class==testing$status)

## K-Nearest Neighbors
library(class)
?knn
Xlag=cbind(Lag1,Lag2)
knn.pred=knn(Xlag[train,],Xlag[testing,],parkinson_data[training],k=1)
table(knn.pred,Direction[testing])
mean(knn.pred==Direction[testing])