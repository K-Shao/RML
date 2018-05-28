library("png")
library(class)
#library(magick)
#library(randomForest)
library(MASS)

#library(party)

#GENERAL

setwd("~/Desktop/Lawrenceville/Term 9/RML")
rawtrain = read.csv(("train.csv"))
for(i in 1:784) {
  colnames(rawtrain)[i+1] = paste(toString(ceiling(i/28)),toString(i%%28),sep = ".")
}
colnames(rawtrain)[1] = "true"
#true = rawtrain[,1]
rawtest = read.csv(("Final/test.csv"))

for(i in 1:784) {
  colnames(rawtest)[i+1] = paste(toString(ceiling(i/28)),toString(i%%28),sep = ".")
}
colnames(rawtest)[1] = "truetest"


trainlabel = rawtrain[,1]
testlabel = rawtest[,1]
#knn
ktrain = subset(rawtrain,select= -c(1))
ktest = subset(rawtest,select= -c(1))

ktest = ktest[c(400:600),]

knnpredict = knn(train = ktrain, test = ktest, cl = trainlabel, k=5)

check = data.frame(knnpredict,testlabel[c(400:600)])

names(check) = c('Predicted','Actual')

length(which(check$Predicted!=check$Actual))/length(knnpredict)

#show = function(x) {
  #rawimg = image_read("C:/Users/imjef/Documents/Schoolwork/Machine Learning/Project 2/Eight.png")
#   rawimg = image_read("C:/Users/imjef/Documents/Schoolwork/Machine Learning/Project 2/RML/Write-up/Test.png")
#   
#   print(rawimg)
#   rawimg = image_scale(rawimg,"28")
#   
#   img = readPNG(image_write(rawimg,format = 'png'))
#   
#   gray = ((img)*-255)+255
#   
#   input = c()
#   for(i in c(1:28)) {
#     input <- c(input, gray[i,]) 
#   }
#   
#   result = knn(train = ktrain, test = input, cl = trainlabel, k=x)
#   
#   ktrain2 = ktrain[which(trainlabel!=result),]
#   trainlabel2 = trainlabel[which(trainlabel!=result)]
#   
#   result2 = knn(train = ktrain2, test = input, cl = trainlabel2,k=x)
#   
#   ktrain3 = ktrain2[which(trainlabel2!=result2),]
#   trainlabel3 = trainlabel2[which(trainlabel2!=result2)]
#   
#   result3 = knn(train = ktrain3, test = input, cl = trainlabel3,k=x)  
#   
#   print(result)
#   print(result2)
#   print(result3)
# }
#show(5)


for(i in c(1:785)) {
  ktrain[60000,i] <- sum(ktrain[c(1:59999),i])/59999
}
dimtrain = ktrain[c(1,which(ktrain[60000,]>10))]
dimtest = ktest[c(1,which(ktrain[60000,]>10))]

dimtrain = dimtrain[c(1:59999),]
dimtest = dimtest[c(400:600),]

dimpredict = knn(train = dimtrain, test = dimtest, cl = trainlabel, k=5)

dimcheck = data.frame(dimpredict,testlabel[c(400:600)])

names(dimcheck) = c('Predicted','Actual')

length(which(dimcheck$Predicted!=dimcheck$Actual))/length(dimpredict)

