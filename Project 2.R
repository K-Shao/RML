library("png")
library(class)
library(magick)
library(randomForest)
library(MASS)

library(party)

#GENERAL

rawtrain = read.csv(("C:/Users/imjef/Documents/Schoolwork/Machine Learning/Project 2/mnist_train.csv"))
for(i in 1:784) {
  colnames(rawtrain)[i+1] = paste(toString(ceiling(i/28)),toString(i%%28),sep = ".")
}
colnames(rawtrain)[1] = "true"
#true = rawtrain[,1]
rawtest = read.csv(("C:/Users/imjef/Documents/Schoolwork/Machine Learning/Project 2/mnist_test.csv"))

for(i in 1:784) {
  colnames(rawtest)[i+1] = paste(toString(ceiling(i/28)),toString(i%%28),sep = ".")
}
colnames(rawtest)[1] = "truetest"


trainlabel = rawtrain[,1]
testlabel = rawtest[,1]
#knn
ktrain = subset(rawtrain,select= -c(1))
ktest = subset(rawtest,select= -c(1))

ktest = ktest[c(1:200),]

knnpredict = knn(train = ktrain, test = ktest, cl = trainlabel, k=5)

check = data.frame(knnpredict,testlabel[c(1:200)])

names(check) = c('Predicted','Actual')

length(which(check$Predicted!=check$Actual))/length(knnpredict)

show = function(x) {
  #rawimg = image_read("C:/Users/imjef/Documents/Schoolwork/Machine Learning/Project 2/Eight.png")
  rawimg = image_read("C:/Users/imjef/Documents/Schoolwork/Machine Learning/Project 2/RML/Write-up/Test.png")
  
  print(rawimg)
  rawimg = image_scale(rawimg,"28")
    
  img = readPNG(image_write(rawimg,format = 'png'))
    
  gray = ((img)*-255)+255
      
  input = c()
  for(i in c(1:28)) {
     input <- c(input, gray[i,]) 
  }
  
  result = knn(train = ktrain, test = input, cl = trainlabel, k=x)
  
  ktrain2 = ktrain[which(trainlabel!=result),]
  trainlabel2 = trainlabel[which(trainlabel!=result)]
  
  result2 = knn(train = ktrain2, test = input, cl = trainlabel2,k=x)
  
  ktrain3 = ktrain2[which(trainlabel2!=result2),]
  trainlabel3 = trainlabel2[which(trainlabel2!=result2)]
  
  result3 = knn(train = ktrain3, test = input, cl = trainlabel3,k=x)  
  
  print(result)
  print(result2)
  print(result3)
}
show(5)

#RANDOM TREES

subtrain = rawtrain[c(1:2000),]
subtest = rawtrain[c(1:500),]

subtest = subset(subtest,select= -c(1))

kforest = randomForest(formula = true ~ ., data = subtrain)

gforest <- ctree(true ~ ., data=subtrain)
plot(gforest, type="simple")

length(which(round(predict(kforest))!=trainlabel[c(1:2000)]))/2000

kimportance = importance(kforest, type = 2)

kimportance[order(-kimportance)]

forestres = predict(kforest,newdata = subtest)

forestcheck = data.frame(round(forestres), testlabel[c(1:500)])

names(forestcheck) = c('Predicted','Actual')
length(which(forestcheck$Predicted!=forestcheck$Actual))/500

table(data = round(predict(kforest)), reference = trainlabel[c(1:2000)])

table(data=forestcheck$Predicted, 
                reference=forestcheck$Actual)


#qda duck kevin

qsubtrain = rawtrain[c(1:20000),]
qsubtest = rawtest[c(1:5000),]

for(i in c(1:785)) {
  qsubtrain[20001,i] <- sum(qsubtrain[c(1:20000),i])
}

#daoptimize = function(x){
  qdatrain = qsubtrain[c(1,which(qsubtrain[20001,]>1000000))]
  qdatest = qsubtest[c(1,which(qsubtrain[20001,]>1000000))]
  
  qdatrain = qdatrain[c(1:20000),]
  qdatest = subset(qdatest,select= -c(1))
  
  kqda = qda(true~.,data =qdatrain)

  qdares = predict(kqda,newdata = qdatest)
  
  qdacheck = data.frame(qdares$class,testlabel[c(1:5000)])
  
  names(qdacheck) = c('Predicted','Actual')
  
  length(which(predict(kqda)$class != trainlabel[1:20000]))/20000
  
  length(which(qdacheck$Predicted != qdacheck$Actual))/5000
#}
#qdaoptimize(1000)
#gldaoptimize = data.frame()
#for(i in c(1:40)){
#  gldaoptimize[i,1]<- 50*i
#  gldaoptimize[i,2]<- ldaoptimize(50*i)
  #gldaoptimize[i,3]<- length(which(ldacheck$Predicted != ldacheck$Actual))/500
#}
#gldaoptimize

table(data = predict(kqda)$class, reference = trainlabel[1:20000])
table(data = qdacheck$Predicted, reference = qdacheck$Actual)


#lda

subtrain = rawtrain[c(1:2000),]
subtest = rawtest[c(1:500),]

for(i in c(1:785)) {
  subtrain[2001,i] <- sum(subtrain[c(1:2000),i])
}

ldaoptimize = function(x){
  ldatrain = subtrain[which(subtrain[2001,]>x)]
  ldatest = subtest[which(subtrain[2001,]>x)]
  
  ldatrain = ldatrain[c(1:2000),]
  ldatest = subset(ldatest,select= -c(1))
  
  
  klda = lda(true~.,data =ldatrain)
  
  ldares = predict(klda,newdata = ldatest)
  
  ldacheck = data.frame(ldares$class,testlabel[c(1:500)])
  
  names(ldacheck) = c('Predicted','Actual')
  
  #length(which(predict(klda)$class != trainlabel[1:2000]))/2000
  
  length(which(ldacheck$Predicted != ldacheck$Actual))/500
}
ldaoptimize(1000)
gldaoptimize = data.frame()
for(i in c(1:40)){
  gldaoptimize[i,1]<- 50*i
  gldaoptimize[i,2]<- ldaoptimize(50*i)
  #gldaoptimize[i,3]<- length(which(ldacheck$Predicted != ldacheck$Actual))/500
}
gldaoptimize

table(data = predict(klda)$class, reference = trainlabel[1:2000])
table(data = ldacheck$Predicted, reference = ldacheck$Actual)


#lda2

subtrain2 = rawtrain[c(1:10000),]
subtest2 = rawtest[c(1:2500),]

ldatrain2 = data.frame(subtrain2[,1])
for(i in c(1:nrow(subtrain2))) {
  ldatrain2[i,2] <- length(which(subtrain2[i,]>0))
}  
names(ldatrain2)=c("true", "fill")
klda2 = lda(formula = true~fill,data = ldatrain2)

ldatest2 = data.frame()
for(i in c(1:nrow(subtest2))) {
  ldatest2[i,1] <- length(which(subtest2[i,]>0))
} 
names(ldatest2)="fill"
ldares2 = predict(klda2, newdata = ldatest2)

ldacheck2 = data.frame(ldares2$class,testlabel[c(1:2500)])

names(ldacheck2) = c('Predicted','Actual')

length(which(ldacheck2$Predicted != ldacheck2$Actual))/2500

table(data = ldacheck2$Predicted, reference = ldacheck2$Actual)

ldacheck2

klda2
