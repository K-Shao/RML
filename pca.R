#rawtrain = read.csv(("C:/Users/imjef/Documents/Schoolwork/Machine Learning/Project 2/mnist_train.csv"))
for(i in 1:784) {
  colnames(rawtrain)[i+1] = paste(toString(ceiling(i/28)),toString(i%%28),sep = ".")
}
colnames(rawtrain)[1] = "true"
#true = rawtrain[,1]
#rawtest = read.csv(("C:/Users/imjef/Documents/Schoolwork/Machine Learning/Project 2/mnist_test.csv"))

for(i in 1:784) {
  colnames(rawtest)[i+1] = paste(toString(ceiling(i/28)),toString(i%%28),sep = ".")
}
colnames(rawtest)[1] = "truetest"

trainlabel = rawtrain[,1]
testlabel = rawtest[,1]
#knn
ktrain = subset(rawtrain,select= -c(1))
ktest = subset(rawtest,select= -c(1))

full = rbind(ktrain, ktest)

pca = prcomp(full)

pc = summary(pctrain)$importance

pcatrain = data.frame(trainlabel, pca$x[1:59999,c(1:100)])
length(pcatrain)

pcatest = data.frame(testlabel, pca$x[60000:69998,c(1:100)])



