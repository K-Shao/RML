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

pctrain = prcomp(ktrain)

pctest = prcomp(ktest)

screeplot(pctrain, npcs = 784, xlab = "PC", main = "Importance of Principal Components")
screeplot(pctrain, npcs = 100, xlab = "PC", main = "Importance of Principal Components")

pc = summary(pctrain)$importance

pc[3,]

pctrain$x[,c(1:100)]

pcatrain = data.frame(trainlabel, pctrain$x[,c(1:100)])
pcatrain
length(pcatrain)

pcatest = data.frame(testlabel, pctest$x[,c(1:100)])
