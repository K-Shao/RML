setwd("~/Desktop/Lawrenceville/Term 9/RML/")
train = read.csv("train.csv")
test = read.csv('Final/test.csv')

test_pca = prcomp(test[1:9999, 2:785])
train_pca = prcomp(train[1:9999, 2:785])
