train_less = train[, apply(train, 2, var, na.rm=TRUE) != 0]
train_sub = train_less[1:2000,]
train_sub_less = train_sub[, apply(train_sub, 2, var, na.rm=TRUE) !=0]

lda = lda(formula=X5~., data=train_less[,0:1000])
predict(lda)$class==train$X5
