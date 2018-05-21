setwd("~/Desktop/Lawrenceville/Term 9/RML/")
train = read.csv("train.csv")
test = read.csv('Final/test.csv')

pixel_positions = matrix(nrow=49, ncol=16)
for (i in 1:7) {
  for (j in 1:7) {
    pixel_positions[i+7*j-7,1] = (i-1)*4 + (j-1)*112 + 1
    pixel_positions[i+7*j-7,2] = (i-1)*4 + (j-1)*112 + 2
    pixel_positions[i+7*j-7,3] = (i-1)*4 + (j-1)*112 + 3
    pixel_positions[i+7*j-7,4] = (i-1)*4 + (j-1)*112 + 4
    pixel_positions[i+7*j-7,5] = (i-1)*4 + (j-1)*112 + 29
    pixel_positions[i+7*j-7,6] = (i-1)*4 + (j-1)*112 + 30
    pixel_positions[i+7*j-7,7] = (i-1)*4 + (j-1)*112 + 31
    pixel_positions[i+7*j-7,8] = (i-1)*4 + (j-1)*112 + 32
    pixel_positions[i+7*j-7,9] = (i-1)*4 + (j-1)*112 + 57
    pixel_positions[i+7*j-7,10] = (i-1)*4 + (j-1)*112 + 58
    pixel_positions[i+7*j-7,11] = (i-1)*4 + (j-1)*112 + 59
    pixel_positions[i+7*j-7,12] = (i-1)*4 + (j-1)*112 + 60
    pixel_positions[i+7*j-7,13] = (i-1)*4 + (j-1)*112 + 85
    pixel_positions[i+7*j-7,14] = (i-1)*4 + (j-1)*112 + 86
    pixel_positions[i+7*j-7,15] = (i-1)*4 + (j-1)*112 + 87
    pixel_positions[i+7*j-7,16] = (i-1)*4 + (j-1)*112 + 88
  }
}

pixel_positions = matrix(nrow=196,ncol=4)
for (i in 1:14) {
  for (j in 1:14) {
    pixel_positions[i+14*j-14,1] = (i-1)*2 + (j-1)*56 + 1
    pixel_positions[i+14*j-14,2] = (i-1)*2 + (j-1)*56 + 2
    pixel_positions[i+14*j-14,3] = (i-1)*2 + (j-1)*56 + 29
    pixel_positions[i+14*j-14,4] = (i-1)*2 + (j-1)*56 + 30
  }
}

data = as.matrix(test_sub_long[3,])
data = matrix(data, 7, 7)
data = data[,c(7:1)]
image(data)

data = as.matrix(test_sub_long[3,])
data = matrix(data, 14, 14)
data = data[,c(14:1)]
image(data)

test_sub = matrix(nrow = 9999, ncol = 49)

for (i in 1:49) {
  test_sub[,i] = test[,c(pixel_positions[i]) + 1 ]
}

train_sub = matrix(nrow = 59999, ncol = 49)

for (i in 1:49) {
  train_sub[,i] = train[,c(pixel_positions[i]) + 1]
}

test_sub_long = matrix(nrow = 9999, ncol = 196)

for (i in 1:196) {
  test_sub_long[,i] = test[,c(pixel_positions[i]) + 1 ]
}

train_sub_long = matrix(nrow = 59999, ncol = 196)

for (i in 1:196) {
  train_sub_long[,i] = train[,c(pixel_positions[i]) + 1]
}


