load_data <- function () {
  setwd("~/Desktop/Lawrenceville/Term 9/RML/MNIST")
  train = read.csv("train.csv")
  test = read.csv("test.csv")
  train_matrix = data.matrix(train[1:60000, 2:785])
}


knn_case <- function(input_case, k = 7) {
  input_data = data.matrix(test[input_case, 2:50])
  knn(input_data, k)
}


knn <- function (input, k = 7) {
  #arr = data.matrix(train[1:60000,2:785])
  arr = train_matrix
  differences = sweep(arr, 2, input, "-", check.margin = FALSE )
  differences = differences * differences
  distances = rowSums(differences)
  indices = order(distances)[1:k]
  votes = train[indices, 1]
  prediction = strtoi(names(which.max(table(votes))))
  #print(votes)
  return (prediction)
}

check <- function (input, truth, input_case = -1) { #I'm using input case -1 to denote outside input
  guess = knn(data.matrix(input))
  sprintf("%s, %s, %s, %s", input_case, guess, truth, guess==truth)
  #sprintf("Index: %s | KNN: %s | Actual: %s | Correct: %s ", input_case, guess, truth, guess==truth)
}

check_case <- function (input_case) {
  check(test[input_case, 2:50], test[input_case,1], input_case)
}

show <- function (input) {
  data = matrix(input, 7, 7)
  data = data[,c(7:1)]
  image(data)
}


show_case <- function (case) {
  array = as.matrix(test[case,2:50])
  show(array)
}

save <- function (case, name) {
  data = as.matrix(test[case,2:50])
  data = matrix(data, 7, 7)
  data = data[,c(7:1)]
  png(paste("~/Desktop/Lawrenceville/Term 9/RML/Final/Images2/",name,".png", sep=""))
  image(data)
  dev.off()
}

full <- function(case) {
  print(check_case(case))
  show_case(case)
}

#This will take a long time! 10000 cases * ~6 seconds/case = ~16-17 hours
classify_all <- function () {
  total = 0
  correct = 0
  for (i in 1:10000) {
    guess = knn(i)
    truth = test[i,1]
    if (guess==truth) {
      correct = correct + 1
    }
    total = total + 1
    logFile = "~/Desktop/Lawrenceville/Term 9/RML/Final/log_file2.txt"
    cat(check_case(i), file=logFile, append = TRUE, sep = "\n")
    save(i, as.character(i))
    print(i)
  }
}



