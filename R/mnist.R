# Load the MNIST digit recognition dataset into R
# http://yann.lecun.com/exdb/mnist/
# assume you have all 4 files and gunzip'd them
# creates train$n, train$x, train$y  and test$n, test$x, test$y
# e.g. train$x is a 60000 x 784 matrix, each row is one digit (28x28)
# call:  show_digit(train$x[5,])   to see a digit.
# brendan o'connor - gist.github.com/39760 - anyall.org

load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <<- load_image_file('mnist//train-images.idx3-ubyte')
  test <<- load_image_file('mnist//t10k-images.idx3-ubyte')
  
  train$y <<- load_label_file('mnist//train-labels.idx1-ubyte')
  test$y <<- load_label_file('mnist//t10k-labels.idx1-ubyte')  
}

load_mnist()

show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}

show_digit(train$x[3,]) 
dim(train$x)

set.seed(123)
train_ind <- sample(seq_len(60000),
                    size = floor(0.2 * 60000))
train_smp <- list()
train_smp$x <- train$x[train_ind,]
train_smp$y <- train$y[train_ind]

set.seed(345)
test_ind <- sample(seq_len(10000),
                    size = floor(0.5 * 10000))
test_smp <- list()
test_smp$x <- test$x[test_ind,]
test_smp$y <- test$y[test_ind]
z <- apply(train_smp$x, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
z[is.nan(z)] = 0

library(e1071)
svm_data <- data.frame(x = z, y = as.factor(train_smp$y))
svm_model <- svm(y~., data = svm_data, method = "class",
                 kernel = 'sigmoid', scale = FALSE)

y <- apply(test_smp$x, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
y[is.nan(y)] = 0
svm_test <- data.frame(y)
data_names <- names(svm_data)
#length(data_names[data_names != "y"])
names(svm_test) <- data_names[data_names != "y"]
#pred <- predict(object = svm_model, svm_test, response='class')
#table(pred=pred,true=test_smp$y)





library(neuralnet)
library(nnet)
nn_model <- nnet(y~., data=svm_data, size = 10, maxit = 500, MaxNWts = 40000)

pred_nn <- predict(nn_model, svm_test, type = "class")
pred_svm <- predict(svm_model, svm_test, type = "class")
table(pred=pred,true=test_smp$y)

sprintf("Model Accuracy: %2.5f",
        length(test_smp$y[test_smp$y == pred_nn])/length(test_smp$y))
sprintf("Model Accuracy: %2.5f",
        length(test_smp$y[test_smp$y == pred_svm])/length(test_smp$y))
