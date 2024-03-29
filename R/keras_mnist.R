library(keras)
#reticulate::use_python("/usr/bin/python")
#install_keras()

mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

mat1 <- x_train[3,1:28,1:28]
mat1 <- apply(mat1, 2, rev)
image(1:28, 1:28, t(mat1), asp = 0.75, xlim = c(1,28))
mat1[17:22,3:9]
y_train[1:5]

dim(x_train) <- c(nrow(x_train), 784)
dim(x_test) <- c(nrow(x_test), 784)
x_train <- x_train / 255
x_test <- x_test / 255
x_train[3,539:544]

y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)
y_train[1:5,]

model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

summary(model)



model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

#model2 <- load_model_hdf5('/home/ruser1/ShinyApps/ml_demo/mnist_model')
history <- model %>% fit(
  x_train, y_train,
  epochs = 10, batch_size = 128,
  validation_split = 0.2
)

plot(history)
#save_model_hdf5(model2, '/home/ruser1/ShinyApps/ml_demo/mnist_model.hdf5')
#model <- load_model_hdf5('/home/ruser1/ShinyApps/ml_demo/mnist_model.hdf5')

model %>% evaluate(x_test, y_test)



Y_test_hat <- predict_classes(model, x_test)
table(mnist$test$y, y_test_hat)
mean(mnist$test$y == Y_test_hat)


