##### Titanic Decision Tree Tutorial
## http://trevorstephens.com/kaggle-titanic-tutorial/getting-started-with-r/

library(ggplot2)
library(dplyr)
library(rpart)
library(rpart.plot)
library(scales)


## 75% of the sample size
ptitanic$row_id <- row.names(ptitanic)
main_smp_size <- floor(0.95 * nrow(ptitanic))
set.seed(246)
main_ind <- sample(seq_len(nrow(ptitanic)), size = main_smp_size)
main_titanic <- ptitanic[main_ind, ]
secret_test <- ptitanic[-main_ind, ]

smp_size <- floor(0.75 * nrow(main_titanic))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(main_titanic)), size = smp_size)

train <- main_titanic[train_ind, ]
test <- main_titanic[-train_ind, ]

ggplot(train) +
  geom_histogram(aes(x = survived, fill = survived), stat = "count", position = "stack") +
  geom_text(aes(x = survived, y = ((..count..)/sum(..count..)),
                label = scales::percent((..count..)/sum(..count..))), colour = "yellow", stat = "count", vjust = -10.2) +
  scale_fill_manual(values = c("Red", "Blue")) +
  labs(title = "Survival rates", y = "People", x = "")

ggplot(train, aes(x = as.factor(sex))) +
  geom_bar(aes(y = (..count..)/sum(..count..), fill = survived)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Male/Female Survival rates", y = "Percent", x = "Gender") +
  scale_fill_manual(values = c("Red", "Blue"))

ggplot(train) + geom_histogram(aes(x = age, fill = survived), binwidth = 5, position = "dodge") +
  scale_fill_manual(values = c("Red", "Blue")) +
  labs(title = "Survival rates by age band", y = "Percent", x = "Age band")

names(train)
pairs(~survived+sex+age,data=train, 
      main="Simple Scatterplot Matrix")

summary(train)

Titanic

fit <- rpart(survived ~ sex + age,
             data=train,
             method="class")

fit <- rpart(survived ~ sex + age,
             data=train,
             method="class", 
             control=rpart.control(minsplit=15, cp=0))

cols <- ifelse(fit$frame$yval == 1, "darkred", "green4") # green if survived


prp(fit, main="assorted arguments",
    extra=106,           # display prob of survival and percent of obs
    nn=TRUE,             # display the node numbers
    fallen.leaves=TRUE,  # put the leaves on the bottom of the page
    shadow.col="gray",   # shadows under the leaves
    branch.lty=3,        # draw branches using dotted lines
    branch=.5,           # change angle of branch lines
    faclen=0,            # faclen=0 to print full factor names
    trace=1,             # print the automatically calculated cex
    split.cex=1.2,       # make the split text larger than the node text
    split.prefix="is ",  # put "is " before split text
    split.suffix="?",    # put "?" after split text
    col=cols, border.col=cols,   # green if survived
    split.box.col="lightgray",   # lightgray split boxes (default is white)
    split.border.col="darkgray", # darkgray border on split boxes
    split.round=.5)              # round the split box corners a tad

fit <- rpart(survived ~ pclass + sex + age + sibsp + parch,
             data=train,
             method="class", 
             control=rpart.control(minsplit=2, cp=0))

Prediction <- predict(fit, train, type = "class")
submit <- data.frame(RowId = train$row_id,
                     trueSurvived = train$survived,
                     predSurvived = Prediction)
nrow(train[submit$trueSurvived == submit$predSurvived,])/nrow(train)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(RowId = test$row_id,
                     trueSurvived = test$survived,
                     predSurvived = Prediction)
nrow(test[submit$trueSurvived == submit$predSurvived,]) / nrow(test)

Prediction <- predict(fit, secret_test, type = "class")
submit <- data.frame(RowId = secret_test$row_id,
                     trueSurvived = secret_test$survived,
                     predSurvived = Prediction)
nrow(secret_test[submit$trueSurvived == submit$predSurvived,]) / nrow(secret_test)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class", 
             control=rpart.control(minsplit=2, cp=0))





