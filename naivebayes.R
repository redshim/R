install.packages("caret")
install.packages("e1071")

library("caret")
require("e1071")

head(iris)
names(iris)
x = iris[,-5]
y = iris$Species


model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
model

predict(model$finalModel,x)
table(predict(model$finalModel,x)$class,y)
naive_iris <- NaiveBayes(iris$Species ~ ., data = iris)
plot(naive_iris)
