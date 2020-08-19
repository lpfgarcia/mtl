if(!require("rpart")) {
  install.packages("rpart")
}

if(!require("RWeka")) {
  install.packages("RWeka")
}

if(!require("kknn")) {
  install.packages("kknn")
}

if(!require("e1071")) {
  install.packages("e1071")
}

CART <- function(tran, test) {
  model = rpart(class ~., tran, method="class")
  pred = predict(model, test[,-ncol(test)], type="class")
  return(pred)
}

kNN <- function(tran, test) {
  pred = kknn(class ~., tran, test[,-ncol(test)], k=3)$fitted.values
  names(pred) = rownames(test)
  return(pred)
}

SVM <- function(tran, test) {
  model = svm(class ~., tran, kernel="radial")
  pred = predict(model, test[,-ncol(test)])
  return(pred)
}

acuracia <- function(test, pred) {
  tab = table(test$class, pred)
  sum(diag(tab))/sum(tab)
}

classificadores <- function(tran, test) {
  sapply(c("CART", "kNN"), function(clas) {
    pred = do.call(clas, list(tran, test))
    acuracia(test, pred)
  })
}

desempenho <- function(data) {

  df = kfold(data, 10)
  acuracia = mapply(function(tran, test) {
    classificadores(tran, test)
  }, tran=df$tran, test=df$test)

  return(acuracia)
}