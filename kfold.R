# codigo R
# ERAMIA-SP 2020 - Introducao a meta-aprendizado
# Ana C. Lorena e Luis P. F. Garcia

if(!require("caret")) {
  install.packages("caret")
}

require("caret")

kfold <- function(data, folds=10) {

  id = createFolds(data$class, k=folds, list=FALSE)

  tran = lapply(1:folds, function(i) {
    subset(data, id %in% setdiff(1:folds, i))
  })

  test = lapply(1:folds, function(i) {
    subset(data, id %in% i)
  })

  tmp = list()
  tmp$data = data
  tmp$tran = tran
  tmp$test = test
  return(tmp)
}