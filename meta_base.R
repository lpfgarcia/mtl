# codigo R
# ERAMIA-SP 2020 - Introducao a meta-aprendizado
# Ana C. Lorena e Luis P. F. Garcia

require("foreign")
require("mfe")

source("classificadores.R")

files = list.files(path="dataset", full.names=TRUE)

amostras = lapply(files[1:100], function(file) {

  print(file)
  data = read.arff(file)
  medidas = metafeatures(class ~ ., data)

  acuracia = desempenho(data)
  algoritmo = as.numeric(which.max(rowMeans(acuracia)))
  c(medidas, class=algoritmo)
})

metabase = do.call("rbind", amotras)
metabase$class = as.factor(metabase$class)


