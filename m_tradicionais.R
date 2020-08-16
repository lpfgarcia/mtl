# codigo R
# ERAMIA-SP 2020 - Introducao a meta-aprendizado
# Ana C. Lorena e Luis P. F. Garcia

if(!require("foreign")) {
  install.packages("foreign")
}

require("foreign")

if(!require("devtools")) {
  install.packages("devtools")
}

devtools::install_github("rivolli/mfe")
require("mfe")

files = list.files(path="dataset", full.names=TRUE)

amostras = lapply(files, function(file) {

  print(file)
  data = read.arff(file)
  medidas = metafeatures(class ~ ., data)
  print(medidas)
  return(medidas)
})

base = do.call("rbind", amostras)

# paralelo?
