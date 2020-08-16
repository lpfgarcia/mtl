# codigo R
# ERAMIA-SP 2020 - Introducao a meta-aprendizado
# Ana C. Lorena e Luis P. F. Garcia

install.packages("OpenML")
install.packages("farff")
install.packages("foreign")
install.packages("dplyr")

require("OpenML")
require("foreign")
require("dplyr")

df = listOMLDataSets()

ids = which(
  #data.id
  #df$name
  #df$version
  #df$format
  #df$tags
  df$status == "active" &
  df$number.of.instances <= 1000 &
  df$number.of.features <= 100 &
  df$number.of.missing.values == 0 &
  df$number.of.classes >= 2 &
  df$number.of.classes <= 10 &
  df$number.of.missing.values == 0
  #df$majority.class.size
  #df$max.nominal.att.distinct.values
  #df$minority.class.size
  #df$number.of.numeric.features
  #df$number.of.instances.with.missing.values
  #df$number.of.symbolic.features
)

bases = df[ids,]

generate <- function(i) {

  id = bases[i, "data.id"]
  dataset = OpenML::getOMLDataSet(data.id = id)
  name = bases[i, "name"]

  aux = dataset$data

  # sugestão: evitando problemas com o nome dos atributos
  colnames(aux) = make.names(colnames(aux))

  # sugestão: evitando problemas com a classe
  if(dataset$target.feature != "class") {
    aux$class = dataset$data[,dataset$target.features]
    aux[,dataset$target.features] = NULL
  }

  # remover atributos pre-catalogados como inuteis
  if(!is.na(dataset$desc$ignore.attribute)) {
    aux[,dataset$desc$ignore.attribute] = NULL
  }

  # sugestao - evitando problemas de formatação
  aux$class = as.factor(as.numeric(aux$class))

  aux = aux %>% select(-class,class)

  #verificacoes adicionais
  #  se o numero de linhas eh diferente de zero
  #  se o numero de colunas eh diferente de zero
  #  numero de amostras por classe maior que x
  #  se algum atributo "pode ser ID"

  # sugestão: evitando problema na inducao dos modelos
  if(min(summary(aux$class)) > 10) {
    foreign::write.arff(x = aux, file = paste0("dataset/", id, "_", name, ".arff"))
  }
}

for(i in 1:nrow(bases)) {
  try(generate(i), silent=TRUE)
}

