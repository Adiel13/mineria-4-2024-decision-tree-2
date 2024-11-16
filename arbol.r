install.packages("arules")
library(arules)
install.packages("genero")
library(genero)

data <- read.csv('C:/Users/kevin/OneDrive/Documentos/data.csv', sep = ";", fileEncoding = "latin1")

data$cui <- format(data$cui, scientific =FALSE)

data$nota[data$nota == "SDE"] <- -1
data$final[data$final == "SDE"] <- -1
data$final[data$final == "NSP"] <- -1

data$nombre1 <- sapply(strsplit(data$nombre, " "), `[`, 1)
data$nombre2 <- sapply(strsplit(data$nombre, " "), `[`, 2)

genero("LUIS")
genero("EMILY")

data$genero <- genero(data$nombre1)

subset(data, is.na(data$genero))

data$genero <- ifelse(is.na(data$genero), genero(data$nombre2), data$genero)

data[77, "genero"] <- "male"
data[113, "genero"] <- "male"
data[119, "genero"] <- "female"
data[120, "genero"] <- "male"
data[179, "genero"] <- "female"
data[185, "genero"] <- "male"
data[202, "genero"] <- "male"
data[225, "genero"] <- "male"
data[250, "genero"] <- "male"
data[276, "genero"] <- "female"
data[363, "genero"] <- "female"
data[473, "genero"] <- "female"
data[487, "genero"] <- "male"
data[566, "genero"] <- "male"

data$genero <- ifelse(data$genero == "male", 1, 2)

data$anio_carne <- substr(data$carne, start=1, stop=4)

subset(data, anio_carne > 8000)

data$anio_carne <- ifelse(data$anio_carne > 8000, as.numeric(substr(data$anio_carne, 1,2))+1900, data$anio_carne)

data$edad <- as.integer(data$anio) - as.integer(data$anio_carne) +18

data$municipio <- substr(data$cui, nchar(data$cui) -1, nchar(data$cui))
data$departamento <- substr(data$cui, nchar(data$cui) -3, nchar(data$cui)-2)

data_apriori <- data[, c("lab", "zona", "final", "nota", "anio", "sem", "genero", "edad", "municipio", "departamento")]

data_apriori$gana <- ifelse(data_apriori$nota > 60, 1, 0)

install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

arbol <- rpart(gana ~ genero + edad + sem, data = data_apriori, method = "class")

arbol2 <- rpart(gana ~ lab + edad + municipio, data = data_apriori, method = "class")

arbol3 <- rpart(gana ~ sem + edad + zona, data = data_apriori, method = "class")

rpart.plot(arbol, type=4, extra=104, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción ganar o no", cex = 1)


estudiante <- data.frame(
  genero=c(2),
  edad=c(20),
  sem=c(1)
)

result2 <- predict(arbol,estudiante, type="class")
result2



estudiante <- data.frame(
  lab=c(61),
  edad=c(20),
  municipio=c("01")
)

result2 <- predict(arbol2,estudiante, type="class")
result2
rpart.plot(arbol2, type=4, extra=104, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción ganar o no", cex = 1)

estudiante <- data.frame(
  sem=c(1),
  edad=c(20),
  zona=c(55)
)

result2 <- predict(arbol3,estudiante, type="class")
result2

rpart.plot(arbol3, type=4, extra=104, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción ganar o no", cex = 1)
