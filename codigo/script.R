# Cargar el dataset separar campos por comas
TITANIC <- 
  read.table("C:/Users/omaestre/Downloads/titanic_dataset.csv", 
  header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

# Ver atributos y dimensiones
names(TITANIC)
dim(TITANIC)

# Resumen estadistico
summary(TITANIC)

# Reemplazar valores NA de edad con media de edades
TITANIC$age[which(is.na(TITANIC$age))] <- mean(TITANIC$age,na.rm = TRUE)
summary(TITANIC$age)

# Convertir a int las edades
TITANIC$age <- as.integer(TITANIC$age)

# Histograma age
with(NUEVO_DS, Hist(age, scale="frequency", breaks="Sturges", col="darkgray"))

# Diagrama de caja
Boxplot( ~ age, data=NUEVO_DS, id.method="identify")

# Seleccion de campos a trabajar
NUEVO_DS<-TITANIC[,c(1,2,4,5)]

# Normalizar campos pclass, age y survived
library(dplyr)
NUEVO_DS <- mutate(NUEVO_DS, clase = forcats::fct_recode(as.factor(pclass), "PRIMERA" = "1", 
"SEGUNDA" = "2", "TERCERA" = "3"), sobreviviente = forcats::fct_recode(as.factor(survived), 
"NO" = "0", "SI" = "1"))
NUEVO_DS <- mutate(NUEVO_DS, edad = forcats::fct_recode(as.factor(ifelse(age>=18, "ADULTO", "MENOR"))))

# Varianza y desviacion estandar
var(NUEVO_DS$age)
sd(NUEVO_DS$age)

# Graficas de barras por grupos
with(NUEVO_DS, Barplot(sobreviviente, by=clase, style="divided", legend.pos="above", xlab="sobreviviente", 
  ylab="Frequency"))
with(NUEVO_DS, Barplot(sobreviviente, by=sex, style="divided", legend.pos="above", xlab="sobreviviente", 
  ylab="Frequency"))
with(NUEVO_DS, Barplot(sobreviviente, by=edad, style="divided", legend.pos="above", xlab="sobreviviente", 
  ylab="Frequency"))

# Arbol de desicion
library(C50)
varX <- NUEVO_DS[,c(3,5,6)] 
y <- NUEVO_DS[,7]
modelo_arbol <- C50::C5.0(varX, y, rules= TRUE)
plot(modelo_arbol)

# Grafica de barras por 3 variables
# edad y clase
ggplot(NUEVO_DS) +
   geom_bar(aes(edad, fill = sobreviviente), position = "fill") + 
   facet_wrap(~ clase) + 
   labs(y = "Viven/Mueren", title = "Sobrevivientes por edad y clase")

# Sexo y clase
ggplot(NUEVO_DS) +
    geom_bar(aes(sex, fill = sobreviviente), position = "fill") + 
    facet_wrap(~ clase) + 
    labs(y = "Viven/Mueren", title = "Sobrevivientes por sexo y clase")

# Exportar el dataset con los datos finales
write.table(NUEVO_DS, "dataset_final.csv", sep=";")




