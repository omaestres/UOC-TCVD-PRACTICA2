TITANIC <- 
  read.table("C:/Users/omaestre/Downloads/titanic_dataset.csv", 
  header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

NUEVO_DS<-TITANIC[,c(1,2,4,5)]

TITANIC$age[which(is.na(TITANIC$age))] <- mean(TITANIC$age,na.rm = TRUE)

library(dplyr)

NUEVO_DS <- mutate(NUEVO_DS, clase = forcats::fct_recode(as.factor(pclass), "PRIMERA" = "1", 
"SEGUNDA" = "2", "TERCERA" = "3"), sobreviviente = forcats::fct_recode(as.factor(survived), 
"NO" = "0", "SI" = "1"))

##NUEVO_DS <- mutate(NUEVO_DS, edad = ifelse(age>=18, "ADULTO", "MENOR"))
NUEVO_DS <- mutate(NUEVO_DS, edad = forcats::fct_recode(as.factor(ifelse(age>=18, "ADULTO", "MENOR"))))

with(NUEVO_DS, Barplot(sobreviviente, by=clase, style="divided", legend.pos="above", xlab="sobreviviente", 
  ylab="Frequency"))
with(NUEVO_DS, Barplot(sobreviviente, by=sex, style="divided", legend.pos="above", xlab="sobreviviente", 
  ylab="Frequency"))
with(NUEVO_DS, Barplot(sobreviviente, by=edad, style="divided", legend.pos="above", xlab="sobreviviente", 
  ylab="Frequency"))

library(C50)
 varX <- NUEVO_DS[,c(3,5,6)]
> y <- NUEVO_DS[,7]

modelo_arbol <- C50::C5.0(varX, y, rules= TRUE)
plot(modelo_arbol)

write.table(NUEVO_DS, "dataset_final.csv", sep=";")
