toBeInstalled<-c("mice"
                ,"VIM"
                ,"DMwR"
                ,"kernlab"
                ,"e1071"
                ,"randomForest"
                ,"pROC"
                ,"gbm"
                ,"Amelia" 
                ,"leaps"
                ,"lda"
                ,"mvoutlier")
install.packages(toBeInstalled)
library(mvoutlier)
library(foreign)
library(lda)
library(leaps)
library(MASS)
library(dplyr)
library(ggplot2)
library(Amelia)
library(reshape2)
library(corrplot)
library(caret)
library(gbm)
library(pROC)
library(randomForest)
library(e1071)
library(kernlab)
library(lattice)
library(DMwR)
library(VIM)
library(mice)
library(readxl)
library(mvoutlier)

############################################
#<---$$$$$    Carga de archivo    $$$$$--->#
############################################

#<><><><><><><><>  Carga alternativa desde Internet<><><><><><><><><><><><><><>#
# Carga de archivo desde el enlace de internet
# 1. Se define las URL exacta donde se encuentra el archivo, 
# por favor revisar la URL, porque podria traer basura
url<-"http://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data"

# 2. En el simbolo ?, se especifica como el usuario debe entrar los valores 
# que esten definidos como missing values
# 3. En header, se define si el data set tiene cabecera o no.
Cancer<-read.csv(url,header = F,na.strings = "?")

#<><><><><><><><> Carga alternativa desde computador><><><><><><><><><><><><><>#
## Carga de archivo desde el computador - Codigo alternativo opcional
## 1. Se define el directorio de carga del archivo
# dir <- "c:...."
## 2. Se pone el directorio donde se encuentra el archivo con el cual se trabaja
## como un valor default. 
# setwd(dir)
## Se carga el archivo teniendo en cuenta el header y los missing values
# Cancer<-read.csv(file,header = F,na.strings = "?")

#<><><><><><><><> Asigno nombres al data set        ><><><><><><><><><><><><><>#
# Si el dataset no tiene nombres se le ponen de la siguiente forma: 
# Cabe anotar que el vector de nombres debe ser del mismo tamano que el numero 
# columnas.
Cancer<-Cancer[-1]
names(Cancer)<-c("Ri","Na","Mg","Al","Si","K","Ca","Ba","Fe","Type")

#<><><><><><><><>     Exploracion del data set      ><><><><><><><><><><><><><>#
# Si se desea explorar el archivo se observa la cabecera, y la estructura. 
head(Cancer)
str(Cancer)


##<><><><><><><><>     Cambio de tipos de variable   <><><><><><><><><><><><><>#
## Si existe la necesidad de cambiar un tipo de variable, digamos de categorica
## a numerica, este seria la parte ideal para hacerlo.
# El siguiente comando no soporta missing values. 
# Cancer$nombreVariable <- as.numeric(Cancer$nombreVariable)
Cancer$Type<-as.factor(Cancer$Type)

##<><><><><><><><>     Explorar variables numericas  <><><><><><><><><><><><><>#
## Si se desean explorar variables numericas de un data.frame se podrian 
## las siguientes graficas para observar la distribucion. 
## Graficas de histogramas
## 1. se genera un vetor con las columnas que tienen valores numericos en 
## nuestro data.frame. Estos valores se guardan en cols. <OJO> las variables 
## deben ser numericas. 
## !!!!! DESDE AQUI SE DEFINEN LAS COLUMNAS NUMERICAS !!!!!
cols<-c(1:9)
## 2. Luego se genera un data.frame temporal d, donde se guardan los valores
## que van a ser mostrados.
d <- melt(Cancer[,cols])
## 3. La columna variable de este dataframe d debe ser convertida a factor. 
d$variable<-as.factor(d$variable)
## 4. Se imprime el histograma. 
ggplot(d,aes(x = value,fill=variable)) + 
  facet_wrap(~variable,scales = "free_x") +
  geom_histogram(colour ="black",bins=9)
## 5. Se imprime el boxplot. 
ggplot(d,aes(variable,value)) + 
  facet_wrap(~variable,scales = "free") +
  scale_x_discrete(labels=c("","","","","","","","","",""))+
  geom_boxplot(aes(fill=variable))
## 6.Se imprime las correlaciones que puedan existir con las variables numericas
## Recuerde que el metodo puede ser circle, number, square, con este cambia la 
## grafica
cor <- cor(na.omit(Cancer[,cols]))
corrplot(cor,method="circle")

##<><><><><><><><>    Explorar variables categoricas <><><><><><><><><><><><><>#
# Este codigo esta para explorar la variable categorica llamada "Diagnosis"
# 1. por favor reemplazar Diagnosis si se quiere una variable diferente. 
dataCancerVis<-NULL

dataCancerVis<-data.frame(table(Cancer$Type),Type="Datos Crudos")
#Generacion de respuesta
Cancer$Type<-ifelse(Cancer$Type==7,1,0)
table(Cancer$Type)

dataCancerVis<-data.frame(table(Cancer$Type),Type="Nuevos datos")
#dataCancerVis<-rbind(dataCancerVis,data.frame(table(trainCancer$Diagnosis),Type="Datos de Entrenamiento"))
#dataCancerVis<-rbind(dataCancerVis,data.frame(table(testCancer$Diagnosis),Type="Datos de Test"))
# 2. En el data frame, dataCancerVis, esta el data frame con la tabla sumarizada
# de valores categoricos.
nombreVariableCategorica<-"Tipo.de.Glass"
conteoDeCasosPorCategoria<-"Numero.de.Casos"
tipoDeDatos<-"Tipo.Datos"# Pueden ser testing, training y crudos
names(dataCancerVis)<-c(nombreVariableCategorica,
                        conteoDeCasosPorCategoria,
                        tipoDeDatos)
# 3. Se genera el codigo de Barra. 
q<-ggplot(dataCancerVis,aes_string(tipoDeDatos,y=conteoDeCasosPorCategoria,fill=nombreVariableCategorica))+geom_bar(stat="identity",position = "dodge")
q<-q+xlab(tipoDeDatos) + ylab(conteoDeCasosPorCategoria) +  theme_bw()
q



##<><><><><><><><>     Explorar Outlier values       <><><><><><><><><><><><><>#
# Todo lo que se trabaje en estas librerias deben estar en un data.frame
##<><><><><><>Outliers a traves de metodos multivariados<><><><><><<><><><><><>#
# 1. Se grafica el chisq el cual es una funcion interactiva, donde con un click
# se debe eliminar los valores outliers que se encuentren alejados del set de 
# datos. 
chisq.plot(Cancer[,cols])
## 2. El uniplot es para ver la influencia de las variables sobre los outliers. 
## IMPORTANTE ESTA FUNCION maneja MAXIMO 10 columnas. Si son mas de 10 hacerlo
## en 2 grupos. 
uni.plot(Cancer[,cols])
##<><><><><><>Outliers a traves de metodos de densidades<><><><><><<><><><><><>#
## Este es el metodo por densidades.
##1. Se cargan los puntajes de los outliers, con base en un k-vecinos igual a 8
## Entre mayor el numero de k vecinos, eliminara mas outliers del set de datos. 
## los puntajes de los outliers se guardan en la variable outlier.scores
outlier.scores <- lofactor(Cancer[,cols], k=8)
## 2. Se verifica cuantos outliers se identificaron
sum(is.na(outlier.scores))
## 3. Luego se grafica la densidad de los diferentes. Verifique que tenga valores 
##de outliers para imprimir los valores
# plot(density(outlier.scores))
## 4. Se organizan los outliers de forma decreciente, en este caso se toman los 
## primerols outliers, si quiere los n, cambie [1:n]
# outliers <- order(outlier.scores, decreasing=T)[1:5]
## 5. Se imprime el numero de fila donde fue detectado el outlier.  
# print(outliers)
## 6. Se detectan los outliers y se guardan en un data.frame
# outlier.df<-row.names(red3)[outliers]


##<><><><><><><><>     Explorar missing values       <><><><><><><><><><><><><>#
# 1. Verificar cantidad de missing values con la funcion aggr. 
aggr(Cancer,prop=F,numbers=T,cex.lab = 1.2,cex.axis=0.9,gap=2,sortVars = TRUE)
# 2. Si desea revisar antes de la imputacion algun par de variables es 
# recomendable, que revise el par de variables que desee de la siguiente forma
# OJO en colPair, se agregan los pares de columnas que se agregaran para visualizar
# OJO maximo 2 columnas. 
colPair<-c(1,1)
marginplot(Cancer[,colPair],col=1:3,delimiter="imp", alpha=0.9)
# 3. Se deben imputar los missing values. las variables importantes
# m es la cantidad de veces que se imputan los valores, 
# defaultMethod  son los metodos usados para realizar la imputacion
CancerImputed <- mice(Cancer,seed=9293, m=5,
                defaultMethod = c("pmm","logreg", "polyreg", "polr") )
# 4. Se obtiene el valor completo obtenido del proceso de imputacion
CancerFilled <- complete(CancerImputed)
# 5. Se confirma visualmente que el data.frame ya no tiene missing
# values. 
aggr(filled,prop=F,numbers=T,cex.lab = 1.2,cex.axis=0.9,gap=2,sortVars = TRUE)
# 6. Se hace una regresion lineal con los valores imputados. En la variable 
# En la variable fit se almacenaran los diferentes resultados de la regresion
fit    <- with(imputed,lm( thalach~age+sex+cp+trestbps+chol+fbs+restecg+exang+oldpeak+slope))
# 7. Se observa los resultados de la regresion, se obtendra el mejor 
# valor donde se observe el R^2 con menor valor. 
summary(fit)
# 8. Un resumen de los parametros obtenidos. 
pooled <-pool(fit)


# 1 - Carga de datos general, pasar la url, o la diraccion del PC donde esta el archivo
# 2 - Verificacion de tipos de columnas (numericas, y categoricas), data frame con solo numericas, y solo categoricas.
# 3 - Graficar datos numericos ( Distribuciones )
# 4 - Graficar datos categoricos (Bar charts, Heat maps )
# 5 - Tecnicas de Reduccion  (Correlaciones para los datos numericos )
# 6 - Tecnicas de Reduccion Categoricas (Goodman Kruskal )
# 7 - Analisis de NA e imputaciones 
# 8 - Analisis de outliers
