
##Tarea 8
##COA-501 Herramientas de cómputo para investigadores (R y Python)
## Alumno: Ivan Fermin Quiroz Ibañez


#1. Realice un análisis de componentes principales ACP con el conjunto de datos Boston.csv adjunto
#y el script PCA proporcionado. Escale el conjunto de características, calcule y grafique la matriz
#de covarianzas, la contribución de cada componente principal (PC) para explicar la varianza de
#los datos, grafique PC1 vs PC2, y PC1 vs PC3. Comente si es conveniente efectuar una reducción
#de dimensionalidad del conjunto de datos Boston.

boston <- read.csv("Boston_dataset.csv")
CP <- princomp(scale(boston[,-1]))
summary(CP)
plot(CP$scores[,1],CP$scores[,2])
biplot(CP)


#2. Realice una regresión con componentes principales con el conjunto de datos Boston.csv. Con
#base en el script proporcionado. Utilice como variable dependiente la mediana del precio de
#vivienda. Elabore una gráfica con los residuales del modelo (eje Y) versus los valores predichos
#(eje X). y una gráfica valores predichos versus valores observados. Reporte el resumen estadístico
#del modelo. Comente sus resultados.


require(pls)
set.seed (1000)

boston <- read.csv("Boston_dataset.csv")

?pcr

pcr_model <- pcr(medv~., data = boston, scale = TRUE, validation = "CV")

summary(pcr_model)

# Plot the root mean squared error
validationplot(pcr_model)

# Plot the cross validation MSE
validationplot(pcr_model, val.type="MSEP")


# Plot the R2

validationplot(pcr_model, val.type = "R2")


# plot the predicted vs measured values using predplot function 

predplot(pcr_model)

# regression coefficients can be plotted using the coefplot function

coefplot(pcr_model)


#3. Busque conjuntos de datos (en internet o cualquier libro de diseño de experimentos) para aplicar
#la prueba de Wilcoxon y modifique el script visto en clase

#a) Comparar dos muestras de datos apareados

#Se desea conocer el efecto de suministrar Zn en la dieta de cerdos en la
#disminución de morfoanomalías en la célula espermática de terdos adultos, para ello el investigador 
#toma una muestra de semen de 15 cerdos y examina una cantidad constante de células espermáticas y 
#cuenta el número de morfoanomalías de cada muestra, posteriormente, proporciona un mineral a los cerdos y 
#vuelve a obtener las muestras y contar el número de morfo-anomalías.
#Libro: BIOESTADÍSTICA EN CIENCIAS VETERINARIAS: Procedimientos de Análisis de datos con SAS. Herrera, 2014.

Verracos <-  c(1, 2, 3, 4, 5,6, 7, 8, 9, 10, 11, 2, 13, 14, 15)
Antes <-  c(18, 9, 5, 19, 13,6, 10, 5, 22, 8, 16, 10, 9, 14, 17)
Despues <- c(8,10, 5, 11,15, 2, 11,5,17, 5,9, 13, 5, 6,8)

my_data <- data.frame(
  group = rep(c("Antes", "Despues"), each = 15),
  respuesta = c(Antes,  Despues)
)
my_data

base_verracos <- cbind(Antes,Despues) 
base_verracos
row.names(base_verracos) <- Verracos

res <- wilcox.test(respuesta~group, data = my_data,
                   exact = FALSE,paired=TRUE)
res

library("ggpubr")
ggboxplot(my_data, x = "group", y = "respuesta", 
color = "group", palette = c("#00AFBB", "#E7B800"),
ylab = "respuesta", xlab = "Groups")+stat_summary(fun.y="mean", col=c("#00AFBB", "#E7B800"))


#Conclusión hay diferencias significativas en los dos grupos

#b) Comparar dos muestras de datos independientes
#Comparación de valores de testosterona entre varones nunca fumadores y varones que fuman más de 30 cigarros al día
#https://rpubs.com/luisrmacias/wilcoxon
NF <- c(0.44,0.44,0.43,0.56,0.85,0.68,0.96,0.72,0.92,0.87)
MF <- c(0.45,0.25,0.40,0.27,0.34,0.62,0.47,0.30,0.35,0.54)

#Varibles sin normalidad
hist(NF)
hist(MF)
shapiro.test(NF)
shapiro.test(MF)

my_data2 <- data.frame(
  group = rep(c("NF", "MF"), each = 10),
  respuesta = c(NF,  MF)
)
my_data2

library("ggpubr")
ggboxplot(my_data2, x = "group", y = "respuesta", 
  color = "group", palette = c("#00AFBB", "#E7B800"),
  ylab = "respuesta", xlab = "Groups")+
  stat_summary(fun.y="mean", col=c("#00AFBB", "#E7B800"))


res2 <- wilcox.test(respuesta~group, data = my_data2,
                   exact = FALSE, paired=F)
res2

#Conclusión hay diferencias significativas en los dos grupos


#c) Comparar los tratamientos de un diseño completamente al azar.

#Datos procedentes de un diseño completamente al azar, en el cual se evaluaron cuatro tratamientos 
#(dosis de fertilizante) sobre la altura, a los 20 días de una especie vegetal (cm).
#Fuente: Curso DEA, 2017. COLPOS.

a <- c(10,12,11,12)
b <- c(15,18,17,18)
c <- c(25,30,28,27)
d <- c(28,27,27,29)


base3 <- data.frame(cbind(a,b,c,d))


my_data3 <- data.frame(
  fertilizante = rep(c("a", "b","c","d"), each = 4),
  respuesta = c(a,b,c,d)
)
my_data3

library(agricolae)

#ANOVA
deca <- aov(respuesta~fertilizante, data = my_data3)
summary(deca)

#Homocedasticidad
bartlett.test(my_data3$respuesta~my_data3$fertilizante)

#Normalidad del modelo
shapiro.test(deca$residuals)
hist(deca$residuals)

#Prueba de Tukey para medias
agricolae::HSD.test(deca,"fertilizante", group=TRUE,console=TRUE)


library("ggpubr")
ggboxplot(my_data3, x = "fertilizante", y = "respuesta", 
          color = "fertilizante", palette = c("#00AFBB", "#E7B800","#DF0101","#088A08"),
          ylab = "respuesta", xlab = "Groups")+
  stat_summary(fun.y="mean", col=c("#00AFBB", "#E7B800","#DF0101","#088A08"))


#d) Reporte igualmente las gráficas de boxplot a colores de los incisos a, b y c.



save.image("T8COA501IFQI.RData")
