
##Tarea 8
##COA-501 Herramientas de cómputo para investigadores (R y Python)
## Alumno: Ivan Fermin Quiroz Ibañez


#1. Realice un análisis de componentes principales ACP con el conjunto de datos Boston.csv adjunto
#y el script PCA proporcionado. Escale el conjunto de características, calcule y grafique la matriz
#de covarianzas, la contribución de cada componente principal (PC) para explicar la varianza de
#los datos, grafique PC1 vs PC2, y PC1 vs PC3. Comente si es conveniente efectuar una reducción
#de dimensionalidad del conjunto de datos Boston.

#base boston
boston <- read.csv("Boston_dataset.csv")

#matriz de covarianzas
mcv <- cov(boston[,-1])
pairs(mcv)

mcr <- cor(boston[,-1])
pairs(mcr)
pairs(boston[,-1])


CP <- princomp(scale(boston[,-1]), cor = F) #cor=T, se usa la matriz de correlaciones o viceversa, o se escala o se usa matriz de correlaciones
summary(CP)
plot(CP)
biplot(CP)

#Conclusiones: de acuardo con el ACP es evidente que hay variables redundantes que pueden ser eliminadas del
#de la base de datos, por lo tanto el análisis es útil en este caso por que existen correlaciones entre las variables.


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


# regression coefficients can be plotted using the coefplot function

coefplot(pcr_model)


#residuales vs predichos

plot(pcr_model$residuals,pcr_model$fitted.values)


#Predichos vs observados

# plot the predicted vs measured values using predplot function 

predplot(pcr_model)

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

#verificar normalidad
shapiro.test(Antes)
hist(Antes)
library(car)
car::qqPlot(Antes)

shapiro.test(Despues)
hist(Despues)
library(car)
car::qqPlot(Despues)


my_data <- data.frame(
  Grupos= rep(c("Antes", "Despues"), each = 15),
  Respuesta = c(Antes,  Despues)
)
my_data

base_verracos <- cbind(Antes,Despues) 
base_verracos
row.names(base_verracos) <- Verracos

res <- wilcox.test(Respuesta~Grupos, data = my_data,
                   exact = FALSE,paired=TRUE)
res

#Boxplot
library("ggpubr")
box_p1 <- ggboxplot(my_data, x = "Grupos", y = "Respuesta", 
color = "Grupos", palette = c("#00AFBB", "#E7B800"),
ylab = "morfo anomalías espermáticas", xlab = "alimentación")+
  stat_summary(fun="mean", col=c("#00AFBB", "#E7B800"))+
theme_gray()+rremove("legend")
box_p1

#Conclusión hay diferencias estadísticamente significativas en los dos grupos

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
library(car)
car::qqPlot(NF)
car::qqPlot(MF)

my_data2 <- data.frame(
  Grupos = rep(c("NF", "MF"), each = 10),
  Respuesta = c(NF,  MF)
)
my_data2

library("ggpubr")
box_p2 <- ggboxplot(my_data2, x = "Grupos", y = "Respuesta", 
  color = "Grupos", palette = c("#00AFBB", "#E7B800"),
  ylab = "niveles de testosterona", xlab = "categorías de fumadores")+
  stat_summary(fun="mean", col=c("#00AFBB", "#E7B800"))+
  theme_gray()+rremove("legend")
box_p2

res2 <- wilcox.test(Respuesta~Grupos, data = my_data2,
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
#d <- agricolae::HSD.test(deca,"fertilizante", group=TRUE,console=TRUE)
d <- TukeyHSD(deca)

#Para Boxplot con letras

library(ggplot2)
library(multcompView)
library(dplyr)

cld <- multcompLetters4(deca, d)


# tabla con factores y 3er cuantil
r <- my_data3 %>% group_by(fertilizante) %>%
  summarise(mean=mean(respuesta), quant = quantile(respuesta, probs = 0.75)) %>%
  arrange(desc(mean))

# extraccion de letras añadiendo a la tabla r del paso anterior
cld <- as.data.frame.list(cld$fertilizante)
r$cld <- cld$Letters

#Boxplot con letras
box_p3 <- ggplot(my_data3, aes(fertilizante, respuesta )) + 
  geom_boxplot(show.legend = F) +
  labs(x="fertilizante", y="respuesta (alturan en cm)") +
  theme_bw() + 
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_gray()+
  geom_text(data = r, aes(x = fertilizante, y = quant, label = cld), size = 6, vjust=-1, hjust =-1)+
  stat_summary(fun="mean")
box_p3

#cajas con diferente color
library("ggpubr")
box_p4 <- ggboxplot(my_data3, x = "fertilizante", y = "respuesta", 
          color = "fertilizante", palette = c("#00AFBB", "#E7B800","#DF0101","#088A08"),
          ylab = "respuesta (altura en cm)", xlab = "fertilizante")+
  stat_summary(fun="mean", col=c("#00AFBB", "#E7B800","#DF0101","#088A08"))+
  geom_text(data = r, aes(x = fertilizante, y = quant, label = cld), size = 6, vjust=-1, hjust =-1)+
  theme_gray()+rremove("legend")
box_p4



#d) Reporte igualmente las gráficas de boxplot a colores de los incisos a, b y c.

#Ejercicio a
box_p1
#Ejercicio b
box_p2
#Ejercicio c
box_p3
box_p4

#guardar espacio de trabajo
save.image("T8COA501IFQI.RData")
