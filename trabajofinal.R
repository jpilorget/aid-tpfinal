#Limpio el ambiente, elijo el directorio y cargo las librer?as
rm(list = ls())
setwd("~/TP")
library(dplyr)
library(plyr)
library(ggplot2)
library(corrplot)
library(biotools)
library(mvnormtest)
library(PerformanceAnalytics)
library(cluster)
library(factoextra)
library(MASS)
library(car)
library(caret)
library(vegan)
library(moments)

##-----PREPROCESAMIENTO DEL DATASET DE RESULTADOS ELECTORALES-----##
##-----Informaci?n obtenida de http://www.datos.gob.ar------------##

#Cargo el dataset y hago subsets por partido
resultados <- read.csv("Resultados por municipio.csv")
resultados_1 <- filter(resultados, vot_parCodigo == 131)
resultados_2 <- filter(resultados, vot_parCodigo == 132)
resultados_3 <- filter(resultados, vot_parCodigo == 133)
resultados_4 <- filter(resultados, vot_parCodigo == 135)
resultados_5 <- filter(resultados, vot_parCodigo == 137)
resultados_6 <- filter(resultados, vot_parCodigo == 138)
resultados_1 <- resultados_1[,c(1,2,4,5)]
resultados_2 <- resultados_2[,c(1,2,4,5)]
resultados_3 <- resultados_3[,c(1,2,4,5)]
resultados_4 <- resultados_4[,c(1,2,4,5)]
resultados_5 <- resultados_5[,c(1,2,4,5)]
resultados_6 <- resultados_6[,c(1,2,4,5)]


#Modifico los nombres de las columnas de cada subset para que coincida con el partido
colnames(resultados_1)[4] <- "fpv"
colnames(resultados_2)[4] <- "gen"
colnames(resultados_3)[4] <- "acf"
colnames(resultados_4)[4] <- "pro"
colnames(resultados_5)[4] <- "fit"
colnames(resultados_6)[4] <- "una"

#Creo la tabla con los resultados por columna
resultados_total <- join_all(list(resultados_1,resultados_2,resultados_3,resultados_4,resultados_5,
      resultados_6),by = c("vot_proCodigoProvincia","vot_munCodigoMunicipio","munNombre" ))

#Escribo un csv con los resultados del preprocesamiento
write.csv(resultados_total, "2015_resultados_por_municipio.csv", row.names = F)

#Cargo el csv recientemente creado
resultados <- read.csv("2015_resultados_por_municipio.csv")



#Modifico el nombre de la columna Municipio
colnames(resultados)[3] <- "nombre"

#Creo la columna de votos v?lidos
resultados$validos <- rowSums(resultados[,4:9])

#Calculo los porcentajes de votos v?lidos para cada fuerza
resultados$fpv <- round(resultados$fpv / resultados$validos,4)
resultados$gen <- round(resultados$gen / resultados$validos,4)
resultados$acf <- round(resultados$acf / resultados$validos,4)
resultados$pro <- round(resultados$pro / resultados$validos,4)
resultados$fit <- round(resultados$fit / resultados$validos,4)
resultados$una <- round(resultados$una / resultados$validos, 4)

#Digo qu? partido fue el ganador en cada municipio
resultados$masvotos <- pmax(resultados$fpv, resultados$acf, resultados$una, resultados$pro)
resultados$ganador <- ifelse(resultados$masvotos == resultados$fpv, "fpv", 
                             ifelse(resultados$masvotos == resultados$pro, "pro",
                                    ifelse(resultados$masvotos == resultados$una, "una",
                                           ifelse(resultados$masvotos == resultados$acf, "acf",0))))

##-----PREPROCESAMIENTO DE LA INFORMACI?N CENSAL-----##
##-----Informaci?n obtenida del Censo 2010-----------##
##-----Variables para A.C.P.-------------------------##

#Cargo los datasets
canthogares <- read.csv("muni- canthogares.csv", sep = ";")
cantnbi <- read.csv("muni- cantnbi.csv", sep = ";")
incalserv_def <- read.csv("muni- incalserv_def.csv", sep = ";")
inmat_def <- read.csv("muni- inmat_def.csv", sep = ";")
jcondact <- read.csv("muni- jcondact.csv", sep = ";")
jnivins <- read.csv("muni- jnivins.csv", sep = ";")
totpers <- read.csv("muni- totpers.csv", sep = ";")

#Creo la tabla con los indicadores consolidados
indicadores_hogares <- join_all(list(canthogares,cantnbi,incalserv_def,inmat_def,jcondact,
                    jnivins, totpers), by = c("idprov","iddpto","nomdpto","muni", "nombre"))

#Escribo un csv con los resultados del preprocesamiento
write.csv(indicadores_hogares, "indicadores_hogares.csv", row.names = F)

#Cargo el csv recientemente creado
indicadores <- read.csv("indicadores_hogares.csv")

#Convierto las variables a porcentaje
indicadores$cantnbi <- round(indicadores$cantnbi / indicadores$canthogares,2)
indicadores$incalserv_def <- round(indicadores$incalserv_def / indicadores$canthogares,4)
indicadores$inmat_def <- round(indicadores$inmat_def / indicadores$canthogares,4)
indicadores$jcondact <- round(indicadores$jcondact / indicadores$canthogares,4)
indicadores$jnivins <- round(indicadores$jnivins / indicadores$canthogares,4)
indicadores$totpers <- round(indicadores$totpers,2)

#Mapeo los valores de provincia del Censo para asignarlos a los resultados de la elecci?n
ind_idprov <- data.frame(table(indicadores$idprov))[,1]
res_idprov <- data.frame(table(resultados$vot_proCodigoProvincia))[,1]
map = setNames(ind_idprov,res_idprov)
resultados$idprov <- map[resultados$vot_proCodigoProvincia] 
resultados$nombre <- trimws(resultados$nombre)
indicadores$nombre <- trimws(indicadores$nombre)

#Veo c?mo da el cruce de ambos datasets
base_apc <- merge(resultados, indicadores, by = c("idprov", "nombre"))
#Estandarizo las variables
base_std <- base_apc
base_std[,5:11] <- scale(base_std[,5:11])
base_std[,17:23] <- scale(base_std[,17:23])
base_std <- na.omit(base_std)

#Hago un subset de la base APC y la estandarizo
base_apc_subset <- na.omit(base_apc[,c(5,8,10,11,18:23)])
base_std_subset[,1:10] <- scale(base_apc_subset[,1:10])
colnames(base_std_subset) <- c("FpV", "Cambiemos", "UNA", "Electores","% NBI", "% INCALSERV", 
                               "% INMAT", "% Jefe ocupado", "% Jefe sec. comp.", "Cant. miembros")

#Hago dos gr?ficos de correlaci?n entre las variables (y los superpongo)
corrplot(cor(base_std_subset),type = "lower", method = "square", tl.col = "black", tl.cex = 0.8, 
         tl.pos = "lt", cl.pos = "n")
corrplot(cor(base_std_subset),type = "upper", method = "number", add = T,
         tl.cex = 0.8, tl.col = "black", tl.pos = "n", diag = F, cl.pos = "n", number.cex = 0.8)

# Hago un boxplot para cada grupo de variables
par(mar=c(4,4,2,2)) 
boxplot(base_apc_subset[,1:3], cex.axis = .7, col = c("blue", "gold", "dark green", 
  rep("grey",5)), main = "DistribuciĂłn de porcentaje de votos", las = 2, frame = F)

boxplot(base_apc_subset[,5:9], cex.axis = .7, col = c("gray"),
        main = "Variables sociodemogrĂĄficas", las = 2, frame = F)

boxplot(base_std_subset, cex.axis = .7, col = c("blue", "gold", "dark green", 
      rep("grey",7)), main = "Boxplots de las variables estandarizadas", las = 2)

#AnĂĄlisis de momentos
kurtosis(base_apc_subset$`% INCALSERV`)

## PRIMER MĂTODO: ANĂLISIS DE COMPONENTES PRINCIPALES
# Hago un an?lisis de componentes principales con las variables num?ricas
# Las variables son: Votos FpV, Cambiemos y UNA; NBI, INMAT deficiente, 
# Condici?n de actividad del jefe, Nivel de instrucci?n del jefe, Total de personas en el hogar
cp_base <- princomp(base_std_subset, cor = T)
summary(cp_base)

#Observo el grĂĄfico de sedimentaciĂłn y las cargas
plot(cp_base, type="l", main = "GrĂĄfico de sedimentaciĂłn")
View(cp_base$loadings[,1:3])
biplot(cp_base, col = c("dark grey", "black"), expand = .7, choices = c(1,3))

#Hago una base con scores
base_scores <- data.frame(base_std, cp_base$scores[,1:2])

## SEGUNDO MĂTODO: ANĂLISIS DISCRIMINANTE LINEAL
#Agrego el ganador a la base
base_lda <- na.omit(base_apc[,c(13,10,18:23)])
colnames(base_lda) <- c("Ganador", "Electores","% NBI", "% INCALSERV", 
                        "% INMAT", "% Jefe ocupado", "% Jefe sec. comp.", "Miembros del hogar")
base_lda <- dplyr::filter(base_lda, Ganador == "fpv" | Ganador == "pro")
base_lda[,2:8] <- scale(base_lda[,2:8])

# Hago los tests correspondientes
#Testeo normalidad con Shapiro multivariado
mshapiro.test(t(base_lda[,-1]))
#Testeo homocedasticidad con M de Box (sensible a falta de normalidad)
boxM(base_lda[,-1],base_lda[,1])
#Testeo homocedasticidad con Levene multivariado
permutest(betadisper(vegdist(base_lda[,-1]),base_lda[,1]), pairwise = T)
anova(betadisper(vegdist(base_lda[,-1]),base_lda[,1]))

#Creo los conjuntos de entrenamiento y validaciĂłn
set.seed(123)
trainrows <- sample(seq_len(nrow(base_lda)), size = floor(0.7 * nrow(base_lda)))
train <- base_lda[trainrows, ]
test <- base_lda[-trainrows, ]

#Construyo un discriminante cuadrĂĄtico (porque rechaza homocedasticidad)
#SĂłlo utilizo las variables del Censo
discriminante <- qda(train[,c(2:8)], train[,1])
prediccion <- predict(discriminante, test[,-1])
confusionMatrix(test$Ganador, prediccion$class)

## TERCER MĂTODO: CLUSTERING
# Hago un anĂĄlisis de clusters jerĂĄrquicos
distancia <- dist(base_std_subset[,4:10])
dendrograma <- (hclust(distancia, method = "average"))
plot(dendrograma)
rect.hclust(dendograma, 3)

#Hago un clustering no jerĂĄrquico
#Calculo la cantidad Ăłptima de clusters
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:5

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 10
data <- base_std_subset[,4:10]
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Cantidad de grupos (K)",
     ylab="Suma de cuadrados dentro de los grupos",
     main="SelecciĂłn de cantidad de grupos en K-Means")
#Calculo el K-Means
clustering_kmeans <- kmeans(base_std_subset[,4:10],centers=2, nstart = 10)

#Defino la paleta
palette(c("blue", "gold"))

#Hago un grĂĄfico segĂşn porcentaje por partido
plot(base_apc_subset$FpV,base_apc_subset$Cambiemos, col=clustering_kmeans$cluster, frame = F,
     main = "Agrupamiento no jerĂĄrquico", cex = .5, xlab = "% FpV", ylab = "% Cambiemos") 

#Hago un grĂĄfico segĂşn variabilidad explicada
clusplot(base_std_subset, clustering_kmeans$cluster, main = "Agrupamiento no jerĂĄrquico",
         diss = F, labels = 4, plotchar = T, shade = T, frame = F)
points(clustering_kmeans$centers, pch = 19, cex = 2, col = c("blue","gold"))
