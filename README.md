# aid-tpfinal
Trabajo final de la materia Análisis Inteligente de Datos (Maestría en DM - FCEN - UBA).

##Descripción general
Para el trabajo final se utilizaron dos fuentes de información:
* Resultados de las elecciones generales a presidente de Argentina de octubre de 2015.
* Resultados del Censo Nacional de Población, Hogares y Viviendas de 2010.

Ambas fuentes de datos se analizaron a nivel de municipio. Una vez creado el dataset se aplicaron técnicas de minería de datos.

##Dataset
Los datos fueron obtenidos del repositorio no oficial dat.ar (http://datar.noip.me/) y se los vinculó a partir del ID de municipio. Para obtener la información censal a nivel de municipio se trabajó con la información a nivel de microdato en una base PostgreSQL. Luego, se utilizó R para vincular ambos conjuntos de datos y realizar el análisis posterior.

##Análisis descriptivo
Se realizaron gráficos de dispersión, histogramas y correlogramas para el análisis exploratorio de los datos.

##Técnicas aplicadas
Para el trabajo final de la materia se aplicaron técnicas estudiadas durante la cursada, principalmente tres:
* Análisis de Componentes Principales (PCA, por su sigla en inglés).
* Análisis Discriminante Cuadrático (no se realiza lineal por no cumplir el supuesto de homocedasticidad).
* Clustering no jerárquico.

##Resultados
Se observa una asociación entre las variables sociodemográficas analizadas y los porcentajes de voto de las principales fuerzas, fenómeno que puede apreciarse tanto en las cargas de los componentes principales como en la capacidad predictiva del análisis discriminante. No es tan claro el fenómeno en el clustering no jerárquico, siendo este un punto a mejorar.
