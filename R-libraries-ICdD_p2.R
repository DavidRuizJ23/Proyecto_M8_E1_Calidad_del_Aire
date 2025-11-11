library(baguette)
library(broom)
library(caret) 
library(cluster)
library(ClusterR)
library(clusterSim)
library(corrplot) #Visualizaciones de la matriz de correlación
library(data.table)
library(discrim)
library(dplyr) #Operaciones entre DataFrames
library(embed)
library(factoextra)
library(FactoMineR)
library(FactoMineR) 
library(forcats)
library(funModeling)
library(GGally)
library(gganimate)
library(ggExtra) #visualizaciones
library(ggforce)
library(ggplot2) #visualizaciones
library(ggplot2movies)
library(grid)
library(gridExtra)
library(hexbin) # Auxiliar para visualización
library(Hmisc)
library(janitor) #Para embellecer o limpiar los DF
library(kableExtra)
library(knitr)
library(lattice)
library(magrittr) #Operadores de Asignación
library(mlr)
library(modeldata)
library(moments) #Calcular la Kurtosis y la Skewness
library(NbClust)
library(palmerpenguins) #Data Set de los pingüinos
library(plotly)
library(polycor)
library(psych)
library(readr)
library(recipes)
library(rgl)
library('rstatix')
library(Rtsne)
library(scales)
library(scattermore)
library(skimr) #Para generar métricas
library(SmartEDA)
library(tidyclust)
library(tidymodels)
library(tidytext)
library(tidyverse)
library(tsne)
library(uwot)
library(vegan)
library(viridis)
library(visdat) #Visualizador de los datos Faltantes
require(igraph)

# Librerías
library(dbscan)
library(caret)
library(uwot)
library(ggplot2)
library(dplyr)
library(ggforce)  # para geom_mark_ellipse

library(tidyverse)
library(tidymodels)
library(cluster)      
library(factoextra)
library(patchwork)  


#Librerias HDBSCAN

library(dbscan)    # HDBSCAN
library(caret)     # dummyVars (one-hot encoding)
library(uwot)      # UMAP para visualización
library(ggplot2)   # gráficos
library(dplyr)     # manipulación
library(ggforce)   # elipses por cluster
library(cluster)   # silhouette
suppressWarnings(suppressMessages({
  library(mclust)  # Adjusted Rand Index (externa, opcional)
  if (!requireNamespace("aricode", quietly = TRUE)) {
    message("Nota: 'aricode' no está instalado; NMI se omitirá a menos que lo instales.")
  }
}))
#install.packages("aricode", dependencies = TRUE)
library(aricode)




#library(ggord)
#library(autoplot)
theme_set(theme_bw(16))

library(tidymodels)
library(tidyverse)
library(tidyclust)
library(factoextra)
library(FactoMineR)
library(cluster)
library(mlr)
library(GGally)
library(ClusterR)
library(vegan)
library(NbClust)
library(gridExtra)
library(grid)
library(lattice)
require(igraph)
library(umap)

#df %>% glimpse()
