
install.packages("corrplot")
library(sf)
library(ggplot2)
library(corrplot)


# Calcular a correlação ---

correlacao <- cor(shape_final$proporc, shape_final$Ttl_d_F)

cat("Coeficiente de correlação Pearson:", round(correlacao, 4), "\n\n")
