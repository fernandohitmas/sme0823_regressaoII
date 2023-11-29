library(tidyverse)
library(ggplot2)

# Leitura de Dados ----
dt <- fread("./data/alb_homes.csv")

# Verifica colunas que possuem NA
# "yearbuilt": nao tem necessidade de se manter ja que a variavel de idade do imovel indica a mesma coisa
# "halfbath" : serao removidos os na, uma vez que a quantidade eh muito pequena, 4 entre 3025
colnames(dt)[ apply(dt, 2, anyNA)]

# linhas que possuem NA
dt[!complete.cases(dt), ]

# Remocao de NA e remocao da coluna yearbuilt
dt <- dt[,-1]
dt <- na.omit(dt)

# Numero de variveis independentes (p) e numero de linhas (n)
p <- ncol(dt)-1
n <- nrow(dt)

# Categoricas: Sex, ChestPainType, FastingBS, RestingECG, ExerciseAngina, ST_Slope, HeartDisease
# Continuas: Age, RestingBP, Cholesterol, MaxHR, Oldpeak
cat(colnames(dt), sep = ', ')

# Valore unicos por variavel
print(lapply(lapply(dt, unique),sort))

