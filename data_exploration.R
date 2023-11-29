library(tidyverse)
library(ggplot2)

# Leitura de Dados ----
dt <- fread("./data/alb_homes.csv")

# Verifica colunas que possuem NA
# "yearbuilt": nao tem necessidade de se manter ja que a variavel de idade do imovel
# "halfbath" : 
colnames(dt)[ apply(dt, 2, anyNA)]
dt <- dt[,-1]

table(dt[, list(fullbath,halfbath)])



ggplot(data = dt)+
  geom_point(aes(x=fullbath, y=halfbath))

dt[!complete.cases(dt), ]

dt <- na.omit(dt)

# Numero de variveis independentes (p) e numero de linhas (n)
p <- ncol(dt)-1
n <- nrow(dt)

# Categoricas: Sex, ChestPainType, FastingBS, RestingECG, ExerciseAngina, ST_Slope, HeartDisease
# Continuas: Age, RestingBP, Cholesterol, MaxHR, Oldpeak
cat(colnames(dt), sep = ', ')

# Valore unicos por variavel
print(lapply(lapply(dt, unique),sort))

