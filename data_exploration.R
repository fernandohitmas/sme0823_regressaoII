library(tidyverse)
library(ggplot2)
library(GGally)

# Leitura de Dados ----
dt <- read_delim("./data/alb_homes.csv")

# Verifica colunas que possuem NA
# "yearbuilt": nao tem necessidade de se manter ja que a variavel de idade do imovel indica a mesma coisa
# "halfbath" : serao removidos os na, uma vez que a quantidade eh muito pequena, 4 entre 3025
colnames(dt)[ apply(dt, 2, anyNA)]

# linhas que possuem NA
dt[!complete.cases(dt),]

# Remocao de NA e remocao da coluna yearbuilt
dt <- dt[,-1]
dt <- na.omit(dt)

# Remocao de duplicatas
dt <- dt[!duplicated(dt),]

# Numero de variveis independentes (p = 13) e numero de linhas (n = 3015)
p <- ncol(dt)-1
n <- nrow(dt)

# Categoricas: "cooling", "bedroom", fullbath", "halfbath", "esdistrict", "msdistrict", "hsdistrict", "censustract", "condition", "fp"
# Continuas: "finsqft", "lotsize", "totalvalue", "age"
# Variavel Target: "totalvalue"
cat(colnames(dt), sep = ', ')
char_cols <- c("cooling", "bedroom", "fullbath", "halfbath", "esdistrict", "msdistrict", "hsdistrict", "censustract", "condition", "fp")
dt[,char_cols] <- lapply(dt[,char_cols], as.factor)

# Apenas realocacao da variavel target para o final do conjunto de dados
dt <- dt %>% relocate(totalvalue, .after = fp)

# Valore unicos por variavel
print(lapply(lapply(dt, unique),sort))


char_cols <- c("cooling", "fullbath", "halfbath", "esdistrict", "msdistrict", "hsdistrict", "censustract", "condition", "fp")

dt[dt["censustract"] == 111,]



head(dt)

dt[,c("finsqft")]


names(gplot)
ggpairs(dt)

gplot <- GGally::ggpairs(dt, columns = 1:4, aes(color=cooling))
gplot$nrow <- 4
print(gplot)

col <- "fp"

for (col in colnames(dt)) {
  hist(x = dt[[grep(col, colnames(dt))]])
}


