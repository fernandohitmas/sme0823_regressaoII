library(tidyverse)
library(ggplot2)
library(GGally)
library(xtable)
library(data.table)

# Install
install.packages("wesanderson")
# Load
library(wesanderson)

theme_set(theme_bw())

# Funcoes de grafico ----
plot_teste <- function(data, xlab, ylab, title){
  
}


setwd("/Users/user/Documents/Pessoal/regressao II/sme0823_regressaoII/")

# Leitura de Dados ----
dt_raw <- read_delim("./data/alb_homes.csv")

# Verifica colunas que possuem NA
# "yearbuilt": nao tem necessidade de se manter ja que a variavel de idade do imovel indica a mesma coisa
# "halfbath" : serao removidos os na, uma vez que a quantidade eh muito pequena, 4 entre 3025
colnames(dt_raw)[apply(dt_raw, 2, anyNA)]

# linhas que possuem NA
dt_na <- copy(dt_raw[!complete.cases(dt_raw),c(1:7,15)])
print(
  xtable(
    dt_na,
    type = "latex"),
    NA.string = "NA",
    include.rownames = FALSE,
  file = "./tex/dt_na.tex")

# Remocao de NA e remocao da coluna yearbuilt
# Correlacao entre yearbuilt e age é -1
na_index <- which(is.na(dt_raw$yearbuilt))
cor(dt_raw$yearbuilt[-na_index], dt_raw$age[-na_index])
data <- copy(dt_raw[,-1])
data <- na.omit(data)

# Remocao de duplicatas
dt_duplicatas <- arrange(data[duplicated(data) | duplicated(data, fromLast = TRUE),c(1,7:14)], totalvalue) # Lista de replicas
print(
  xtable(
    dt_duplicatas,
    type = "latex"),
  include.rownames = FALSE,
  file = "./tex/dt_duplicatas.tex")
dt <- data[!duplicated(data),]


# Numero de variveis independentes (p = 13) e numero de linhas (n = 3015)
p <- ncol(dt)-1
n <- nrow(dt)

# Apenas realocacao da variavel target para o final do conjunto de dados
dt <- dt %>% relocate(totalvalue, .after = fp)

# Valores unicos por variavel
print(lapply(lapply(dt, unique),sort))

# Categoricas: "cooling", "bedroom", fullbath", "halfbath", "esdistrict", "msdistrict", "hsdistrict", "censustract", "condition", "fp"
# Continuas: "finsqft", "lotsize", "totalvalue", "age"
# Variavel Target: "totalvalue"
cat(colnames(dt), sep = ', ')
char_cols <- c("cooling", "bedroom", "fullbath", "halfbath", "esdistrict", "msdistrict", "hsdistrict", "censustract", "condition", "fp")
dt[,char_cols] <- lapply(dt[,char_cols], as.factor)

# Transformacao da variavel resposta
dt$logtotalvalue <- log(dt$totalvalue)
dt$loglotsize <- log(dt$lotsize)

dt[dt["censustract"] == 111,]
unique(dt$censustract)

ggplot(dt) +
  geom_point(aes(x = finsqft, y = age, color = bedroom))

cor(dt$finsqft, dt$lotsize)

for (c in char_cols) {
  p <- ggplot(dt) +
    geom_histogram(aes_string(fill = c, x = "logtotalvalue"), color="black")
  print(p)
}
dt %>% ggplot(aes(hsdistrict, after_stat(count))) + 
  geom_bar(aes(fill = msdistrict), position = "dodge")

# plot de interesse 1
ggplot(dt) +
  geom_boxplot(aes_string(y = "logtotalvalue", x = "bedroom", fill = "fp"), color="black")

# plot de interesse 2
ggplot(dt) +
  geom_boxplot(aes_string(y = "logtotalvalue", fill = "bedroom", x = "condition"), color="black")

# plot de interesse 3
ggplot(dt) +
  geom_boxplot(aes_string(y = "logtotalvalue", x = "condition"), color="black")

# plot de interesse 4
ggplot(dt) +
  geom_point(aes_string(y = "logtotalvalue", x = "loglotsize", color="fp"))#, color="black")

# plot de interesse 4
ggplot(dt) +
  geom_histogram(aes_string(x = "loglotsize"))#, color="black")

  
as.data.frame(table(dt$censustract))

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


