library(tidyverse)
library(ggplot2)
library(GGally)
library(xtable)
library(data.table)
library(gghalves)
library(egg)

theme_set(theme_bw())

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

# condition:
# 6 - excellent
# 5 - good
# 4 - average
# 3 - fair
# 2 - poor
# 1 - substandard
{
  dt$condition[dt$condition == 'substandard'] <- 1
  dt$condition[dt$condition == 'poor'] <- 2
  dt$condition[dt$condition == 'fair'] <- 3
  dt$condition[dt$condition == 'average'] <- 4
  dt$condition[dt$condition == 'good'] <- 5
  dt$condition[dt$condition == 'excellent'] <- 6
}

# Transformacao da variavel resposta
dt$logtotalvalue <- log(dt$totalvalue)

# BOXPLOTS ----
# esdistrict = Murray parece um fator mais importante. hs e ms district não parecem relevantes
dt_o <- dt
group_ordered <- with(dt, reorder(esdistrict, logtotalvalue, median))
dt_o$esdistrict <- factor(dt_o$esdistrict, levels = levels(group_ordered))
box_es <- ggplot(dt_o) +
  geom_boxplot(aes_string(y = "logtotalvalue", x = "esdistrict"), color="black") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

esdis <- c('Scottsville', 'Red Hill', 'Crozet', 'Brownsville', 'Meriwether Lewis', 'Murray')
as.data.frame(table(dt$esdistrict[dt$esdistrict %in% esdis]))

###### msdistrict 
group_ordered <- with(dt, reorder(msdistrict, logtotalvalue, median))
dt_o <- dt
dt_o$msdistrict <- factor(dt_o$msdistrict, levels = levels(group_ordered))
box_ms <- ggplot(dt_o) +
  geom_boxplot(aes_string(y = "logtotalvalue", x = "msdistrict"), color="black")


msdis <- c('Walton', 'Henley')
as.data.frame(table(dt$msdistrict[dt$msdistrict %in% msdis]))

###### hsdistrict 
group_ordered <- with(dt, reorder(hsdistrict, logtotalvalue, median))
dt_o <- dt
dt_o$hsdistrict <- factor(dt_o$hsdistrict, levels = levels(group_ordered))
box_hs <- ggplot(dt_o) +
  geom_boxplot(aes_string(y = "logtotalvalue", x = "hsdistrict"), color="black")


hsdis <- c('Western Albemarle')
as.data.frame(table(dt$hsdistrict[dt$hsdistrict %in% hsdis]))

###### censustract 
group_ordered <- with(dt, reorder(censustract, logtotalvalue, median))
dt_o <- dt
dt_o$censustract <- factor(dt_o$censustract, levels = levels(group_ordered))
box_ct <- ggplot(dt_o) +
  geom_boxplot(aes_string(y = "logtotalvalue", x = "censustract"), color="black")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

unique(dt$censustract)
census <- c(114, 107, 113.02, 113.01, 104.02, 102.02, 110)
as.data.frame(table(dt$censustract[dt$censustract %in% census]))
group_ordered <- with(dt, reorder(censustract, logtotalvalue, median))

###### cooling
group_ordered <- with(dt, reorder(cooling, logtotalvalue, median))
dt_o <- dt
dt_o$cooling <- factor(dt_o$cooling, levels = levels(group_ordered))
box_cooling <- ggplot(dt_o) +
  geom_boxplot(aes_string(y = "logtotalvalue", x = "cooling"), color="black")

###### bedroom=
group_ordered <- with(dt, reorder(bedroom, logtotalvalue, median))
dt_o <- dt
dt_o$bedroom <- factor(dt_o$bedroom, levels = levels(group_ordered))
box_bed <- ggplot(dt_o) +
  geom_boxplot(aes_string(y = "logtotalvalue", x = "bedroom"), color="black")

###### fullbath
group_ordered <- with(dt, reorder(fullbath, logtotalvalue, median))
dt_o <- dt
dt_o$fullbath <- factor(dt_o$fullbath, levels = levels(group_ordered))
box_full <- ggplot(dt_o) +
  geom_boxplot(aes_string(y = "logtotalvalue", x = "fullbath"), color="black")

###### halfbath
group_ordered <- with(dt, reorder(halfbath, logtotalvalue, median))
dt_o <- dt
dt_o$halfbath <- factor(dt_o$halfbath, levels = levels(group_ordered))
box_half <- ggplot(dt_o) +
  geom_boxplot(aes_string(y = "logtotalvalue", x = "halfbath"), color="black")

###### condition
group_ordered <- with(dt, reorder(condition, logtotalvalue, median))
dt_o <- dt
dt_o$condition <- factor(dt_o$condition, levels = levels(group_ordered))
box_cond <- ggplot(dt_o) +
  geom_boxplot(aes_string(y = "logtotalvalue", x = "condition"), color="black")

###### fp
group_ordered <- with(dt, reorder(fp, logtotalvalue, median))
dt_o <- dt
dt_o$fp <- factor(dt_o$fp, levels = levels(group_ordered))
box_fp <- ggplot(dt_o) +
  geom_boxplot(aes_string(y = "logtotalvalue", x = "fp"), color="black")

dt %>%
  ggplot(aes(x = fullbath, y = logtotalvalue, fill = bedroom)) + 
  geom_boxplot() 


# Plots para o relatorio
plota_hist_rug <- function(dados, x, xlab, ylab, titulo){
  dados %>% 
    ggplot() +
    geom_histogram(aes_string(x = x), color="black", alpha = 0.7) +
    geom_rug(
      aes_string(x = x),
      sides="b") + 
    scale_x_continuous(
      #labels = function(x) format(x, scientific = FALSE),
      name=xlab,
      breaks = scales::pretty_breaks(n = 5)) + 
    scale_y_continuous(
      name=ylab,
      breaks = scales::pretty_breaks(n = 9)) +
    ggtitle(titulo,)+
    theme(
      plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'),
      legend.position = "bottom"
    )
}

# Histograma de totalvalue
hist_totalvalue <- plota_hist_rug(
  dados = dt,
  x = "totalvalue", 
  xlab = "Valor da Casa", 
  ylab = "Contagem", 
  titulo = "Histograma da variável resposta Totalvalue") 

hist_logtotalvalue <- plota_hist_rug(
  dados = dt,
  x = "logtotalvalue", 
  xlab = "log(Valor da Casa)", 
  ylab = "Contagem", 
  titulo = "Histograma da variável resposta log(Totalvalue)")

grid1 <- grid.arrange(hist_totalvalue, hist_logtotalvalue, nrow = 1, ncol = 2)#widths = c(1.1,1,1))
ggsave("./images/totalvalue_hist.jpeg", grid1, width = 25, height = 10, units = "cm")

grid2 <- grid.arrange(box_bed, box_full, box_half, nrow = 1, ncol = 3, top="Boxplots de ln(totalvalue) para quartos e banheiros")
ggsave("./images/bed_bath_hist.jpeg", grid2, width = 40, height = 15, units = "cm")

grid3 <- grid.arrange(box_es, box_ms, box_hs, box_ct, nrow = 2, ncol = 2, top="Boxplots de ln(totalvalue) para diferentes distritos")
ggsave("./images/districts_hist.jpeg", grid3, width = 40, height = 25, units = "cm")

grid4 <- grid.arrange(box_cond, box_cooling, box_fp, nrow = 1, ncol = 3, top="Boxplots de ln(totalvalue) para condição, resfriamento e fp")
ggsave("./images/cond_cooling_pf_hist.jpeg", grid4, width = 40, height = 15, units = "cm")


# Dispersao - variaveis continuas ----
cor1 <- cor(dt[,c("logtotalvalue", "finsqft")], method="spearman")[2,1]
scatter_finsqft <- ggplot(dt) +
  geom_point(aes(x = logtotalvalue,y = finsqft), alpha = 0.7 )+
  geom_text(aes(label = sprintf("Correlação de\nSpearman: %s",round(cor1,3)), x = 10.3, y =6000), color="red")

cor2 <- cor(dt[,c("logtotalvalue", "lotsize")], method = "spearman")[2,1]
scatter_lotsize <- ggplot(dt) +
  geom_point(aes(x = logtotalvalue,y = lotsize), alpha = 0.7 )+
  geom_text(aes(label = sprintf("Correlação de\nSpearman: %s",round(cor2,3)), x = 10.3, y =450), color="red")

cor3 <- cor(dt[,c("logtotalvalue", "age")], method = "spearman")[2,1]
scatter_age <- ggplot(dt) +
  geom_point(aes(x = logtotalvalue,y = age), alpha = 0.7 )+
  geom_text(aes(label = sprintf("Correlação de\nSpearman: %s",round(cor3,3)), x = 10.3, y =200), color="red")

grid5 <- grid.arrange(scatter_finsqft, scatter_age, scatter_lotsize, nrow = 1, ncol = 3, top="Gráfico de dispersão para variáveis contínuas e totalvalue")
ggsave("./images/scatter_continuous.jpeg", grid5, width = 40, height = 15, units = "cm")

ggpairs(dt, cardinality_threshold = 20)

