library(data.table)
library(ggplot2)
library(scales)
library(egg)
library(gridExtra)

# Leitura de Dados ----
dt <- fread("./data/heart.csv")

# Numero de variveis independentes (p) e numero de linhas (n)
p <- ncol(dt)-1
n <- nrow(dt)

# Categoricas: Sex, ChestPainType, FastingBS, RestingECG, ExerciseAngina, ST_Slope, HeartDisease
# Continuas: Age, RestingBP, Cholesterol, MaxHR, Oldpeak
cat(colnames(dt), sep = ', ')

# Valore unicos por variavel
print(lapply(lapply(dt, unique),sort))

# Retirada de valores com zero em RestingBP e Cholesterol
dt <- dt[RestingBP != 0 & Cholesterol !=0 & Oldpeak > 0] # https://d-nb.info/1242792767/34

# Tranformacao para variavel categorica
dt[,c(2,3,6,7,9,11,12)] <- lapply(dt[,c(2,3,6,7,9,11,12)], as.factor)


# Histogramas e graficos de barra ----
# Grafico de variaveis continuas ----

c1 <- ggplot(dt, aes(x = Age, fill = HeartDisease)) +
  geom_histogram(color = 'black', bins=15) +
  scale_x_continuous(
    name="Idade",
    limits=c(20, 80),
    breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(
    name="Contagem",
    limits=c(0, 150),
    breaks = scales::pretty_breaks(n = 9)) +
  ggtitle("Histograma das Observações para variável Idade",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'),
    legend.position = "bottom"
  )

c2 <- ggplot(dt, aes(x = Cholesterol, fill = HeartDisease)) +
  geom_histogram(color = 'black', bins=15) +
  scale_x_continuous(
    name="Idade",
    limits=c(0, 650),
    breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(
    name="Contagem",
    limits=c(0, 300),
    breaks = scales::pretty_breaks(n = 10)) +
  ggtitle("Histograma das Observações para variável Idade",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'),
    legend.position = "bottom"
  )

c3 <- ggplot(dt, aes(x = RestingBP, fill = HeartDisease)) +
  geom_histogram(color = 'black', bins=15) +
  scale_x_continuous(
    name="Idade",
    limits=c(80, 220),
    breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(
    name="Contagem",
    limits=c(0, 180),
    breaks = scales::pretty_breaks(n = 10)) +
  ggtitle("Histograma das Observações para variável Idade",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'),
    legend.position = "bottom"
  )

c4 <- ggplot(dt, aes(x = MaxHR, fill = HeartDisease)) +
  geom_histogram(color = 'black', bins=15) +
  scale_x_continuous(
    name="Idade",
    limits=c(50, 220),
    breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(
    name="Contagem",
    limits=c(0, 80),
    breaks = scales::pretty_breaks(n = 10)) +
  ggtitle("Histograma das Observações para variável Idade",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'),
    legend.position = "bottom"
  )

c5 <- ggplot(dt, aes(x = Oldpeak, fill = HeartDisease)) +
  geom_histogram(color = 'black', bins = 15) +
  scale_x_continuous(
    name="Idade",
    limits=c(0, 7),
    breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(
    name="Contagem",
    limits=c(0, 120),
    breaks = scales::pretty_breaks(n = 10)) +
  ggtitle("Histograma das Observações para variável Idade",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'),
    legend.position = "bottom"
  )
grid2 <- grid.arrange(c1, c2,c3,c4,c5, nrow = 2,ncol=3)
ggsave("./images/grid2.jpeg", grid2, width = 50, height = 28, units = "cm")



# Grafico de variaveis categoricas----
# Grafico de Sexo
g1 <- ggplot(dt,aes(x =Sex, fill=HeartDisease)) +
  theme_bw() + 
  geom_bar(color = 'black') +
  geom_text(
    aes(
      y=after_stat(count) + 10,
      label = after_stat(count)
      ),
    stat = "count",
    position = "stack"
    ) + 
  xlab("Sexo") + 
  ylab("Contagem") +
  ggtitle("Observações por Sexo",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'),
    legend.position = "bottom"
  )

# Grafico de Dores
g2 <- ggplot(dt,aes(x = ChestPainType, fill=HeartDisease)) +
  theme_bw()+
  geom_bar(color = 'black') +
  geom_text(
    aes(
      y=after_stat(count) + 10,
      label = after_stat(count)
    ),
    stat = "count",
    position = "stack"
  ) + 
  xlab("Tipo de Dor") + 
  ylab("Contagem") +
  ggtitle("Observações por Tipo de Dor no Peito",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'),
    legend.position = "bottom"
  )
# Glicose no Sangue em Jejum
g3 <- ggplot(dt,aes(x = FastingBS, fill=HeartDisease)) +
  theme_bw()+
  geom_bar(color = 'black') +
  geom_text(
    aes(
      y=after_stat(count) + 10,
      label = after_stat(count)
    ),
    stat = "count",
    position = "stack"
  ) + 
  xlab("Indicador de Diabetes") + 
  ylab("Contagem") +
  ggtitle("Observações com idicativo de diabetes",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'),
    legend.position = "bottom"
  )

# Eletrocardiograma em Repouso  
g4 <- ggplot(dt,aes(x = RestingECG, fill=HeartDisease)) +
  theme_bw()+
  geom_bar(color = 'black') +
  geom_text(
    aes(
      y=after_stat(count) + 10,
      label = after_stat(count)
    ),
    stat = "count",
    position = "stack"
  ) + 
  xlab("Resultados do ECG") + 
  ylab("Contagem") +
  ggtitle("Observações para os resultados do ECG",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'),
    legend.position = "bottom"
  )

# Declive do segmento ST
g5 <- ggplot(dt,aes(x = ST_Slope, fill=HeartDisease)) +
  theme_bw()+
  geom_bar(color = 'black') +
  geom_text(
    aes(
      y=after_stat(count) + 10,
      label = after_stat(count)
    ),
    stat = "count",
    position = "stack"
  ) + 
  xlab("Tipos de declive do ST") + 
  ylab("Contagem") +
  ggtitle("Observações para os resultados de declive do ST",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'),
    legend.position = "bottom"
  )

# Indicador de doenca cardiaca
g6 <- ggplot(dt,aes(x = HeartDisease, fill=HeartDisease)) +
  theme_bw()+
  geom_bar(color = 'black') +
  geom_text(
    aes(
      y=after_stat(count) + 10,
      label = after_stat(count)
    ),
    stat = "count",
    position = "stack"
  ) + 
  xlab("Indicador de Doença Cardíaca") + 
  ylab("Contagem") +
  ggtitle("Observações do indicador de Doença Cardíaca",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold'),
    legend.position = "bottom"
  )

grid1 <- grid.arrange(g1, g2,g3,g4,g5,g6, nrow = 2,ncol=3)
ggsave("./images/grid1.jpeg", grid1, width = 50, height = 28, units = "cm")
