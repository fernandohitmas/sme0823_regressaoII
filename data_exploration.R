library(data.table)
library(ggplot2)
library(scales)

# FUNCOES ----
grafico_barra <- function(data, )

# Leitura de Dados ----
dt <- fread("./data/heart.csv")

p <- ncol(dt)

cat(colnames(dt), sep = ', ')

# Valore unicos por variavel
print(lapply(dt, unique))

# Tranformacao para variavel categorica
dt[,c(2,3,6,7,9,11,12)] <- lapply(dt[,c(2,3,6,7,9,11,12)], as.factor)


# Histogramas e graficos de barra ----

# Grafico de variaveis categoricas----
# Grafico de Sexo
g1 <- ggplot(dt,aes(x =Sex)) +
  geom_bar(color = 'black') +
  geom_text(
    aes(
      y=after_stat(count) + 20,
      label = after_stat(count)
      ),
    stat = "count") + 
  xlab("Sexo") + 
  ylab("Contagem") +
  ggtitle("Contagem de Observações por Sexo",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold')
  )
ggsave("./images/Contagem")

# Grafico de Dores
ggplot(dt,aes(x = ChestPainType)) +
  geom_bar(color = 'black') +
  geom_text(
    aes(
      y=after_stat(count) + 20,
      label = after_stat(count)
    ),
    stat = "count") + 
  xlab("Tipo de Dor") + 
  ylab("Contagem") +
  ggtitle("Contagem de Observações por Tipo de Dor no Peito",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold')
  )

# Glicose no Sangue em Jejum
ggplot(dt,aes(x = FastingBS)) +
  geom_bar(color = 'black') +
  geom_text(
    aes(
      y=after_stat(count) + 20,
      label = after_stat(count)
    ),
    stat = "count") + 
  xlab("Indicador de Diabetes") + 
  ylab("Contagem") +
  ggtitle("Contagem de Observações com idicativo de diabetes",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold')
  )

# Eletrocardiograma em Repouso  
ggplot(dt,aes(x = RestingECG)) +
  geom_bar(color = 'black') +
  geom_text(
    aes(
      y=after_stat(count) + 20,
      label = after_stat(count)
    ),
    stat = "count") + 
  xlab("Resultados do ECG") + 
  ylab("Contagem") +
  ggtitle("Contagem de Observações para os resultados do ECG",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold')
  )

# Declive do segmento ST
ggplot(dt,aes(x = ST_Slope)) +
  geom_bar(color = 'black') +
  geom_text(
    aes(
      y=after_stat(count) + 20,
      label = after_stat(count)
    ),
    stat = "count") + 
  xlab("Tipos de declive do ST") + 
  ylab("Contagem") +
  ggtitle("Contagem de Observações para os resultados de declive do ST",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold')
  )

# Indicador de doenca cardiaca
ggplot(dt,aes(x = HeartDisease)) +
  geom_bar(color = 'black') +
  geom_text(
    aes(
      y=after_stat(count) + 20,
      label = after_stat(count)
    ),
    stat = "count") + 
  xlab("Indicador de Doença Cardíaca") + 
  ylab("Contagem") +
  ggtitle("Contagem de Observações do indicador de Doença Cardíaca",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold')
  )


# Grafico de variaveis continuas ----

ggplot(dt, aes(x = Age, fill = HeartDisease)) +
  geom_histogram(color = 'black', bins=15) +
  scale_x_continuous(
    name="Idade",
    limits=c(20, 80),
    breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(
    name="Contagem",
    limits=c(0, 180),
    breaks = scales::pretty_breaks(n = 9)) +
  ggtitle("Histograma das Observações para variável Idade",)+
  theme(
    plot.title=element_text( hjust=0.5, vjust=0.5, face='bold')
  )
ggsave("./images/hist_idade.jpeg")
