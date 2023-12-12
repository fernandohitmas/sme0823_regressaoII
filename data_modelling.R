library(tidyverse)
library(ggplot2)
library(GGally)
library(xtable)
library(data.table)
library(gamlss)

dt_raw <- read_delim("./data/alb_homes.csv")

colnames(dt_raw)[apply(dt_raw, 2, anyNA)]

# Remocao de valores faltantes
dt_na <- copy(dt_raw[!complete.cases(dt_raw),c(1:7,15)])

na_index <- which(is.na(dt_raw$yearbuilt))
cor(dt_raw$yearbuilt[-na_index], dt_raw$age[-na_index])
data <- copy(dt_raw[,-1])
data <- na.omit(data)

# Remocao de duplicatas
dt_duplicatas <- arrange(data[duplicated(data) | duplicated(data, fromLast = TRUE),c(1,7:14)], totalvalue) # Lista de replicas

dt <- data[!duplicated(data),]

# Apenas realocacao da variavel target para o final do conjunto de dados
dt <- dt %>% relocate(totalvalue, .after = fp)

# Transformacao da variavel resposta
dt$logtotalvalue <- log(dt$totalvalue)

# Condicao como ordinal
{
  dt$condition[dt$condition == 'Substandard'] <- 1
  dt$condition[dt$condition == 'Poor'] <- 2
  dt$condition[dt$condition == 'Fair'] <- 3
  dt$condition[dt$condition == 'Average'] <- 4
  dt$condition[dt$condition == 'Good'] <- 5
  dt$condition[dt$condition == 'Excellent'] <- 6
}

{
  dt$centralair <- 0
  dt$centralair[dt$cooling == 'Central Air'] = 1
}

{
  #dt$fp <- as.factor(dt$fp)
  #dt$centralair <- as.factor(dt$centralair)
  dt$censustract <- as.factor(dt$censustract)
  dt$condition <- as.numeric(dt$condition)
}

{
  dt$esdistrict <- as.factor(dt$esdistrict)
  dt$msdistrict <- as.factor(dt$msdistrict)
  dt$hsdistrict <- as.factor(dt$hsdistrict)
}

##########
# 3 esdistricts com menor e maior mediana
esdis <- c('Scottsville', 'Red Hill', 'Crozet', 'Brownsville', 'Meriwether Lewis', 'Murray')
# top e bottom msdistrict
msdis <- c('Walton', 'Henley')
# top hsdistrict
hsdis <- c('Western Albemarle')
# 4 de baixo, 3 de cima
census <- c('114', '107', '113.02', '113.01', '104.02', '102.02', '110')
##########
{
  dtse <- subset(dt, select = -c(cooling, esdistrict, msdistrict, hsdistrict, censustract))
  for(d in esdis) {
    dstr <- paste('es', d, sep = '')
    dstr <- gsub(" ", "", dstr)
    dtse[, dstr] <- 0
    dtse[dt$esdistrict == d, dstr] <- 1
  }
  for(d in msdis) {
    dstr <- paste('ms', d, sep = '')
    dstr <- gsub(" ", "", dstr)
    dtse[, dstr] <- 0
    dtse[dt$msdistrict == d, dstr] <- 1
  }
  for(d in hsdis) {
    dstr <- paste('hs', d, sep = '')
    dstr <- gsub(" ", "", dstr)
    dtse[, dstr] <- 0
    dtse[dt$hsdistrict == d, dstr] <- 1
  }
  for(c in census) {
    cstr <- paste('c', c, sep = '')
    print(cstr)
    cstr <- gsub("\\.", "_", cstr)
    print(cstr)
    dtse[, cstr] <- 0
    dtse[dt$hsdistrict == d, cstr] <- 1
  }
}


# BoxCox T
m1_bct <- gamlss(formula = logtotalvalue ~ 1, 
             family = BCT(mu.link = "identity", 
                          sigma.link = "log", 
                          nu.link = "identity", 
                          tau.link = "log"),  
             data = dtse, control = gamlss.control(n.cyc = 200),  
             trace = FALSE)

m1_tf2 <- gamlss(formula = logtotalvalue ~ 1,
             family = TF2(mu.link = "identity",
                          sigma.link = "log", 
                          nu.link = "logshiftto2"), 
             data = dtse,  
             control = gamlss.control(n.cyc = 200), trace = FALSE)

m1_logno <- gamlss(formula = totalvalue ~ 1, 
             family = LOGNO(mu.link = "identity",
                            sigma.link = "log"), 
             control = gamlss.control(n.cyc = 200),
             data = dtse, trace = FALSE)

# Definindo numero de nucleos para serem utilizados
nC <- detectCores()

gamlss::stepGAIC(m1_bct,
                 scope = c(lower = ~ 1,
                           upper = ~ finsqft + bedroom + fullbath + halfbath + lotsize + 
                              age + condition + fp + centralair + esScottsville + esRedHill +         
                              esCrozet + esBrownsville + esMeriwetherLewis + esMurray +            
                              msWalton + msHenley + hsWesternAlbemarle + 
                              c114 + c107 + c113_02 + c113_01 + c104_02 + c102_02 + c110
                              + (condition) 
                              : (c114 + c107 + c113_02 + c113_01 + c104_02 + c102_02 + c110)
                 )
                 ,direction = "both", parallel="snow",  ncpus=nC)

gamlss::stepGAIC(m1_tf2,
                  scope = c(lower = ~ 1,
                            upper = ~ finsqft + bedroom + fullbath + halfbath + lotsize + 
                            age + condition + fp + centralair + esScottsville + esRedHill +         
                            esCrozet + esBrownsville + esMeriwetherLewis + esMurray +            
                            msWalton + msHenley + hsWesternAlbemarle + 
                            c114 + c107 + c113_02 + c113_01 + c104_02 + c102_02 + c110
                            + (condition) 
                            : (c114 + c107 + c113_02 + c113_01 + c104_02 + c102_02 + c110)
                  )
                  ,direction = "both", parallel="snow",  ncpus=nC)

gamlss::stepGAIC(m1_logno,
                  scope = c(lower = ~ 1,
                            upper = ~ finsqft + bedroom + fullbath + halfbath + lotsize + 
                            age + condition + fp + centralair + esScottsville + esRedHill +         
                            esCrozet + esBrownsville + esMeriwetherLewis + esMurray +            
                            msWalton + msHenley + hsWesternAlbemarle + 
                            c114 + c107 + c113_02 + c113_01 + c104_02 + c102_02 + c110
                            + (condition) 
                            : (c114 + c107 + c113_02 + c113_01 + c104_02 + c102_02 + c110)
                  )
                  ,direction = "both", parallel="snow",  ncpus=nC)


# BoxCox T
m2_bct <- gamlss(formula = logtotalvalue ~ finsqft + fullbath +  
         esScottsville + lotsize + fp + esMurray + condition +  
         age + esMeriwetherLewis + esRedHill + centralair +  
         esBrownsville + msWalton + esCrozet, family = BCT(mu.link = "identity",  
                                                           sigma.link = "log", nu.link = "identity", tau.link = "log"),  
       data = dtse, control = gamlss.control(n.cyc = 200),  
       trace = FALSE)

m2_tf2 <- gamlss(formula = logtotalvalue ~ finsqft + fullbath +  
         esScottsville + lotsize + fp + condition + esMurray +  
         age + esMeriwetherLewis + esRedHill + centralair +  
         esBrownsville + msWalton + esCrozet, family = TF2(mu.link = "identity",  
                                                           sigma.link = "log", nu.link = "logshiftto2"), data = dtse,  
       control = gamlss.control(n.cyc = 200), trace = FALSE)

m2_logno <- gamlss(formula = totalvalue ~ finsqft + lotsize + condition +  
                     fullbath + fp + age + esScottsville + esMurray +  
                     centralair + esBrownsville + esMeriwetherLewis +  
                     esRedHill + halfbath + bedroom, family = LOGNO(mu.link = "identity",  
                                                                    sigma.link = "log"), data = dtse, trace = FALSE)


# SUMARIO ---- 
summary(m2_bct)
summary(m2_tf2)
summary(m2_logno)

# PLOTS ---- 
plot(m2_bct)
plot(m2_tf2)
plot(m2_logno)

# resíduos do ajuste
{
  res <- resid(m2_logno)
  fit <- fitted(m2_logno)
  
  data.frame(fit, res) %>%
          ggplot(aes(fit,res)) + 
          geom_point() + 
          geom_hline(yintercept = 0)
}

# qqplot
{
  qqnorm(res, ylim = c(-10,10))
  qqline(res, col = "steelblue", lwd = 2)
}

#histograma do log do preço e do preço estimado
{
h1 <- hist(dtse$logtotalvalue, breaks=200)
h2 <- hist(predict(object = m2_bct, new_data = dtse$logtotalvalue), breaks=200)
plot( h1, col=rgb(0,0,1,1/4), xlim=c(10,16), ylim=c(0,180))
plot( h2, col=rgb(1,0,0,1/4), xlim=c(10,16), add=T)
}


dtse$predicted <- predict(object = stepmodel, new_data = dtse$totaltotalvalue)
ggplot(dtse) +
  geom_point(aes(x = totalvalue, y = predicted))

m2 <- gamlss(formula = logtotalvalue ~ finsqft + fullbath +  
               esScottsville + lotsize + fp + condition + esMurray +  
               age + esMeriwetherLewis + esRedHill + centralair +  
               esBrownsville + msWalton + esCrozet, family = TF2(mu.link = "identity",  
                                                                 sigma.link = "log", nu.link = "logshiftto2"), data = dtse,  
             control = gamlss.control(n.cyc = 200), trace = FALSE)
summary(m2)
plot(m2)
edf(m2)
