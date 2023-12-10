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
colnames(dtse)[-10]
dtse

hist(dtse$totalvalue)


m1 <- gamlss(totalvalue ~ 1, data = dtse, 
            family = WEI3(mu.link = "log", sigma.link = "log"),
            control = gamlss.control(n.cyc = 200)
            )

gamlss::stepGAIC(m1,
                 scope = c(lower = ~ 1,
                           upper = ~ finsqft + bedroom + fullbath + halfbath + lotsize + 
                             age + condition + fp + centralair + esScottsville + esRedHill +         
                             esCrozet + esBrownsville + esMeriwetherLewis + esMurray +            
                             msWalton + msHenley + hsWesternAlbemarle + 
                             c114 + c107 + c113_02 + c113_01 + c104_02 + c102_02 + c110
                             + (condition) 
                             : (c114 + c107 + c113_02 + c113_01 + c104_02 + c102_02 + c110)
                           )
                 ,direction = "both")


# dtstep <- dtse[, c('totalvalue', 'finsqft', 'lotsize', 'condition',
#                    'fullbath', 'fp', 'age', 'esScottsville', 'esMurray',
#                    'centralair', 'esCrozet', 'esMeriwetherLewis',
#                    'esRedHill', 'halfbath', 'msWalton', 'bedroom', 'c114')]
# 
# stepmodel <- gamlss(formula = totalvalue ~ . + condition:c114 - c114, 
#                     family = LOGNO(mu.link = "identity", sigma.link = "log"),
#                     data = dtstep, trace = FALSE)


# modelo final
stepmodel <- gamlss(formula = totalvalue ~ finsqft + lotsize + esMurray +  
                      esScottsville + msWalton + esRedHill + condition +  
                      esMeriwetherLewis + centralair + fullbath + age +  
                      bedroom + esBrownsville + fp, family = WEI3(mu.link = "log",  
                                                                  sigma.link = "log"), data = dtse, control = gamlss.control(n.cyc = 200),  
                    trace = FALSE)

summary(stepmodel)

# gráficos de resíduo prontos, pode dar erro
plot(stepmodel)

# resíduos do ajuste
{
  res <- resid(stepmodel)
  plot(fitted(stepmodel), res)
  abline(0,0)
}

# qqplot
{
  qqnorm(res, ylim = c(-10,10))
  qqline(res, col = "steelblue", lwd = 2)
}

#histograma do log do preço e do preço estimado
hist(dtse$logtotalvalue, xlim=c(9,16))
hist(predict(object = stepmodel, new_data = dtse$totaltotalvalue), xlim=c(9,16))

# options(warn=-1)
# # 10 fold cross validation of model
# n <- dim(dtstep)[1]
# k = 10
# set.seed(2, sample.kind = "Rounding")
# groups <- c(rep(1:k,floor(n/k)),1:(n-floor(n/k)*k))
# set.seed(3, sample.kind = "Rounding")
# cvgroups <- sample(groups,n)
# predictvalsGLM <- rep(-1,n)
# for (i in 1:k) {
#   groupi <- (cvgroups == i)
#   fit = gamlss(formula = totalvalue ~ ., 
#                family = LOGNO2(mu.link = "log", sigma.link = "log"), data = dtstep[!groupi,])
#   predictvalsGLM[groupi] = predict(object = fit, new_data = dtstep[groupi,], type = "response")
# }
# predictvalsGLM
