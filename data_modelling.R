#install.packages("data.table")
library(data.table)
#install.packages("gamlss")
library(gamlss)
#install.packages("woe")
library(woe)
#install.packages("xtable")
library(xtable)
library(tidyverse)

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

# Tranformacao para variavel categorica ----
# dt[,c(2,3,6,7,9,11,12)] <- lapply(dt[,c(2,3,6,7,9,11,12)], as.factor)


#Simplify ChestPainType, only check for Asymptomatic pain
dt$ChestPainASY <- 0
dt$ChestPainASY[dt$ChestPainType == "ASY"] <- 1
dt$ChestPainASY <- as.factor(dt$ChestPainASY)

#Simplify RestingECG, only check for ST
dt$RestingECGST <- 0
dt$RestingECGST[dt$RestingECG == "ST"] <- 1
dt$RestingECGST <- as.factor(dt$RestingECGST)

#Simplify ST_Slope, only check for Down/Flat
dt$ST_SlopeDownFlat <- 1
dt$ST_SlopeDownFlat[dt$ST_Slope == "Up"] <- 0
dt$ST_SlopeDownFlat <- as.factor(dt$ST_SlopeDownFlat)

#Turn Sex into factor
dt$Sex_fct <- 1
dt$Sex_fct[dt$Sex == "F"] <- 0
dt$Sex_fct <- as.factor(dt$Sex_fct)

dt$ExerciseAngina_fct <- 1
dt$ExerciseAngina_fct[dt$ExerciseAngina == "N"] <- 0
dt$ExerciseAngina_fct <- as.factor(dt$ExerciseAngina_fct)

removecols <- c("ChestPainType","Cholesterol_imp","RestingBP_imp","Oldpeak_imp","RestingECG","ST_Slope","ExerciseAngina","Sex")
dt <- dt[,c("Age","RestingBP", "Cholesterol", "FastingBS", "MaxHR", "Oldpeak",
            "HeartDisease", "ChestPainASY", "RestingECGST", 
            "ST_SlopeDownFlat", "Sex_fct", "ExerciseAngina_fct")]
#names(dt)[!names(dt) %in% removecols]
#Dataset is looking better
#Add heartdisease back to the data frame
#heart_imp$HeartDisease <- heart$HeartDisease




# Selecao de variaveis (IV e GAIC)
iv_age <- woe(Data=dt, Independent="Age", Continuous=TRUE, Dependent="HeartDisease", C_Bin=10, Bad=0, Good=1)[,c("MIN", "MAX", "IV")]
iv_restingbp <- woe(Data=dt, Independent="RestingBP", Continuous=TRUE, Dependent="HeartDisease", C_Bin=10, Bad=0, Good=1)[,c("MIN", "MAX", "IV")]
iv_choles <- woe(Data=dt, Independent="Cholesterol", Continuous=TRUE, Dependent="HeartDisease", C_Bin=10, Bad=0, Good=1)[,c("MIN", "MAX", "IV")]
iv_maxhr <- woe(Data=dt, Independent="MaxHR", Continuous=TRUE, Dependent="HeartDisease", C_Bin=10, Bad=0, Good=1)[,c("MIN", "MAX", "IV")]
iv_oldpeak <- woe(Data=dt, Independent="Oldpeak", Continuous=TRUE, Dependent="HeartDisease", C_Bin=10, Bad=0, Good=1)[,c("MIN", "MAX", "IV")]

# faz arquivo .tex da tabela 
print(xtable(iv_age, type = "latex"), file = "iv_age.tex")
print(xtable(iv_restingbp, type = "latex"), file = "iv_restingbp.tex")
print(xtable(iv_choles, type = "latex"), file = "iv_choles.tex")
print(xtable(iv_maxhr, type = "latex"), file = "iv_maxhr.tex")
print(xtable(iv_oldpeak, type = "latex"), file = "iv_oldpeak.tex")


iv_sum <- data.frame(
  Age = sum(iv_age$IV),
        # IV colesterol < 0.1
  Cholesterol = sum(iv_choles$IV),
  MaxHR = sum(iv_maxhr$IV),
        # IV oldpeak > 1
        # é a leitura do cardiograma e é coerente que esteja
        # relacionado com a presença de doença
  Oldpeak = sum(iv_oldpeak$IV),
  Resting_BP = sum(iv_restingbp$IV)
)

print(xtable(iv_sum, type = "latex"), file = "iv_sum.tex")








#woe(Data=mtcars,"cyl",FALSE,"am",10,Bad=0,Good=1)
model <- gamlss(HeartDisease ~ Sex_fct*ChestPainASY, data = dt, family = BI(mu.link=logit))
gamlss::stepGAIC(model,
                 scope = c(lower = ~ 1,
                           upper = ~ Age + RestingBP + FastingBS + 
                           RestingECGST + MaxHR + ExerciseAngina_fct + 
                           Oldpeak + ST_SlopeDownFlat + Sex_fct*ChestPainASY),
                 direction = "both")



step_variables <- c("Sex_fct", "ChestPainASY", "Oldpeak", "ST_SlopeDownFlat", 
                    "ExerciseAngina_fct", "Age", "RestingBP", "HeartDisease")
heart_step <- dt[,c("Sex_fct", "ChestPainASY", "Oldpeak", "ST_SlopeDownFlat",
                    "ExerciseAngina_fct", "Age", "RestingBP", "HeartDisease")]

step_model <- gamlss(formula = HeartDisease ~ .,  
                     family = BI(mu.link = logit), data = heart_step, trace = FALSE)

summary(step_model)



options(warn=-1)
# 10 fold cross validation of model
n <- dim(heart_step)[1]
k = 10
set.seed(2, sample.kind = "Rounding")
groups <- c(rep(1:k,floor(n/k)),1:(n-floor(n/k)*k))
set.seed(3, sample.kind = "Rounding")
cvgroups <- sample(groups,n)
predictvalsGLM <- rep(-1,n)
for (i in 1:k) {
  groupi <- (cvgroups == i)
  fit = gamlss(formula = HeartDisease ~ ., family = BI(mu.link = probit), data = heart_step[!groupi,])
  predictvalsGLM[groupi] = predict(object = fit, new_data = heart_step[groupi,], type = "response")
}


library(pROC)

PROC_obj <- roc(predictor = predictvalsGLM, response=dt$HeartDisease,
                       curve=TRUE)
plot(PROC_obj)

hist(predictvalsGLM)



#Find the best threshold value
probRng <- 20:80
errorRateMtx <- matrix(nrow = length(probRng), ncol = 4)
colnames(errorRateMtx) <- c("Threshold","ErrorRate","FalsePositive","FalseNegative")

for (i in 1:length(probRng)) {
  threshold <- probRng[i]/100
  PredictedHDGLM <- rep(0, n)
  PredictedHDGLM[predictvalsGLM >= threshold] <- 1
  
  tblGLM <- table(PredictedHDGLM, dt$HeartDisease)
  errorRate <- (tblGLM[1,2]+tblGLM[2,1])/n
  falsePos <-  tblGLM[2,1]/(tblGLM[2,2]+tblGLM[2,1])
  falseNeg <- tblGLM[1,2]/(tblGLM[1,1]+tblGLM[1,2])
  
  errorRateMtx[i,1] <- threshold
  errorRateMtx[i,2] <- errorRate
  errorRateMtx[i,3] <- falsePos
  errorRateMtx[i,4] <- falseNeg
}

plot(y = errorRateMtx[,2], x = errorRateMtx[,1], col = "red", pch = 20, ylim = c(0.1,0.2))
lines(errorRateMtx[,3], x = errorRateMtx[,1], col = "green", lty = 2)
lines(errorRateMtx[,4], x = errorRateMtx[,1], col = "blue", lty = 2)
legend("topleft", legend = c("Total error", "False positive","False Negative"), col=c("red", "green", "blue"), lty=c(20,2,2), cex=0.8)

minerror <- min(errorRateMtx[,2])
errorRateMtx[errorRateMtx[,2]==minerror,1]
errorRateMtx[errorRateMtx[,2]==minerror,2]
