#install.packages("data.table")
library(data.table)
#install.packages("gamlss")
library(gamlss)
#install.packages("woe")
library(woe)
#install.packages("xtable")
library(xtable)

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
dt[,c(2,3,6,7,9,11,12)] <- lapply(dt[,c(2,3,6,7,9,11,12)], as.factor)

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


sum(iv_age$IV)
sum(iv_restingbp$IV)
# IV colesterol < 0.1
sum(iv_choles$IV)
sum(iv_maxhr$IV)
# IV oldpeak > 1
# é a leitura do cardiograma e é coerente que esteja
# relacionado com a presença de doença
sum(iv_oldpeak$IV)



#woe(Data=mtcars,"cyl",FALSE,"am",10,Bad=0,Good=1)
model <- gamlss(HeartDisease ~ Sex*ChestPainType, data = dt, family = BI(mu.link=probit))
gamlss::stepGAIC(model,
                 scope = c(lower = ~ 1,
                           upper = ~ Age + RestingBP + FastingBS + 
                           RestingECG + MaxHR + ExerciseAngina + 
                           oldpeak + ST_Slope + Sex*ChestPainType),
                 direction = "forward")
