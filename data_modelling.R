#install.packages("data.table")
library(data.table)
#install.packages("gamlss")
library(gamlss)
#install.packages("woe")
library(woe)

# LEITURA DOS DADOS ----
dt <- fread("./data/heart.csv")
    
colnames(dt)

# Tranformacao para variavel categorica ----
dt[,c(2,3,6,7,9,11,12)] <- lapply(dt[,c(2,3,6,7,9,11,12)], as.factor)

woe(Data=dt, Independent="RestingBP", Continuous=TRUE, Dependent="HeartDisease", C_Bin=8, Bad=0, Good=1)

woe(Data=mtcars,"cyl",FALSE,"am",10,Bad=0,Good=1)
model <- gamlss(HeartDisease ~ (Sex + ChestPainType)^2 , data = dt)
gamlss::stepGAIC(model)

