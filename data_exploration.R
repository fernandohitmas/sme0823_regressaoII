library(data.table)

# read data ----

dt <- fread("./data/heart.csv")

p <- ncol(dt)

cat(colnames(dt),sep='}\n\\item \\textit{')

# Valore unicos por variavel
lapply(dt, unique)

# boxplot
hist(dt[[4]])

plot(table(dt[[3]]))


hist(dt[,])
