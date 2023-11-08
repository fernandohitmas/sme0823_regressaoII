library(data.table)

# read data ----

dt <- fread("./data/heart.csv")

p <- ncol(dt)

cat(colnames(dt), sep = ', ')

# Valore unicos por variavel
print(lapply(dt, unique))

# boxplot
for (i in 1:p) {
  hist(dt[[i]])
}

plot(table(dt[[3]]))


hist(dt[[1]])
