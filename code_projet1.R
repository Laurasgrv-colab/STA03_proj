rm(list=objects())
graphics.off()

library(ggplot2)
library(FactoMineR)

###
######################## Partie I ########################## 
###

df = read.table("Music_2026.txt", header=TRUE, sep=";",dec='.')
df$GENRE = as.factor(df$GENRE)
dim(df)
n = nrow(df)
p = ncol(df)
# On a 7773 observations et 192 variables

################
### Q1    
################
## Analyse univariée

summary(df)
str(df)
barplot(table(df$GENRE))

pdf("boxplots.pdf")
par(mfrow=c(3,3))
for (i in 1:(p-1)) {
  boxplot(df[,i] ~ df$GENRE, xlab = colnames(df)[i], horizontal = TRUE)
}
dev.off()

pdf("histo.pdf")
par(mfrow=c(3,3))
for (i in 1:(p-1)) {
  hist(df[,i], xlab = colnames(df)[i], horizontal = TRUE)
}
dev.off()

pdf("plots.pdf")
par(mfrow=c(3,3))
for (i in 1:(p-1)) {
  plot(df[,i],df$GENRE, xlab = colnames(df)[i])
}
dev.off()


## Analyse bivariée