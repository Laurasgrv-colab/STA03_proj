rm(list=objects())
graphics.off()

library(ggplot2)
library(FactoMineR)
library(factoextra)
library(cluster)
#library(Rmixmod)
#library(alluvial)

set.seed (1)


###
######################## Partie I ########################## 
###
#setwd(dir = "C:/Users/laura/Desktop/Lauu/ENSTA/2A/info/STA03_proj")
#setwd(dir = "C:/Users/laura/Desktop/Lauu/ENSTA/2A/info/STA03_proj")
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

pdf("music-corrplot.pdf", width = 20, height = 20)
par(mfrow=c(1,1))
corrplot::corrplot(cor(df[1:(p-1)]), method = "square", tl.cex = 0.5)
dev.off()

## Proportion
tb = table(df$GENRE)
tb/sum(tb)
#     Blues Classical      Jazz       Pop      Rock 
# 0.1381706 0.2982118 0.2619323 0.1328959 0.1687894 


## Variables PAR_SC_V et PAR_ASC_V
summary(df$PAR_SC_V)
summary(df$PAR_ASC_V)
boxplot(df$PAR_SC_V)
boxplot(df$PAR_ASC_V)
# Il y a un écart de 10^4 entre le min et le max de chaque variable
# De plus, le boxplot a du mal a analyser la répartition des valeurs
# Essayons de passer au log
df$PAR_SC_V = log(df$PAR_SC_V)
df$PAR_ASC_V = log(df$PAR_ASC_V)
summary(df$PAR_SC_V)
summary(df$PAR_ASC_V)
boxplot(df$PAR_SC_V)
boxplot(df$PAR_ASC_V)
# Les résultats sont plus visuels

## Variables 148 a 167 
duplicates = duplicated(t(df))
which(duplicates)
# PAR_MFCCV1  PAR_MFCCV2  PAR_MFCCV3  PAR_MFCCV4  PAR_MFCCV5  PAR_MFCCV6  PAR_MFCCV7  PAR_MFCCV8 
# 148         149         150         151         152         153         154         155 
# PAR_MFCCV9 PAR_MFCCV10 PAR_MFCCV11 PAR_MFCCV12 PAR_MFCCV13 PAR_MFCCV14 PAR_MFCCV15 PAR_MFCCV16 
# 156         157         158         159         160         161         162         163 
# PAR_MFCCV17 PAR_MFCCV18 PAR_MFCCV19 PAR_MFCCV20 
# 164         165         166         167 

# Comme indiqué dans la description du jeu de données o) parameters 148-167: the same as 128-147
# les variables sont dupliquées. Nous allons donc les supprimer.

df = df[,-148:-167]
dim(df)
# on a 7773 observations et 172 variables maintenant
p = ncol(df)
n = nrow(df)



## Variables très corrélées
C = cor(df[,1:p-1])-diag(1,p-1)
which(C>0.99) # [1] 27353 27526 28033 28376
which(C>0.99, arr.ind = TRUE)
c(C[164,160], C[166,161], C[160,164], C[161, 166]) # grandes corrélations
# [1] 0.9957849 0.9937875 0.9957849 0.9937875

# On récupère les indices des colonnes à supprimer (la deuxième de chaque paire)
indices_to_remove <- which(C > 0.99, arr.ind = TRUE)
cols_to_drop <- unique(indices_to_remove[, 2]) 

# Affichons les noms pour vérifier
colnames(df)[cols_to_drop]

# Supprimons les variables très corrélées car elles sont redondantes
df <- df[, !(names(df) %in% cols_to_drop)]

p = ncol(df)

liste = c("PAR_ASE_M", "PAR_ASE_MV", "PAR_SFM_M", "PAR_SFM_MV")
mat_corr = cor(df[, liste], use = "complete.obs")
mat_corr
# Les variables PAR_ASE_M, PAR_ASE_MV, PAR_SFM_M et PAR_SFM_MV ne semblent pas très corrélées enre elles

boxplot(PAR_SFM_M ~ GENRE, data = df)
plot(df$PAR_ASE_M, df$PAR_ASE_MV, col = as.factor(df$GENRE)) 
# On ne voit rien de particulier

################
### Q2
################

res = PCA(df[,-p])
V = res$var
plot(res,choix="var", cex = 0.3, shadow = TRUE)  

# Représentation du jeu de données sur les deux premiers plans principaux
par(mfrow=c(1,2))  
plt1 = plot(res,axes = c(1,2), choix = "var", cex = 0.3, shadow = TRUE)
plt2 = plot(res,axes = c(2,3), choix = "var", cex = 0.3, shadow = TRUE)
cowplot::plot_grid(plt1, plt2, ncol = 2, nrow = 1)
# Ces plans ne permettent pas de bien discriminer les observations

################
### Q3
################

res2 = hclust(dist(df),method="ward.D2")

p = ncol(df)

pltw =lapply(2:4, function(k) fviz_cluster(hcut(df[,-p], k = k, hc_method = "ward.D2"),
                                           main = paste("Ward.D2 - k =", k)))
cowplot::plot_grid(pltw[[1]],pltw[[2]],pltw[[3]],nrow=1,ncol=3)
# à ré-essayer --------------------------------------------------------------------

# indice de silhouette
sil = silhouette(cutree(res1,k=4), dist(df[, -p]))
mean(sil[,3])         # indice moyen 
fviz_silhouette(sil)  # visualisation

# Comparer avec la classification définie par la variable GENRE dont vous tracerez également la silhouette.

################
### Q3 (2e essai)
################

# Calcul de la matrice de distance (nécessaire pour la silhouette)
# On utilise df[,-p] pour exclure la colonne GENRE du calcul de distance
dists = dist(df[, -p])

# Silhouette pour la classification hiérarchique (Ward) avec k=5 (car 5 genres) ou 6 ??
res_ward = hcut(df[, -p], k = 5, hc_method = "ward.D2")
sil_ward = silhouette(res_ward$cluster, dists)
fviz_silhouette(sil_ward, main = "Silhouette - Clustering Ward (k=5)")

#   cluster size ave.sil.width
# 1       1 7745          0.99
#2       2    3          0.27
#3       3   22          0.49
#4       4    2          1.00
#5       5    1          0.00

# Silhouette pour la classification réelle (GENRE)
# On transforme le facteur GENRE en vecteurs d'entiers pour la fonction silhouette
sil_genre = silhouette(as.integer(df$GENRE), dists)
fviz_silhouette(sil_genre, main = "Silhouette - Classification par GENRE")

# Comparaison des indices moyens
mean_sil_ward = mean(sil_ward[, 3])
mean_sil_genre = mean(sil_genre[, 3])

cat("Indice de silhouette moyen (Ward k=5) :", mean_sil_ward, "\n")
# Indice de silhouette moyen (Ward k=5) : 0.9903702 

cat("Indice de silhouette moyen (GENRE) :", mean_sil_genre, "\n")
# Indice de silhouette moyen (GENRE) : -0.5026282 

################
### Q4
################

set.seed(103)
train = sample(c(TRUE,FALSE),n,rep=TRUE,prob=c(2/3,1/3))

df_train = df[train == TRUE,]

###
######################## Partie II ########################## 
###

################
### Q1
################

#On filtre les échantillons pour ne garder que les genres Classical et Jazz

df_train_nouveau = df_train[df_train$GENRE %in% c("Classical", "Jazz"), ]
df_test = df[train == FALSE,] #à mettre q4?
df_test_nouveau = df_test[df_test$GENRE %in% c("Classical", "Jazz"), ]

### ModT
ModT = glm(GENRE~.,data = df_train_nouveau, family = binomial) # . si on a supprimé les variables non significatives question 1 
resT = summary(ModT)
### Mod1
var_sign1= names(which(resT$coefficients[,4][-1]<0.05))
nom_var_sign1=paste(var_sign1,collapse = "+")
#formula_mod1=as.formula(paste("GENRE ~",nom_var_sign1)
Mod1 = glm(GENRE~as.formula(nom_var_sign1), data=df_train_nouveau,family=binomial)
#peut-être mettre as.formula devant nom_var_sign1

### Mod2
var_sign2= names(which(resT$coefficients[,4][-1]<0.2))
nom_var_sign2=paste(var_sign2,collapse = "+")
Mod2 =glm(GENRE~nom_var_sign2, data=df_train_nouveau,family=binomial)

### ModAIC
