rm(list=objects())
graphics.off()

library(ggplot2)
library(FactoMineR)
library(factoextra)
library(cluster)
library(MASS)

#library(Rmixmod)
#library(alluvial)

set.seed (1)


###
######################## Partie I ########################## 
###
setwd(dir = "C:/Users/laura/Desktop/Lauu/ENSTA/2A/info/STA03_proj")
df = read.table("Music_2026.txt", header=TRUE, sep=";",dec='.')

df$GENRE = as.factor(df$GENRE)

#  # Vérifions que GENRE est un facteur et supprimons les lignes avec des NA
#  df <- na.omit(df) # Supprime les lignes contenant des valeurs manquantes
#  df$GENRE <- as.factor(df$GENRE)


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
which(abs(C)>0.99) # [1] 27353 27526 28033 28376
which((C)>0.99, arr.ind = TRUE)
c(C[164,160], C[166,161], C[160,164], C[161, 166]) # grandes corrélations
# [1] 0.9957849 0.9937875 0.9957849 0.9937875

# On récupère les indices des colonnes à supprimer (la deuxième de chaque paire)
indices_to_remove = which(abs(C) > 0.99, arr.ind = TRUE)
cols_to_drop = unique(indices_to_remove[, 2]) 

# Affichons les noms pour vérifier
colnames(df)[cols_to_drop]

# Supprimons les variables très corrélées car elles sont redondantes
df_clean = df[, -cols_to_drop]
df = df_clean

p = ncol(df)

liste = c("PAR_ASE_M", "PAR_ASE_MV", "PAR_SFM_M", "PAR_SFM_MV")
mat_corr = cor(df[, liste], use = "complete.obs")
mat_corr
# Les variables PAR_ASE_M, PAR_ASE_MV, PAR_SFM_M et PAR_SFM_MV ne semblent pas très corrélées enre elles

boxplot(PAR_SFM_MV ~ GENRE, data = df)
boxplot(PAR_SFM_M ~ GENRE, data = df)
boxplot(PAR_ASE_M ~ GENRE, data = df)
boxplot(PAR_ASE_MV ~ GENRE, data = df)

plot(df$PAR_ASE_M, df$PAR_ASE_MV, col = as.factor(df$GENRE)) 
plot(df$PAR_SFM_M, df$PAR_SFM_MV, col = as.factor(df$GENRE)) 

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

pltw =lapply(5, function(k) fviz_cluster(hcut(df[,-p], k = k, hc_method = "ward.D2"),
                                           main = paste("Ward.D2 - k =", k)))
pltw
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
p = ncol(df)
dists = dist(df[, -p])

# Silhouette pour la classification hiérarchique (Ward) avec k=5 (car 5 genres)
# unique(df$GENRE)
res_ward = hcut(df[, -p], k = 5, hc_method = "ward.D2")
sil_ward = silhouette(res_ward$cluster, dists)
fviz_silhouette(sil_ward, main = "Silhouette - Clustering Ward (k=5)")

#   cluster size ave.sil.width
#1       1 7745          0.99
#2       2    3          0.27
#3       3   22          0.49
#4       4    2          1.00
#5       5    1          0.00

# Le clustering n'a pas réussi à séparer les genres musicaux. 
# Il a simplement isolé quelques valeurs aberrantes (outliers) très éloignées des autres 
# et a mis tout le reste dans un seul et même groupe.
# Essayons de centrer et réduire les variables pour éviter ce phénomène.

df_scaled <- scale(df[, -p]) # On centre et réduit
dists = dist(df_scaled)
res_ward = hcut(df_scaled, k = 5, hc_method = "ward.D2")
sil_ward = silhouette(res_ward$cluster, dists)
fviz_silhouette(sil_ward, main = "Silhouette - Clustering Ward (k=5)")
# Les résultats sont bien plus satisfaisants.



## Silhouette pour la classification réelle (GENRE)
# On transforme le facteur GENRE en vecteurs d'entiers pour la fonction silhouette
sil_genre = silhouette(as.integer(df$GENRE), dists)
fviz_silhouette(sil_genre, main = "Silhouette - Classification par GENRE")

# Comparaison des indices moyens
mean_sil_ward = mean(sil_ward[, 3])
mean_sil_genre = mean(sil_genre[, 3])

cat("Indice de silhouette moyen (Ward k=5) :", mean_sil_ward, "\n")
# Indice de silhouette moyen (Ward k=5) : 0.07058402  

cat("Indice de silhouette moyen (GENRE) :", mean_sil_genre, "\n")
# Indice de silhouette moyen (GENRE) : -0.04031123  

# Les scoressont très bas: les clusters se chevauchent. 
# Ceci peut être expliqué en partie par des morceaux de "Jazz-Rock" ou de "Pop-Rock" à cheval entre deux groupes.
# De plus, le grand nombre de données (168) fait que tous les points 
# se retrouvent à des distances similaires les uns des autres.

# On remarque également que Ward fait un peu mieux que le Genre (0.07 > 0.04), 
# ce qui indique qu'il existe une structure dans les données, 
# même si elle ne correspond pas exactement aux étiquettes des genres musicaux.

# Enfin, ce faible score justifie l'utilisation de méthodes plus puissantes 
# comme la régression logistique ou les modèles de mélange que nous verrons par la suite.

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
df_test = df[train == FALSE,]
df_test_nouveau = df_test[df_test$GENRE %in% c("Classical", "Jazz"), ]
# Ils contiennent bien 2851 et 1503 observations.

### ModT
ModT = glm(GENRE~.,data = df_train_nouveau, family = binomial) # . si on a supprimé les variables non significatives question 1 
resT = summary(ModT)

### Mod1
var_sign1= names(which(resT$coefficients[,4][-1]<0.05))
nom_var_sign1=paste(var_sign1,collapse = "+")
formula_1=paste("GENRE ~",nom_var_sign1)
formula_mod1=as.formula(formula_1)
Mod1 = glm(formula_mod1, data=df_train_nouveau,family=binomial)

### Mod2
var_sign2= names(which(resT$coefficients[,4][-1]<0.2))
nom_var_sign2=paste(var_sign2,collapse = "+")
formula_2=paste("GENRE ~",nom_var_sign2)
formula_mod2=as.formula(formula_2)
Mod2 =glm(formula_mod2, data=df_train_nouveau,family=binomial)

### ModAIC
ModAIC = stepAIC(ModT, direction = "both", trace = FALSE)
summary(ModAIC)
round(ModAIC$coef,4)  
# Modèle retenu
# (Intercept)                 PAR_TC               PAR_ASE1 
# 4.017408e+02          -5.297000e-01          -3.200088e+02 
# PAR_ASE3               PAR_ASE5               PAR_ASE6 
# -3.229232e+02          -3.356653e+02          -1.277462e+02 
# PAR_ASE7               PAR_ASE8               PAR_ASE9 
# -2.234951e+02          -1.953392e+02          -1.897731e+02 
# PAR_ASE10              PAR_ASE11              PAR_ASE12 
# -1.841722e+02          -2.230740e+02          -1.158250e+02 
# PAR_ASE13              PAR_ASE14              PAR_ASE15 
# -1.770242e+02          -2.208544e+02          -2.164849e+02 
# PAR_ASE16              PAR_ASE17              PAR_ASE18 
# -2.573883e+02          -2.507636e+02          -1.363516e+02 
# PAR_ASE19              PAR_ASE20              PAR_ASE21 
# -1.295356e+02          -2.854632e+02          -2.431016e+02 
# PAR_ASE22              PAR_ASE23              PAR_ASE24 
# -1.227824e+02          -1.246515e+02          -7.067640e+01 
# PAR_ASE25              PAR_ASE26              PAR_ASE27 
# -1.704210e+02          -2.166178e+02          -1.357561e+02 
# PAR_ASE28              PAR_ASE29              PAR_ASE31 
# -3.930570e+02          -2.566679e+02          -2.945783e+02 
# PAR_ASE32              PAR_ASE33              PAR_ASE34 
# -1.918572e+02           6.648523e+03          -6.984708e+03 
# PAR_ASE_M              PAR_ASEV1              PAR_ASEV2 
# 7.132336e+03           4.503135e+04           4.627690e+04 
# PAR_ASEV3              PAR_ASEV4              PAR_ASEV5 
# 4.152587e+04           4.627274e+04           4.456144e+04 
# PAR_ASEV6              PAR_ASEV7              PAR_ASEV8 
# 4.416120e+04           4.446861e+04           4.358042e+04 
# PAR_ASEV9             PAR_ASEV10             PAR_ASEV11 
# 4.415221e+04           4.455948e+04           4.435857e+04 
# PAR_ASEV12             PAR_ASEV13             PAR_ASEV14 
# 4.401401e+04           4.531181e+04           4.465301e+04 
# PAR_ASEV15             PAR_ASEV16             PAR_ASEV17 
# 4.461401e+04           4.508285e+04           4.468240e+04 
# PAR_ASEV18             PAR_ASEV19             PAR_ASEV20 
# 4.389647e+04           4.284917e+04           4.320916e+04 
# PAR_ASEV21             PAR_ASEV22             PAR_ASEV23 
# 4.378921e+04           4.441215e+04           4.549726e+04 
# PAR_ASEV24             PAR_ASEV25             PAR_ASEV26 
# 4.852548e+04           4.870720e+04           4.735826e+04 
# PAR_ASEV27             PAR_ASEV28             PAR_ASEV29 
# 4.563857e+04           4.936871e+04           4.188291e+04 
# PAR_ASEV30             PAR_ASEV31             PAR_ASEV32 
# 4.519467e+04           4.489741e+04           4.555407e+04 
# PAR_ASEV34             PAR_ASE_MV                PAR_ASC 
# 8.622330e+04          -1.517262e+06          -3.310300e+00 
# PAR_ASC_V                PAR_ASS              PAR_ASS_V 
# -5.580000e-01          -3.508600e+00           1.583470e+01 
# PAR_SFM1               PAR_SFM2               PAR_SFM3 
# 3.424222e+03           3.317112e+03           3.385255e+03 
# PAR_SFM4               PAR_SFM5               PAR_SFM6 
# 3.430242e+03           3.408331e+03           3.387009e+03 
# PAR_SFM7               PAR_SFM8               PAR_SFM9 
# 3.395653e+03           3.433338e+03           3.443551e+03 
# PAR_SFM10              PAR_SFM11              PAR_SFM12 
# 3.503018e+03           3.500144e+03           3.370007e+03 
# PAR_SFM13              PAR_SFM14              PAR_SFM15 
# 3.437785e+03           3.337116e+03           3.400500e+03 
# PAR_SFM16              PAR_SFM17              PAR_SFM18 
# 3.647436e+03           3.444270e+03           3.460939e+03 
# PAR_SFM19              PAR_SFM20              PAR_SFM21 
# 3.345324e+03           3.337802e+03           3.442207e+03 
# PAR_SFM22              PAR_SFM23              PAR_SFM_M 
# 3.527748e+03           3.384037e+03          -8.231387e+04 
# PAR_SFMV1              PAR_SFMV2              PAR_SFMV3 
# 2.069053e+06           2.034145e+06           2.069880e+06 
# PAR_SFMV4              PAR_SFMV5              PAR_SFMV6 
# 2.069711e+06           2.068479e+06           2.070420e+06 
# PAR_SFMV7              PAR_SFMV8              PAR_SFMV9 
# 2.069693e+06           2.069750e+06           2.070072e+06 
# PAR_SFMV10             PAR_SFMV11             PAR_SFMV12 
# 2.069809e+06           2.071646e+06           2.072596e+06 
# PAR_SFMV13             PAR_SFMV14             PAR_SFMV15 
# 2.070732e+06           2.068167e+06           2.068163e+06 
# PAR_SFMV16             PAR_SFMV17             PAR_SFMV18 
# 2.064686e+06           2.068788e+06           2.070356e+06 
# PAR_SFMV19             PAR_SFMV20             PAR_SFMV21 
# 2.065554e+06           2.072183e+06           2.069851e+06 
# PAR_SFMV22             PAR_SFMV23             PAR_SFMV24 
# 2.069002e+06           2.069437e+06           1.894888e+06 
# PAR_SFM_MV              PAR_MFCC3              PAR_MFCC4 
# -4.966303e+07          -8.198900e+00           3.285300e+00 
# PAR_MFCC6              PAR_MFCC8              PAR_MFCC9 
# -5.256600e+00           9.191300e+00          -3.295500e+00 
# PAR_MFCC10             PAR_MFCC11             PAR_MFCC12 
# 7.142400e+00          -7.543900e+00           8.481200e+00 
# PAR_MFCC15             PAR_MFCC16             PAR_MFCC17 
# 5.838200e+00           6.159400e+00           8.099000e+00 
# PAR_MFCC19             PAR_MFCC20       PAR_THR_2RMS_TOT 
# -8.798900e+00          -6.274100e+00          -1.308417e+02 
# PAR_THR_3RMS_TOT PAR_THR_1RMS_10FR_MEAN  PAR_THR_1RMS_10FR_VAR 
# -2.091768e+02           8.369890e+01           4.261496e+02 
# PAR_THR_2RMS_10FR_VAR PAR_THR_3RMS_10FR_MEAN  PAR_THR_3RMS_10FR_VAR 
# -5.957601e+03           5.913243e+02          -2.520829e+04 
# PAR_PEAK_RMS10FR_MEAN   PAR_PEAK_RMS10FR_VAR           PAR_3RMS_TCD 
# -6.890000e-02           1.000000e-04           8.626749e+02 
# PAR_1RMS_TCD_10FR_VAR PAR_2RMS_TCD_10FR_MEAN PAR_3RMS_TCD_10FR_MEAN 
# -9.715445e+02           1.841691e+02          -3.728549e+02

################
### Q2
################
library(ROCR)

#df_train_nouveau$GENRE <- droplevels(df_train_nouveau$GENRE) #chat on a vu ça en cours??? 

#Sur l'échantillon d'apprentissage : 
predprobaT_train=predict(ModT,type="response", data = df_train_nouveau)
predT_train = prediction(predprobaT_train,df_train_nouveau$GENRE)
ROCT_train = performance(predT_train,"tpr","fpr")
plot(ROCT_train,main="ModT Apprentissage")

#Sur l'échantillon de test : 
predprobaT_test=predict(ModT,type="response", newdata = df_test_nouveau)
predT_test = prediction(predprobaT_test,df_test_nouveau$GENRE)
ROCT_test = performance(predT_test,"tpr","fpr")
plot(ROCT_test,main="ModT Test")

#Superposition
plot(ROCT_train,main="Courbes ROC ModT")
plot(ROCT_test,col="red",add=TRUE)

#A FINIR