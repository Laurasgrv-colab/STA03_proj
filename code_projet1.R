rm(list=objects())
graphics.off()

library(ggplot2)
library(FactoMineR)
library(factoextra)
library(cluster)
library(MASS)
library(glmnet)


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
# Les variables PAR_ASE_M, PAR_ASE_MV, PAR_SFM_M et PAR_SFM_MV ne semblent pas très corrélées entre elles

summary(df$PAR_SFM_MV)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0002265 0.0005505 0.0006296 0.0006291 0.0007101 0.0012263 
summary(df$PAR_SFM_M)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.02342 0.05626 0.06263 0.06221 0.06861 0.09044 
summary(df$PAR_ASE_MV)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 6.486e-05 3.261e-04 3.951e-04 4.401e-04 5.042e-04 1.457e-03 
summary(df$PAR_ASE_M)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.1692 -0.1646 -0.1634 -0.1635 -0.1623 -0.1575 

# Faible dispersion: Pour PAR_SFM_M et PAR_ASE_M, l'écart entre le premier et 
# le troisième quartile est extrêmement réduit. Les données sont assez homogènes
# dans notre dataset.
# On remarque également des ordres de grandeur très différents : PAR_ASE_M est négative (autour de -0.16), 
# tandis que PAR_SFM_MV est proche de 0.0006.

# Cette différence d'échelle montre qu'une normalisation (centrage-réduction) sera 
# indispensable pour la suite du projet, notamment pour la classification hiérarchique 
# (Question 3), afin d'éviter qu'une variable n'écrase les autres par son simple poids numérique.

df_scaled <- scale(df[, -p]) # On centre et réduit

################
### Q2
################

res = PCA(df_scaled)
V = res$var

plot(res,choix="var", cex = 0.3, shadow = TRUE)
#  Bonne représentation dans le premier plan principal: 30,45% de la variance totale
# et flèches longues.


# Représentation du jeu de données sur les deux premiers plans principaux
pdf("PCA_1et2.pdf")
par(mfrow=c(1,2))  
plt1 = plot(res,axes = c(1,2), choix = "var", cex = 0.3, shadow = TRUE)
plt2 = plot(res,axes = c(2,3), choix = "var", cex = 0.3, shadow = TRUE)
cowplot::plot_grid(plt1, plt2, ncol = 2, nrow = 1)
dev.off()
# Ces plans ne permettent pas de bien discriminer les observations, 
# mais confirment la corrélation vue précédemment (flèches regroupées).


################
### Q3
################

# Calcul de la matrice de distance (nécessaire pour la silhouette)
# On utilise df[,-p] pour exclure la colonne GENRE du calcul de distance
p = ncol(df)
dists = dist(df_scaled)

# Silhouette pour la classification hiérarchique (Ward) avec k=5 (car 5 genres)
# unique(df$GENRE)
res_ward = hcut(df_scaled, k = 5, hc_method = "ward.D2")
sil_ward = silhouette(res_ward$cluster, dists)
fviz_silhouette(sil_ward, main = "Silhouette - Clustering Ward (k=5)")

# Avant normalisation, le clustering n'avait pas réussi à séparer les genres musicaux. 
# Il avait simplement isolé quelques valeurs aberrantes (outliers) très éloignées des autres 
# et avait mis tout le reste dans un seul et même groupe.
#   cluster size ave.sil.width
#1       1 7745          0.99
#2       2    3          0.27
#3       3   22          0.49
#4       4    2          1.00
#5       5    1          0.00
# Après normalisation nous obtenons:
# cluster size ave.sil.width
# 1       1 1516         -0.04
# 2       2 2244         -0.01
# 3       3 2489          0.08
# 4       4  766          0.10
# 5       5  758          0.25


## Silhouette pour la classification réelle (GENRE)
# On transforme le facteur GENRE en vecteurs d'entiers pour la fonction silhouette
sil_genre = silhouette(as.integer(df$GENRE), dists)
fviz_silhouette(sil_genre, main = "Silhouette - Classification par GENRE")
# cluster size ave.sil.width
# 1       1 1074          0.03
# 2       2 2318          0.12
# 3       3 2036         -0.10
# 4       4 1033          0.00
# 5       5 1312          0.16

# Comparaison des indices moyens
mean_sil_ward = mean(sil_ward[, 3])
mean_sil_genre = mean(sil_genre[, 3])

cat("Indice de silhouette moyen (Ward k=5) :", mean_sil_ward, "\n")
# Indice de silhouette moyen (Ward k=5) : 0.07058402  

cat("Indice de silhouette moyen (GENRE) :", mean_sil_genre, "\n")
# Indice de silhouette moyen (GENRE) : 0.04031123  

# Les scores sont très bas: les clusters se chevauchent. 
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

#df_train_nouv_scaled <- scale(df_train_nouveau[,-p])

df_test = df[train == FALSE,]
df_test_nouveau = df_test[df_test$GENRE %in% c("Classical", "Jazz"), ]
# Ils contiennent bien 2851 et 1503 observations.

#df_test_nouv_scaled <- scale(df_test_nouveau[,-p])


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

df_train_nouveau$GENRE <- droplevels(df_train_nouveau$GENRE) 
df_test_nouveau$GENRE <- droplevels(df_test_nouveau$GENRE) 
# pour garder seulement jazz et classical en mémoire (et non pas 3 genres en plus avec 0 observations)

#Sur l'échantillon d'apprentissage : 
predprobaT_train=predict(ModT,type="response", data = df_train_nouveau)
predT_train = prediction(predprobaT_train,df_train_nouveau$GENRE)
ROCT_train = performance(predT_train,"tpr","fpr")

par(mfrow=c(1,1))  
plot(ROCT_train,main="ModT Apprentissage")



#Sur l'échantillon de test : 
predprobaT_test=predict(ModT,type="response", newdata = df_test_nouveau)
predT_test = prediction(predprobaT_test,df_test_nouveau$GENRE)
ROCT_test = performance(predT_test,"tpr","fpr")
plot(ROCT_test,main="ModT Test")

#Superposition
pdf("Superposition Courbes ROC ModT.pdf")
plot(ROCT_train,main="Courbes ROC ModT")
plot(ROCT_test,col="red",add=TRUE)
lines(c(0,1),c(0,1),lty=2, col = 'blue')          # règle aléatoire 
segments(x0=0,y0=1,x1=1,y1=1,lty=2 , col = 'green') # règle parfaite
segments(x0=0,y0=0,x1=0,y1=1,lty=2 , col = 'green') 
legend(0.5,0.4,legend=c("ModT : Apprentissage", "ModT : Données" , "Règle aléatoire", "Règle parfaite"),col=c("red","black","blue","green"),lty=c(1,1,2,2))
dev.off()


## Autres modeles sur echantillon test
#ModT
perfT = performance(predT_test, "auc") # Calcul de l'AUC (Area Under the Curve) (aire sous la courbe)
AUCT=round(unlist(perfT@y.values),4) 
plot(ROCT_test,col="purple",main="Courbes ROC : échantillon Test")


#Mod1
predproba1_test=predict(Mod1,type="response",newdata=df_test_nouveau)
pred1_test = prediction(predproba1_test,df_test_nouveau$GENRE)
ROC1_test = performance(pred1_test,"tpr","fpr")

perf1 = performance(pred1_test, "auc")
AUC1=round(unlist(perf1@y.values),4)

plot(ROC1_test,col="red",add=TRUE)

#Mod2
predproba2_test=predict(Mod2,type="response",newdata=df_test_nouveau)
pred2_test = prediction(predproba2_test,df_test_nouveau$GENRE)
ROC2_test = performance(pred2_test,"tpr","fpr")

perf2 = performance(pred2_test, "auc")
AUC2=round(unlist(perf2@y.values),4)

plot(ROC2_test,col="green",add=TRUE)

#ModAIC
predprobaAIC_test=predict(ModAIC,type="response",newdata=df_test_nouveau)
predAIC_test = prediction(predprobaAIC_test,df_test_nouveau$GENRE)
ROCAIC_test = performance(predAIC_test,"tpr","fpr")

perfAIC = performance(predAIC_test, "auc")
AUCAIC=round(unlist(perfAIC@y.values),4)

plot(ROCAIC_test,col="black",add=TRUE)

abline(a=0, b=1, lty=2, col="blue") # Règle aléatoire
segments(x0=0,y0=1,x1=1,y1=1,lty=2 , col = 'grey') # Règle parfaite
segments(x0=0,y0=0,x1=0,y1=1,lty=2 , col = 'grey') 

legend(0.45,0.6,
       legend=c(paste("ModT(AUC =", AUCT, ")"), paste("Mod1(AUC =", AUC1, ")"), 
                         paste("Mod2(AUC =", AUC2, ")"), paste("ModAIC(AUC =", AUCAIC, ")"), 
                         "Aléatoire (AUC = 0.5)", "Parfaite (AUC = 1)"),
       col=c("purple", "red","green","black", "blue"),lty=c(1,1,1,1,1, 2))

# L'aire sous la courbe (AUC) la plus élevée est obtenue pour ModAIC (AUC = 0,9633)
# Sa courbe (en noir) est la plus proche du coin supérieur gauche, 
# ce qui signifie qu'il offre le meilleur compromis entre le taux de vrais positifs 
# et le taux de faux positifs.
# Même s'il est très proche du ModT (AUC = 0,962), 
# le ModAIC est préférable car il a été optimisé par une procédure de sélection stepwise, 
# ce qui réduit le nombre de variables et limite les risques de sur-apprentissage.

# Vérifions son adéquation: 
# Cf. TP9

error_classif=function(data,modele,seuil=0.5)
{
  predproba = predict(modele,type="response",newdata=data)
  glm.pred = ifelse(predproba>seuil,1,0)
  return(mean((as.numeric(data$GENRE)-1 - glm.pred)^2))
}

##ModT
#Apprentissage
error_classif(df_train_nouveau,ModT) # 0.06839705
#Test
error_classif(df_test_nouveau,ModT) # 0.09447771
##Mod1
#Apprentissage
error_classif(df_train_nouveau,Mod1) # 0.1536303
#Test
error_classif(df_test_nouveau,Mod1) # 0.1696607
##Mod2
#Apprentissage
error_classif(df_train_nouveau,Mod2) # 0.1459137
#Test
error_classif(df_test_nouveau,Mod2) # 0.1556886
##ModAIC
#Apprentissage
error_classif(df_train_nouveau,ModAIC) # 0.07050158
#Test
error_classif(df_test_nouveau,ModAIC) # 0.09314704


# Sur l'échantillon d'apprentissage, ModT a une erreur légèrement plus faible que ModAIC (0,068 contre 0,070). 
# C'est normal : ModT utilise plus de variables, il "colle" donc plus aux données d'entraînement.
# Bien que proche de celle de ModT, l'erreur de classification de ModAIC 
# reste la plus faible sur l'échantillon de test. Le modèle retenu sera donc ModAIC.

# Remarque: ModAIC était bien plus long à calculer que ModT !



################
### Q3
################

# Avec 168 variables, un modèle classique risque de capturer le "bruit" des données 
# plutôt que le signal réel. La pénalité $L_2$ de Ridge "écrase" les coefficients vers zéro 
# sans les annuler, ce qui réduit la complexité du modèle et améliore sa capacité de généralisation sur de nouvelles données.
# Ridge permet donc de trouver un compromis optimal : on accepte un léger biais 
# (en ne laissant pas les coefficients s'ajuster parfaitement aux données d'apprentissage) 
# pour obtenir une réduction massive de la variance, garantissant ainsi des prédictions plus robustes.

library(glmnet)

# Normalisation indispensable pour ridge:
df_scaled_nouveau <- scale(df_train_nouveau[,-p])

grid = 10^seq(10, -2, length = 100) # la grille de lambda
x = as.matrix(df_scaled_nouveau)
y = df_train_nouveau$GENRE

ridge.fit = glmnet(x,y,alpha=0,lambda=grid, family = "binomial")

dim(coef(ridge.fit)) # [1] 168 100

## Cas lambda = 10^10 : très fortement contraint, régularisé
coef(ridge.fit)[, 1] # Avec 168 coefficients, le résultat est difficilement exploitable
# Extraction des coefficients pour le premier lambda (le plus grand)
# On convertit en vecteur numérique et on enlève l'Intercept (ligne 1)
coeffs_lambda1 <- as.vector(coef(ridge.fit)[, 1])[-1]
names(coeffs_lambda1) <- rownames(coef(ridge.fit))[-1]
# On les trie par valeur absolue (pour voir l'importance réelle, positive ou négative)
coeffs_tries <- sort(abs(coeffs_lambda1), decreasing = TRUE)
# Affichons les 10 variables les plus importantes
head(coeffs_tries, 10) # Valeurs très faibles, de l'ordre de 10^-11
# Le modèle prédira presque toujours la moyenne de $y$. 
# C'est du sous-apprentissage, le biais est très élevé, mais la variance est nulle.


# Cas lambda = 10^-2 : pénalité négligeable, liberté totale
coef(ridge.fit)[, 100]
# On fait de même:
coeffs_lambda2 <- as.vector(coef(ridge.fit)[, 100])[-1]
names(coeffs_lambda2) <- rownames(coef(ridge.fit))[-1]
# On les trie par valeur absolue (pour voir l'importance réelle, positive ou négative)
coeffs_tries <- sort(abs(coeffs_lambda2), decreasing = TRUE)
# Affichons les 10 variables les plus importantes
head(coeffs_tries, 10) # de l'ordre de 10^-1
# Le modèle est très complexe. Le risque est le sur-apprentissage.
# Dans le cercle des corrélations (ACP), ces variables (SFM, ASE, MFCC) étaient 
# celles qui avaient les flèches les plus longues. Ridge confirme ici 
# ce que l'ACP montrait visuellement : ce sont elles qui portent l'essentiel de la variance explicative du genre.

pdf("coefficients_ridge_surlambda.pdf")
par(mfrow=c(1,1))  
plot(ridge.fit, xvar = "lambda", label = TRUE)
dev.off()
# Plus lambda diminue plus les coefficientss s'expriment et se rapprochent 
# de la valeur de la regression linéaire classique


################
### Q4
################
library(forecast)    

set.seed(123)

#x = as.matrix(df_train_nouveau[, colnames(df_train_nouveau) != "GENRE"])
x = as.matrix(df_scaled_nouveau)

train = sample(1:nrow(x), 2*nrow(x)/3) # proportion 1/3 -test et 2/3 -apprentissage
test= -train

cv.out = cv.glmnet(x[train,],as.numeric(y[train]) - 1,alpha=0,nfolds = 10,lambda=grid)

pdf("Fig-Ridge-cv.pdf")
par(mfrow=c(1,1))
plot(cv.out)
dev.off()

## Algorithme de cv.glmnet:
# Il effectue une validation croisée en k segments (nfolds = 10):  
# Premièrement, il divise l'échantillon train en 10 groupes égaux.
# Pour chaque valeur de lambda de grid, il entraîne le modèle sur 9 groupes 
# et calcule l'erreur sur le 10ème (groupe de validation).
# Il répète l'opération 10 fois (chaque groupe sert de test une fois). 
# Ensuite, Il calcule l'erreur moyenne et son écart-type (les barres grises sur la figure) 
# pour chaque lambda.
# Enfin, il retient le lambda qui offre la meilleure stabilité prédictive.


## Interprétation du graphique 
# Partie gauche du graphique (Grand lambda) : L'erreur est élevée et constante. 
# La pénalité est trop forte, les coefficients sont écrasés vers 0. 
# Le modèle est trop simple et ne parvient pas à apprendre : c'est le sous-apprentissage.

# Partie droite du graphique (Petit lambda) : On observe que l'erreur (MSE) chute 
# rapidement à mesure que la pénalité lambda diminue.
# Le modèle gagne en liberté et commence à capturer 
# les caractéristiques discriminantes entre le "Jazz" et le "Classical".
# Le modèle a besoin d'une régularisation faible pour être performant sur ces données. 
# Le nombre 167 en haut confirme que Ridge conserve toutes les variables, 
# stabilisant leurs coefficients sans les annuler.

# La première ligne verticale en pointillés à gauche correspond à lambda.min : 
# c'est la valeur de lambda qui minimise l'erreur de validation croisée.
# La seconde ligne (plus à droite) correspond à lambda.1se : 
# c'est le modèle le plus simple (plus régularisé) dont l'erreur reste à moins 
# d'un écart-type du minimum


## lambda minimisant l'erreur de prédiction par VC
meilleur_lam = cv.out$lambda.min # 0.01
log(meilleur_lam) # -4.60517

# Estimation de l'erreur sur l'échantillon de test
ridge.prob = predict(cv.out, s = meilleur_lam, newx = x[test,], type = "response")
ridge.pred = ifelse(ridge.prob > 0.5, 1, 0)

y_test_reel = as.numeric(as.factor(df_train_nouveau$GENRE[test])) - 1

erreur_ridge = mean(ridge.pred != y_test_reel)
print(erreur_ridge) # 0.09568875 (soit environ 9,6% d'erreur)
# Notons que ModAIC affichait une erreur de 9,31% et ModT environ 9,44%. 
# Ainsi, obtenir 9,6% avec la régression Ridge semble cohérent.

# Remarque: ModAIC était bien plus long à calculer que Ridge, et les erreurs sont proches...




################
### Q5
################

# Idée:
# À corriger !!!!!

pred_ridge = prediction(ridge.prob, df_test_nouveau$GENRE)
ROC_ridge = performance(pred_ridge, "tpr", "fpr")
AUC_ridge = performance(pred_ridge, "auc")@y.values[[1]]

# On trace d'abord la base (ModT Test ou ModAIC Test)
plot(ROCT_test, col="blue", main="Superposition des courbes ROC (Test)")

# On ajoute la courbe Ridge en une nouvelle couleur (ex: orange)
plot(ROC_ridge, col="orange", add=TRUE)

# On ajoute les autres si nécessaire (ModAIC par exemple)
predprobaAIC_test = predict(ModAIC, newdata = df_test_nouveau, type="response")
predAIC_test = prediction(predprobaAIC_test, df_test_nouveau$GENRE)
ROCAIC_test = performance(predAIC_test, "tpr", "fpr")
plot(ROCAIC_test, col="green", add=TRUE)

# 4. Légende pour comparer
legend("bottomright", legend=c(
  paste("ModT (AUC =", round(performance(predT_test,"auc")@y.values[[1]], 4), ")"),
  paste("ModAIC (AUC =", round(performance(predAIC_test,"auc")@y.values[[1]], 4), ")"),
  paste("Ridge (AUC =", round(AUC_ridge, 4), ")")
), col=c("blue", "green", "orange"), lty=1)