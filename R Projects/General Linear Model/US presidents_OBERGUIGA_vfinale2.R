

rm(list=ls())

#Importation et nettoyage des données
library(readxl)
d1=read_excel("C:/Users/oussa/Downloads/Data Science/Master Statistique Big Data Dauphine/Module 2/Modèle Linéaire Généralisé/US presidents.xls",sheet=1)

d2=read_excel("C:/Users/oussa/Downloads/Data Science/Master Statistique Big Data Dauphine/Module 2/Modèle Linéaire Généralisé/US presidents.xls",sheet=2)

colnames(d1)=c("Année","Nom","Taille","Naissance","President","VP","Gov","Senate","House","StateLeg","General","Cabinet","Party")
colnames(d2)=c("Année","Nom","Taille","Naissance","President","VP","Gov","Senate","House","StateLeg","General","Cabinet","Party")

d1=d1[1:31,]
d2=d2[1:31,]
d1$Elu=1
d2$Elu=0

US_presidents=rbind(d1,d2)




US_presidents[,c(6,7,8,9,10,11,12)]=lapply(US_presidents[,c(6,7,8,9,10,11,12)],as.numeric)


US_presidents[is.na(US_presidents)]=0
#afin d'éviter un probleme de suraprentissage, je n'utilise pas l'Année de l'Election ni la date de Naissance directement, je génère l'âge des candidats

US_presidents$Age=US_presidents$Année-US_presidents$Naissance

US_presidents$Party[US_presidents$Party =="R "]="R"
US_presidents$Party[US_presidents$Party =="D "]="D"

#exploration rapide des données

head(US_presidents)
str(US_presidents)
summary(US_presidents)
attach(US_presidents)

boxplot(Age~Elu)
boxplot(Taille~Elu)
mosaicplot(Elu~Party,col=c("red","blue"))
boxplot(Age~Party)


nrow(US_presidents[US_presidents$Elu== 1 & US_presidents$President> 0,])/(nrow(US_presidents[US_presidents$Elu== 1 & US_presidents$President> 0,])+nrow(US_presidents[US_presidents$Elu== 0 & US_presidents$President> 0,]))

#la plupart (75 %) des présidents ont été reconduits pour un second mandat
#les Républicains sont plus agés que les Démocrates
#les présidents élus sont majoitairement Républicains, plus âgés et plus grands

#Choix des variables en tatonnant(suppression progressive des moins bons coefficients de Student,on retire les champs "Année","Nom" et "Naissance")
#on part de reg=glm(Elu~Taille+President+VP+Gov+Senate+House+StateLeg+General+Party+Cabinet+Age,family=binomial,data=US_presidents)


reg=glm(Elu~Taille+Senate+StateLeg+Age,family=binomial,data=US_presidents)
summary(reg)


#Utilsiation des méthodes pas à pas
modele_complet=glm(Elu~.-Naissance-Nom-Année,family=binomial,data=US_presidents)

library(MASS)

choix_modele_backward=step(modele_complet,direction="backward")
choix_modele_backward

choix_modele_forward=step(modele_complet,direction="forward")
choix_modele_forward

choix_modele_both=step(modele_complet,direction="both")
choix_modele_both

reg_step=glm(Elu~Taille+Senate+StateLeg+General,family=binomial,data=US_presidents)
summary(reg_step)

#Contradiction : p-value= 0.99 pour la covariable "General" mais meilleur AIC obtenu.. 

#Utilisation d'autres méthodes de sélection de variables 
library(leaps)
library(bestglm)

US_presidents$Party=as.factor(US_presidents$Party)

regsubsets(x=US_presidents[,-c(1,2,4,14)],y=US_presidents[,c(14)],nvmax=ncol(US_presidents[,-c(1,2,4,14)]))

#regsubsets ne marche pas ..

Xy=cbind(US_presidents[,-c(1,2,4,14)],US_presidents[,c(14)])
choix_modele_best_BIC=bestglm(Xy,IC="BIC",family=gaussian)
choix_modele_best_BIC
choix_modele_best_BIC$Subsets

choix_modele_best_AIC=bestglm(Xy,IC="AIC",family=gaussian)
choix_modele_best_AIC
choix_modele_best_AIC$Subsets




reg_best_AIC=glm(Elu~Senate+Taille+StateLeg+Age,family=binomial,data=US_presidents)
summary(reg_best_AIC)

regBIC=glm(Elu~Senate+Taille,family=binomial,data=US_presidents)
summary(regBIC)

#test de déviance


reg=glm(Elu~Taille+Senate+StateLeg+Age,family=binomial,data=US_presidents)
summary(reg)
1-pchisq(85.950-70.465,4)
1-pchisq(70.465,57)


reg_step=glm(Elu~Taille+Senate+StateLeg+General,family=binomial,data=US_presidents)
summary(reg_step)
1-pchisq(85.950-69.557,4)
1-pchisq(69.557,57)


reg_best_AIC=glm(Elu~Senate+Taille+StateLeg+Age,family=binomial,data=US_presidents)
summary(reg_best_AIC)

1-pchisq(85.950-70.465,4)
1-pchisq(70.465,57)

#selon le test de déviance le meilleur modèle semble être reg_step car la p-valeur 1 est la plus faible (on réfute l'hypothese à 1 degré de liberté pour les pi) et la p valeur 2 la plus élevée (on réfute l'hypothese de modèle saturé)
#c'est ce modèle qui est retenu par la suite

#split train test aléatoire du modèle


#Tirage aléatoire
?sample
indexes = sample(1:nrow(X), size=0.8*nrow(X))

# Split data
Train = US_presidents[indexes,c("Taille","Senate","StateLeg","General","Elu")]
dim(Train)  
Test = US_presidents[-indexes,c("Taille","Senate","StateLeg","General","Elu")]
dim(Test) 

#test du modèle

reg_test=glm(Elu~Taille+Senate+StateLeg+General,family=binomial,data=Train)
summary(reg_test)

?predict
predict(reg_test,new=Test[,c("Taille","Senate","StateLeg","General")],type="response")

t=(predict(reg_test,new=Test[,c("Taille","Senate","StateLeg","General")],type="response")>=0.5)
TC=table(t,Test[,c("Elu")])
(TC[1,1]+TC[2,2])/sum(TC)
#Taux de succès à 46%.. c'est faible 



#graphique illustratif sur 2 composantes les plus significatives

attach(US_presidents)
plot(Taille,Senate,col=ifelse(Elu==1,"green","red"))
abline(-reg_step$coefficients[1]/reg_step$coefficients[3],-reg_step$coefficients[2]/reg_step$coefficients[3])

#test de corrélations entre covariables

cor(data.matrix(US_presidents[,c(5,6,7,8,9,10,11,12,15)]))



#il ne semble pas y avoir de corrélation entre les variables explicatives quantitatives

library(FactoMineR)


res.pca = PCA(US_presidents[,-c(2,13,14)], scale.unit=TRUE, ncp=5, graph=T)




#pour aller plus loin comparaison avec d'autres algorithmes vus en cours de "Introducation à la Classification Supervisée : classifacateur gaussien, classifiteur quadratique et arbre de décisions

#analyse discriminante linéaire (classificateur bayesien)
reg_lda=lda(Elu~.,prior=c(0.5,0.5),Train)
t=predict(reg_lda,Test)$class
TC=table(t,Test[,c("Elu")])
(TC[1,1]+TC[2,2])/sum(TC)

#On a légèrement amélioré le score

#analyse discriminante quadratique
reg_qda=qda(Elu~.,prior=c(0.5,0.5),Train)
t=predict(reg_qda,Test)$class
TC=table(t,Test[,c("Elu")])
(TC[1,1]+TC[2,2])/sum(TC)

#arbre de décision
library(rpart)
reg_arbre=rpart(Elu~.,data=Train,method="class")
text(reg_arbre)

t=predict(reg_arbre,newdata=Test,type="class")

TC=table(t,Test[,c("Elu")])
(TC[1,1]+TC[2,2])/sum(TC)



#Conclusion :
#les meilleures covariables qui expliquent l'élection ou non sont : Taille,StateLeg,General et Senate.L'AIC obtenu est de 79,6.
#les covariables Taille, StateLeg et General influent positivement sur la probabiltié d'êter élu,
#la variable Senate influe négativement
#le classificateur bayesien (linéaire ou quadratique) semble obtenir le meilleur résulat, mais qui reste faible.
#On a un modèle peu performant (54% de taux de succès), ce qui s'explique par : la faible quantité de données disponibles (une soixantaine d'observations),corrrélation entre Age et Party, 
#et l'absence de variables explicatives(Sexe,Ethnie,Niveau d'éducation,etc.)
#Concernant la variable Général, le cas unique du Général Eisenhower fausse le résultat, l'échantillon n'est pas assez important pour que cette variable soit significative
#Je ne comprends pas pourquoi la covariable "President" n'est pas retenu sachant qu'une majorité des présidents a été reconduit pour un second mandat..De même pour la variable "Party"
#On peut aussi remarquer que les observations ne sont pas indépendantes dans le temps 
#(probabilité d'une alternance politique entre 2 élections,probabilité qu'un président élu soit rélu pour un second mandat, etc.)
#les conditions de l'expérience statistique ne sont pas identiques (présidents décédés ou démission en cours de mandat, modification de la constitution après Roosevelt pour empecher plus de 2 mandats consécutifs,crise économique,guerre mondiale,guerre froide etc.)