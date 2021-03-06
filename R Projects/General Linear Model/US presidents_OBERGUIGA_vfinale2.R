

rm(list=ls())

#Importation et nettoyage des donn�es
library(readxl)
d1=read_excel("C:/Users/oussa/Downloads/Data Science/Master Statistique Big Data Dauphine/Module 2/Mod�le Lin�aire G�n�ralis�/US presidents.xls",sheet=1)

d2=read_excel("C:/Users/oussa/Downloads/Data Science/Master Statistique Big Data Dauphine/Module 2/Mod�le Lin�aire G�n�ralis�/US presidents.xls",sheet=2)

colnames(d1)=c("Ann�e","Nom","Taille","Naissance","President","VP","Gov","Senate","House","StateLeg","General","Cabinet","Party")
colnames(d2)=c("Ann�e","Nom","Taille","Naissance","President","VP","Gov","Senate","House","StateLeg","General","Cabinet","Party")

d1=d1[1:31,]
d2=d2[1:31,]
d1$Elu=1
d2$Elu=0

US_presidents=rbind(d1,d2)




US_presidents[,c(6,7,8,9,10,11,12)]=lapply(US_presidents[,c(6,7,8,9,10,11,12)],as.numeric)


US_presidents[is.na(US_presidents)]=0
#afin d'�viter un probleme de suraprentissage, je n'utilise pas l'Ann�e de l'Election ni la date de Naissance directement, je g�n�re l'�ge des candidats

US_presidents$Age=US_presidents$Ann�e-US_presidents$Naissance

US_presidents$Party[US_presidents$Party =="R "]="R"
US_presidents$Party[US_presidents$Party =="D "]="D"

#exploration rapide des donn�es

head(US_presidents)
str(US_presidents)
summary(US_presidents)
attach(US_presidents)

boxplot(Age~Elu)
boxplot(Taille~Elu)
mosaicplot(Elu~Party,col=c("red","blue"))
boxplot(Age~Party)


nrow(US_presidents[US_presidents$Elu== 1 & US_presidents$President> 0,])/(nrow(US_presidents[US_presidents$Elu== 1 & US_presidents$President> 0,])+nrow(US_presidents[US_presidents$Elu== 0 & US_presidents$President> 0,]))

#la plupart (75 %) des pr�sidents ont �t� reconduits pour un second mandat
#les R�publicains sont plus ag�s que les D�mocrates
#les pr�sidents �lus sont majoitairement R�publicains, plus �g�s et plus grands

#Choix des variables en tatonnant(suppression progressive des moins bons coefficients de Student,on retire les champs "Ann�e","Nom" et "Naissance")
#on part de reg=glm(Elu~Taille+President+VP+Gov+Senate+House+StateLeg+General+Party+Cabinet+Age,family=binomial,data=US_presidents)


reg=glm(Elu~Taille+Senate+StateLeg+Age,family=binomial,data=US_presidents)
summary(reg)


#Utilsiation des m�thodes pas � pas
modele_complet=glm(Elu~.-Naissance-Nom-Ann�e,family=binomial,data=US_presidents)

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

#Utilisation d'autres m�thodes de s�lection de variables 
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

#test de d�viance


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

#selon le test de d�viance le meilleur mod�le semble �tre reg_step car la p-valeur 1 est la plus faible (on r�fute l'hypothese � 1 degr� de libert� pour les pi) et la p valeur 2 la plus �lev�e (on r�fute l'hypothese de mod�le satur�)
#c'est ce mod�le qui est retenu par la suite

#split train test al�atoire du mod�le


#Tirage al�atoire
?sample
indexes = sample(1:nrow(X), size=0.8*nrow(X))

# Split data
Train = US_presidents[indexes,c("Taille","Senate","StateLeg","General","Elu")]
dim(Train)  
Test = US_presidents[-indexes,c("Taille","Senate","StateLeg","General","Elu")]
dim(Test) 

#test du mod�le

reg_test=glm(Elu~Taille+Senate+StateLeg+General,family=binomial,data=Train)
summary(reg_test)

?predict
predict(reg_test,new=Test[,c("Taille","Senate","StateLeg","General")],type="response")

t=(predict(reg_test,new=Test[,c("Taille","Senate","StateLeg","General")],type="response")>=0.5)
TC=table(t,Test[,c("Elu")])
(TC[1,1]+TC[2,2])/sum(TC)
#Taux de succ�s � 46%.. c'est faible 



#graphique illustratif sur 2 composantes les plus significatives

attach(US_presidents)
plot(Taille,Senate,col=ifelse(Elu==1,"green","red"))
abline(-reg_step$coefficients[1]/reg_step$coefficients[3],-reg_step$coefficients[2]/reg_step$coefficients[3])

#test de corr�lations entre covariables

cor(data.matrix(US_presidents[,c(5,6,7,8,9,10,11,12,15)]))



#il ne semble pas y avoir de corr�lation entre les variables explicatives quantitatives

library(FactoMineR)


res.pca = PCA(US_presidents[,-c(2,13,14)], scale.unit=TRUE, ncp=5, graph=T)




#pour aller plus loin comparaison avec d'autres algorithmes vus en cours de "Introducation � la Classification Supervis�e : classifacateur gaussien, classifiteur quadratique et arbre de d�cisions

#analyse discriminante lin�aire (classificateur bayesien)
reg_lda=lda(Elu~.,prior=c(0.5,0.5),Train)
t=predict(reg_lda,Test)$class
TC=table(t,Test[,c("Elu")])
(TC[1,1]+TC[2,2])/sum(TC)

#On a l�g�rement am�lior� le score

#analyse discriminante quadratique
reg_qda=qda(Elu~.,prior=c(0.5,0.5),Train)
t=predict(reg_qda,Test)$class
TC=table(t,Test[,c("Elu")])
(TC[1,1]+TC[2,2])/sum(TC)

#arbre de d�cision
library(rpart)
reg_arbre=rpart(Elu~.,data=Train,method="class")
text(reg_arbre)

t=predict(reg_arbre,newdata=Test,type="class")

TC=table(t,Test[,c("Elu")])
(TC[1,1]+TC[2,2])/sum(TC)



#Conclusion :
#les meilleures covariables qui expliquent l'�lection ou non sont : Taille,StateLeg,General et Senate.L'AIC obtenu est de 79,6.
#les covariables Taille, StateLeg et General influent positivement sur la probabilti� d'�ter �lu,
#la variable Senate influe n�gativement
#le classificateur bayesien (lin�aire ou quadratique) semble obtenir le meilleur r�sulat, mais qui reste faible.
#On a un mod�le peu performant (54% de taux de succ�s), ce qui s'explique par : la faible quantit� de donn�es disponibles (une soixantaine d'observations),corrr�lation entre Age et Party, 
#et l'absence de variables explicatives(Sexe,Ethnie,Niveau d'�ducation,etc.)
#Concernant la variable G�n�ral, le cas unique du G�n�ral Eisenhower fausse le r�sultat, l'�chantillon n'est pas assez important pour que cette variable soit significative
#Je ne comprends pas pourquoi la covariable "President" n'est pas retenu sachant qu'une majorit� des pr�sidents a �t� reconduit pour un second mandat..De m�me pour la variable "Party"
#On peut aussi remarquer que les observations ne sont pas ind�pendantes dans le temps 
#(probabilit� d'une alternance politique entre 2 �lections,probabilit� qu'un pr�sident �lu soit r�lu pour un second mandat, etc.)
#les conditions de l'exp�rience statistique ne sont pas identiques (pr�sidents d�c�d�s ou d�mission en cours de mandat, modification de la constitution apr�s Roosevelt pour empecher plus de 2 mandats cons�cutifs,crise �conomique,guerre mondiale,guerre froide etc.)