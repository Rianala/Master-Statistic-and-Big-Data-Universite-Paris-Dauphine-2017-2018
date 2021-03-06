          ##########################################
          #         UNIVERSITE PARIS DAUPHINE      #
          #         MASTER STATISTIC AND BIG DATA  #
          #         GLM et CHOIX DE MODELE         #
          #         DEVOIR MAISON OBLIGATOIRE      #
          #         Oussama BERGUIGA               #
          ##########################################


getwd()

setwd("C:\\Users\\oussa\\Downloads\\Data Science\\Master Statistique Big Data Dauphine\\Module 2\\Mod�le Lin�aire G�n�ralis�")

list.files()

accidents=read.csv("baseassuranceauto.csv",header=T,row.names=1)

#I)MODELSIATION DE LA SURVENANCE D'ACCIDENTS MATERIELS (Surv1)

#1)Exploration des donn�es 

head(accidents)
str(accidents)

#on enl�ve le num�ro de police pour �viter un surapprentissage 
#et on se restreint a Surv1 et Nb1

accidents=accidents[,-c(1,18,20)]

summary(accidents)

cor(data.matrix(accidents))
cor(accidents$Nb1,accidents$Surv1)#0.90

#corr�lation entre Sous-Group et Groupe d'une part, Nb et Surv d'autre part

attach(accidents)
hist(Surv1)

sum(Surv1)/nrow(accidents)
#beaucoup plus de "non sinistre 1" que de sinistre 1 (12.3 % de sinistre 1) 

tableau_gender=table(Surv1,Gender)
tableau_gender
tableau_gender[2,1]/tableau_gender[1,1]#12%
tableau_gender[2,2]/tableau_gender[1,2]#15%

#le taux de Surv1 plus �lev� chez les hommes ques les femmes

boxplot(Age~Nb1)

#en moyenne, les conducteurs ag�s, 
#donc plus exp�riment�s ont
#moins d'accidents que les plus jeunes

boxplot(Poldur~Nb1)
#le nombre d'accidents survenus les plus �lev�s (> 4) ont lieu pour des contrats plus r�cents (< 5 ans)

plot(Nb1,Density)
#les Nb1 �lev�s (> 4) ont lieu dans les zones les plus denses ( >= 150 hab/km�)

mean(accidents[accidents$Occupation=="Employed","Nb1"])#0.17
mean(accidents[accidents$Occupation=="Self-employed","Nb1"])#0.12
mean(accidents[accidents$Occupation=="Housewife","Nb1"])#0.18
mean(accidents[accidents$Occupation=="Unemployed","Nb1"])#0.2
mean(accidents[accidents$Occupation=="Retired","Nb1"])#0.04

#les demandeurs d'emploi sont ceux qui ont 
#le plus d'accidents,les retrait�s le moins d'accidents

#2)Mod�lisation

#Pour expliquer la Survenance de sinisres materiels, un modele de 
#regression logistique s'impose :
#P[Yi]=1=exp(tXiBeta)/(1+exp(tXiBeta))

#Nous pouvons essayer les fonctions de lien "logit" et "probit"

#la variable categorielle SubGroup2 comporte 
#de trop nombreuses valeurs possibles
#pour �tre utilis�e en regression logistique
#Nb1 car fortement correle a Surv1
#en cons�quence nous n'utiliserons pas 
#SubGroup2 et Nb1 dans la regression logistique

modele_probit=step(glm(Surv1~.,data=accidents[,-c(12,16)],family=binomial(link="probit")),direction="both")
summary(modele_probit)#AIC=64254

modele_logit=step(glm(Surv1~.,data=accidents[,-c(12,16)],family=binomial(link="logit")),direction="both")
summary(modele_logit)#AIC=64100



#le modele retenu est celui avec toutes les covariables excepte "Value"
#la plupart des des coefficients sont significatifs d'apr�s la p-valeur de Student (< 5%) 
#sauf pour "Categorie Small" et quelques facteurs de "Group2"

#logit � un AIC plus faible donc pr�f�rable

#Bien que nous ayons vu l'option offset pour les covariables temporelles
#dans une regression de Poisson uniquement,testons un offset sur "Exppdays" 
#car cette covariable est temporelle
#interpretation : le log rapport de cote ln(pi/(1-pi)) augmente proportonnellement au log(Exppdays) (coefficient de proportionalit� = 1)


modele_offset=glm(Surv1~CalYear+Gender+Type+Category+Occupation+Age+Group1+Bonus+Poldur+Adind+Group2+Density+offset(log(Exppdays)),data=accidents[,-c(12,16)],family=binomial(link="logit"))
summary(modele_offset)#AIC=64079

#ce modele avec offset pr�sente un AIC legerement plus faible mais ce n'est pas significatif


#3)Validation crois�e d�coupage du jeu de donn�e en train et 

#test puis mesure du taux de classification 

?sample




validation_croisee= function(x){

  VC_global=rep(0,x)
  VC_vrais_negatifs=rep(0,x)
  VC_vrais_positifs=rep(0,x)  
  
  
  
for (i in 1:x)
  
{


indexes = sample(1:nrow(accidents), size=0.8*nrow(accidents))


Train = accidents[indexes,-c(12,16)]
dim(Train) 

Test = accidents[-indexes,-c(12,16)]
dim(Test) 

modele_train=glm(Surv1~.-Value,data=Train,family=binomial(link="logit"))



y_chapeau_test=predict(modele_train,type="response",newdata=Test)
TC=table(y_chapeau_test>=0.5,Test$Surv1)
TC

VC_global[i]=(TC[1,1]+TC[2,2])/sum(TC)


VC_vrais_negatifs[i]=TC[1,1]/sum(TC[,1])


VC_vrais_positifs[i]=TC[2,2]/sum(TC[,2])

}


return(data.frame("VC_global"=round(mean(VC_global),digits=3)
                  ,"VC_vrais_negatifs"=round(mean(VC_vrais_negatifs),digits=3)
                  ,"VC_vrais_positifs"=round(mean(VC_vrais_positifs),digits=3)))
  

}

validation_croisee(10)

#88.0 % de bonne classification au global mais ...

#99.4 % de bonne classification au sein des "non sinistre 1" mais ...

#6.5% de bonne classification seulement au sein des "sinistre 1" !


#rajoutons une p�nalit� pour les sinitres 0 :
#on pond�re par 1 si sinitre 0 bien class�, 
#par un coeffcient p si sinitre 1 bien class�
?round





validation_croisee_penalisee= function(x,p){

VC_global_penalise=rep(0,x)  
    
  for (i in 1:x)
    
  {
    
    
    indexes = sample(1:nrow(accidents), size=0.8*nrow(accidents))
    
    
    Train = accidents[indexes,-c(12,16)]
    dim(Train) 
    
    Test = accidents[-indexes,-c(12,16)]
    dim(Test) 
    
    modele_train=glm(Surv1~.-Value,data=Train,family=binomial(link="logit"))
    
  
    y_chapeau_test=predict(modele_train,type="response",newdata=Test)
    TC=table(y_chapeau_test>=0.5,Test$Surv1)
    TC
    
    VC_global_penalise[i]=(TC[1,1]+p*TC[2,2])/sum(TC[1,]+p*TC[2,])
    
    
  }
  
  
  return(data.frame("VC_global"=round(mean(VC_global_penalise),digits=3)))
  
}

#Nous avions vu que le taux de classement  
#Surv1=0 �tait de l'orde de 99.4 %
#et que celui de  Surv1=1 �tait de l'ordre de 6.5%

99.4/6.5

#Les vrais n�gatifs sont environ 15 fois mieux class�s que les vrais positifs

validation_croisee_penalisee(10,15)#nous avons r�duit la taux de classification globale � 83.9% 
validation_croisee_penalisee(10,100)#nous avons r�duit la taux de classification globale � 71.4%

summary(modele_logit)

#Conclusion : toutes choses �gales par ailleurs, 
#le logarithme du rapport de c�te de survenance d'un accident de type 1 :
# - augmente de 0.1 par CalYear 
# - augmente de 0.34 pour les hommes
# - augmente de 0.08 pour le Type B par rapport au Type A
# - diminue de -0.0086 pour la Category Medium par rapport a la categorie Large
# - diminue de -0.5 pour un retraite par rapport a un employe
# - diminue de 0.03 par age
# - augmente de 0.01 par bonus
# - augmente de 0.006 par densit�
# - augmente de 0.004 par jours d'exposition
# - diminue de 0.13 si l'indicateur Adind vaut 1
# - etc.

#le taux global de bonne classification est fauss� car le taux de classification 
#des vrais positifs (sinistre 1 =1) est proche de 6%
#le taux global de bonne classification a pu �tre corrig� avec une pond�ration
#pour am�liorer le taux de classification des Surv1=1 on pourrait �tre tent� reequilibrer
#le jeu de donn�es avec  mois d'observations correspondant � la survenance Surv1=0
#afin de r�quilibrer le ratio de quantit�s de donn�es Surv1=0 et Surv1=1 ?
#or ceci est un mauvaise idee car induirait un surapprentissage sur cet echantillon


#en conclusion, cet exemple nous montre les limites de la regression logistique
#dont la sufrace de separation entre les 2 classes est lineaire
#nous verrons surement dans le cadre du module 3 d'autres algorithmes plus performants
#(SVM, qui permet une s�paration entre les classes non lineaire, For�ts al�atoires, Grandient Boosting)
#qui pourraient am�liorer consid�rablement le taux de classification des Surv1=1


#II)MODELISATION DU NOMBRE D'ACCIDENTS MATERIELS(Nb1)

summary(accidents$Nb1)

#le nombre de sinistres RC materiels est un nombre entier inf�rieur � 7: 
#on peut tester une regression de Poisson ou une regression ordinale

#A)Regression de Poisson


mean(accidents$Nb1)
sd(accidents$Nb1)^2

mean(accidents$Nb1)/sd(accidents$Nb1)^2-1


#la variance et la moyenne sont proches 
#Remarques : il se pourrait donc bien que la variable Nb1 
#suit une loi de Poisson 


#test adequation de Poisson

table(accidents$Nb1)
lambda=sum(accidents$Nb1)/nrow(accidents)
round(nrow(accidents)*dpois(0:7,lambda))
chisq.test(table(accidents$Nb1),round(nrow(accidents)*dpois(0:7,lambda)))

#pvalue=0.26>>5% : on peut accepter l'hypothese que Nb1 
#suit une loi de Poisson
#meme si ceci n'est en aucun cas un pr� requis 
#indispensable pour une regression de Poisson
#pour laquelle chacune des observations suit 
#sa propre Loi de Poisson de parametre "Lambda i"

modele_poisson=step(glm(Nb1~.,data=accidents[,-c(12)],family=poisson),direction="both")#AIC=27803
summary(modele_poisson)

#Nb1 s'expliquer par les covariables Gender,Type,Age,
#Group1, Bonus,Poldur,Density,Exppdays,Surv1 avec un AIC de 27803
#Remarque : l'algorotihme step utilis� ici mets beaucoup de temps
#a converger sur mon ordinateur..

system.time(step(glm(Nb1~.,data=accidents[,-c(12)],family=poisson),direction="both"))#1396.23 secondes,soit environ 23 minutes !!


summary(glm(Nb1~Gender+Type+Age+Group1+Bonus+Poldur+
              Density+Exppdays+Surv1,data=accidents[,-c(12)],family=poisson))#AIC=27803

#ajout offset




summary(glm(Nb1~Gender+Type+Age+Group1+Bonus+Poldur+
              Density+offset(log(Exppdays))+Surv1,data=accidents[,-c(12)],family=poisson))#AIC=28206


#l'AIC sans offset est plus faible donc on ne garde pas l'offset


#test des r�sidus

require(boot)
library(car)

#REMARQUE: le test du residu sur la base complete de converge pas avec mon ordinateur
#je le fais donc sur une petite partie de cette base tir�e al�atoirement, 
#et ce PLUSIEURS FOIS



indexes = sample(1:nrow(accidents), size=0.2*nrow(accidents))


Train = accidents[indexes,-c(12)]


modele_poisson_retenu=glm(Nb1~Gender+Type+Age+Group1+Bonus+
                            Poldur+Density+Exppdays+Surv1,data=Train,family=poisson)
summary(modele_poisson_retenu)



residualPlots(modele_poisson_retenu)

system.time(residualPlots(modele_poisson_retenu))



#les lignes vertes semblent align�s sur 0 ce qui est tr�s bon signe
#interpretation : les r�sidus partiels evoluent de mani�re lineaire avec les covariables

#les p values sont >> 5 % (sauf pour Age) : l'hypothese Ho selon 
#laquelle les r�sidus partiels sont lineaires en les covariables est acceptee






glm.diag.plots(modele_poisson_retenu)

#remarque : ces deux algorithmes mettent �norm�ment de temps � converger sur mon ordinateur ..

#les r�sidus ne sont pas gaussiens d'apres le qqplot 
#a noter une dizaine de points aberrants


#b)Regression ordinale

require(ordinal)

#on convertit Nb1 en facteurs

accidents=as.factor(accidents)
str(accidents)



modele_ordinal=step(clm(Nb1~.,data=accidents[,-c(12,17)]),direction="both")
summary(modele_ordinal)

#algorithme step mets beaucoup de temps � converger sur mon ordinateur
# AIC = 76707 avec 
#CalYear+Gender+Type+Category+Occupation+Age+Group1+
#Bonus+Poldur+Adind+Group2+Density+Exppdays
#on reste donc sur le modele de Poisson dont l'AIC est bien plus faible
#on peut se poser la question de comment comparer un modele de regression de Poisson
#et un modele de classification ordinale ?

modele_ordinal_retenu=clm(Nb1 ~ CalYear + Gender + Type + Category + Occupation + Age + 
                     Group1 + Bonus + Poldur + Value + Adind + Group2 + Density + 
                     Exppdays,data=accidents[,-c(12,17)])

  
  
summary(modele_ordinal_retenu)




modele_ordinal_qualitatif=clm(Nb1 ~ Gender + Type + Category + Occupation,data=accidents[,-c(12,17)])
summary(modele_ordinal_qualitatif)

#test de classification


indexes = sample(1:nrow(accidents), size=0.8*nrow(accidents))


Train_clm = accidents[indexes,-c(12,17)]


Test_clm = accidents[-indexes,-c(12,17)]






modele_ordinal_retenu_Train=clm(Nb1 ~ CalYear + Gender + Type + Category + Occupation + Age + 
                       Group1 + Bonus + Poldur + Value + Adind + Group2 + Density + 
                       Exppdays,data=Train_clm)

summary(modele_ordinal_retenu_Train)

predict(modele_ordinal_Train,newdata=Test_clm)

d=cbind(Test_clm$Nb1,predict(modele_ordinal_Train,newdata=Test_clm))

#ne marche pas...



#Conclusion
summary(modele_poisson_retenu)

#Toutes choses egales par ailleurs, le nombre d'accident 
#peut etre modelise par une regression de Poisson. Toutes choses egales par ailleurs,
#l'esperance du nombre d'incidents :

#-XXXXX
#-XXXXX
#-XXXXX
#-XXXXX
#-XXXXX
#-XXXXX






