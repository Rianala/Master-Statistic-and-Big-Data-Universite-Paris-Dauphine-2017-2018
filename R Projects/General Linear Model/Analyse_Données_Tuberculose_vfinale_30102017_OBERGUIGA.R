          ###################################################
          #         UNIVERSITE PARIS DAUPHINE               #
          #         MASTER STATISTIQUE ET BIG DATA          # 
          #         GLM et CHOIX DE MODELE                  #
          #         DEVOIR MAISON OBLIGATOIRE               #   
          #         Analyse de  donnees sur la  tuberculose #
          #         Oussama BERGUIGA                        #
          ###################################################     



#1)IMPORTATION DES DONNEES

getwd()

setwd("C:\\Users\\oussa\\Downloads\\Data Science\\Master Statistique Big Data Dauphine\\Module 2\\Modèle Linéaire Généralisé")

list.files()

require(readxl)

tuberculose=read_excel("tb_real.xlsx",sheet=1)

#2)EXPLORATION DES DONNEES

View(tuberculose)

head(tuberculose)

#nous n'avons pas besoin du champ "observation" qui va 
#generer un surapprentissage
#en revanche on garde l'identifiant 
#de la ferme dans un premier temps :
#1)en effet certaines fermes semblent contenir 
#bien plus de cas d'infections que la moyenne
#2)le numero des fermes pourrait bien etre representatif 
#de la localisation geographique de la ferme
#or en biologie il me semble qu'il y a un lien fort
#entre epidemie et zone geographique
#en particulier on a vu en cours que l'epidemie etait liee 
#a la cohabitation entre racoons et bovins
#donc les fermes en presence geographique de racoon 
#ont probablement plus de cas d'infections


tuberculose=tuberculose[,c(-1)]
attach(tuberculose)

str(tuberculose)
summary(tuberculose)

#type, sex et age sont clairement des variables categorielles, 
#on les convertit donc en facteur
#la variable farm_id pouvant traduire une continuite geographique, je la laisse 
#en variable numerique
#cette option nous laisse la possibilite de faire un "predict" sur une ferme 
#non deja presente dans la base de donnee

tuberculose$type=as.factor(type)
tuberculose$sex=as.factor(sex)
tuberculose$age=as.factor(age)



str(tuberculose)
summary(tuberculose)


pairs(tuberculose)

cor(data.matrix(tuberculose))

plot(par,reactors)


#apparement pas de correlation evidente entre les covariables 
#eventuellement une faible correlation (0.5) entre "reactors" et "par" 
#ce qui est logique : plus il y a d'individus-jours a risque 
#plus il y a d'individus infectes




summary(reactors)
boxplot(tuberculose$reactors)
tuberculose[tuberculose$reactors>=5,]
nrow(tuberculose[tuberculose$reactors>=5,])/nrow(tuberculose)


#la quantite "reactors" varie de 1 à 29
#surtout, il semble y a avoir des valeurs extremes : 
#les observations ou il y a plus de
#5 animaux infectes semblent etre des outliers
#dans la plus grande majorite des cas (environ 90%) 
#le nombre d'infections est inferieur a 5 par observation
#les observations extremes (> 5 cas d'infection) correspondent aux observations :
#45,98,118,119,120,121,124,129,133
#les fermes concernees sont 4002,5005,5120,7175,7006 et 8023

table(reactors)

hist(reactors)

tuberculose[,c("reactors","farm_id")]

boxplot(reactors~farm_id)
tapply(reactors,farm_id,sum)
tapply(par,farm_id,sum)

rbind(tapply(reactors,farm_id,sum),tapply(par,farm_id,sum))
(tapply(reactors,farm_id,sum)/tapply(par,farm_id,sum))
boxplot(reactors/par~farm_id)

#les fermes qui comportent le plus d'individus contamines sont 5005,7175,8006,8023
#on peut interpreter cette information de differentes facons :

#- soit ces fermes ont plus d'infections car le nombre d'animaux-jours risque 
#est plus important
#- soit ces fermes ont plus d'infections car elles seraient la source d'expansion
#de la maladie 
#-ou il y a une population de racoon importante a proximite

#on pourra tester un modele par la suite sans certaines de ces fermes


boxplot(reactors~age)
boxplot(reactors~sex)
boxplot(reactors~type)


#les femelles,les types 2 et 3,ainsi que les plus ages 
#semblent plus touches que les autres


#3)MODELISATION


#nous allons essayer d'expliquer 
#le nombre d'infections par 
#une regression de Poisson


#3)a)Modele de Poisson

#Modele de Poisson : P[Yi=k]=exp(-lambdai)*lambdai^k/k! avec lambdai=exp(tXi*Beta)
#E[Yi]=lambdai

modele1=glm(reactors~.,family=poisson,data=tuberculose)
summary(modele1)#AIC=467

#testons step pour trouver le modele le plus pertinent (i.e. AIC minimal)

modele2=step(glm(reactors~.,family=poisson,data=tuberculose),direction="both")
summary(modele2)#AIC=467

#le modele retenu comprend les covariables farm_id, type,sex, age et par 
#tous les coefficients de Student sont significativement inferieurs à 5 % 
#sauf pour quelques categories de "type"

#la variable "par" dependant du temps, on peut tester l'ajout d'un offset sur cette variable
modele3=step(glm(reactors~farm_id+sex+age+type+offset(log(par)),family=poisson,data=tuberculose),direction="both")

summary(modele3)#AIC 484 : je suis surpris que l'offset n'ait pas diminue l'AIC ..
#peut etre ceci est fausse par des valeurs aberrantes.. 
#AIC est augmente ici (484 vs 467) on conserve donc le modele precedent sans offset

#on peut tester un modele quasipoisson

modele4=glm(reactors~farm_id+sex+age+type+offset(log(par)),family=quasipoisson,data=tuberculose)

summary(modele4)

#je ne comprends pas pourquoi l'AIC n'est pas affiche pour le quasipoisson
#les coefficients de Student on significativement augmente
#ce qui n'est pas bon
#on retient donc le modele 2 pour l'instant (plus faible AIC)


#4)TEST DU MODELE RETENU

library(car)

residualPlots((modele2))


#les lignes vertes sont plutot proches des lignes pointilles 
#ce qui est en faveur du modele, les residus de Pearson simules
#par regression non parametriques sont proches de 0

#interpretation : les residus partiels sont 
#plutot lineaires en les covariables


#cependant : le test de linearite des residus partiels 
#(Ho : residu lineaire par rapport a une covariable donnee )
#n'est pas concluant  pour la variable "farm_id" et "par" les p-valeurs 
#de residus de Pearson sont < 5% 
#ce qui signifierait que 
#les residus ne sont pas forcement lineaire en ces covariables, et qu'il faudrait 
#chercher une transformation de ces covariables ?

library(boot)
glm.diag.plots(modele2,iden=T)

#la variance des residus est importante a cause des valeurs extremes
#le qqplot est plutot proche de la 
#premiere bissectrice sauf aux extremites
#nous avons peut-etre un defaut de normalite du 
#aux valeurs extremes




#en utilisant l'option "iden" de la fonction glm.diag.plots  
#on arrive a identifier plusieurs points aberrants 
#d'apres la distance de Cook
#cela correspond aux observations 45,129 et 133
#on remarque que ces observations correspondent 
#aux valeurs extremes vues precedemment





#on va tester un modele sans celles-ci (sans puis avec l'offset)

modele5=step(glm(reactors~farm_id+sex+age+type+par,
                 family=poisson,data=tuberculose[-c(45,129,133),]),direction="both")

summary(modele5)#AIC=347


modele6=step(glm(reactors~farm_id+sex+age+type+offset(log(par)),
                 family=poisson,data=tuberculose[-c(45,129,133),]),direction="both")

summary(modele6)#AIC=339



#on a abaisse l'AIC à 339 on conserve donc ce modele


#remarque importante : on ne retire que 3 observations sur 
#un jeu de donnees qui en comporte 134, 
#cela reste donc raisonnable (moins de 5 % du nombre d'observations)




#on reteste le nouveau modele :

residualPlots((modele6))
glm.diag.plots(modele6,iden=T)

#les residus de Pearson sont centres en 0 (lignes "verte" proche de ligne "pointille"), ce qui va dans le sens d'une linearite
#des residus par rapport au covariables
#mais la p valeur du test de linearite des residus pour farm_id 
#n'est tjrs pas satisfaisante (tenter une transformation de variable ?)

#recherche d'un terme polynomial
modele7=update(modele6,~.+I(farm_id^2)+I(farm_id^3)+I(farm_id^4)+I(farm_id^5))
summary(modele7)#AIC 300..il semblerait qu'on peut 
#regresser farm_id sous forme d'une serie infinie de polynomes 
#(cf Theoreme de Weierstrass ?) pour abaisser AIC

modele8=step(modele7)
summary(modele8)



residualPlots((modele8))#les graphs du residualPlots et les p-valeurs associes montrent que le modele
#n'est pas lineaire en farm_id mais polynomial

glm.diag.plots(modele8,iden=T)
#la plupart des residus sont centres et compris entre moins -2 et 2,
#d'ou une certaine homoscedasticite
#on a ameliore la gaussianite des residus d'apres le qqplot
#(un leger defaut de normalite aux extremites)



modele_retenu=glm(reactors~farm_id+I(farm_id^2)+I(farm_id^3)+I(farm_id^4)+I(farm_id^5)+age+offset(log(par)),family=poisson,
                  data=tuberculose[-c(45,129,133),])
summary(modele_retenu)#AIC 296

# p-valeurs de Student tres significatifs pour polynome(farm_id) et age (<5%) 

#5)CONCLUSION

#Nous avons reussi a expliquer le nombre de cas d'infections 
#par un modele de Poisson en retirant quelques valeurs 
#qui semblent aberrantes de notre echantillon
#Toutes choses egales par ailleurs: 
 #l'esperance du nombre de cas d'infection est multiplie par :

 
 #exp(2.4) pour la categorie age 1 par rapport a age 0
 #exp(2.2) pour la categorie age 2 par rapport a age 0
 #exp(polynome(numero de ferme) de degre 5) 
#MAIS AUSSI avec l'offset: 
 #l'esperance du nombre de cas d'infection est aussi 
 #proportionnelle à la covariable "par"

#CRITIQUE DU MODELE : la fiabilite de ce modele est limite par la
#relativement faible quantite d'observations
#(en particulier par rapport a l'exercice "Assurance" 
#10^2 observations vs 10^5 observations)
#d'autre part il semblerait que cette regression de Poisson ne soit pas robuste : 
#changer une ou 2 observations (en particulier les valeurs extremes) 
#change completement le modele optimal (argmin AIC)

