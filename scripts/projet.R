setwd("C:/Users/alex4/Desktop/School/BIO500/ECL611/data")

db <- read.csv("donnees_ecl611.csv")

db["volume_cm3"] <- pi*(db["Diametre_rameau_mm"]/10/2)^2*db["Longueur_rameau_cm"]

db["SLA"] <- db["Aire_feuilles_cm2"]/db["Masse_feuilles_g"]

db["I_foliaire"] <- db["Nb_feuilles_rameau"]/db["volume_cm3"]

db["I_foliaire_log"] <- log(db["I_foliaire"])
db["Masse_feuilles_log"] <- log(db["Masse_feuilles_g"])
db["SLA_log"] <- log(db["SLA"])


db <- as.data.frame(unclass(db),                     # Convert all columns to factor
                    stringsAsFactors = TRUE)

#Comme dans les ressources du cours, pur avoir les données moyennes par espèce

donnee.sp<-aggregate(.~db$Espece+db$Groupe_fonctionnel, data=db,mean)

#Étapes pour avoir les barplots de groupe fonctionnel vs intensité foliaire -> pas significatif

moy.If.gf<-tapply(donnee.sp$I_foliaire,list(donnee.sp$Groupe_fonctionnel),mean)
moy.If.gf

sd.If.gf<-tapply(donnee.sp$I_foliaire,list(donnee.sp$Groupe_fonctionnel),sd)
sd.If.gf

n.If.gf<-tapply(donnee.sp$I_foliaire,list(donnee.sp$Groupe_fonctionnel),length)
n.If.gf

mids.gf<-barplot(moy.If.gf, xlab="Groupes fonctionnels", ylab ="Intensit? foliaire", ylim=c(0,60))

arrows(mids.gf, moy.If.gf+sd.If.gf, mids.gf, moy.If.gf- sd.If.gf, angle=90, code=3, length=0.1)

#graphiques regression simple sans log vs avec log, meilleure correlation avec log-log (comme dans les articles)

plot(donnee.sp$Masse_feuilles_g~donnee.sp$I_foliaire)
plot(donnee.sp$Aire_feuilles_cm2~donnee.sp$I_foliaire)

plot(log(donnee.sp$Masse_feuilles_g)~log(donnee.sp$I_foliaire))
plot(log(donnee.sp$Aire_feuilles_cm2)~log(donnee.sp$I_foliaire))

#SLA demeure la meilleure correlation avec les groupes fonctionnels

summary(lm(donnee.sp$SLA~donnee.sp$Groupe_fonctionnel))

plot(donnee.sp$SLA~donnee.sp$Groupe_fonctionnel)

#Scatterplot pour voir les correlations entre les variables, on peut jouer avec les differentes variables

library(car)
scatterplotMatrix(~Tolerance + Masse_feuilles_log + Nb_feuilles_rameau + 
                    Exposition_num + SLA_log + I_foliaire_log, data=db)        

#tests de corrélations

#test le plus significatif à date, avec l'addition du SLA et de la masse des feuilles

fit1 <- lm(I_foliaire_log ~ Masse_feuilles_log+SLA_log, data=db)
summary(fit1)

require(ggplot2)
require(ggiraph)
require(ggiraphExtra)
require(plyr)
ggplot(db,aes(y=I_foliaire_log,x=Masse_feuilles_log,color=SLA_log))+geom_point()+stat_smooth(method="lm",se=FALSE)
ggPredict(fit1,interactive=TRUE)

#autres tests

summary(lm(Exposition_num~ SLA, data=db))

