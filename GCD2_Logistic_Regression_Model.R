# ce projet est réaliser par une équipe de ENSA Hoceima (Maroc)
# l'équipe de projet : Walid Benjehd, Med Benyakoub, Abdallah Qesmoun

# set the work directory 
setwd("C:/Users/WalidBenjehd/Documents/R/project")
# check the work directory 
getwd()

#lire les données complete 1000 lignes
DF.data <-read.csv("R/project/german_credit.csv",header = TRUE, sep=",")
#lire a sample (proportion) des données 98 lignes
View(DF.data)

#si le data Frame est est fixé alors les noms de colonnes peuvent être directement appelés
attach(DF.data)

#generer les index d'un Échantillon aléatoire de 50% des donées de base
indexes = sample(1:nrow(DF.data), size=0.5*nrow(DF.data))

#training data contient les données indexé par indexes
train50 <- DF.data[indexes,]
#test data contient le rest
test50 <- DF.data[-indexes,]

# construction du module logistique avec les données d'entraînement (training data) et l'évaluation des données d'essai (test data)
#modulede base 
LogisticModel50 <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Value.Savings.Stocks + Length.of.current.employment + Sex...Marital.Status + Most.valuable.available.asset + Type.of.apartment + Concurrent.Credits + Duration.of.Credit..month.+ Credit.Amount + Age..years., family=binomial, data = train50)
#doc pour glm fonction
?glm
#module finale
LogisticModel50final <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Length.of.current.employment + Sex...Marital.Status, family=binomial, data = train50)


#fit le module finale : extraire les valeurs ajustées par le module finale
fit50 <- fitted.values(LogisticModel50final)

#repliquer la valeur 0 500 fois et stocker dans le variable suivant
Threshold50 <- rep(0,500)

for (i in 1:500){
  #si les valeurs ajustée est inferieur à 0.5 affecté la valeur 1 au case correspond du threshold
  if(fit50[i] >= 0.5) Threshold50[i] <- 1
  
}
#affiche le tableau de contigence entre Creditability du donnée de train50 et threshold
CrossTable(train50$Creditability, Threshold50, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=F, data=Train50)

#install the package ROCR pour la methode performance 
install.packages("ROCR")
library("ROCR")

#cree prediction object depuis fit50
pred<-predict(fit50,train50)
?predict
perf <- performance(pred, "tpr", "fpr")

Plot(perf)