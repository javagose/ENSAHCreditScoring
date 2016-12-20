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

#Random sample of 50% of row numbers created
indexes = sample(1:nrow(DF.data), size=0.5*nrow(DF.data))

#training data contient les données indexé par indexes
train50 <- DF.data[indexes,]
#test data contient le rest
test50 <- DF.data[-indexes,]
#Random sample of 50% of row numbers created
indexes = sample(1:nrow(DF.data), size=0.5*nrow(DF.data))
#training data contient les données indexé par indexes
train50 <- DF.data[indexes,]
#test data contient le rest
test50 <- DF.data[-indexes,]

#si le data Frame est est fixé alors les noms de colonnes peuvent être directement appelés
attach(DF.data)

#install le packge randam forest
install.packages(randomForest)
library(randomForest)
#appliqué la methode randomforest pour obtenir le module predicté
rf50 <- randomForest(Creditability ~., data = train50, ntree=200, importance=T, proximity=T)

#affiche le plot trees/errors
plot(rf50, main="")
#gerenre la prediction
Test50_rf_pred <- predict(rf50, test50, type="class")
#affiche le tableau
table(Test50_rf_pred, test50$Creditability)
#affiché l'imprtance des vars predicté
importance(rf50)
#affiché le plot
varImpPlot(rf50,  main="", cex=0.8)
