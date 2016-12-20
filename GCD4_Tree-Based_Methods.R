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

#si le data Frame est est fixé alors les noms de colonnes peuvent être directement appelés
attach(DF.data)

#???install le package tree
install.packages("tree")
library(tree)
?tree
#
Train50_tree <- tree(as.factor(Creditability)~ Account.Balance+Duration.of.Credit..month.+Payment.Status.of.Previous.Credit+Purpose+Credit.Amount+Value.Savings.Stocks+Length.of.current.employment+Instalment.per.cent+Sex...Marital.Status+Guarantors+Duration.in.Current.address+Most.valuable.available.asset+Age..years.+Concurrent.Credits+Type.of.apartment+No.of.Credits.at.this.Bank+Occupation+No.of.dependents+Telephone, data=train50, method="class")
# afficher la sommaire du variable train50_tree
summary(Train50_tree)
# afficher l'arbre
plot(Train50_tree)
text(Train50_tree, pretty=0,cex=0.6)

Test50_pred <- predict(Train50_tree, test50, type="class")
table(Test50_pred, test50$Creditability)

Train50_prune8 <- prune.misclass(Train50_tree, best=8)

plot(Train50_prune8)
#afficher l'arbre d'apres la variable predicté
text(Train50_prune8, pretty=0,cex=0.6)
Test50_prune8_pred <- predict(Train50_prune8, test50, type="class")

table(Test50_prune8_pred, test50$Creditability)
