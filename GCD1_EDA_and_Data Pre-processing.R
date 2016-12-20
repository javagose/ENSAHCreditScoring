# ce projet est réaliser par une équipe de ENSA Hoceima (Maroc)
# l'équipe de projet : Walid Benjehd, Med Benyakoub, Abdallah Qesmoun  
setwd("C:/Users/WalidBenjehd/Documents/R/project")
getwd()

#lire les données complete 1000 lignes
DF.data <-read.csv("german_credit.csv",header = TRUE, sep=",")
#lire les données 
View(DF.data)

# il nous faut fixer le data frame  pour que les noms de colonnes peuvent être directement appelés

attach(DF.data)
#afficher la proprtion de chaque classification d'une variable catégorique
#pour la proportion de Duration.in.Current.address
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),1)
#pour la proportion de  Most.valuable.available.asset
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),2)
#pour la proportion de Concurrent.Credits,
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),3)
#pour la proportion de No.of.Credits.at.this.Bank
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),4)
#pour la proportion de Occupation
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),5)
#pour la proportion de No.of.dependents
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),6)
#pour la proportion de Telephone
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),7)
#pour la proportion de Foreign.Worker
margin.table(prop.table(table(Duration.in.Current.address, Most.valuable.available.asset, Concurrent.Credits,No.of.Credits.at.this.Bank,Occupation,No.of.dependents,Telephone, Foreign.Worker)),8)

# tableau de contingence entre  deux vars
# d'abord il faut installer le package gmodel pour la methode cross table
install.packages("gmodels")
library(gmodels)

#affiche le tableau de contigence entre Creditability et account balance 
CrossTable(Creditability, Account.Balance, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
#affiche le tableau de contigence entre Creditability et Payment.Status.of.Previous.Credit 
CrossTable(Creditability, Payment.Status.of.Previous.Credit, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
#affiche le tableau de contigence entre Creditability et purpose (objectif de credit) 
CrossTable(Creditability, Purpose, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

# sommaires sont imprimés pour  le variable Duration.of.Credit..month.
summary (Duration.of.Credit..month.)  
#Bins pour le histogramme de la frequence du mois de crédit 
brksCredit <- seq(0, 80, 10)
# produit  histogramme pour imprimer la frequence du mois de durée de crédit par mois 
hist(Duration.of.Credit..month., breaks=brksCredit, xlab = "Credit Month", ylab = "Fréquence", main = " ", cex=0.4) 

# sommaires sont imprimés pour  le variable Credit Amount 
summary(Credit.Amount)
# beans pour le hsitograme de credit.amount
brksAmount <- seq(0,20000,1000)
# produit  histogramme pour imprimer la frequence du Credit.Amount 
hist(Credit.Amount, breaks=brksAmount, xlab = "Credit Month", ylab = "Fréquence", main = " ", cex=0.4)

# sommaires sont imprimés pour  le variable  Age 
summary(Age..years.)
# beans pour le hsitograme de Age..years.
brksAge <- seq(0,80,10)
# produit  histogramme pour imprimer la frequence du Age..years. 
hist(Age..years., breaks=brksAge, xlab = "Credit Month", ylab = "Fréquence", main = " ", cex=0.4)

#  boxplot pour Duration.of.Credit..month. 
boxplot(Duration.of.Credit..month., bty="n",xlab = "Credit Month", cex=0.4) 

#  boxplot pour Credit Amount 
boxplot(Credit.Amount, bty="n",xlab = "Credit Amount", cex=0.4) 

#  boxplot pour Age 
boxplot(Duration.of.Credit..month., bty="n",xlab = "Age", cex=0.4) 
