install.packages("plyr")
install.packages("ggplot2")
library(plyr)
library(ggplot2)

setwd("F:/DATA SCIENCE COURSE/PROJECT")
getwd()
admission <- read.csv("College_admission.csv")
View(admission)

str(admission)
summary(admission)

ggplot(admission, aes(x=gpa)) + geom_bar()
ggplot(admission, aes(x=gre)) + geom_density()

max(admission$gre)
min(admission$gre)

results<-lm(formula= admit~gpa+gre+Race+ses+rank, data=admission)
results

#descriptive analysis
admission_gre_cat <- transform(admission, categorized = ifelse(gre<=440, "low",ifelse(gre<580, "medium", "high")))
admission_gre_cat

str(admission_gre_cat)
summary(admission_gre_cat)

table(admission_gre_cat$gpa, admission_gre_cat$categorized )
mean(admission_gre_cat$gpa)

summary(table(admission_gre_cat$gpa, admission_gre_cat$categorized ))

ggplot(admission_gre_cat, aes(x= gpa, y = categorized)) + geom_point()

#svm model
install.packages("e1071", "caret")
library(e1071)
adm_churn<-read.csv("College_admission.csv")
count(adm_churn$admit)
svm_churn <- svm(admit~.,adm_churn)

library(caret)
confusionMatrix(adm_churn$admit,predict(svm_churn), positive='1')

prediction<- predict(svm_churn,adm_churn[-1])
prediction_results<- table(prediction, true=adm_churn)
print(prediction_results)

#decision tree
install.packages("mlbench", "rpart")
library(mlbench)
library(rpart)
adm2<- read.csv("College_admission.csv")
adm2$admit<-sapply(adm2$admit,factor)
tree_model<-rpart(admit~., data=adm2, method="class")
tree_model

printcp(tree_model)
plotcp(tree_model)
print(tree_model)
summary(tree_model)
plot(tree_model)

