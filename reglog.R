library(ISLR)
Default
head(Default)
tail(Default)
str(Default)
plot(Default$income,Default$balance)
plot(Default$income,Default$balance,xlab="income",ylab="balance")
pairs(Default[c("income","balance")],
      main="Matrix plot",pch=22,
      bg=c("Red","yellow")[unclass(Default$default)])
ndefault=dim(Default[Default$default=="Yes",]);ndefault
pcdefault=ndefault[1]/dim(Default)[1];pcdefault

ratabalance1=mean(Default[Default$default=="Yes","balance"]);ratabalance1
ratabalance=mean(Default[Default$default=="No","balance"]);ratabalance

rataincome1=mean(Default[Default$default=="Yes","income"]); rataincome1
rataincome=mean(Default[Default$default=="No","income"]);rataincome

library(ggplot2)
ggplot(Default,aes(balance,fill=default))+
  geom_density(alpha=0.5)+
  geom_vline(data=Default,
             mapping=aes(xintercept=ratabalance),color="red")+
  geom_vline(data=Default,
             mapping = aes(xintercept=ratabalance1),color="blue")

ggplot(Default,aes(income,fill=default))+
  geom_density(alpha=0.5)+
  geom_vline(data=Default,
             mapping=aes(xintercept=rataincome),color="red")+
  geom_vline(data=Default,
             mapping = aes(xintercept=rataincome1),color="blue")

ggplot(Default,aes(student,..count..))+
  geom_bar(aes(fill=default),position = "dodge")
default.lofit=glm(default~student+balance+income,family = binomial,data = Default)
summary(default.lofit)

library(lattice)
library(caret)
set.seed(303)
intrain<-sample(nrow(Default),nrow(Default)*0.8)
trainfix<-Default[intrain,]
testfix<-Default[-intrain,]

table(Default$default)
library(MASS)
library(dplyr)
model1=glm(default~student+balance+income,family = binomial,data = trainfix)
backward2<-stepAIC(model1, direction='backward')
probabilities <- backward2 %>% predict(testfix[2:3], type = "response")
predicted.classes1 <- ifelse(probabilities > 0.5, "No", "Yes")
predicted.classes1
table(testfix$default,predicted.classes1)
str(testfix)
str(predicted.classes1)
predicted.classes1=factor(predicted.classes1)
log_conf<-confusionMatrix(testfix$default,predicted.classes1)
log_conf



probabilities1 <- model1 %>% predict(testfix[-1], type = "response")
predicted.classes2<- ifelse(probabilities1 > 0.5, "No", "Yes")
predicted.classes2
table(testfix$default,predicted.classes2)
str(testfix)
str(predicted.classes2)
predicted.classes2=factor(predicted.classes2)
log_conf<-confusionMatrix(testfix$default,predicted.classes2)
log_conf


