##Portuguese

##Please set your working directory to the folder containing student-mat.csv and student-por.csv

##Load CSV file
pordata = read.csv("student-por.csv", header = TRUE, sep = ';')

##Exploratory Analysis - Box Plots 
par(mfrow=c(3, 4))
plot(pordata$school, pordata$G3, main="G3 by school")
plot(pordata$sex, pordata$G3, main="G3 by sex")
plot(as.factor(pordata$age), pordata$G3, main="G3 by age")
plot(pordata$address, pordata$G3, main="G3 by address")
plot(pordata$famsize, pordata$G3, main="G3 by famsize")
plot(pordata$Pstatus, pordata$G3, main="G3 by Pstatus")
plot(as.factor(pordata$Medu), pordata$G3, main="G3 by Medu")
plot(as.factor(pordata$Fedu), pordata$G3, main="G3 by Fedu")
plot(pordata$Mjob, pordata$G3, main="G3 by Mjob")
plot(pordata$Fjob, pordata$G3, main="G3 by Fjob")
plot(pordata$reason, pordata$G3, main="G3 by reason")
plot(pordata$guardian, pordata$G3, main="G3 by guardian")

par(mfrow=c(3, 4))
plot(as.factor(pordata$traveltime), pordata$G3, main="G3 by travel time")
plot(as.factor(pordata$studytime), pordata$G3, main="G3 by study time")
plot(as.factor(pordata$failures), pordata$G3, main="G3 by failures")
plot(pordata$schoolsup, pordata$G3, main="G3 by schoolsup")
plot(pordata$famsup, pordata$G3, main="G3 by famsup")
plot(pordata$fatherd, pordata$G3, main="G3 by paid")
plot(pordata$activities, pordata$G3, main="G3 by activities")
plot(pordata$nursery, pordata$G3, main="G3 by nursery")
plot(pordata$higher, pordata$G3, main="G3 by higher")
plot(pordata$internet, pordata$G3, main="G3 by internet")
plot(pordata$romantic, pordata$G3, main="G3 by romantic")
plot(as.factor(pordata$famrel), pordata$G3, main="G3 by famrel")

par(mfrow=c(3, 4))
plot(as.factor(pordata$freetime), pordata$G3, main="G3 by freetime")
plot(as.factor(pordata$goout), pordata$G3, main="G3 by goout")
plot(as.factor(pordata$Dalc), pordata$G3, main="G3 by goout")
plot(as.factor(pordata$Walc), pordata$G3, main="G3 by goout")
plot(as.factor(pordata$health), pordata$G3, main="G3 by health")
plot(pordata$absences, pordata$G3, main="G3 by absences")

##Full model
fullmodelpor = glm(G3 ~ school+sex+age+address+famsize+Pstatus+Medu+Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+schoolsup+famsup+fatherd+activities+nursery+higher+internet+romantic+famrel+freetime+goout+Dalc+Walc+health+absences, family = "poisson", data=pordata)
summary(fullmodelpor)

##Significant Variables model (Reduced Model)
reducedmodelpor = glm(G3 ~ sex + studytime + failures + famsup + schoolsup + higher, family = "poisson", data = pordata)
summary(reducedmodelpor)

##Formal Model Selection
##Forward
step(reducedmodelpor, scope = list(lower=reducedmodelpor, upper=fullmodelpor), direction="forward")
##Backward
step(fullmodelpor, scope = list(lower=reducedmodelpor, upper=fullmodelpor), direction="backward")
selectedmodelpor = glm(G3 ~ sex + studytime + failures + famsup + schoolsup + higher + school + Dalc + Fedu + health, family = "poisson", data = pordata)

##Residual Analysis and Influential Points
par(mfrow=c(2, 2))
plot(selectedmodelpor)

##Alternative Model
which(pordata$G3 == 0)
newpordata=pordata[-c(164, 441, 520, 564, 568, 584, 587, 598, 604, 606, 611, 627, 638, 640, 641), ]
selectedmodelpor_newdata=glm(G3~sex + studytime + failures + famsup + schoolsup + higher + school + Dalc + Fedu + health, family = "poisson", data = newpordata)
