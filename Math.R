##Math

##Please set your working directory to the folder containing student-mat.csv and student-por.csv

##Load CSV file
mathdata = read.csv("student-mat.csv", header = TRUE, sep = ';')

##Exploratory Analysis - Box Plots 
pairs(~G1+G2+G3,data=mathdata, main="Scatterplot for G1, G2 and G3 for Portugese") ##Scatterplot for G1, G2 and G3

par(mfrow=c(3, 4)) ##Boxplots
plot(mathdata$school, mathdata$G3, main="G3 by school")
plot(mathdata$sex, mathdata$G3, main="G3 by sex")
plot(as.factor(mathdata$age), mathdata$G3, main="G3 by age")
plot(mathdata$address, mathdata$G3, main="G3 by address")
plot(mathdata$famsize, mathdata$G3, main="G3 by famsize")
plot(mathdata$Pstatus, mathdata$G3, main="G3 by Pstatus")
plot(as.factor(mathdata$Medu), mathdata$G3, main="G3 by Medu")
plot(as.factor(mathdata$Fedu), mathdata$G3, main="G3 by Fedu")
plot(mathdata$Mjob, mathdata$G3, main="G3 by Mjob")
plot(mathdata$Fjob, mathdata$G3, main="G3 by Fjob")
plot(mathdata$reason, mathdata$G3, main="G3 by reason")
plot(mathdata$guardian, mathdata$G3, main="G3 by guardian")

par(mfrow=c(3, 4))
plot(as.factor(mathdata$traveltime), mathdata$G3, main="G3 by travel time")
plot(as.factor(mathdata$studytime), mathdata$G3, main="G3 by study time")
plot(as.factor(mathdata$failures), mathdata$G3, main="G3 by failures")
plot(mathdata$schoolsup, mathdata$G3, main="G3 by schoolsup")
plot(mathdata$famsup, mathdata$G3, main="G3 by famsup")
plot(mathdata$paid, mathdata$G3, main="G3 by paid")
plot(mathdata$activities, mathdata$G3, main="G3 by activities")
plot(mathdata$nursery, mathdata$G3, main="G3 by nursery")
plot(mathdata$higher, mathdata$G3, main="G3 by higher")
plot(mathdata$internet, mathdata$G3, main="G3 by internet")
plot(mathdata$romantic, mathdata$G3, main="G3 by romantic")
plot(as.factor(mathdata$famrel), mathdata$G3, main="G3 by famrel")

par(mfrow=c(3, 4))
plot(as.factor(mathdata$freetime), mathdata$G3, main="G3 by freetime")
plot(as.factor(mathdata$goout), mathdata$G3, main="G3 by goout")
plot(as.factor(mathdata$Dalc), mathdata$G3, main="G3 by goout")
plot(as.factor(mathdata$Walc), mathdata$G3, main="G3 by goout")
plot(as.factor(mathdata$health), mathdata$G3, main="G3 by health")
plot(mathdata$absences, mathdata$G3, main="G3 by absences")

##Full model
fullmodel = glm(G3 ~ school+sex+age+address+famsize+Pstatus+Medu+Fedu+Mjob+Fjob+reason+guardian+traveltime+studytime+failures+schoolsup+famsup+paid+activities+nursery+higher+internet+romantic+famrel+freetime+goout+Dalc+Walc+health+absences, family = "poisson", data=mathdata)
summary(fullmodel)

##Significant Variables model (Reduced Model)
reducedmodel = glm(G3 ~ sex + age + studytime + failures + schoolsup + famsup + higher + romantic + goout + absences, family = "poisson", data = mathdata)
summary(reducedmodel)

##Formal Model Selection
##Forward
step(reducedmodel, scope = list(lower=reducedmodel, upper=fullmodel), direction="forward")
##Backward
step(fullmodel, scope = list(lower=reducedmodel, upper=fullmodel), direction="backward")
selectedmodel = glm(G3 ~ sex + age + address + famsize + Medu + Mjob + studytime + failures + schoolsup + famsup + higher + romantic + freetime + goout + health + absences, family = "poisson", data = mathdata)

##Residual Analysis and Influential Points
par(mfrow=c(2, 2))
plot(selectedmodel)

Imath = influence.measures(selectedmodel)
cookmath = Imath$infmat[,7]
plot(cookmath,type="h",lwd=3,col="red", ylab = "Cook’s Distance", main = "Cook distance for selected model")
which(cookmath == max(cookmath))
     
##Alternative Model
which(mathdata$G3 == 0)
newmathdata=mathdata[-c(129, 131, 132, 135, 136, 137, 138, 141, 145, 147, 149, 151, 154, 161, 163, 169, 171, 174, 222, 240, 243, 245, 260, 265, 270, 297, 311, 317, 333, 334, 335, 338, 342, 344, 368, 384, 388, 390), ]
selectedmodel_newdata=glm(G3~sex + age + address + famsize + Medu + Mjob + studytime + failures + schoolsup + famsup + higher + romantic + freetime + goout + health + absences, family = "poisson", data = newmathdata)

##Quasi-Poisson Model
quasipoissonmodel = glm(G3 ~ sex + age + address + famsize + Medu + Mjob + studytime + failures + schoolsup + famsup + higher + romantic + freetime + goout + health + absences, family = "quasipoisson", data = mathdata)
summary(quasipoissonmodel)

