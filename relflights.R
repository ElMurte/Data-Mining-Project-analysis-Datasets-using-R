summary(flights)
nrow(flights)
is.factor(flights$origin)
is.factor(flights$carrier)
is.factor(flights$dest)
#flights$carrier<-asn.factor(flights$carrier)
#flights$origin<-as.factor(flights$origin)
#flights$dest<-as.factor(flights$dest)
flights<-na.omit(flights)#esclude the rows who contains NA values
dataaa = sort(sample(nrow(flights), nrow(flights)*.02))#divide dataset in a subset
flights<-flights[dataaa,]#insert the subset on the dataset we use
par(mfrow=c(1,2))
hist(flights$arr_delay, prob=TRUE) #histogrammi ritardi o anticipo
hist(log(flights$arr_delay), prob=TRUE)#non si può normalizzare visto che ci sono valori negativi(aerei in anticipo)
dati <- flights[,c('arr_delay','dep_time','dep_delay','arr_time', 'carrier', 'origin', 'dest')]
#relazioni esplicative continue
pairs(dati[,1:4])
#Relazioni con le esplicative qualitative?!
boxplot(dati$arr_delay~dati$carrier, main='arr_delay vs dep_time') 
boxplot(dati$arr_delay~dati$origin, main='arr_delay vs dep_delay')
boxplot(dati$arr_delay~dati$dest, main='arr_delay vs arr_time')

#Relazioni con le esplicative continue & qualitative?! #da non considerare se non settate come fattori...
with(dati, plot(arr_delay~dep_time,col=carrier, main='dep_time vs carrier'))##
with(dati, plot(arr_delay~dep_time,col=origin, main='dep_time vs origin'))##
with(dati, plot(arr_delay~dep_time,col=dest, main='dep_time vs dest'))##
with(dati, plot(arr_delay~dep_delay,col=carrier, main='dep_delay vs carrier'))##
with(dati, plot(arr_delay~dep_delay,col=origin, main='dep_delay vs origin'))##
with(dati, plot(arr_delay~dep_delay,col=dest, main='dep_delay vs dest'))##

m <- lm(arr_delay ~ dep_delay, data=dati)#first model dep_delay only
summary(m)
par(mfrow=c(2,2))
plot(m)
m2 <- lm(arr_delay ~ I(dep_delay^2)+dep_delay*dest, data=dati)
summary(m2)
m3 <- lm(arr_delay ~ dep_delay+dep_delay*dest, data=dati)
summary(m3)
m4 <- lm(arr_delay ~ dep_delay+dep_time, data=dati)
summary(m4)
anova(m,m4)
m5 <- lm(arr_delay ~ dep_delay+dep_time+dep_delay*origin+dep_delay*carrier+dep_delay*dest, data=dati)
summary(m5)
anova(m,m2)#...
anova(m,m3)
par(mfrow=c(2,2))
plot(m2)
plot(m)
plot(m5)
#vince il mio modello m perchè piu semplice ed un R^2 MOLTO SIMILE
#############################

sp.deptime <- smooth.spline(x=dati$dep_delay, y=dati$arr_delay, cv=TRUE)
summary(dati)
?gam
library(gam)
library(glmnet)
m.gam <- gam(arr_delay ~ s(dep_delay), data=dati)
summary(m.gam)
m.gam2 <- gam(arr_delay ~ s(dep_delay,2), data=dati)
summary(m.gam2)
m.gam3 <- gam(arr_delay ~ s(dep_delay,3), data=dati)
summary(m.gam3)
anova(m.gam,m.gam2)
library(MASS)
cv.err.m <- cv.glm(dati, m, K=10)
par(mfrow=c(1,3))
plot(m.gam2, se=TRUE)
par(mfrow=c(1,2))
plot(dati$arr_delay, fitted(m), xlab="valori stimati",ylab = "valori osservati",main = "valori stimati vs valori obsrv")
abline(0,1,col="red")
plot(dati$arr_delay, fitted(m.gam2),xlab="valori stimati",ylab = "valori osservati",main = "valori stimati vs valori obsrv")
abline(0,1,col="blue")
AIC(m)
AIC(m.gam2)
AIC(m.gam)
######################fine spline

library(glmnet)
?lm
#library(xlsx)
#write.xlsx(mydata, "C:\Users\Elvis\Desktop\flights.xlsx")
m.lm <- lm(arr_delay ~ ., data=flights)
X <- model.matrix(m.lm)[,-1]
nrow(X)
X
y <- flights$arr_delay
y
#ridge
m.ridge <- glmnet(x=X, y=y, alpha=0)
par(mfrow=c(1,1))
plot(m.ridge, xvar='lambda',main="RIDGE")
set.seed(123)
m.ridge.cv <- cv.glmnet(x=X, y=y, alpha=0)
plot(m.ridge.cv)
m.ridge.min <- glmnet(x=X, y=y, alpha=0, lambda=m.ridge.cv$lambda.min)
m.ridge.se <- glmnet(x=X, y=y, alpha=0, lambda=m.ridge.cv$lambda.1se)
cbind(coef(m.ridge.min))
?cbind
plot(m.ridge.cv,main="variazione stima del test error in base a log(lambda)")
#you shuld test the model with other data but the result is the same using a part of the initial RData
previsioni.ridge <- predict(m.ridge.min, newx=X)
previsioni.ridgese <- predict(m.ridge.se, newx=X)
plot(previsioni.ridge, y,xlab="valori osservati di arr_delay",ylab="valori stimati",main="valori stimati vs osservati")
abline(0,1,col="red")
plot(previsioni.ridgese, y,xlab="valori osservati di arr_delay",ylab="valori stimati",main="ridge lamda1se")
abline(0,1,col="red")
#lasso
m.lasso <- glmnet(x=X, y=y, alpha=1)
plot(m.lasso, xvar='lambda',main="LASSO")
set.seed(123)
m.lasso.cv <- cv.glmnet(x=X, y=y, alpha=1)
plot(m.lasso.cv)
m.lasso.min <- glmnet(x=X, y=y, alpha=1, lambda=m.lasso.cv$lambda.min)
m.lasso.se<- glmnet(x=X, y=y, alpha=1, lambda=m.lasso.cv$lambda.1se)
cbind(coef(m.lm), coef(m.lasso.min), coef(m.lasso.min))
cbind(coef(m.lasso.se))
previsioni.lasso <- predict(m.lasso.min, newx=X)
previsioni.lassose <- predict(m.lasso.se, newx=X)
par(mfrow=c(1,1))
plot(previsioni.lasso, y)
abline(0,1,col ="green")
plot(previsioni.lassose, y,xlab="valori osservati di arr_delay",ylab="valori stimati",main="lasso lamda1se")
abline(0,1,col="green")
#Lasso vs Ridge
min(m.ridge.cv$cvm)#MSE
m.ridge.cv$lambda.1se
m.ridge.cv$lambda.min
min(m.lasso.cv$cvm)#preferisco lasso
m.lasso.cv$lambda.1se
m.lasso.cv$lambda.min
