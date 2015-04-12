# Return from rpartPathFinder is a data frame with 3 columns
# "var" "nodenumber" "rpartpaths"

data(airquality)
data(kyphosis)
data(iris)


# Error cases
# 1) Error Case : Not a rpart tree
lmObj <- lm(Ozone~Temp,data=airquality)
rpartPathFinder(lmObj) # Raises the error "Not an rpart object"

# 2) Error Case : Only root node 
onlyRoot <- rpart(Ozone~Temp,data=airquality[1,])
rpartPathFinder(onlyRoot) # Raises the error "Tree has only root node"


# Legit Cases
# 1) Without categoricals
ozone <- rpart(Ozone ~ ., data=airquality)
df<-rpartPathFinder(rpartmodel=ozone)

fit <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis)
rpartPathFinder(fit)
fit2 <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis,
		parms=list(prior=c(.65,.35), split='information'))
rpartPathFinder(fit2)
fit3 <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis,
		control=rpart.control(cp=.05))
rpartPathFinder(rpartmodel=fit3)

# 2) With Categoricals
d<-data.frame(y=sample(letters[1:5],1000,replace = T),x1=sample(letters[1:10],1000,replace = T),x2=sample(letters[1:5],1000,replace = T))
rcatobj<- rpart(y~x1+x2,data=d)
df<-rpartPathFinder(rcatobj)

kyphcat <- rpart(Age~.,data=kyphosis)
rpartPathFinder(kyphcat)

rpartPathFinder(rpart(Sepal.Length~Species,data=iris))

data(swiss)
swiss$FertilityFactor <- cut(swiss$Fertility,breaks=quantile(swiss$Fertility))
swiss$AgricultureFactor <- cut(swiss$Agriculture,breaks=quantile(swiss$Agriculture,probs=seq(0,1,0.1)))
swiss$ExaminationFactor <- cut(swiss$Examination,breaks=quantile(swiss$Examination,probs=seq(0,1,0.5)))
swisstree<- rpart(ExaminationFactor~FertilityFactor +AgricultureFactor,swiss)
df<-rpartPathFinder(swisstree)
df




# -- REMOVING REDUNDANT RULES

ozone <- rpart(Ozone ~ ., data=airquality)
df<-rpartPathFinder(rpartmodel=ozone)
library(plyr)

# TEST CASE

data(swiss)
swiss$FertilityFactor <- cut(swiss$Fertility,breaks=quantile(swiss$Fertility))
swiss$AgricultureFactor <- cut(swiss$Agriculture,breaks=quantile(swiss$Agriculture,probs=seq(0,1,0.1)))
swiss$ExaminationFactor <- cut(swiss$Examination,breaks=quantile(swiss$Examination,probs=seq(0,1,0.5)))
swisstree<- rpart(ExaminationFactor~FertilityFactor +AgricultureFactor,swiss)
df<-rpartPathFinder(swisstree)
dfWithSimple <- reduceRules(df,swisstree)



fit3 <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis,
		control=rpart.control(cp=.05))
df<-rpartPathFinder(rpartmodel=fit3)
dfWithSimple <- reduceRules(df,fit3)
