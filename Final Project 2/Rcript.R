library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gmodels)
library(vcd)
library(caret)
library(xtable)
library(kableExtra)

test.org <- read.csv2(
  "/Users/huongtran/OU /Course Work/SES 4/STA5224/Final Project 2/data/test.csv", 
  header = T, sep = ",")
survive <- read.csv(
  "/Users/huongtran/OU /Course Work/SES 4/STA5224/Final Project 2/data/gender_submission.csv")
train.org <- read.csv2(
  "/Users/huongtran/OU /Course Work/SES 4/STA5224/Final Project 2/data/train.csv", 
  header = T, sep = ",")
train <- train.org

############################################################
#### Missing Value 
############################################################
Empty.train <- c()
Empty.test  <- c()
for (i in colnames(train.org)[-2]){
  Empty.train <- c(Empty.train, sum(train.org[,i] == ""))
  Empty.test <- c(Empty.test, sum(test.org[,i] == ""))
  
}

mysummary <- data.frame(NA.train = colSums(is.na(train.org[, - 2])), Empty.train) %>%
  mutate(Percent.train = round( (NA.train + Empty.train) / nrow(train.org)*100, 2)) %>%
  cbind(data.frame(NA.test = colSums(is.na(test.org)), Empty.test = Empty.test)) %>% 
  mutate(Percent.test = round( (NA.test + Empty.test) / nrow(test.org)*100, 2))

library(gridExtra)
pdf("mysummary.pdf")       # Export PDF
grid.table(mysummary)
dev.off()


############################################################
### Title
############################################################
train["Title"] <- str_split_fixed(train$Name, ", ", n = 2)[,2]
train["Title"] <- str_split_fixed(train$Title, ". ", n = 2)[, 1]
t <- train %>% group_by(Title) %>% summarize(count=n())
t[nrow(t) + 1,] <- NA
t <- cbind(t[1:9,], t[10:18,])


pdf("/Users/huongtran/OU /Course Work/SES 4/STA5224/Final Project 2/figure/t.pdf")       # Export PDF
grid.table(t)
dev.off()

train["Title"] <- gsub("Major", "Officer", train[,"Title"], fixed = T)
train["Title"] <- gsub("Capt", "Officer", train[,"Title"], fixed = T)
train["Title"] <- gsub("Col", "Officer", train[,"Title"], fixed = T)
train["Title"] <- gsub("Rev", "Officer", train[,"Title"], fixed = T)
train["Title"] <- gsub("Don", "Officer", train[,"Title"], fixed = T)
train["Title"] <- gsub("Mme", "Miss", train[,"Title"], fixed = T)
train["Title"] <- gsub("Mlle", "Miss", train[,"Title"], fixed = T)
train["Title"] <- gsub("th", "Miss", train[,"Title"], fixed = T)
train["Title"] <- gsub("Jonkheer", "Mr", train[,"Title"], fixed = T)
train["Title"] <- gsub("Ms", "Miss", train[,"Title"], fixed = T)
train["Title"] <- gsub("Ms", "Miss", train[,"Title"], fixed = T)
train["Title"] <- gsub("Ms", "Mrs", train[,"Title"], fixed = T)
train["Title"] <- gsub("Lady", "Mrs", train[,"Title"], fixed = T)
train["Title"] <- gsub("Sir", "Mr", train[,"Title"], fixed = T)


df.title <- as.data.frame(table(train$Survived, train$Title))
ggplot(df.title, aes(x = Var2, y = Freq, fill = Var1)) + geom_col(position = "fill") 

############################################################
###Age
############################################################
train$Age <- as.numeric(train$Age)
age.missing <- train[is.na(train$Age), ]
ggplot(age.missing, aes(Title, as.factor(Pclass))) + geom_jitter()
m <- mean(train[which(train$Title %in% c("Mr", "Mrs", "Miss") & train$Pclass == 3), "Age"], na.rm =  T)
train$Age <- replace_na(train$Age, m)
train <- train %>% mutate(Age.char = case_when(Age < 15 ~ "young", 
                                               Age < 50 ~ "middle.age",
                                               Age < 100 ~ "old"))

ggplot(train, aes(as.factor(Pclass), Age, fill = Sex)) + geom_boxplot() + facet_grid(cols = vars(Survived))
ggplot(train, aes(Pclass, Age.char, colour = Survived)) + geom_jitter()

############################################################
### Family size 
############################################################
train <- train %>% mutate(family.size = SibSp + Parch  +  1 )
train$Fare <- as.numeric(train$Fare)
train <- train %>% mutate(price = Fare / family.size )
for (i in 1:3){
  m <- mean(train[which(train$Pclass == i & train$price != 0), "price"])
  train[which(train$Pclass == i & train$price == 0), "price"] <- m
}

ggplot(train, aes(as.character(family.size), log(Fare))) + geom_boxplot()
############################################################
##Ticket and Embarked:
mosaic(~ Sex + Pclass + Survived, data = train,
       gp = shading_Friendly, legend= T )

ggplot(train, aes(Survived, Pclass, colour = Embarked)) + geom_jitter() 
############################################################
B <- train[grep("B", train$Cabin),]
train$Embarked[train$Embarked == ""] <- "C"
############################################################
### Train and Valid
############################################################
library(caret)
set.seed(3456)
train <- train %>% select(-c("PassengerId", "Name", "Cabin", "Ticket")) 


trainIndex <- createDataPartition(train$Survived, p = .8,
                                  list = FALSE,
                                  times = 1)
train.mod <- train[trainIndex,]
valid.mod <- train[-trainIndex,]
cat("Number of observation in train.mod is: ", nrow(train.mod), "\n ", 
    "Number of observation in test.mod is: ", nrow(valid.mod))

############################################################
### Model Selection 
############################################################
library(SignifReg)
null.model <- glm(Survived ~ 1, family = binomial, data = train.mod)
full.model <- glm(Survived ~. , family = binomial, data = train.mod)
scope = list(lower = formula(null.model), upper = formula(full.model))
var.sel <- step(null.model, scope = scope, scale = 0, trace = 0, direction = "both")
keep <- c("Survived","Title", "Pclass", "family.size", "Age", "Fare", "price")
train.mod <- train.mod[, keep]
valid.mod <- valid.mod[, keep]

############################################################
### Logistic Regression 
############################################################
cut.point <- sum(train.mod$Survived) / nrow(train.mod)
mod.reg <- glm(Survived ~ ., data = train.mod, family = binomial)
mod.fit <- as.numeric(fitted(mod.reg) >=  cut.point)
fit.tab <- xtabs(~ train.mod$Survived + mod.fit)
reg.acc.fit  <- round((fit.tab[1,1] + fit.tab[2,2]) / sum(fit.tab), 2)

############################################################
### Decicsion Tree
############################################################
library(rpart)
library(rattle)
library(rpart.plot)
tree.fit <- predict(tree, train.mod, type = "class")
tree.tab.fit <- table(train.mod$Survived, tree.fit)
tree.acc.fit <- round(sum(diag(tree.tab.fit)) / sum(tree.tab.fit), 2)
cat("Fitted accuracy for Decision Tree is: ", tree.acc.fit)
fancyRpartPlot(tree)


############################################################
### Random Foresst
############################################################

library(randomForest)
set.seed(456)
fit.acc <- c()
ntree  <- c()
forest <- c()
#creat 10 forest of size ntree = i, and take the average of these
for (i in  seq(5, 50, by = 1)){
  temp <- c()
  for (j in 1:50){
    rf <- randomForest(as.factor(Survived) ~., data = train.mod, ntree = i)
    conf.matrix <- rf$confusion
    temp <- c(temp,  sum(diag(conf.matrix)) / sum(conf.matrix))
    j <- j+1 
  }
  #take average of all size i trees
  fit.acc <- c(fit.acc, mean(temp))
  ntree <- c(ntree, i)
}
plot(ntree, fit.acc)
num.tree <- ntree[which.max(fit.acc)]

rf <- randomForest(as.factor(Survived) ~., data = train.mod, ntree = num.tree)
rf.acc.fit <- round(sum(diag(rf$confusion)) / sum(rf$confusion), 2)
cat("Fitted accuracy of random forest is: ", rf.acc.fit)


############################################################
### SVM
############################################################
library(e1071)
svm.mod <- svm(as.factor(Survived) ~., data = train.mod, scale =  F)
svm.tab <- table(as.character(train.mod$Survived), svm.mod$fitted)
svm.acc.fit <- round(sum(diag(svm.tab)) / sum(svm.tab), 2)
cat("Fitted accuracy of SVM is: ", svm.acc.fit)

############################################################
### Model comparsion 
############################################################
library(xtable)
library(kableExtra)

reg.pred <-  as.numeric(predict(mod.reg, valid.mod) > 0.5)
reg.tab.pred <- xtabs(~ valid.mod$Survived + reg.pred)
reg.acc.pred  <- round((reg.tab.pred [1,1] + reg.tab.pred [2,2]) / sum(reg.tab.pred), 2)


tree.pred <- predict(tree, valid.mod, type = "class")
tree.tab.pred <- xtabs(~ valid.mod$Survived + tree.pred)
tree.acc.pred  <- round((tree.tab.pred [1,1] + tree.tab.pred [2,2]) / sum(tree.tab.pred), 2)

rf.pred <- predict(rf, valid.mod)
rf.tab.pred <- xtabs(~ valid.mod$Survived + rf.pred)
rf.acc.pred <- round((rf.tab.pred [1,1] + rf.tab.pred [2,2]) / sum(rf.tab.pred), 2)

svm.pred <- predict(svm.mod, valid.mod, decision.values = FALSE)
svm.tab.pred <-  xtabs(~ valid.mod$Survived + svm.pred)
svm.acc.pred <- round((svm.tab.pred [1,1] + svm.tab.pred [2,2]) / sum(svm.tab.pred), 2)


#svm.pred <- predict(svm.mod, valid.mod)
r <- data.frame(logistic = c(reg.acc.fit, reg.acc.pred), 
                tree = c(tree.acc.fit, tree.acc.pred),
                random.forest = c(rf.acc.fit, rf.acc.pred),
                svm.mod  = c(svm.acc.fit, svm.acc.pred))

rownames(r) <- c("Fit accuarcy", "Prediction accuracy")

r.trans <- as.data.frame(t(as.matrix(r)))

pdf("/Users/huongtran/OU /Course Work/SES 4/STA5224/Final Project 2/figure/r.pdf")       # Export PDF
grid.table(r.trans)
dev.off()


############################################################
### Interpretation of Logistic Regression:
############################################################
suppressMessages(library(pROC))
suppressMessages(
  rocPlot <- roc(Survived ~ fitted(mod.reg), data = train.mod)
)
cat("AUC of logistic regression is: ", auc(rocPlot))
x <- data.frame(rocPlot$sensitivities,rocPlot$specificities, rocPlot$thresholds)
thePar <- par(pty = "s")
plot.roc(rocPlot, legacy.axes = TRUE, asp = F)

############################################################
## Make prediction to Kaggle
############################################################
test <- test.org
# Title feature:

test["Title"] <- str_split_fixed(test$Name, ", ", n = 2)[,2]
test["Title"] <- str_split_fixed(test$Title, ". ", n = 2)[, 1]
test["Title"] <- gsub("Major", "Officer", test[,"Title"], fixed = T)
test["Title"] <- gsub("Capt", "Officer", test[,"Title"], fixed = T)
test["Title"] <- gsub("Col", "Officer", test[,"Title"], fixed = T)
test["Title"] <- gsub("Rev", "Officer", test[,"Title"], fixed = T)
test["Title"] <- gsub("Don", "Officer", test[,"Title"], fixed = T)
test["Title"] <- gsub("Mme", "Miss", test[,"Title"], fixed = T)
test["Title"] <- gsub("Mlle", "Miss", test[,"Title"], fixed = T)
test["Title"] <- gsub("th", "Miss", test[,"Title"], fixed = T)
test["Title"] <- gsub("Jonkheer", "Mr", test[,"Title"], fixed = T)
test["Title"] <- gsub("Ms", "Miss", test[,"Title"], fixed = T)
test["Title"] <- gsub("Ms", "Miss", test[,"Title"], fixed = T)
test["Title"] <- gsub("Ms", "Mrs", test[,"Title"], fixed = T)
test["Title"] <- gsub("Lady", "Mrs", test[,"Title"], fixed = T)
test["Title"] <- gsub("Sir", "Mr", test[,"Title"], fixed = T)

#This title is only in test set:
test["Title"] <- gsub("Officera", "Officer", test[,"Title"], fixed = T)
test$Fare <- as.numeric(test$Fare)
test$Age <- as.numeric(test$Age)
m.test <- mean(test[which(test$Title %in% c("Mr", "Mrs", "Miss") & test$Pclass == 3), "Age"], na.rm =  T)
test$Age <- replace_na(test$Age, m.test)
test <- test %>% mutate(family.size = SibSp + Parch  +  1 ) %>%
  mutate(price = Fare / family.size )
for (i in 1:3){
  m <- mean(test[which(test$Pclass == i & test$price != 0), "price"], na.rm = T)
  test[which(test$Pclass == i & test$price == 0), "price"] <- m
  test$price <- replace_na(test$price, m)
}

#find the missing position of test$Fare:
pos <- which(is.na(test$Fare) == T)
test$Fare[pos] <- test$family.size[pos] * test$price[pos]

keep.test <- keep[-1]
test.mod <- test[, keep.test]

reg.test <-  as.numeric(predict(mod.reg, test.mod) > cut.point)
reg.test <- data.frame(PassengerId = test$PassengerId, Survived = reg.test)

tree.test <- predict(tree, test.mod, type = "class")
tree.test <- data.frame(PassengerId = test$PassengerId, Survived = tree.test)

write.csv(x=reg.test, file="reg.result", row.names = F)
write.csv(x=tree.test, file="tree.result", row.names = F)




