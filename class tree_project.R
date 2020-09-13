rm(list = ls())
#TASK1 1)
library(readxl)
DATA <- as.data.frame(read_xlsx("C:/Users/vt135/Downloads/Data analysis for env engr/Class project/Data analysis project data.xlsx", sheet="Class_Tree"))
TP_pass = DATA[,30]
X = DATA[,1:29]
#fit a classification tree to the data
#use Exhaustive Splitting 
#use Gini Index as the node impurity index

#Step 1: fit the full tree (no pruning for now)

library(rpart)
library(rpart.plot)

set.seed(123)
A  <- rpart(TP_pass ~ .,data = DATA, method ='class',
            parms = list(split ='Gini'),
            control=rpart.control(minsplit=10, 
                                  cp=0,minleaf = 1))

printcp(A)
Relative_Error = A$cptable[7,3]
RootNodeError = 0.17944

MR = Relative_Error*RootNodeError
MR
prp(A, faclen = 0, cex = 0.8, extra = 1)

#PART 3: use 50-fold cross validation to create 50 
#trained classification trees 
#we can evaluate (each tree will be trained on 196 
#points)

#Step 1: train 50 trees and then test 50 trees

#load package plyr
library(plyr)

set.seed(126)
form <- "TP_pass ~ EQT_Cap + DAF_COD_Removal + AXT_GPD + ATone_DO_mg_L + ATone_pH + ATone_Temp + ATone_SVI_mL_g 
+ ATtwo_DO_mg_L + ATtwo_pH + ATtwo_Temp + ATtwo_SVI_mL_g + ATthree_DO_mg_L + ATthree_pH + ATthree_Temp + ATthree_SVI_mL_g + 
ATfour_DO_mg_L + ATfour_pH + ATfour_Temp + ATfour_SVI_mL_g + ATfive_DO_mg_L + ATfive_pH + ATfive_Temp + ATfive_SVI_mL_g + 
ATsix_DO_mg_L + ATsix_pH + ATsix_Temp + ATsix_SVI_mL_g + SRT_day + RAS_Flow_GPD"

K = 107
folds <- split(DATA, cut(sample(1:nrow(DATA)),K))#Creates K FOLDS
errs <- matrix(NA, nrow = length(folds),ncol = nrow(DATA)/K)
#EMPTY VEC TO HOLD ERROR


for (i in 1:length(folds)) {
  test <- ldply(folds[i], data.frame)#test data
  train <- ldply(folds[-i], data.frame)#all else is training
  
  tmp.train <- rpart(form, train, method = "class",
                     parms = list(split ='Gini'),
                     control=rpart.control(minsplit=10, 
                                           cp=0,minleaf = 1))
  
  tmp.predict <- predict(tmp.train, newdata = test, 
                         type = "class",
                         parms = list(split ='Gini'))
  
  errs[i,] <- abs(as.numeric(as.matrix(tmp.predict))-(as.numeric(test$TP_pass)))
}

#STEP2: Calculate Misclassification Rate
Misclass_Rate = mean(errs)
Misclass_Rate



#PART 4: perform cross validation at different cp levels of 
#the tree and evaluate the average misclassification error 
#(recal that cp is associated with tree depth)


#step 1: estimate the misclassification rate at each cp level 
#in the original tree (full tree)

mse = matrix(NA,nrow = 7)
CP = matrix(NA,nrow = 7)
xerror = matrix(NA,nrow = 7)

for (i in 1:7){
  Relative_Error_cp = A$cptable[8-i,3]
  RootNodeError_cp = 0.17944
  mse[i] = Relative_Error_cp*RootNodeError_cp
  CP[i] = A$cptable[8-i,1]*RootNodeError_cp
  xerror[i] = A$cptable[8-i,4]
}


#step 2: estimate the extent to which K (50) training trees 
#misclassify their test data (2 points each) at each cp level

set.seed(123)
A2  <- rpart(TP_pass ~ .,data = DATA, method ='class',
             parms = list(split ='Gini'),
             control=rpart.control(minsplit=10, 
                                   cp=0,minleaf = 1,
                                   xval = 107))#NEW BIT

printcp(A2)
plotcp(A2)
bestcp = 0.028
A3 = prune(A2,cp = bestcp) #trim tree at bestcp 

prp(A3, faclen = 0, cex = 0.8, extra = 1)
