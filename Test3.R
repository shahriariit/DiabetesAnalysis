library(caret)

dataset3=read.csv('diabetes-f3.csv')

dataset3[,1:5]=scale(dataset3[,1:5])

control <- trainControl(method="repeatedcv", number=10, repeats=10)
metric <- "Accuracy"


rf<- function(x){
  set.seed(7)
  return(train(as.character(Outcome)~., data=dataset3, metric=metric, method=x, trControl=control)) 
} 

#fit.adaboost=rf("adaboost")
fit.treebag3=rf("treebag")
fit.glmboost3=rf("glmboost")
fit.gamboost3=rf("gamboost")
fit.logitboost3=rf("LogitBoost")
fit.rpart3=rf("rpart")
fit.ctree3=rf("ctree")
fit.bstLm3=rf("BstLm")
fit.knn3=rf("knn")
fit.nb3=rf("nb")
#fit.j48=rf("J48")
fit.c53=rf("C5.0")
fit.lda3=rf("lda")
#fit.ada=rf("ada")
fit.svmLinear3=rf("svmLinear")
fit.svmRadial3=rf("svmRadial")
fit.hdda3=rf("hdda")
fit.qda3=rf("qda")
fit.LMT3=rf("LMT")
fit.nnet3=rf("nnet")
fit.pda3=rf("pda")
fit.mda3=rf("mda")
fit.lvq3=rf("lvq")
fit.Mlda3=rf("Mlda")
fit.fda3=rf("fda")
fit.pam3=rf("pam")
fit.kernelpls3=rf("kernelpls")
fit.pls3=rf("pls")
fit.gpls3=rf("gpls")
#fit.rf=rf("rf")
fit.mlp3=rf("mlp")
fit.glm3=rf("glm")

results3=resamples(list(treebag=fit.treebag3,glmboost=fit.glmboost3,gamboost=fit.gamboost3,rpart=fit.rpart3,
                       ctree=fit.ctree3,knn=fit.knn3,nb=fit.nb3,LDA=fit.lda3,C5=fit.c53,SVMLinear=fit.svmLinear3, 
                       SVMRadial=fit.svmRadial3, LMT=fit.LMT3, PDA=fit.pda3,       
                       HDDA=fit.hdda3,QDA=fit.qda3, MDA=fit.mda3, LVQ=fit.lvq3,
                       LogitBoost=fit.logitboost3, MLDA=fit.Mlda3,FDA=fit.fda3,PAM=fit.pam3,
                       KERNELPLS=fit.kernelpls3,GPLS=fit.gpls3,MLP=fit.mlp3,GLM=fit.glm3))
summary(results3)
bwplot(results3)
