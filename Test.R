library(caret)

dataset=read.csv('diabetes.csv')

control <- trainControl(method="repeatedcv", number=10, repeats=10)
metric <- "Accuracy"


rf<- function(x){
  set.seed(7)
  return(train(as.character(Outcome)~., data=dataset, metric=metric, method=x, trControl=control)) 
} 



fit.adaboost=rf("adaboost")
fit.treebag=rf("treebag")
fit.glmboost=rf("glmboost")
fit.gamboost=rf("gamboost")
fit.logitboost=rf("LogitBoost")
fit.rpart=rf("rpart")
fit.ctree=rf("ctree")
fit.bstLm=rf("BstLm")
fit.knn=rf("knn")
fit.nb=rf("nb")
fit.j48=rf("J48")
fit.c5=rf("C5.0")
fit.lda=rf("lda")
fit.ada=rf("ada")
fit.svmLinear=rf("svmLinear")
fit.svmRadial=rf("svmRadial")
fit.hdda=rf("hdda")
fit.qda=rf("qda")
fit.LMT=rf("LMT")
fit.nnet=rf("nnet")
fit.pda=rf("pda")
fit.mda=rf("mda")
fit.lvq=rf("lvq")
fit.Mlda=rf("Mlda")
fit.fda=rf("fda")
fit.pam=rf("pam")
fit.kernelpls=rf("kernelpls")
fit.pls=rf("pls")
fit.gpls=rf("gpls")
fit.rf=rf("rf")
fit.mlp=rf("mlp")
fit.glm=rf("glm")



results=resamples(list(adaboost=fit.adaboost,treebag=fit.treebag,
                       glmboost=fit.glmboost,gamboost=fit.gamboost,rpart=fit.rpart,
                       ctree=fit.ctree,knn=fit.knn,nb=fit.nb,J48=fit.j48,
                       LDA=fit.lda,C5=fit.c5,SVMLinear=fit.svmLinear, 
                       SVMRadial=fit.svmRadial, LMT=fit.LMT, PDA=fit.pda,       
                       HDDA=fit.hdda,QDA=fit.qda, MDA=fit.mda, LVQ=fit.lvq,
                       LogitBoost=fit.logitboost, MLDA=fit.Mlda,FDA=fit.fda,PAM=fit.pam,
                       KERNELPLS=fit.kernelpls,GPLS=fit.gpls,RF=fit.rf,MLP=fit.mlp
                       ,GLM=fit.glm))
summary(results)
bwplot(results)