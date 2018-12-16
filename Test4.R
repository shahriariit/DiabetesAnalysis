library(caret)

dataset4=read.csv('diabetes-f4.csv')

dataset4[,1:5]=scale(dataset4[,1:5])

control <- trainControl(method="repeatedcv", number=10, repeats=10)
metric <- "Accuracy"


rf<- function(x){
  set.seed(7)
  return(train(as.character(Outcome)~., data=dataset4, metric=metric, method=x, trControl=control)) 
} 

#fit.adaboost=rf("adaboost")
fit.treebag4=rf("treebag")
fit.glmboost4=rf("glmboost")
fit.gamboost4=rf("gamboost")
fit.logitboost4=rf("LogitBoost")
fit.rpart4=rf("rpart")
fit.ctree4=rf("ctree")
fit.bstLm4=rf("BstLm")
fit.knn4=rf("knn")
fit.nb4=rf("nb")
#fit.j48=rf("J48")
fit.c54=rf("C5.0")
fit.lda4=rf("lda")
#fit.ada=rf("ada")
fit.svmLinear4=rf("svmLinear")
fit.svmRadial4=rf("svmRadial")
fit.hdda4=rf("hdda")
fit.qda4=rf("qda")
fit.LMT4=rf("LMT")
fit.nnet4=rf("nnet")
fit.pda4=rf("pda")
fit.mda4=rf("mda")
fit.lvq4=rf("lvq")
fit.Mlda4=rf("Mlda")
fit.fda4=rf("fda")
fit.pam4=rf("pam")
fit.kernelpls4=rf("kernelpls")
fit.pls4=rf("pls")
fit.gpls4=rf("gpls")
#fit.rf=rf("rf")
fit.mlp4=rf("mlp")
fit.glm4=rf("glm")

results4=resamples(list(treebag=fit.treebag4,glmboost=fit.glmboost4,gamboost=fit.gamboost4,rpart=fit.rpart4,
                        ctree=fit.ctree4,knn=fit.knn4,nb=fit.nb4,LDA=fit.lda4,C5=fit.c54,SVMLinear=fit.svmLinear4, 
                        SVMRadial=fit.svmRadial4, LMT=fit.LMT4, PDA=fit.pda4,       
                        HDDA=fit.hdda4,QDA=fit.qda4, MDA=fit.mda4, LVQ=fit.lvq4,
                        LogitBoost=fit.logitboost4, MLDA=fit.Mlda4,FDA=fit.fda4,PAM=fit.pam4,
                        KERNELPLS=fit.kernelpls4,GPLS=fit.gpls4,MLP=fit.mlp4,GLM=fit.glm4))
summary(results4)
bwplot(results4)
