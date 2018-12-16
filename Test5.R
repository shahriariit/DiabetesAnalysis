library(caret)

dataset5=read.csv('diabetes-f5.csv')

dataset5[,1:4]=scale(dataset5[,1:4])

control <- trainControl(method="repeatedcv", number=10, repeats=10)
metric <- "Accuracy"


rf<- function(x){
  set.seed(7)
  return(train(as.character(Outcome)~., data=dataset4, metric=metric, method=x, trControl=control)) 
} 

#fit.adaboost=rf("adaboost")
fit.treebag5=rf("treebag")
fit.glmboost5=rf("glmboost")
fit.gamboost5=rf("gamboost")
fit.logitboost5=rf("LogitBoost")
fit.rpart5=rf("rpart")
fit.ctree5=rf("ctree")
fit.bstLm5=rf("BstLm")
fit.knn5=rf("knn")
fit.nb5=rf("nb")
#fit.j48=rf("J48")
fit.c55=rf("C5.0")
fit.lda5=rf("lda")
#fit.ada=rf("ada")
fit.svmLinear5=rf("svmLinear")
fit.svmRadial5=rf("svmRadial")
fit.hdda5=rf("hdda")
fit.qda5=rf("qda")
fit.LMT5=rf("LMT")
fit.nnet5=rf("nnet")
fit.pda5=rf("pda")
fit.mda5=rf("mda")
fit.lvq5=rf("lvq")
fit.Mlda5=rf("Mlda")
fit.fda5=rf("fda")
fit.pam5=rf("pam")
fit.kernelpls5=rf("kernelpls")
fit.pls5=rf("pls")
fit.gpls5=rf("gpls")
#fit.rf=rf("rf")
fit.mlp5=rf("mlp")
fit.glm5=rf("glm")

results5=resamples(list(treebag=fit.treebag5,glmboost=fit.glmboost5,gamboost=fit.gamboost5,rpart=fit.rpart5,
                        ctree=fit.ctree5,knn=fit.knn5,nb=fit.nb5,LDA=fit.lda5,C5=fit.c55,SVMLinear=fit.svmLinear5, 
                        SVMRadial=fit.svmRadial5, LMT=fit.LMT5, PDA=fit.pda5,       
                        HDDA=fit.hdda5,QDA=fit.qda5, MDA=fit.mda5, LVQ=fit.lvq5,
                        LogitBoost=fit.logitboost5, MLDA=fit.Mlda5,FDA=fit.fda5,PAM=fit.pam5,
                        KERNELPLS=fit.kernelpls5,GPLS=fit.gpls5,MLP=fit.mlp5,GLM=fit.glm5))
summary(results5)
bwplot(results5)