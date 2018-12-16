library(caret)

dataset6=read.csv('diabetes-f6.csv')

dataset6[,1:3]=scale(dataset6[,1:3])

control <- trainControl(method="repeatedcv", number=10, repeats=10)
metric <- "Accuracy"


rf<- function(x){
  set.seed(7)
  return(train(as.character(Outcome)~., data=dataset6, metric=metric, method=x, trControl=control)) 
} 

#fit.adaboost=rf("adaboost")
fit.treebag6=rf("treebag")
fit.glmboost6=rf("glmboost")
fit.gamboost6=rf("gamboost")
fit.logitboost6=rf("LogitBoost")
fit.rpart6=rf("rpart")
fit.ctree6=rf("ctree")
fit.bstLm6=rf("BstLm")
fit.knn6=rf("knn")
fit.nb6=rf("nb")
#fit.j48=rf("J48")
fit.c56=rf("C5.0")
fit.lda6=rf("lda")
#fit.ada=rf("ada")
fit.svmLinear6=rf("svmLinear")
fit.svmRadial6=rf("svmRadial")
fit.hdda6=rf("hdda")
fit.qda6=rf("qda")
fit.LMT6=rf("LMT")
fit.nnet6=rf("nnet")
fit.pda6=rf("pda")
fit.mda6=rf("mda")
fit.lvq6=rf("lvq")
fit.Mlda6=rf("Mlda")
fit.fda6=rf("fda")
fit.pam6=rf("pam")
fit.kernelpls6=rf("kernelpls")
fit.pls6=rf("pls")
fit.gpls6=rf("gpls")
#fit.rf=rf("rf")
fit.mlp6=rf("mlp")
fit.glm6=rf("glm")

results6=resamples(list(treebag=fit.treebag6,glmboost=fit.glmboost6,gamboost=fit.gamboost6,rpart=fit.rpart6,
                        ctree=fit.ctree6,knn=fit.knn6,nb=fit.nb6,LDA=fit.lda6,C5=fit.c56,SVMLinear=fit.svmLinear6, 
                        SVMRadial=fit.svmRadial6, LMT=fit.LMT6, PDA=fit.pda6,       
                        HDDA=fit.hdda6,QDA=fit.qda6, MDA=fit.mda6, LVQ=fit.lvq6,
                        LogitBoost=fit.logitboost6, MLDA=fit.Mlda6,FDA=fit.fda6,PAM=fit.pam6,
                        KERNELPLS=fit.kernelpls6,GPLS=fit.gpls6,MLP=fit.mlp6,GLM=fit.glm6))
summary(results6)
bwplot(results6)