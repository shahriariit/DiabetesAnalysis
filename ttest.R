library(caret)
library(mlbench)

dataset=read.csv('diabetes.csv')

dataset[,1:8]=scale(dataset[,1:8])

control <- trainControl(method="repeatedcv", number=10, repeats=10)
control2 <- trainControl(method="repeatedcv", number=10, repeats=10, classProbs=TRUE, summaryFunction=twoClassSummary)
control3 <- trainControl(method="repeatedcv", number=10, repeats=10, classProbs=TRUE, summaryFunction=mnLogLoss)


rf<- function(x){
  set.seed(7)
  return(train(as.character(Outcome)~., data=dataset, metric=metric, method=x, trControl=control)) 
} 

#fit.adaboost=rf("adaboost")
fit.treebag=rf("treebag")
fit.vglmAdjCat=rf("vglmAdjCat")
fit.bayesglm=rf("bayesglm")
fit.rpart1SE=rf("rpart1SE")
fit.rpart2=rf("rpart2")
fit.ctree2=rf("ctree2")
fit.vglmContRatio=rf("vglmContRatio")
fit.rpartcost=rf("rpartCost")
fit.vglmCumulative=rf("vglmCumulative")
fit.gamLoess=rf("gamLoess")
fit.RFlda=rf("RFlda")
fit.glmnet=rf("glmnet")
fit.protoclass=rf("protoclass")
fit.kknn=rf("kknn")
fit.svmLinearWeights2=rf("svmLinearWeights2")
fit.svmLinear3=rf("svmLinear3")
fit.lda2=rf("lda2")
fit.loclda=rf("loclda")
fit.mlpML=rf("mlpML")
fit.gcvEarth=rf("gcvEarth")
fit.naive_bayes=rf("naive_bayes")
fit.null=rf("null")
fit.parRF=rf("parRF")
fit.widekernelpls=rf("widekernelpls")
fit.pda=rf("pda")
fit.pda2=rf("pda2")
fit.plr=rf("plr")
fit.multinom=rf("multinom")
fit.rda=rf("rda")
fit.regLogistic=rf("regLogistic")
fit.linda=rf("Linda")
fit.RSimca=rf("RSimca")
fit.rocc=rf("rocc")
fit.part=rf("PART")
fit.sda=rf("sda")
fit.c5rules=rf("C5.0Rules")
fit.c5trees=rf("C5.0Tree")
fit.oneR=rf("OneR")
fit.sdwd=rf("sdwd")
fit.sparseLDA=rf("sparseLDA")
fit.slda=rf("slda")
fit.gbm=rf("gbm")
fit.glmboost=rf("glmboost")
fit.gamboost=rf("gamboost")
fit.logitboost=rf("LogitBoost")
fit.rpart=rf("rpart")
fit.ctree=rf("ctree")
fit.bstLm=rf("BstLm")
fit.knn=rf("knn")
fit.nb=rf("nb")
#fit.j48=rf("J48")
fit.c5=rf("C5.0")
fit.lda=rf("lda")
#fit.ada=rf("ada")
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
#fit.rf=rf("rf")
fit.mlp=rf("mlp")
fit.glm=rf("glm")



#results=resamples(list(adaboost=fit.adaboost,treebag=fit.treebag,
#                       glmboost=fit.glmboost,gamboost=fit.gamboost,LogitBoost=fit.logitboost,
#                       rpart=fit.rpart,ctree=fit.ctree,knn=fit.knn,nb=fit.nb,J48=fit.j48,
#                       LDA=fit.lda,C5=fit.c5,SVMLinear=fit.svmLinear, 
#                       SVMRadial=fit.svmRadial, LMT=fit.LMT, PDA=fit.pda,       
#                       HDDA=fit.hdda,QDA=fit.qda, MDA=fit.mda, LVQ=fit.lvq,
#                        MLDA=fit.Mlda,FDA=fit.fda,PAM=fit.pam,
#                       KERNELPLS=fit.kernelpls,GPLS=fit.gpls,RF=fit.rf,MLP=fit.mlp
#                       ,GLM=fit.glm))

results=resamples(list(TREEBAG=fit.treebag,VGLMADJCAT=fit.vglmAdjCat, BAYESGLM=fit.bayesglm,
                       RPART1SE=fit.rpart1SE,RPART2=fit.rpart2,CTREE2=fit.ctree2,
                       VGLMCONTRATIO=fit.vglmContRatio, RPARTCOST=fit.rpartcost,
                       VGLMCUMULATIVE=fit.vglmCumulative, GAMLOESS=fit.gamLoess,
                       RFLDA=fit.RFlda,GLMSTEPAIC=fit.glmStepAIC,GLMNET=fit.glmnet,
                       PROTOCLASS=fit.protoclass,KKNN=fit.kknn,SVMLINEARWEIGHT2=fit.svmLinearWeights2,
                       SVMLINEAR3=fit.svmLinear3,LDA2=fit.lda2,LOCLDA=fit.loclda,MLPML=fit.mlpML,
                       GCVEarth=fit.gcvEarth,NAIVEBAYES=fit.naive_bayes,NULLMODEL=fit.null,PARRF=fit.parRF,
                       WIDEKERNELPLS=fit.widekernelpls,PDA2=fit.pda2,PLR=fit.plr,MULTINOM=fit.multinom,
                       RDA=fit.rda,REGLOGISTIC=fit.regLogistic,LINDA=fit.linda, RSIMCA=fit.RSimca,ROCC=fit.rocc,
                       PART=fit.part,SDA=fit.sda, C5RULES=fit.c5rules,C5TREES=fit.c5trees,
                       ONER=fit.oneR,SDWD=fit.sdwd, SPARSELDA=fit.sparseLDA, SLDA=fit.slda, GBM=fit.gbm,
                       GLMBOOST=fit.glmboost,GAMBOOST=fit.gamboost,RPART=fit.rpart,
                       CTREE=fit.ctree,KNN=fit.knn,NB=fit.nb,LDA=fit.lda,c5.0=fit.c5,SVMLINEAR=fit.svmLinear, 
                       SVMRADIAL=fit.svmRadial, LMT=fit.LMT, PDA=fit.pda,       
                       HDDA=fit.hdda,QDA=fit.qda, MDA=fit.mda, LVQ=fit.lvq,
                       LogitBOOST=fit.logitboost, MLDA=fit.Mlda,FDA=fit.fda,PAM=fit.pam,
                       KERNELPLS=fit.kernelpls,GPLS=fit.gpls,MLP=fit.mlp,GLM=fit.glm))


summary(results)
bwplot(results)