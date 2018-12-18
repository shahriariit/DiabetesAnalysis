library(caret)
library(mlbench)

data(PimaIndiansDiabetes)
dataset=PimaIndiansDiabetes

dataset[,1:8]=scale(dataset[,1:8])

control2 <- trainControl(method="repeatedcv", number=10, repeats=10, classProbs=TRUE, summaryFunction=twoClassSummary)
metric2 <- "ROC"


rfs<- function(x){
  set.seed(7)
  return(train(diabetes~., data=dataset, metric=metric2, method=x, trControl=control2)) 
} 

fr.gamboost=rfs("gamboost")
fr.regLogistic=rfs("regLogistic")
fr.multinom=rfs("multinom")
fr.bayesglm=rfs("bayesglm")
fr.plr=rfs("plr")
fr.glm=rfs("glm")
fr.sparseLDA=rfs("sparseLDA")
fr.pda=rfs("pda")
fr.lda=rfs("lda")
fr.LMT=rfs("LMT")
fr.fda=rfs("fda")
fr.kernelpls=rfs("kernelpls")
fr.sda=rfs("sda")
fr.sdwd=rfs("sdwd")
fr.glmboost=rfs("glmboost")
fr.pda2=rfs("pda2")
fr.gbm=rfs("gbm")
fr.gcvEarth=rfs("gcvEarth")
fr.svmLinear=rfs("svmLinear")
fr.glmStepAIC=rfs("glmStepAIC")
fr.gamLoess=rfs("gamLoess")
fr.hdda=rfs("hdda")
fr.parRF=rfs("parRF")
fr.svmRadial=rfs("svmRadial")
fr.c5=rfs("C5.0")
fr.mlp=rfs("mlp")
fr.mda=rfs("mda")
fr.nb=rfs("nb")
fr.rpart2=rfs("rpart2")
fr.c5rules=rfs("C5.0Rules")
fr.treebag=rfs("treebag")


resultsroc=resamples(list(GAMBOOST=fr.gamboost,REGLOGISTIC=fr.regLogistic,MULTINOM=fr.multinom,BAYESGLM=fr.bayesglm,
                       PLR=fr.plr,GLM=fr.glm,SPARSELDA=fr.sparseLDA,PDA=fr.pda,LDA=fr.lda,LMT=fr.LMT,FDA=fr.fda,
                       KERNELPLS=fr.kernelpls,SDA=fr.sda,SDWD=fr.sdwd,GLMBOOST=fr.glmboost,PDA2=fr.pda2,GBM=fr.gbm,GCVEARTH=fr.gcvEarth,
                       SVMLINEAR=fr.svmLinear,GLMSTEPAIC=fr.glmStepAIC,GAMLOESS=fr.gamLoess,
                       HDDA=fr.hdda,PARRF=fr.parRF,SVMRADIAL=fr.svmRadial,C5=fr.c5,MLP=fr.mlp,
                       MDA=fr.mda,NB=fr.nb,RPART2=fr.rpart2,C5RULES=fr.c5rules,TREEBAG=fr.treebag))




summary(resultsroc)
dotplot(resultsroc)