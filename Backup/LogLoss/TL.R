library(caret)
library(mlbench)

data(PimaIndiansDiabetes)
dataset=PimaIndiansDiabetes

dataset[,1:8]=scale(dataset[,1:8])

control3 <- trainControl(method="repeatedcv", number=10, repeats=10, classProbs=TRUE, summaryFunction=mnLogLoss)
metric3 <- "logLoss"


rfl<- function(x){
  set.seed(7)
  return(train(diabetes~., data=dataset, metric=metric3, method=x, trControl=control3)) 
}

fl.gamboost=rfl("gamboost")
fl.regLogistic=rfl("regLogistic")
fl.multinom=rfl("multinom")
fl.bayesglm=rfl("bayesglm")
fl.plr=rfl("plr")
fl.glm=rfl("glm")
fl.sparseLDA=rfl("sparseLDA")
fl.pda=rfl("pda")
fl.lda=rfl("lda")
fl.LMT=rfl("LMT")
fl.fda=rfl("fda")
fl.kernelpls=rfl("kernelpls")
fl.sda=rfl("sda")
fl.sdwd=rfl("sdwd")
fl.glmboost=rfl("glmboost")
fl.gbm=rfl("gbm")
fl.gcvEarth=rfl("gcvEarth")
fl.svmLinear=rfl("svmLinear")
fl.glmStepAIC=rfl("glmStepAIC")
fl.gamLoess=rfl("gamLoess")
fl.hdda=rfl("hdda")
fl.parRF=rfl("parRF")
fl.svmRadial=rfl("svmRadial")
fl.c5=rfl("C5.0")
fl.mlp=rfl("mlp")
fl.mda=rfl("mda")
fl.nb=rfl("nb")
fl.rpart2=rfl("rpart2")
fl.c5rules=rfl("C5.0Rules")
fl.treebag=rfl("treebag")

resultsll=resamples(list(GAMBOOST=fl.gamboost,REGLOGISTIC=fl.regLogistic,MULTINOM=fl.multinom,BAYESGLM=fl.bayesglm,
                          PLR=fl.plr,GLM=fl.glm,SPARSELDA=fl.sparseLDA,PDA=fl.pda,LDA=fl.lda,LMT=fl.LMT,FDA=fl.fda,
                          KERNELPLS=fl.kernelpls,SDA=fl.sda,SDWD=fl.sdwd,GLMBOOST=fl.glmboost,PDA2=fl.pda2,GBM=fl.gbm,GCVEARTH=fl.gcvEarth,
                          SVMLINEAR=fl.svmLinear,GLMSTEPAIC=fl.glmStepAIC,GAMLOESS=fl.gamLoess,
                          HDDA=fl.hdda,PARRF=fl.parRF,SVMRADIAL=fl.svmRadial,C5=fl.c5,MLP=fl.mlp,
                          MDA=fl.mda,NB=fl.nb,RPART2=fl.rpart2,C5RULES=fl.c5rules,TREEBAG=fl.treebag))

