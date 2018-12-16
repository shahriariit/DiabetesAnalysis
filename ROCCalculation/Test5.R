library(caret)

dataset5=read.csv('diabetes-f5.csv')

dataset5[,1:4]=scale(dataset5[,1:4])

control <- trainControl(method="repeatedcv", number=10, repeats=10)
metric <- "Accuracy"


rf5<- function(x){
  set.seed(7)
  return(train(as.character(Outcome)~., data=dataset5, metric=metric, method=x, trControl=control)) 
} 

fit.gamboost5=rf5("gamboost")
fit.regLogistic5=rf5("regLogistic")
fit.multinom5=rf5("multinom")
fit.bayesglm5=rf5("bayesglm")
fit.plr5=rf5("plr")
fit.glm5=rf5("glm")
fit.gpls5=rf5("gpls")
fit.svmLinear35=rf5("svmLinear3")
fit.sparseLDA5=rf5("sparseLDA")
fit.pda5=rf5("pda")
fit.lda5=rf5("lda")
fit.LMT5=rf5("LMT")
fit.RFlda5=rf5("RFlda")
fit.fda5=rf5("fda")
fit.kernelpls5=rf5("kernelpls")
fit.sda5=rf5("sda")
fit.sdwd5=rf5("sdwd")
fit.glmboost5=rf5("glmboost")
fit.pda25=rf5("pda2")
fit.gbm5=rf5("gbm")
fit.gcvEarth5=rf5("gcvEarth")
fit.svmLinear5=rf5("svmLinear")
fit.glmStepAIC5=rf5("glmStepAIC")
fit.gamLoess5=rf5("gamLoess")
fit.loclda5=rf5("loclda")
fit.hdda5=rf5("hdda")
fit.parRF5=rf5("parRF")
fit.Mlda5=rf5("Mlda")
fit.svmRadial5=rf5("svmRadial")
fit.c55=rf5("C5.0")
fit.mlp5=rf5("mlp")
fit.mda5=rf5("mda")
fit.nb5=rf5("nb")
fit.rpart25=rf5("rpart2")
fit.c5rules5=rf5("C5.0Rules")
fit.treebag5=rf5("treebag")


results5=resamples(list(GAMBOOST=fit.gamboost5,REGLOGISTIC=fit.regLogistic5,MULTINOM=fit.multinom5,BAYESGLM=fit.bayesglm5,
                       PLR=fit.plr5,GLM=fit.glm5,GPLS=fit.gpls5,SVMLINEAR3=fit.svmLinear35,SPARSELDA=fit.sparseLDA5,
                       PDA=fit.pda5,LDA=fit.lda5,LMT=fit.LMT5,RFLDA=fit.RFlda5,FDA=fit.fda5,KERNELPLS=fit.kernelpls5,
                       SDA=fit.sda5,SDWD=fit.sdwd5,GLMBOOST=fit.glmboost5,PDA2=fit.pda25,GBM=fit.gbm5,GCVEARTH=fit.gcvEarth5,
                       SVMLINEAR=fit.svmLinear5,GLMSTEPAIC=fit.glmStepAIC5,GAMLOESS=fit.gamLoess5,LOCLDA=fit.loclda5,
                       HDDA=fit.hdda5,PARRF=fit.parRF5,MLDA=fit.Mlda5,SVMRADIAL=fit.svmRadial5,C5=fit.c55,MLP=fit.mlp5,
                       MDA=fit.mda5,NB=fit.nb5,RPART2=fit.rpart25,C5RULES=fit.c5rules5,TREEBAG=fit.treebag5))
summary(results5)
bwplot(results5)