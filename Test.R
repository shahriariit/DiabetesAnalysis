library(caret)

dataset=read.csv('diabetes.csv')

dataset[,1:8]=scale(dataset[,1:8])

control <- trainControl(method="repeatedcv", number=10, repeats=10)
metric <- "Accuracy"


rf<- function(x){
  set.seed(7)
  return(train(as.character(Outcome)~., data=dataset, metric=metric, method=x, trControl=control)) 
} 

fit.gamboost=rf("gamboost")
fit.regLogistic=rf("regLogistic")
fit.multinom=rf("multinom")
fit.bayesglm=rf("bayesglm")
fit.plr=rf("plr")
fit.glm=rf("glm")
fit.gpls=rf("gpls")
fit.svmLinear3=rf("svmLinear3")
fit.sparseLDA=rf("sparseLDA")
fit.pda=rf("pda")
fit.lda=rf("lda")
fit.LMT=rf("LMT")
fit.RFlda=rf("RFlda")
fit.fda=rf("fda")
fit.kernelpls=rf("kernelpls")
fit.sda=rf("sda")
fit.sdwd=rf("sdwd")
fit.glmboost=rf("glmboost")
fit.pda2=rf("pda2")
fit.gbm=rf("gbm")
fit.gcvEarth=rf("gcvEarth")
fit.svmLinear=rf("svmLinear")
fit.glmStepAIC=rf("glmStepAIC")
fit.gamLoess=rf("gamLoess")
fit.loclda=rf("loclda")
fit.hdda=rf("hdda")
fit.parRF=rf("parRF")
fit.Mlda=rf("Mlda")
fit.svmRadial=rf("svmRadial")
fit.c5=rf("C5.0")
fit.mlp=rf("mlp")
fit.mda=rf("mda")
fit.nb=rf("nb")
fit.rpart2=rf("rpart2")
fit.c5rules=rf("C5.0Rules")
fit.treebag=rf("treebag")


results=resamples(list(GAMBOOST=fit.gamboost,REGLOGISTIC=fit.regLogistic,MULTINOM=fit.multinom,BAYESGLM=fit.bayesglm,
                       PLR=fit.plr,GLM=fit.glm,GPLS=fit.gpls,SVMLINEAR3=fit.svmLinear3,SPARSELDA=fit.sparseLDA,
                       PDA=fit.pda,LDA=fit.lda,LMT=fit.LMT,RFLDA=fit.RFlda,FDA=fit.fda,KERNELPLS=fit.kernelpls,
                       SDA=fit.sda,SDWD=fit.sdwd,GLMBOOST=fit.glmboost,PDA2=fit.pda2,GBM=fit.gbm,GCVEARTH=fit.gcvEarth,
                       SVMLINEAR=fit.svmLinear,GLMSTEPAIC=fit.glmStepAIC,GAMLOESS=fit.gamLoess,LOCLDA=fit.loclda,
                       HDDA=fit.hdda,PARRF=fit.parRF,MLDA=fit.Mlda,SVMRADIAL=fit.svmRadial,C5=fit.c5,MLP=fit.mlp,
                       MDA=fit.mda,NB=fit.nb,RPART2=fit.rpart2,C5RULES=fit.c5rules,TREEBAG=fit.treebag))




summary(results)
dotplot(results)