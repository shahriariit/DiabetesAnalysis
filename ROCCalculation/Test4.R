library(caret)

dataset4=read.csv('diabetes-f4.csv')

dataset4[,1:5]=scale(dataset4[,1:5])

control <- trainControl(method="repeatedcv", number=10, repeats=10)
metric <- "Accuracy"


rf4<- function(x){
  set.seed(7)
  return(train(as.character(Outcome)~., data=dataset4, metric=metric, method=x, trControl=control)) 
} 

fit.gamboost4=rf4("gamboost")
fit.regLogistic4=rf4("regLogistic")
fit.multinom4=rf4("multinom")
fit.bayesglm4=rf4("bayesglm")
fit.plr4=rf4("plr")
fit.glm4=rf4("glm")
fit.gpls4=rf4("gpls")
fit.svmLinear34=rf4("svmLinear3")
fit.sparseLDA4=rf4("sparseLDA")
fit.pda4=rf4("pda")
fit.lda4=rf4("lda")
fit.LMT4=rf4("LMT")
fit.RFlda4=rf4("RFlda")
fit.fda4=rf4("fda")
fit.kernelpls4=rf4("kernelpls")
fit.sda4=rf4("sda")
fit.sdwd4=rf4("sdwd")
fit.glmboost4=rf4("glmboost")
fit.pda24=rf4("pda2")
fit.gbm4=rf4("gbm")
fit.gcvEarth4=rf4("gcvEarth")
fit.svmLinear4=rf4("svmLinear")
fit.glmStepAIC4=rf4("glmStepAIC")
fit.gamLoess4=rf4("gamLoess")
fit.loclda4=rf4("loclda")
fit.hdda4=rf4("hdda")
fit.parRF4=rf4("parRF")
fit.Mlda4=rf4("Mlda")
fit.svmRadial4=rf4("svmRadial")
fit.c54=rf4("C5.0")
fit.mlp4=rf4("mlp")
fit.mda4=rf4("mda")
fit.nb4=rf4("nb")
fit.rpart24=rf4("rpart2")
fit.c5rules4=rf("C5.0Rules")
fit.treebag4=rf4("treebag")


results4=resamples(list(GAMBOOST=fit.gamboost4,REGLOGISTIC=fit.regLogistic4,MULTINOM=fit.multinom4,BAYESGLM=fit.bayesglm4,
                       PLR=fit.plr4,GLM=fit.glm4,GPLS=fit.gpls4,SVMLINEAR3=fit.svmLinear34,SPARSELDA=fit.sparseLDA4,
                       PDA=fit.pda4,LDA=fit.lda4,LMT=fit.LMT4,RFLDA=fit.RFlda4,FDA=fit.fda4,KERNELPLS=fit.kernelpls4,
                       SDA=fit.sda4,SDWD=fit.sdwd4,GLMBOOST=fit.glmboost4,PDA2=fit.pda24,GBM=fit.gbm4,GCVEARTH=fit.gcvEarth4,
                       SVMLINEAR=fit.svmLinear4,GLMSTEPAIC=fit.glmStepAIC4,GAMLOESS=fit.gamLoess,LOCLDA=fit.loclda,
                       HDDA=fit.hdda,PARRF=fit.parRF,MLDA=fit.Mlda,SVMRADIAL=fit.svmRadial,C5=fit.c5,MLP=fit.mlp,
                       MDA=fit.mda,NB=fit.nb,RPART2=fit.rpart2,C5RULES=fit.c5rules,TREEBAG=fit.treebag))
summary(results4)
bwplot(results4)
