library(caret)

dataset2=read.csv('diabetes-f2.csv')

dataset2[,1:5]=scale(dataset2[,1:5])

control <- trainControl(method="repeatedcv", number=10, repeats=10)
metric <- "Accuracy"


rf2<- function(x){
  set.seed(7)
  return(train(as.character(Outcome)~., data=dataset2, metric=metric, method=x, trControl=control)) 
} 

fit.gamboost2=rf2("gamboost")
fit.regLogistic2=rf2("regLogistic")
fit.multinom2=rf2("multinom")
fit.bayesglm2=rf2("bayesglm")
fit.plr2=rf2("plr")
fit.glm2=rf2("glm")
fit.gpls2=rf2("gpls")
fit.svmLinear32=rf2("svmLinear3")
fit.sparseLDA2=rf2("sparseLDA")
fit.pda2=rf2("pda")
fit.lda2=rf2("lda")
fit.LMT2=rf2("LMT")
fit.RFlda2=rf2("RFlda")
fit.fda2=rf2("fda")
fit.kernelpls2=rf2("kernelpls")
fit.sda2=rf2("sda")
fit.sdwd2=rf2("sdwd")
fit.glmboost2=rf2("glmboost")
fit.pda22=rf2("pda2")
fit.gbm2=rf2("gbm")
fit.gcvEarth2=rf2("gcvEarth")
fit.svmLinear2=rf2("svmLinear")
fit.glmStepAIC2=rf2("glmStepAIC")
fit.gamLoess2=rf2("gamLoess")
fit.loclda2=rf2("loclda")
fit.hdda2=rf2("hdda")
fit.parRF2=rf2("parRF")
fit.Mlda2=rf2("Mlda")
fit.svmRadial2=rf2("svmRadial")
fit.c52=rf2("C5.0")
fit.mlp2=rf2("mlp")
fit.mda2=rf2("mda")
fit.nb2=rf2("nb")
fit.rpart22=rf2("rpart2")
fit.c5rules2=rf("C5.0Rules")
fit.treebag2=rf2("treebag")


results2=resamples(list(GAMBOOST=fit.gamboost2,REGLOGISTIC=fit.regLogistic2,MULTINOM=fit.multinom2,BAYESGLM=fit.bayesglm2,
                       PLR=fit.plr2,GLM=fit.glm2,GPLS=fit.gpls2,SVMLINEAR3=fit.svmLinear32,SPARSELDA=fit.sparseLDA2,
                       LDA=fit.lda2,LMT=fit.LMT2,RFLDA=fit.RFlda2,FDA=fit.fda2,KERNELPLS=fit.kernelpls2,
                       SDA=fit.sda2,SDWD=fit.sdwd2,GLMBOOST=fit.glmboost2,GBM=fit.gbm2,GCVEARTH=fit.gcvEarth2,
                       SVMLINEAR=fit.svmLinear2,GLMSTEPAIC=fit.glmStepAIC2,GAMLOESS=fit.gamLoess2,LOCLDA=fit.loclda2,
                       HDDA=fit.hdda2,PARRF=fit.parRF2,MLDA=fit.Mlda2,SVMRADIAL=fit.svmRadial2,C5=fit.c52,MLP=fit.mlp2,
                       MDA=fit.mda2,NB=fit.nb2,RPART2=fit.rpart22,C5RULES=fit.c5rules2,TREEBAG=fit.treebag2))

dotplot(results2)
