library(caret)

dataset1=read.csv('diabetes-f1.csv')

dataset1[,1:5]=scale(dataset1[,1:5])

control <- trainControl(method="repeatedcv", number=10, repeats=10)
metric <- "Accuracy"


rf1<- function(x){
  set.seed(7)
  return(train(as.character(Outcome)~., data=dataset1, metric=metric, method=x, trControl=control)) 
} 

fit.gamboost1=rf1("gamboost")
fit.regLogistic1=rf1("regLogistic")
fit.multinom1=rf1("multinom")
fit.bayesglm1=rf1("bayesglm")
fit.plr1=rf1("plr")
fit.glm1=rf1("glm")
fit.gpls1=rf1("gpls")
fit.svmLinear31=rf1("svmLinear3")
fit.sparseLDA1=rf1("sparseLDA")
fit.pda1=rf1("pda")
fit.lda1=rf1("lda")
fit.LMT1=rf1("LMT")
fit.RFlda1=rf1("RFlda")
fit.fda1=rf1("fda")
fit.kernelpls1=rf1("kernelpls")
fit.sda1=rf1("sda")
fit.sdwd1=rf1("sdwd")
fit.glmboost1=rf1("glmboost")
fit.pda21=rf1("pda2")
fit.gbm1=rf1("gbm")
fit.gcvEarth1=rf1("gcvEarth")
fit.svmLinear1=rf1("svmLinear")
fit.glmStepAIC1=rf1("glmStepAIC")
fit.gamLoess1=rf1("gamLoess")
fit.loclda1=rf1("loclda")
fit.hdda1=rf1("hdda")
fit.parRF1=rf1("parRF")
fit.Mlda1=rf1("Mlda")
fit.svmRadial1=rf1("svmRadial")
fit.c51=rf1("C5.0")
fit.mlp1=rf1("mlp")
fit.mda1=rf1("mda")
fit.nb1=rf1("nb")
fit.rpart21=rf1("rpart2")
fit.c5rules1=rf1("C5.0Rules")
fit.treebag1=rf1("treebag")


results1=resamples(list(GAMBOOST=fit.gamboost1,REGLOGISTIC=fit.regLogistic1,MULTINOM=fit.multinom1,BAYESGLM=fit.bayesglm1,
                       PLR=fit.plr1,GLM=fit.glm1,GPLS=fit.gpls1,SVMLINEAR3=fit.svmLinear31,SPARSELDA=fit.sparseLDA1,
                       PDA=fit.pda1,LDA=fit.lda1,LMT=fit.LMT1,RFLDA=fit.RFlda1,FDA=fit.fda1,KERNELPLS=fit.kernelpls1,
                       SDA=fit.sda1,SDWD=fit.sdwd1,GLMBOOST=fit.glmboost1,GBM=fit.gbm1,GCVEARTH=fit.gcvEarth1,
                       SVMLINEAR=fit.svmLinear1,GLMSTEPAIC=fit.glmStepAIC1,GAMLOESS=fit.gamLoess1,LOCLDA=fit.loclda1,
                       HDDA=fit.hdda1,PARRF=fit.parRF1,MLDA=fit.Mlda1,SVMRADIAL=fit.svmRadial1,C5=fit.c51,MLP=fit.mlp1,
                       MDA=fit.mda1,NB=fit.nb1,RPART2=fit.rpart21,C5RULES=fit.c5rules1,TREEBAG=fit.treebag1))
dotplot(results1)
