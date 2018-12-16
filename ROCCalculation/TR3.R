library(caret)

dataset3=read.csv('diabetes-f3.csv')

dataset3[,1:5]=scale(dataset3[,1:5])

control <- trainControl(method="repeatedcv", number=10, repeats=10)
metric <- "Accuracy"


rf3<- function(x){
  set.seed(7)
  return(train(as.character(Outcome)~., data=dataset3, metric=metric, method=x, trControl=control)) 
} 

fit.gamboost3=rf3("gamboost")
fit.regLogistic3=rf3("regLogistic")
fit.multinom3=rf3("multinom")
fit.bayesglm3=rf3("bayesglm")
fit.plr3=rf3("plr")
fit.glm3=rf3("glm")
fit.gpls3=rf3("gpls")
fit.svmLinear33=rf3("svmLinear3")
fit.sparseLDA3=rf3("sparseLDA")
fit.pda3=rf3("pda")
fit.lda3=rf3("lda")
fit.LMT3=rf3("LMT")
fit.RFlda3=rf3("RFlda")
fit.fda3=rf3("fda")
fit.kernelpls3=rf3("kernelpls")
fit.sda3=rf3("sda")
fit.sdwd3=rf3("sdwd")
fit.glmboost3=rf3("glmboost")
fit.gbm3=rf3("gbm")
fit.gcvEarth3=rf3("gcvEarth")
fit.svmLinear3=rf3("svmLinear")
fit.glmStepAIC3=rf3("glmStepAIC")
fit.gamLoess3=rf3("gamLoess")
fit.loclda3=rf3("loclda")
fit.hdda3=rf3("hdda")
fit.parRF3=rf3("parRF")
fit.Mlda3=rf3("Mlda")
fit.svmRadial3=rf3("svmRadial")
fit.c53=rf3("C5.0")
fit.mlp3=rf3("mlp")
fit.mda3=rf3("mda")
fit.nb3=rf3("nb")
fit.rpart23=rf3("rpart2")
fit.c5rules3=rf3("C5.0Rules")
fit.treebag3=rf3("treebag")


results3=resamples(list(GAMBOOST=fit.gamboost3,REGLOGISTIC=fit.regLogistic3,MULTINOM=fit.multinom3,BAYESGLM=fit.bayesglm3,
                       PLR=fit.plr3,GLM=fit.glm3,GPLS=fit.gpls3,SVMLINEAR3=fit.svmLinear33,SPARSELDA=fit.sparseLDA3,
                       PDA=fit.pda3,LDA=fit.lda3,LMT=fit.LMT3,RFLDA=fit.RFlda3,FDA=fit.fda3,KERNELPLS=fit.kernelpls3,
                       SDA=fit.sda3,SDWD=fit.sdwd3,GLMBOOST=fit.glmboost3,PDA2=fit.pda23,GBM=fit.gbm3,GCVEARTH=fit.gcvEarth3,
                       SVMLINEAR=fit.svmLinear3,GLMSTEPAIC=fit.glmStepAIC3,GAMLOESS=fit.gamLoess3,LOCLDA=fit.loclda3,
                       HDDA=fit.hdda3,PARRF=fit.parRF3,MLDA=fit.Mlda3,SVMRADIAL=fit.svmRadial3,C5=fit.c53,MLP=fit.mlp3,
                       MDA=fit.mda3,NB=fit.nb3,RPART2=fit.rpart23,C5RULES=fit.c5rules3,TREEBAG=fit.treebag3))
summary(results3)
bwplot(results3)
