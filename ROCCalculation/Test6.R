library(caret)

dataset6=read.csv('diabetes-f6.csv')

dataset6[,1:3]=scale(dataset6[,1:3])

control <- trainControl(method="repeatedcv", number=10, repeats=10)
metric <- "Accuracy"


rf6<- function(x){
  set.seed(7)
  return(train(as.character(Outcome)~., data=dataset6, metric=metric, method=x, trControl=control)) 
} 

fit.gamboost6=rf6("gamboost")
fit.regLogistic6=rf6("regLogistic")
fit.multinom6=rf6("multinom")
fit.bayesglm6=rf6("bayesglm")
fit.plr6=rf6("plr")
fit.glm6=rf6("glm")
fit.gpls6=rf6("gpls")
fit.svmLinear36=rf6("svmLinear3")
fit.sparseLDA6=rf6("sparseLDA")
fit.pda6=rf6("pda")
fit.lda6=rf6("lda")
fit.LMT6=rf6("LMT")
fit.RFlda6=rf6("RFlda")
fit.fda6=rf6("fda")
fit.kernelpls6=rf6("kernelpls")
fit.sda6=rf6("sda")
fit.sdwd6=rf6("sdwd")
fit.glmboost6=rf6("glmboost")
fit.pda26=rf6("pda2")
fit.gbm6=rf6("gbm")
fit.gcvEarth6=rf6("gcvEarth")
fit.svmLinear6=rf6("svmLinear")
fit.glmStepAIC6=rf6("glmStepAIC")
fit.gamLoess6=rf6("gamLoess")
fit.loclda6=rf6("loclda")
fit.hdda6=rf6("hdda")
fit.parRF6=rf6("parRF")
fit.Mlda6=rf6("Mlda")
fit.svmRadial6=rf6("svmRadial")
fit.c56=rf6("C5.0")
fit.mlp6=rf6("mlp")
fit.mda6=rf6("mda")
fit.nb6=rf6("nb")
fit.rpart26=rf6("rpart2")
fit.c5rules6=rf6("C5.0Rules")
fit.treebag6=rf6("treebag")

results6=resamples(list(GAMBOOST=fit.gamboost6,REGLOGISTIC=fit.regLogistic6,MULTINOM=fit.multinom6,BAYESGLM=fit.bayesglm6,
                        PLR=fit.plr6,GLM=fit.glm6,GPLS=fit.gpls6,SVMLINEAR3=fit.svmLinear36,SPARSELDA=fit.sparseLDA6,
                        PDA=fit.pda6,LDA=fit.lda6,LMT=fit.LMT6,RFLDA=fit.RFlda6,FDA=fit.fda6,KERNELPLS=fit.kernelpls6,
                        SDA=fit.sda6,SDWD=fit.sdwd6,GLMBOOST=fit.glmboost6,PDA2=fit.pda26,GBM=fit.gbm6,GCVEARTH=fit.gcvEarth6,
                        SVMLINEAR=fit.svmLinear6,GLMSTEPAIC=fit.glmStepAIC6,GAMLOESS=fit.gamLoess6,LOCLDA=fit.loclda6,
                        HDDA=fit.hdda6,PARRF=fit.parRF6,MLDA=fit.Mlda6,SVMRADIAL=fit.svmRadial6,C5=fit.c56,MLP=fit.mlp6,
                        MDA=fit.mda6,NB=fit.nb6,RPART2=fit.rpart26,C5RULES=fit.c5rules6,TREEBAG=fit.treebag6))

summary(results6)
bwplot(results6)