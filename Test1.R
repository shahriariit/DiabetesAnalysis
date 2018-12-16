library(caret)

dataset1=read.csv('diabetes-f1.csv')

dataset1[,1:5]=scale(dataset1[,1:5])

control <- trainControl(method="repeatedcv", number=10, repeats=10)
metric <- "Accuracy"


rf1<- function(x){
  set.seed(7)
  return(train(as.character(Outcome)~., data=dataset1, metric=metric, method=x, trControl=control)) 
} 

fit.bayesglm1=rf1("bayesglm")
fit.rpart21=rf1("rpart2")
fit.svmLinear31=rf1("svmLinear3")
fit.loclda1=rf1("loclda")
fit.gcvEarth1=rf1("gcvEarth")
fit.parRF1=rf1("parRF")
fit.pda21=rf1("pda2")
fit.plr1=rf1("plr")
fit.multinom1=rf1("multinom")
fit.regLogistic1=rf1("regLogistic")
fit.sda1=rf1("sda")
fit.c5rules1=rf1("C5.0Rules")
fit.sdwd1=rf1("sdwd")
fit.sparseLDA1=rf1("sparseLDA")
fit.gbm1=rf1("gbm")
fit.glmboost1=rf1("glmboost")
fit.gamboost1=rf1("gamboost")
fit.nb1=rf1("nb")
fit.c51=rf1("C5.0")
fit.lda1=rf1("lda")
fit.svmLinear1=rf1("svmLinear")
fit.svmRadial1=rf1("svmRadial")
fit.hdda1=rf1("hdda")
fit.LMT1=rf1("LMT")
fit.fda1=rf1("fda")
fit.kernelpls1=rf1("kernelpls")
fit.gpls1=rf1("gpls")
fit.mlp1=rf1("mlp")
fit.glm1=rf1("glm")


#results1=resamples(list(treebag=fit.treebag1,glmboost=fit.glmboost1,gamboost=fit.gamboost1,rpart=fit.rpart1,
#                       ctree=fit.ctree1,knn=fit.knn1,nb=fit.nb1,LDA=fit.lda1,C5=fit.c51,SVMLinear=fit.svmLinear1, 
#                       SVMRadial=fit.svmRadial1, LMT=fit.LMT1, PDA=fit.pda1,       
#                       HDDA=fit.hdda1,QDA=fit.qda1, MDA=fit.mda1, LVQ=fit.lvq1,
#                       LogitBoost=fit.logitboost1, MLDA=fit.Mlda1,FDA=fit.fda1,PAM=fit.pam1,
#                       KERNELPLS=fit.kernelpls1,GPLS=fit.gpls1,MLP=fit.mlp1,GLM=fit.glm1))

results1=resamples(list(BAYESGLM=fit.bayesglm1,RPART2=fit.rpart21,SVMLINEAR3=fit.svmLinear31,LOCLDA=fit.loclda1,
                        GCVEarth=fit.gcvEarth1,PARRF=fit.parRF1,PLR=fit.plr1,MULTINOM=fit.multinom1,
                        REGLOGISTIC=fit.regLogistic1,SDA=fit.sda1,C5RULES=fit.c51,SDWD=fit.sdwd1, SPARSELDA=fit.sparseLDA1, 
                        GBM=fit.gbm1,GLMBOOST=fit.glmboost1,GAMBOOST=fit.gamboost1,NB=fit.nb1,c5.0=fit.c51,LDA=fit.lda1,
                        SVMLINEAR=fit.svmLinear1,SVMRADIAL=fit.svmRadial1,,HDDA=fit.hdda1, LMT=fit.LMT1,FDA=fit.fda1, 
                        KERNELPLS=fit.kernelpls1,GPLS=fit.gpls1,MLP=fit.mlp1,GLM=fit.glm1))
bwplot(results1)
