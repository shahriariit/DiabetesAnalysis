algolistrc=c("gamboost","regLogistic","multinom","bayesglm","plr","glm","sparseLDA","pda",
             "lda","LMT","fda","kernelpls","sda","sdwd","glmboost","gbm","gcvEarth",
             "svmLinear","glmStepAIC","gamLoess","hdda","parRF","svmRadial","C5.0",
             "mlp","mda","nb","rpart2","C5.0Rules","treebag")



summarize_res <- function(x){
  
  modellist=list(GAMBOOST=x[[1]],REGLOGISTIC=x[[2]],MULTINOM=x[[3]],BAYESGLM=x[[4]],PLR=x[[5]],GLM=x[[6]],SPARSELDA=x[[7]],
                 PDA=x[[8]],LDA=x[[9]],LMT=x[[10]],FDA=x[[11]], KERNELPLS=x[[12]],SDA=x[[13]], SDWD=x[[14]], 
                 GLMBOOST=x[[15]],GBM=x[[16]],GCVEARTH=x[[17]],SVMLINEAR=x[[18]],GLMSTEPAIC=x[[19]],GAMLOESS=x[[20]],
                 HDDA=x[[21]],PARRF=x[[22]],SVMRADIAL=x[[23]], C5=x[[24]],MLP=x[[25]], MDA=x[[26]],NB=x[[27]],
                 RPART2=x[[28]],C5RULES=x[[29]],TREEBAG=x[[30]])
  
  return(resamples(modellist))
}