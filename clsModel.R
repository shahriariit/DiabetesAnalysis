library(caret)

control <- trainControl(method="repeatedcv", number=10, repeats=10)
metric <- "Accuracy"

algolist=c("bayesglm", "rpart2","glmnet","svmLinear3","loclda",
           "gcvEarth","parRF","pda2","plr","multinom","regLogistic",
           "sda","C5.0Rules","sdwd","sparseLDA","gbm","glmboost",
           "gamboost","nb","C5.0","lda","svmLinear","svmRadial",
           "hdda","LMT","fda","kernelpls","pls","gpls","mlp","glm")
algolist01=c("bayesglm", "rpart2","glmnet","svmLinear3","loclda",
             "gcvEarth","parRF","pda2","plr","multinom")

rf<-function(x,y){
  set.seed(7)
  return(train(as.character(Outcome)~., data=x, metric=metric, method=y, trControl=control))
}


summarize_res <- function(x){
  
  modellist=list(BAYESGLM=x[[1]],RPART2=x[[2]],SVMLINEAR3=x[[3]],LOCLDA=x[[4]],GCVEarth=x[[5]],PARRF=x[[6]],PDA2=x[[7]],
                 PLR=x[[8]],MULTINOM=x[[9]],REGLOGISTIC=x[[10]],SDA=x[[11]], C5RULES=x[[12]],SDWD=x[[13]], SPARSELDA=x[[14]], 
                 GBM=x[[15]],GLMBOOST=X[[16]],GAMBOOST=x[[17]],NB=x[[18]],c5.0=x[[19]],LDA=x[[20]],
                 SVMLINEAR=x[[21]],SVMRADIAL=x[[22]],,HDDA=x[[23]], LMT=x[[24]],FDA=x[[25]], KERNELPLS=x[[26]],PLS=x[[27]],
                 GPLS=x[[28]],MLP=x[[29]],GLM=x[[30]])
  
  
  return(resamples(modellist))
  
}

diabetesList = list()
diabetesf1List = list()
diabetesf2List = list()
diabetesf3List = list()
diabetesf4List = list()
diabetesf5List = list()
diabetesf6List = list()

for (i in algolist01) {diabetesList[[length(diabetesList)+1]]=rf(diabetes_data$diabetes,i)}
for (i in algolist01) {diabetesf1List[[length(diabetesf1List)+1]]=rf(diabetes_data$diabetesf1,i)}
for (i in algolist) {diabetesf2List[[length(diabetesf2List)+1]]=rf(diabetes_data$diabetesf2,i)}
for (i in algolist) {diabetesf3List[[length(diabetesf3List)+1]]=rf(diabetes_data$diabetesf3,i)}
for (i in algolist) {diabetesf4List[[length(diabetesf4List)+1]]=rf(diabetes_data$diabetesf4,i)}
for (i in algolist) {diabetesf5List[[length(diabetesf5List)+1]]=rf(diabetes_data$diabetesf5,i)}
for (i in algolist) {diabetesf6List[[length(diabetesf6List)+1]]=rf(diabetes_data$diabetesf6,i)}


result_diabetes=summarize_res(diabetesList)
result_diabetesf1=summarize_res(diabetesf1List)
result_diabetesf2=summarize_res(diabetesf2List)
result_diabetesf3=summarize_res(diabetesf3List)
result_diabetesf4=summarize_res(diabetesf4List)
result_diabetesf5=summarize_res(diabetesf5List)
result_diabetesf6=summarize_res(diabetesf6List)

