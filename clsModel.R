library(caret)

control <- trainControl(method="repeatedcv", number=10, repeats=10)
metric <- "Accuracy"


rf<-function(x,y){
  set.seed(7)
  return(train(Outcome~., data=x, metric=metric, method=y, trControl=control))
}


diabetesfitList = list()
diabetesfit1List = list()
diabetesfit2List = list()
diabetesfit3List = list()
diabetesfit4List = list()
diabetesfit5List = list()
diabetesfit6List = list()

for (i in algolistrc) {diabetesfitList[[length(diabetesfitList)+1]]=rf(diabetes_data$diabetes,i)}
for (i in algolistrc) {diabetesfit1List[[length(diabetesfit1List)+1]]=rf(diabetes_data$diabetesf1,i)}
for (i in algolistrc) {diabetesfit2List[[length(diabetesfit2List)+1]]=rf(diabetes_data$diabetesf2,i)}
for (i in algolistrc) {diabetesfit3List[[length(diabetesfit3List)+1]]=rf(diabetes_data$diabetesf3,i)}
for (i in algolistrc) {diabetesfit4List[[length(diabetesfit4List)+1]]=rf(diabetes_data$diabetesf4,i)}
for (i in algolistrc) {diabetesfit5List[[length(diabetesfit5List)+1]]=rf(diabetes_data$diabetesf5,i)}
for (i in algolistrc) {diabetesfit6List[[length(diabetesfit6List)+1]]=rf(diabetes_data$diabetesf6,i)}


result_diabetesfit=summarize_res(diabetesfitList)
result_diabetesfit1=summarize_res(diabetesfit1List)
result_diabetesfit2=summarize_res(diabetesfit2List)
result_diabetesfit3=summarize_res(diabetesfit3List)
result_diabetesfit4=summarize_res(diabetesfit4List)
result_diabetesfit5=summarize_res(diabetesfit5List)
result_diabetesfit6=summarize_res(diabetesfit6List)

