controlrc <- trainControl(method="repeatedcv", number=10, repeats=10, classProbs=TRUE, summaryFunction=twoClassSummary)
metricrc <- "ROC"

rfrc<-function(x,y){
  set.seed(7)
  return(train(Outcome~., data=x, metric=metricrc, method=y, trControl=controlrc))
}



diabetesllList = list()
diabetesfl1List = list()
diabetesfl2List = list()
diabetesfl3List = list()
diabetesfl4List = list()
diabetesfl5List = list()
diabetesfl6List = list()

for (i in algolistrc) {diabetesllList[[length(diabetesllList)+1]]=rfrc(dataset,i)}
for (i in algolistrc) {diabetesfl1List[[length(diabetesfl1List)+1]]=rfrc(diabetes_data$diabetesf1,i)}
for (i in algolistrc) {diabetesfl2List[[length(diabetesfl2List)+1]]=rfrc(diabetes_data$diabetesf2,i)}
for (i in algolistrc) {diabetesfl3List[[length(diabetesfl3List)+1]]=rfrc(diabetes_data$diabetesf3,i)}
for (i in algolistrc) {diabetesfl4List[[length(diabetesfl4List)+1]]=rfrc(diabetes_data$diabetesf4,i)}
for (i in algolistrc) {diabetesfl5List[[length(diabetesfl5List)+1]]=rfrc(diabetes_data$diabetesf5,i)}
for (i in algolistrc) {diabetesfl6List[[length(diabetesfl6List)+1]]=rfrc(diabetes_data$diabetesf6,i)}


result_diabetes=summarize_res(diabetesllList)
result_diabetesfl1=summarize_res(diabetesfl1List)
result_diabetesfl2=summarize_res(diabetesfl2List)
result_diabetesfl3=summarize_res(diabetesfl3List)
result_diabetesfl4=summarize_res(diabetesfl4List)
result_diabetesfl5=summarize_res(diabetesfl5List)
result_diabetesfl6=summarize_res(diabetesfl6List)