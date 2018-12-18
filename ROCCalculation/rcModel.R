controlrc <- trainControl(method="repeatedcv", number=10, repeats=10, classProbs=TRUE, summaryFunction=twoClassSummary)
metricrc <- "ROC"

rfrc<-function(x,y){
  set.seed(7)
  return(train(Outcome~., data=x, metric=metricrc, method=y, trControl=controlrc))
}


summarize_res <- function(x){return(resamples(modellist))}

diabetesList = list()
diabetesf1List = list()
diabetesf2List = list()
diabetesf3List = list()
diabetesf4List = list()
diabetesf5List = list()
diabetesf6List = list()

for (i in algolistrc) {diabetesList[[length(diabetesList)+1]]=rfrc(dataset,i)}
for (i in algolistrc) {diabetesf1List[[length(diabetesf1List)+1]]=rfrc(diabetes_data$diabetesf1,i)}
for (i in algolistrc) {diabetesf2List[[length(diabetesf2List)+1]]=rfrc(diabetes_data$diabetesf2,i)}
for (i in algolistrc) {diabetesf3List[[length(diabetesf3List)+1]]=rfrc(diabetes_data$diabetesf3,i)}
for (i in algolistrc) {diabetesf4List[[length(diabetesf4List)+1]]=rfrc(diabetes_data$diabetesf4,i)}
for (i in algolistrc) {diabetesf5List[[length(diabetesf5List)+1]]=rfrc(diabetes_data$diabetesf5,i)}
for (i in algolistrc) {diabetesf6List[[length(diabetesf6List)+1]]=rfrc(diabetes_data$diabetesf6,i)}


result_diabetes=summarize_res(diabetesList)
result_diabetesf1=summarize_res(diabetesf1List)
result_diabetesf2=summarize_res(diabetesf2List)
result_diabetesf3=summarize_res(diabetesf3List)
result_diabetesf4=summarize_res(diabetesf4List)
result_diabetesf5=summarize_res(diabetesf5List)
result_diabetesf6=summarize_res(diabetesf6List)