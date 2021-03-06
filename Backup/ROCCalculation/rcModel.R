controlrc <- trainControl(method="repeatedcv", number=10, repeats=10, classProbs=TRUE, summaryFunction=twoClassSummary)
metricrc <- "ROC"

rfrc<-function(x,y){
  set.seed(7)
  return(train(Outcome~., data=x, metric=metricrc, method=y, trControl=controlrc))
}


summarize_res <- function(x){return(resamples(modellist))}

diabetesrcList = list()
diabetesfrc1List = list()
diabetesfrc2List = list()
diabetesfrc3List = list()
diabetesfrc4List = list()
diabetesfrc5List = list()
diabetesfrc6List = list()

for (i in algolistrc) {diabetesrcList[[length(diabetesrcList)+1]]=rfrc(diabetes_data$diabetes,i)}
for (i in algolistrc) {diabetesfrc1List[[length(diabetesfrc1List)+1]]=rfrc(diabetes_data$diabetesf1,i)}
for (i in algolistrc) {diabetesfrc2List[[length(diabetesfrc2List)+1]]=rfrc(diabetes_data$diabetesf2,i)}
for (i in algolistrc) {diabetesfrc3List[[length(diabetesfrc3List)+1]]=rfrc(diabetes_data$diabetesf3,i)}
for (i in algolistrc) {diabetesfrc4List[[length(diabetesfrc4List)+1]]=rfrc(diabetes_data$diabetesf4,i)}
for (i in algolistrc) {diabetesfrc5List[[length(diabetesfrc5List)+1]]=rfrc(diabetes_data$diabetesf5,i)}
for (i in algolistrc) {diabetesfrc6List[[length(diabetesfrc6List)+1]]=rfrc(diabetes_data$diabetesf6,i)}


result_diabetes=summarize_res(diabetesrcList)
result_diabetesf1=summarize_res(diabetesfrc1List)
result_diabetesf2=summarize_res(diabetesfrc2List)
result_diabetesf3=summarize_res(diabetesfrc3List)
result_diabetesf4=summarize_res(diabetesfrc4List)
result_diabetesf5=summarize_res(diabetesfrc5List)
result_diabetesf6=summarize_res(diabetesfrc6List)