feature_scaling <- function(x,y){
  return(scale(x[,y]))
}

diabetes_data$diabetes[,1:8]=feature_scaling(diabetes_data$diabetes,1:8)
diabetes_data$diabetesf1[,1:5]=feature_scaling(diabetes_data$diabetesf1,1:5)
diabetes_data$diabetesf2[,1:5]=feature_scaling(diabetes_data$diabetesf2,1:5)
diabetes_data$diabetesf3[,1:5]=feature_scaling(diabetes_data$diabetesf3,1:5)
diabetes_data$diabetesf4[,1:5]=feature_scaling(diabetes_data$diabetesf4,1:5)
diabetes_data$diabetesf5[,1:4]=feature_scaling(diabetes_data$diabetesf5,1:4)
diabetes_data$diabetesf6[,1:3]=feature_scaling(diabetes_data$diabetesf6,1:3)
