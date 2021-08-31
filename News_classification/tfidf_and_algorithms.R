# tf idf
library(superml)
library(data.table)
# algorithms
library(caret)
library(caTools)
library(Matrix)
library(rpart) 
library(rpart.plot)
library(e1071) 
library(dplyr)
library(ggplot2)
library(psych)
library(RWeka)
#------------------------------
# import data
get_data <- function(name_csv){
  data <- read.csv(name_csv)
  return(data)
}
#---------------------------------
#tf idf
create_tf_idf_vectors <- function(data_text){
  # define tfidf model
  tfv <- TfIdfVectorizer$new(ngram_range = c(1,3), max_df = 0.7) 
  # we fit on train data
  tfv$fit(data_text)
  # apply pre-trained tf-idf transformation to test data
  train_tf_features <- tfv$transform(data_text) 
}
#---------------------------------

add_tfidf_into_dataframe <- function (df_tfidf, data_Preprocessing){
  # add tfidf into dtaframe
  for(i in 1:nrow(df_tfidf)) {
    
    data_Preprocessing$tfidf[[i]] <-df_tfidf[[i]]
    return(data_Preprocessing)
  }
}

#---------------------------------
get_confusion_matrix <- function(predict, test_target ){
  table_mat <- table(predict, test_target)
  return(table_mat)  
}
get_model_evaluation <- function(table_mat){
  cm <-confusionMatrix(table_mat)
  print(cm)
  accuracy_Test <-mean(cm$byClass["Sensitivity"])
  print(accuracy_Test)
}
#---------------------------------------------------------- Rpart decision tree
rpart_decision_tree <- function(train, test, test_target){
  fit <- rpart(target~., data = train, method = 'class')
  rpart.plot(fit, extra = 106)
  # Make predictions
  predict_unseen <-predict(fit, test, type = 'class')
  predict_unseen
  # Confusion Matrix
  table_mat <- get_confusion_matrix(predict_unseen, test_target)
  table_mat
  # Model Evaluation
  get_model_evaluation(table_mat)
}
#---------------------------------------------------------- KNN

knn_alg <-  function(train, train_target, test, test_target){
  knn_model <-knn(train=train, test=test, cl=train$target, k=2)
  # Confusion Matrix
  table_mat <- get_confusion_matrix(knn_model, test$target)
  table_mat
  # Model Evaluation
  get_model_evaluation(table_mat)
}

#------------------------------------------------------------ Naive Bayes
naive_bayes <-  function(train, test, test_target){
  nb <- NBTrainer$new()
  nb$fit(train, 'target')
  # Make predictions
  pred <- nb$predict(test)
  # Confusion Matrix
  table_mat <- get_confusion_matrix(pred, test_target)
  # Model Evaluation
  get_model_evaluation(table_mat)
}

#------------------------------------------------------------Xgboost
xgboost <-  function(train, test, test_target){
  xgb <- XGBTrainer$new(objective = "binary:logistic"
                        , n_estimators = 500
                        , eval_metric = "auc"
                        , maximize = T
                        , learning_rate = 0.1
                        ,max_depth = 6)
  xgb$fit(X = train, y = "target", valid = test)
  # Make predictions
  pred <- xgb$predict(test)
  # Confusion Matrix
  table_mat <- get_confusion_matrix(pred, test_target)
  # Model Evaluation
  get_model_evaluation(table_mat)
}

#------------------------------------------------------------Random Forest
random_forest<- function(train, test, test_target){
  rf <- RFTrainer$new(max_features = 50, n_estimators = 100)
  rf$fit(train, 'target')
  # make predictions
  preds <- rf$predict(test)
  print(preds[1:10])
  # Confusion Matrix
  table_mat <- get_confusion_matrix(preds, test_target)
  table_mat
  # Model Evaluation
  get_model_evaluation(table_mat)
}

#-----------------------------------------------------------Random Search
# Random Search
random_search <- function(train){
  rf <- RFTrainer$new()
  rst <- RandomSearchCV$new(trainer = rf,
                            parameters = list(n_estimators = c(10,50), max_depth = c(5,2)),
                            n_folds = 3,
                            scoring = c('accuracy','auc'),
                            n_iter = 3)
  rst$fit(train, "target")
  rst$best_iteration()
}

#----------------------------------------------------------- Grid Search
# Grid Search
grid_search <- function(train){
  xgb <- XGBTrainer$new(objective="binary:logistic")
  gst <-GridSearchCV$new(trainer = xgb,
                         parameters = list(n_estimators = c(10,50),
                                           max_depth = c(5,2)),
                         n_folds = 5,
                         scoring = c('accuracy','auc'))
  gst$fit(train, "target")
  gst$best_iteration()
}

#----------------------------------------------------------- C4.5 
# C4.5 algorithm is an extension of the ID3 algorithm
c4_5 <- function(train, test, test_target){
  # Fit model
  fit <- J48(target~., data=train)
  # Summarize the fit
  summary(fit)
  # Make predictions
  predictions <- predict(fit, test)
  # Confusion Matrix
  table_mat <- get_confusion_matrix(predictions, test_target)
  table_mat
  # Model Evaluation
  get_model_evaluation(table_mat)
}

main <-function(){
  # Get data preprocessing
  data_Preprocessing<-get_data("Preprocessing.csv")
  # Get tfidf
  tf_feats <-create_tf_idf_vectors(data_Preprocessing$title_text)
  data_Preprocessing <-add_tfidf_into_dataframe(data.frame(tf_feats), data_Preprocessing)
  # Get train, test, traget
  df <- data_Preprocessing
  n_rows <- nrow(df)*0.8
  train <- data.frame(tf_feats[1:n_rows,])
  train$target <- df[1:n_rows,]$y
  test <- data.frame(tf_feats[n_rows:nrow(df),])
  test$target <- df[n_rows:nrow(df),'y']
  # Run algorithms
  rpart_decision_tree(train, test, test$target)
  knn_alg(train, train$target, test, test$target)
  naive_bayes(train, test, test$target)
  xgboost(train, test, test$target)
  random_forest(train, test, test$target)
  random_search(train)
  grid_search(train)
  c4_5(train, test, test$target)
} 