

###CREATE TRAIN CONTROL for cross-validation and hyperparameter selection
tune.control <- trainControl(
  method = 'cv',
  number = 10, 
  classProbs = TRUE,
  verboseIter = TRUE, 
  returnData = FALSE, 
  savePredictions = TRUE,
  summaryFunction = twoClassSummary, 
)


### DEFINE MODEL FUNCTION ####
train_model <- function(x, y, specs, tune_grid, meth, label_it){
  set.seed(1)
  model.x <- train(x = x, 
                   y = y, 
                   trControl = specs, 
                   tuneGrid = tune_grid, 
                   method = meth, 
                   weights = model_weights,
                   importance = 'permutation'
                   
  )
  prediction.class <- predict(model.x, test)
  prediction.prob <- predict(model.x, test, type = 'prob')[['drink']]
  assign(paste('predict.',label_it, '.day', sep = ''), prediction.prob, envir = .GlobalEnv)
  assign(paste('predict.', label_it, sep = ''), prediction.class, envir = .GlobalEnv)
  return(model.x)
}

model_train <- as.numeric(label_train == 'not_drink')
model_weights <- ifelse(label_train == 'not_drink',
                        (1/table(model_train)[2]) * 0.5,
                        (1/table(model_train)[1]) * 0.5)

#### TRAIN RANDOM FOREST CLASSIFIER ####

grid.RF = expand.grid(mtry = seq(1,15, by = 1), splitrule = 'gini', min.node.size = 1)
model.RF <- train_model(x = train, y = label_train, specs = tune.control, tune_grid = grid.RF, meth = 'ranger', label_it = 'rf')

##retain label vector for ROC figure
label_test.day <- label_test

sink('day_training.txt')

cat('Model mtry = ', as.numeric(model.RF$bestTune))
##print confusion matrix outcome for cross validation set
besttune_df <- filter(model.RF$pred, mtry == as.numeric(model.RF$bestTune$mtry), min.node.size == as.numeric(model.RF$bestTune$min.node.size))

cat('Confusion matrix for cross validation set')
print(caret::confusionMatrix(besttune_df$pred, besttune_df$obs, positive = 'drink', mode = 'everything'))
print(epi.tests(caret::confusionMatrix(besttune_df$pred, besttune_df$obs, positive = 'drink', mode = 'everything')$table), method = 'exact', conf.level = 0.95)
print(cohen.kappa(caret::confusionMatrix(besttune_df$pred, besttune_df$obs, positive = 'drink', mode = 'everything')$table))
##print confusion matrix outcome for test set

cat('\n\n\n confusion matrix for site 1 test set')
print(caret::confusionMatrix(predict(model.RF, site1_pred), site1_label, positive = 'drink', mode = 'everything'))
print(epi.tests(caret::confusionMatrix(predict(model.RF, site1_pred), site1_label, positive = 'drink', mode = 'everything')$table, method = 'exact', conf.level = 0.95))
print(cohen.kappa(caret::confusionMatrix(predict(model.RF, site1_pred), site1_label, positive = 'drink', mode = 'everything')$table))

cat('\n confusion matrix for site 2 test set')
print(caret::confusionMatrix(predict(model.RF, site2_pred), site2_label, positive = 'drink', mode = 'everything'))
print(epi.tests(caret::confusionMatrix(predict(model.RF, site2_pred), site2_label, positive = 'drink', mode = 'everything')$table, method = 'exact', conf.level = 0.95))
print(cohen.kappa(caret::confusionMatrix(predict(model.RF, site2_pred), site2_label, positive = 'drink', mode = 'everything')$table))

cat('\n\n\n confusion matrix for test set')
print(caret::confusionMatrix(predict.rf, label_test, positive = 'drink', mode = 'everything'))
print(epi.tests(caret::confusionMatrix(predict.rf, label_test, positive = 'drink', mode = 'everything')$table, method = 'exact', conf.level = 0.95))
print(cohen.kappa(caret::confusionMatrix(predict.rf, label_test, positive = 'drink', mode = 'everything')$table))

##print AUC for CV set
cat('\n\n\n AUC for cross validation set\n')
getTrainPerf(model.RF)
##print AUC for test set
cat('\n\n\n AUC for site 1')
pROC::auc(response = site1_label, predictor = predict(model.RF, site1_pred, type = 'prob')[['drink']])
cat('\n\n\n The ICI for test site 1 is', 
    ici(predict(model.RF, site1_pred, type = 'prob')[['drink']], as.numeric(site1_label == 'drink')))
cat('\n\n\n AUC for site 2')
pROC::auc(response = site2_label, predictor = predict(model.RF, site2_pred, type = 'prob')[['drink']])
cat('\n\n\n The ICI for test site 2 is', 
    ici(predict(model.RF, site2_pred, type = 'prob')[['drink']], as.numeric(site2_label == 'drink')))
cat('\n\n\n AUC for full test set')
pROC::auc(response = label_test, predictor = predict.rf.day)
cat('\n\n\n The ICI for the combined test sites sample is', 
    ici(predict(model.RF, test, type = 'prob')[['drink']], as.numeric(label_test == 'drink')))
sink()

sink('day to day importance.txt')
print((varImp(model.RF, scaled = FALSE)$importance) %>% arrange(desc(Overall)))
sink()