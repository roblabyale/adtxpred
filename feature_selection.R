
endpoint_name = 'no_hd'

id_list <- unique(df$ID)
set.seed(1)
sample_it <- sample(c(TRUE, FALSE), size = length(id_list), prob = c(0.8, 0.2), replace = TRUE)

##set no_hd outcome as separate object

df <- filter(df, !is.na(no_hd)) ###remove missing weeks 
outcome <- df$no_hd
id_hold <- df$ID
df <- select(df, -c('no_hd','sesh', 'ID'))

######PARTITION DATASETS INTO TRAINING AND TEST SETS##########
sites <- unique(df$center)
set.seed(1)
hold_out <- sample(sites, 2)
sample_it <- !(df$center %in% hold_out)

train <- df[sample_it, ]
test <- df[!sample_it, ]
label_train <- outcome[sample_it] %>%
  as.matrix %>% factor(labels = c('not_drink','drink'))

label_test <- outcome[!sample_it] %>%
  as.matrix %>% factor(labels = c('not_drink','drink'))

#preprocess data using via imputations and center/scaling 
set.seed(1)
preprocess_it <- preProcess(as.data.frame(train), method = c('bagImpute', 'center', 'scale'))


##modal replacement of missing categorical variables
calc_mode <- function(x){
  distinct_values <- unique(x)
  distinct_values <- distinct_values[!is.na(distinct_values)]
  tab <- tabulate(match(x, distinct_values))
  return(distinct_values[which.max(tab)])
}

train <- mutate(train,
                across(where(is.factor), ~replace_na(.x, calc_mode(.x)
                )
                )
)

test <- mutate(test,
                across(where(is.factor), ~replace_na(.x, calc_mode(.x)
                )
                )
)
##

test <- predict(preprocess_it, newdata = test)
train <- predict(preprocess_it, newdata = train)

###prepare matrix for boruta and pull out the LSO 
df_boruta <- cbind(train, label_train)
colnames(df_boruta)[length(df_boruta)] <- 'no_hd'

train_log <- train
test_log <- test
label_train_log <- label_train
label_test_log <- label_test

site1_pred_log <- test[test$center == hold_out[1], ] %>% select(!center)
site2_pred_log <- test[test$center == hold_out[2], ] %>% select(!center)

site1_label_log <- label_test[test$center == hold_out[1]]
site2_label_log <- label_test[test$center == hold_out[2]]

###train boruta model
set.seed(1)
model.boruta <- Boruta(
  formula = no_hd~.,
  data = df_boruta,
  doTrace = 2, 
  pValue = 0.01,
  maxRun = 400
)

bor_retain <- getSelectedAttributes(model.boruta, withTentative = TRUE)
bor_retain <- append(bor_retain, 'center')

train <- train[,colnames(train) %in% bor_retain]
test <- test[,colnames(test) %in% bor_retain]

site1_pred <- test[test$center == hold_out[1], ] %>% select(!center)
site2_pred <- test[test$center == hold_out[2], ] %>% select(!center)

site1_label <- label_test[test$center == hold_out[1]]
site2_label <- label_test[test$center == hold_out[2]]

train <- select(train, -center)
test <- select(test, -center)
