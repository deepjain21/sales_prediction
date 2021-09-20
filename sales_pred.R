#=====Loading Libraries==============================
library(dplyr)
library(reticulate)
library(xts)
library(lubridate)
library(visdat)
library(DataExplorer)
library(SmartEDA)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(mltools)
library(data.table)
library(caret)
library(caTools)
library(randomForest)

#====== evalution score function
get_msle_score <- function(y_pred,y){
  
  square_log_error <- (y_pred - y)^2
  mean_square_log_error <- (sum(square_log_error)/length(y_pred))*1000
  
  return(mean_square_log_error)
  
}

# reading training data
data <- read.csv("D:/Deepak/datascience_learning_path/analytics_vidya/sales_pred/data/TRAIN.csv",
                 stringsAsFactors = FALSE)
# getting missing data information
missing_info_plot <- DataExplorer::plot_missing(data = data)
missing_info_plot




#getting structure of data 
data_info <- SmartEDA::ExpData(data = data,type =2)
View(data_info)


# data preprocessing 
data2 <- data %>% 
  dplyr::mutate(Store_id = as.factor(Store_id)) %>% 
  dplyr::mutate(Store_Type = as.factor(Store_Type)) %>% 
  dplyr::mutate(Location_Type = as.factor(Location_Type)) %>% 
  dplyr::mutate(Region_Code = as.factor(Region_Code)) %>% 
  dplyr::mutate(Holiday = if_else(Holiday == 1,"Yes", "No")) %>% 
  dplyr::mutate(Holiday = as.factor(Holiday)) %>% 
  dplyr::mutate(Discount = as.factor(Discount)) %>% 
  dplyr::mutate(Date = as.Date(Date,format = "%Y-%m-%d")) %>% 
  dplyr::filter(X.Order != 0 ) %>% 
  dplyr::mutate(Sales = log(Sales)) 
str(data2)

#========== EDA =======================================
# historgram for X.oder distribution
ggplot2::ggplot()+
  geom_bar(data = data,mapping=aes(x=X.Order))

# historgram foro Sales distribution
ggplot2::ggplot()+
  geom_histogram(data = data,mapping=aes(x=Sales))

ggplot()+
  geom_histogram(data = data2,mapping = aes(x = "Store_Type"),
                 stat = "count")

ggplot(data = data2)+
  geom_bar(aes(x = Store_Type,fill=Location_Type))

ggpubr::gghistogram(data = store_type_df,
                    x = "Var1",
                    y = "Freq",
                    color = "white",
                    palette = "jco")

class(data$Store_Type)
plot(data2$Store_Type)
geom_bar(data=data , aes(x = "Store_Type"))

ggplot(data = data, aes(x = Store_Type)) +
  geom_bar() +
  ggpubr::set_palette(palette = "jco")

ggboxplot(data2,x = "Store_Type",
          y ="Sales",
          color ="Store_Type",
          palette = "jco")

ggboxplot(data2,
          x = "Location_Type",
          y = "Sales",
          color = "Discount",
          palette = "jco")

ggboxplot(data2,
          x = "Region_Code",
          y = "Sales",
          color = "t2",
          palette = "jco")


get_categorical_graphs <- function(category_var, target_var){
  
  category_freq_summary <- table(data2[,c(category_var)]) %>% 
    data.frame() %>% 
    dplyr::mutate(per = round(Freq/sum(Freq)*100,0))%>% 
    dplyr::mutate(label = paste0(Var1,":",per,"%"))
  colnames(category_freq_summary)
  
  category_freq_graph <- ggpubr::ggpie(category_freq_summary,
                                       x = "per",
                                       label = "label",
                                       lab.pos = "in",
                                       lab.font = "white",
                                       fill = "Var1",
                                       color = "white",
                                       palette = "jco"
  ) +
    ggtitle(paste0(category_var," Distribution")) +
    labs(fill = category_var) +
    theme(legend.position = "right")
  category_freq_graph
  
  temp_data <- data2 %>% 
    dplyr::select(`category_var`, `target_var`)
  category_sales_summary <- temp_data %>% 
    dplyr::group_by_at(1) %>% 
    dplyr::summarise_at(1,sum) %>% 
    dplyr::mutate(total_sales_per = round(Sales/sum(Sales)*100,0)) %>% 
    dplyr::mutate(label = paste0(.[[1]],":",total_sales_per,"%")) 
  
  category_sales_graph <- ggpubr::ggpie(data = category_sales_summary,
                                        x = "total_sales_per",
                                        label = "label",
                                        lab.pos = "in",
                                        lab.font = "white",
                                        fill = category_var,
                                        color = "white",
                                        palette = "jco") +
    ggtitle(paste0(category_var, " Vs ",target_var))+
    labs(fill = category_var)+
    theme(legend.position = "right")
  category_sales_graph
  
  graph_list = list()
  graph_list[["category_freq_graph"]] <- category_freq_graph
  graph_list[["category_sales_graph"]] <- category_sales_graph
  
  return(graph_list)
}

store_type_graphs <- get_categorical_graphs("Store_Type","Sales")
location_type_graphs <- get_categorical_graphs("Location_Type","Sales")
region_type_graphs <- get_categorical_graphs("Region_Code","Sales")
discount_graphs <- get_categorical_graphs("Discount","Sales")
holiday_graphs <- get_categorical_graphs("Holiday","Sales")

all_cat_graphs <- c(store_type_graphs,location_type_graphs,
                    region_type_graphs,discount_graphs,holiday_graphs)

ggarrange(plotlist = all_cat_graphs,ncol=4,nrow = 3)


#scatter plot 
ggscatter(data = data,
          x = "X.Order",
          y = "Sales",
          color = "pink",
          palette = "jco",
          add = "reg.line",
          conf.int = TRUE)



#===================data feature engineering================
data3 <- data2 %>% 
  dplyr::mutate(dow = weekdays(Date)) %>% 
  dplyr::mutate(dow = as.factor(dow)) %>% 
  dplyr::mutate(month = month(Date)) %>% 
  dplyr::mutate(month = as.factor(month)) %>% 
  dplyr::mutate(is_weekend = if_else(dow %in% c("Saturday","Sunday"),"Yes","No")) %>% 
  dplyr::mutate(is_weekend = as.factor(is_weekend)) %>% 
  dplyr::mutate(store_location_region = paste0(Store_Type,Location_Type,Region_Code)) %>% 
  dplyr::mutate(store_location_region = as.factor(store_location_region)) %>% 
  dplyr::select(-ID,-Date,-X.Order)

str(data3)

#splitting data into trainiing and validatiion set
train_start_index <- 1
train_end_index <- nrow(data3)*80/100
valid_start_index <- train_end_index + 1
valid_end_index <- nrow(data3)

train_df <- data3[train_start_index:train_end_index,]
dim(train_df)
colnames(train_df)
valid_df  <- data3[valid_start_index:valid_end_index,]

#===============making linear model======================
linear_model <- lm(Sales ~ .,data = train_df)
summary(linear_model)
y_pred <- predict(linear_model,valid_df[-6])
linear_model_score <- get_msle_score(y_pred,valid_df$Sales)
linear_model_score


#=============making random forest model===========
rf_model <- randomForest(x = train_df[-6],y = train_df$Sales, 
                         ntree = 100, importance = TRUE)
imp_RF <- importance(rf_model) 
imp_DF <- imp_RF %>%  
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "Variable") %>% 
  dplyr::arrange(desc(.[[2]])) %>% 
  dplyr::select(1:2)
colnames(imp_DF) <- c("Variable","MSE")
View(imp_DF)
imp_graph <- ggplot(data = imp_DF,aes(x = reorder(Variable,MSE), y = MSE, fill = MSE))+
  geom_bar(stat = "identity") +
  coord_flip()+
  theme(legend.position = "none")
imp_graph

y_pred <- predict(rf_model,valid_df[-6])
rf_model_score <- get_msle_score(y_pred,valid_df$Sales)
rf_model_score


#=============xgboost =======================================
train_df_ohe <- one_hot(data.table(train_df))
train_df_ohe <- train_df_ohe %>% 
  dplyr::select(-Sales) %>% 
  data.matrix()
train_label <- train_df$Sales 
train_label

valid_df_ohe <- one_hot(data.table(valid_df))
valid_df_ohe <- valid_df_ohe %>% 
  dplyr::select(-Sales) %>% 
  data.matrix()

xgboost_train_matrix <- xgb.DMatrix(data = train_df_ohe,label = train_label)
xgboost_model <- xgboost(data = xgboost_train_matrix,
                         max.depth = 2, 
                         nrounds = 100)

y_pred <- predict(xgboost_model,valid_df_ohe)
xgboost_model_score <- get_msle_score(y_pred,valid_df$Sales)
xgboost_model_score

#===tunning xgboost using grid search =================s
nrounds <- 400
tune_grid <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 3, # with n folds 
  #index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)

xgb_tune <- caret::train(
  x = train_df_ohe,
  y = train_label,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)

# helper function for the plots
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
    theme_bw()
}

tuneplot(xgb_tune)


#============ converting the test data==============
test_data  <- read.csv("analytics_vidhya/hack2/data/TEST_FINAL.csv",stringsAsFactors = FALSE)
dim(test_data)
colnames(test_data)

#getting structure of data 
test_data_info <- SmartEDA::ExpData(data = test_data,type =2)
View(test_data_info)

# data preprocessing 
test_data2 <- test_data %>% 
  dplyr::mutate(Store_id = as.factor(Store_id)) %>% 
  dplyr::mutate(Store_Type = as.factor(Store_Type)) %>% 
  dplyr::mutate(Location_Type = as.factor(Location_Type)) %>% 
  dplyr::mutate(Region_Code = as.factor(Region_Code)) %>% 
  dplyr::mutate(Holiday = if_else(Holiday == 1,"Yes", "No")) %>% 
  dplyr::mutate(Holiday = as.factor(Holiday)) %>% 
  dplyr::mutate(Discount = as.factor(Discount)) %>% 
  dplyr::mutate(Date = as.Date(Date,format = "%Y-%m-%d"))
 str(data2)

#data feature engineering
test_data3 <- test_data2 %>% 
  dplyr::mutate(dow = weekdays(Date)) %>% 
  dplyr::mutate(dow = as.factor(dow)) %>% 
  dplyr::mutate(month = month(Date)) %>% 
  dplyr::mutate(month = as.factor(month)) %>% 
  dplyr::mutate(is_weekend = if_else(dow %in% c("Saturday","Sunday"),"Yes","No")) %>% 
  dplyr::mutate(is_weekend = as.factor(is_weekend)) %>% 
  dplyr::mutate(store_location_region = paste0(Store_Type,Location_Type,Region_Code)) %>% 
  dplyr::mutate(store_location_region = as.factor(store_location_region)) %>% 
  dplyr::select(-ID,-Date,-Store_id)

# matching level of train and test data
common_columns <- intersect(names(train_df), names(test_data3)) 
common_columns
for (temp_column in common_columns) {
  print(temp_column)
  if (class(train_df[[temp_column]]) == "factor") {
    levels(test_data3[[temp_column]]) = levels(train_df[[temp_column]])
  }
}


# predicting with linear model
lm_test_pred <- predict(linear_model,test_data3)
lm_test_pred <- exp(lm_test_pred)
submission_df <- test_data %>% 
  dplyr::mutate(Sales = lm_test_pred) %>% 
  dplyr::select(ID,Sales)
View(submission_df)
write.csv(submission_df,"analytics_vidhya/hack2/data/sub4_xgboost.csv",row.names = FALSE)


# predicting with random forest
rf_test_pred <- predict(rf_model,test_data3)
rf_test_pred <- exp(rf_test_pred) 

submission_df <- test_data %>% 
  dplyr::mutate(Sales = rf_test_pred) %>% 
  dplyr::select(ID,Sales)
View(submission_df)
write.csv(submission_df,"analytics_vidhya/hack2/data/sub4_xgboost.csv",row.names = FALSE)


# predicting with xgboost 
test_df_ohe <- one_hot(data.table(test_data3)) %>% 
  data.matrix()
xgboost_test_pred <- predict(xgboost_model,test_df_ohe)
xgboost_test_pred <- exp(xgboost_test_pred)

submission_df <- test_data %>% 
  dplyr::mutate(Sales = xgboost_test_pred) %>% 
  dplyr::select(ID,Sales)
View(submission_df)
write.csv(submission_df,"analytics_vidhya/hack2/data/sub5_xgboost.csv",row.names = FALSE)





