library(h2o)
library(tidyverse)
h2o.init()

setwd("C:/Users/Gedvile/Desktop/Project/KTU-DVDA-PROJECT/project/")
path = getwd()


#-------------------Duomenų apjungimas--------------------

#Nuskaitome 1-sample_data duoemnis
df <- read_csv(paste0(path,"/1-data/1-sample_data.csv"))
class(df)

df_list <- list.files(path="C:/Users/Gedvile/Desktop/Project/KTU-DVDA-PROJECT/project/1-data", full.names = TRUE) %>%
  lapply(read_csv)

#Sujungiame 1-sample_data, additional data ir additional_features data
df <- df_list[[1]] %>% 
  bind_rows(df_list[[2]]) %>% 
  left_join(df_list[[3]], by = c("id" = "id"))

#Irasome duomenis i faila "pilnas.csv"
write.csv(df,file=paste0(path,"/1-data/train_data.csv"), row.names = FALSE)


df <- h2o.importFile("../../../project/1-data/train_data.csv")
test_data <- h2o.importFile("../../../project/1-data/test_data.csv")
df
class(df)
summary(df)

y <- "y"
x <- setdiff(names(df), c(y, "id"))
df$y <- as.factor(df$y)
summary(df)


splits <- h2o.splitFrame(df, c(0.6,0.2), seed=123)
train  <- h2o.assign(splits[[1]], "train") # 60%
valid  <- h2o.assign(splits[[2]], "valid") # 20%
test   <- h2o.assign(splits[[3]], "test")  # 20%

aml <- h2o.automl(x = x,
                  y = y,
                  training_frame = train,
                  validation_frame = valid,
                  max_runtime_secs = 60)

aml@leaderboard

model <- aml@leader


model <- h2o.getModel("GBM_1_AutoML_1_20221111_181631")

perf <- h2o.performance(model, train = TRUE)
perf
perf_valid <- h2o.performance(model, valid = TRUE)
perf_valid
perf_test <- h2o.performance(model, newdata = test)
perf_test

h2o.auc(perf)
plot(perf_valid, type = "roc")


h2o.performance(model, newdata = test_data)

predictions <- h2o.predict(model, test_data)

predictions %>%
  as_tibble() %>%
  mutate(id = row_number(), y = p0) %>%
  select(id, y) %>%
  write_csv("../5-predictions/predictions1.csv")

### ID, Y

h2o.saveModel(model, "../4-model/", filename = "my_model")

model <- h2o.loadModel("../4-model/my_model")
h2o.varimp_plot(model)


### deeplearning

dl_model <- h2o.deeplearning(x,
                             y,
                             training_frame = train,
                             validation_frame = valid,
                             activation = "Tanh",
                             hidden = c(50, 50))

perf_test_dl <- h2o.performance(dl_model, newdata = test)
perf_test_dl


# gridsearch

dl_params1 <- list(hidden = list(10, c(10,10), c(10,10,10)))

dl_grid <- h2o.grid("deeplearning",
                    x = x,
                    y = y,
                    training_frame = train,
                    epochs = 3,
                    hyper_params = dl_params1)

h2o.getGrid(grid_id = dl_grid@grid_id,
            sort_by = "auc",
            decreasing = TRUE)


grid_dl <- h2o.getModel(dl_grid@model_ids[[1]])

### 20221125

### GBM

gbm_params1 <- list(max_depth = c(10, 15),
                    sample_rate = c(0.8, 1.0))

gbm_grid1 <- h2o.grid("gbm", 
                      x = x, 
                      y = y,
                      grid_id = "gbm_grid1",
                      training_frame = train,
                      validation_frame = valid,
                      ntrees = 20,
                      seed = 1234,
                      hyper_params = gbm_params1)

# Get the grid results, sorted by validation AUC

gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
                             sort_by = "auc",
                             decreasing = TRUE)
print(gbm_gridperf1)

model_gbm <- h2o.getModel("gbm_grid1_model_2")

model_gbm

h2o.performance(model_gbm, newdata = valid)

h2o.performance(model_gbm, newdata = test)

h2o.saveModel(model_gbm, "../4-model", filename = "gmb_grid")

### Random Forest

rf_model <- h2o.randomForest(x = x,
                             y = y,
                             training_frame = train,
                             validation_frame = valid)
rf_model

h2o.performance(rf_model, valid)

perf_test_rf <- h2o.performance(rf_model, newdata = test)
perf_test_rf

predictions_rf <- h2o.predict(rf_model, test_data) %>%
  as_tibble() # id, y


### GOOGLE COLAB https://colab.research.google.com/drive/1k2ZVMxNED4cY1zW5NLOjmyI7dr0ZkQtD#scrollTo=mzfOwj0vwmW4

model_python <- h2o.import_mojo("../4-model/GBM_1_AutoML_1_20221124_232404.zip")
model_python
h2o.performance(model_python, newdata = test)
predictions <- h2o.predict(model_python, test_data)

# h2o.shutdown()