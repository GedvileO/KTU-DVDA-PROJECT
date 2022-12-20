library(h2o)
library(dplyr)
library(readr)

h2o.init(max_mem_size = "8G")

setwd("C:/Users/Gedvile/Desktop/Project/KTU-DVDA-PROJECT/project/")
path = getwd()

#Duomenų apjungimas

#Nuskaitome 1-sample_data duoemnis
df <- read_csv(paste0(path,"/1-data/1-sample_data.csv"))
class(df)

df_list <- list.files(path="C:/Users/Gedvile/Desktop/Project/KTU-DVDA-PROJECT/project/1-data", full.names = TRUE) %>%
  lapply(read_csv)

#Sujungiame 1-sample_data, additional data ir additional_features data
df <- df_list[[1]] %>% 
  bind_rows(df_list[[2]]) %>% 
  left_join(df_list[[3]], by = c("id" = "id"))

#Irasome duomenis i faila "train_data.csv"
write.csv(df,file=paste0(path,"/1-data/train_data.csv"), row.names = FALSE)


#Nuskaitome apjunta faila

df <- h2o.importFile(paste0(path, "/1-data/train_data.csv"))
df
summary(df)

#train, test ir validation imtys
splits <- h2o.splitFrame(df, c(0.2, 0.5), seed = 1234)
train <- h2o.assign(splits[[1]], "train")
valid <- h2o.assign(splits[[2]], "valid")
test <- h2o.assign(splits[[2]], "test")


y <- "y"
x <- setdiff(colnames(train), c(y, "id"))
x
train$y <- as.factor(train$y)

#Bandom su geriausiu depth reziu
hyper_params = list(
  max_depth = 29,
  sample_rate = seq(0.2,1,0.01),
  col_sample_rate = seq(0.2,1,0.01),
  col_sample_rate_per_tree = seq(0.2,1,0.01),
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
  min_rows = 2^seq(0,log2(nrow(train))-1,1),
  nbins = 2^seq(4,10,1),
  nbins_cats = 2^seq(4,12,1),
  min_split_improvement = c(0,1e-8,1e-6,1e-4),
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin"),
  categorical_encoding = "OneHotExplicit"
)
search_criteria = list(
  strategy = "RandomDiscrete",
  max_runtime_secs = 1800,
  max_models = 100,
  seed = 1234,
  stopping_rounds = 5,
  stopping_metric = "AUC",
  stopping_tolerance = 1e-3
)

grid <- h2o.grid(
  hyper_params = hyper_params,
  search_criteria = search_criteria,
  algorithm = "gbm",
  grid_id = "final_grid",
  x = x,
  y = y,
  training_frame = train,
  validation_frame = test,
  ntrees = 10000,
  learn_rate = 0.005,
  learn_rate_annealing = 0.99,
  max_runtime_secs = 1800,
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "AUC",
  score_tree_interval = 10,
  seed = 1234
)

#Išrikiuojame modelius pagal AUC
sortedGrid <- h2o.getGrid("final_grid", sort_by = "auc", decreasing = TRUE)
sortedGrid

#Geriausias modelis
gbm <- h2o.getModel(sortedGrid@model_ids[[1]])

print(h2o.auc(h2o.performance(gbm, newdata = test)))

h2o.saveModel(gbm, paste0(getwd(), "/4-model"))

preds <- h2o.predict(gbm, test)
head(preds)
gbm@model$validation_metrics@metrics$max_criteria_and_metric_scores




#Susidarome failą kurį reikės įkelti
df_test <- h2o.importFile("C:/Users/Gedvile/Desktop/Project/KTU-DVDA-PROJECT/project/1-data/test_data.csv")
p <- h2o.predict(gbm,df_test)
p$p1 <- h2o.round(p$p1, digits = 3)

library(tidyverse)

p_R <- p %>%
  as_tibble() %>%
  mutate (y = p1) %>%
  select(y) %>%
  rownames_to_column("id")

write_csv(p_R, paste0(path,"/5-predictions/predictions3.csv"))

h2o.shutdown()

