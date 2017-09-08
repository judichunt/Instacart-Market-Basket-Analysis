
###########################################################################################################
#
# Kaggle Instacart competition
# Fabien Vavrand, June 2017
# Simple xgboost starter, score 0.3791 on LB
# Products selection is based on product by product binary classification, with a global threshold (0.21)
#
###########################################################################################################

library(data.table)
library(dplyr)
library(tidyr)


# Load Data ---------------------------------------------------------------
path <- "~/Kaggle/Instacart"

aisles <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
orderp <- fread(file.path(path, "order_products__prior.csv"))
ordert <- fread(file.path(path, "order_products__train.csv"))
orders <- fread(file.path(path, "orders.csv"))
products <- fread(file.path(path, "products.csv"))


# Reshape data ------------------------------------------------------------
aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set <- as.factor(orders$eval_set)
products$product_name <- as.factor(products$product_name)

# products <- products %>% 
#   inner_join(aisles) %>% inner_join(departments) %>% 
#   select(-aisle_id, -department_id)
# rm(aisles, departments)

ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]

orders_products <- orders %>% inner_join(orderp, by = "order_id")

rm(orderp)
gc()


# Products ----------------------------------------------------------------
prd <- orders_products %>%
  arrange(user_id, order_number, product_id) %>%
  group_by(user_id, product_id) %>%
  mutate(product_time = row_number()) %>%
  ungroup() %>%
  group_by(product_id) %>%
  summarise(
    prod_orders = n(),
    prod_reorders = sum(reordered),
    prod_first_orders = sum(product_time == 1),
    prod_second_orders = sum(product_time == 2),
    #
    prod_fourth_orders = sum(product_time == 4),
    prod_max_orders = max(product_time)
    #
  )
#prd %>% inner_join(products) %>% select(-product_name)




prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_frequent <- prd$prod_fourth_orders / prd$prod_first_orders
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders

ais <- prd %>% inner_join(products) %>% select(-product_name) %>%
  group_by(aisle_id) %>% 
  summarise(
    ais_reorder_probability = mean(prod_reorder_probability),
    ais_reorder_frequent =mean(prod_reorder_frequent),
    ais_reorder_times=mean(prod_reorder_times),
    ais_reorder_ratio=mean(prod_reorder_ratio)
  )

dep <- prd %>% inner_join(products) %>% select(-product_name) %>%
  group_by(department_id) %>% 
  summarise(
    dep_reorder_probability = mean(prod_reorder_probability),
    dep_reorder_frequent =mean(prod_reorder_frequent),
    dep_reorder_times=mean(prod_reorder_times),
    dep_reorder_ratio=mean(prod_reorder_ratio)
  )
prd <- prd %>% inner_join(products) %>% select(-product_name) %>%inner_join(ais)%>%
  inner_join(dep) %>% select(-department_id, -aisle_id)




prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders,-prod_fourth_orders)

rm(products)
gc()

# Users -------------------------------------------------------------------
users <- orders %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),
    user_period = sum(days_since_prior_order, na.rm = T),
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T),
    user_mean_order_hour=mean(order_hour),
    user_mean_order_dow=mean(order_hour)
    
    #
    #user_days_max = max(days_since_prior_order),
    #user_days_min = min(days_since_prior_order)
    #
  )

us <- orders_products %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
    user_distinct_products = n_distinct(product_id),
    ##
    user_reorder_distinct_products=sum(reordered == 1)/user_distinct_products,
    user_reorder_distinct_products_ratio=user_reorder_ratio/user_distinct_products
    ##
  )

users <- users %>% inner_join(us)
users$user_average_basket <- users$user_total_products / users$user_orders

us <- orders %>%
  filter(eval_set != "prior") %>%
  select(user_id, order_id, eval_set,
         time_since_last_order = days_since_prior_order)

us$days30 <- (us$time_since_last_order==30)*1

users <- users %>% inner_join(us)
users$days_compare_avg=users$time_since_last_order-users$user_mean_days_since_prior

rm(us)
gc()


# Database ----------------------------------------------------------------
data <- orders_products %>%
  group_by(user_id, product_id) %>% 
  summarise(
    up_orders = n(),
    up_first_order = min(order_number),
    up_last_order = max(order_number),
    up_average_cart_position = mean(add_to_cart_order))

rm(orders_products, orders)

data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id")

data$up_order_rate <- data$up_orders / data$user_orders
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)

data$up_order_rate_this_time <- data$up_order_rate *data$up_orders_since_last_order
data$up_order_rate_this_time_by_first <- data$up_order_rate_since_first_order *data$up_orders_since_last_order
data$up_order_reorder <- data$up_orders*data$prod_reorder_probability
data$up_reorder <- data$user_reorder_ratio*data$prod_reorder_probability
data$up_order_avg_period <- data$user_mean_days_since_prior/data$up_order_rate
data$up_time_compare_avg <- data$time_since_last_order-data$up_order_avg_period

# data$ordr_ordsl_ratio <- data$up_order_rate/data$up_orders_since_last_order
# data$ordr_ordsl_ratio[is.nan(data$ordr_ordsl_ratio)] <- 100
# 
# data$ordr_ordsf_ratio <- data$up_order_rate/data$up_order_rate_since_first_order
# data$ordr_ordsf_ratio[is.nan(data$ordr_ordsf_ratio)] <- 100
# 
# data$ordr_ordreo_ratio <- data$up_order_rate/data$up_order_reorder
# data$ordr_ordreo_ratio [is.nan(data$ordr_ordreo_ratio)] <- 100
# 
# data$ordr_tslord_ratio <- data$up_order_rate/data$time_since_last_order
# data$ordr_tslord_ratio[is.nan(data$ordr_tslord_ratio)] <- 100
# 
# data$ordr_reo_ratio <- data$up_order_rate/data$up_reorder
# data$ordr_reo_ratio[is.nan(data$ordr_reo_ratio)] <- 100



# data$up_reorderXtime_since_last_order <- data$up_reorder*data$time_since_last_order
# data$up_reorderXup_order_rate <-data$up_reorder*data$up_order_rate
# data$up_reorderXup_orders_since_last_order <- data$up_reorder*data$up_order_rate_since_first_order





data <- data %>% 
  left_join(ordert %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))

#write.csv(ordert, file = "~/Kaggle/Instacart/ordert.csv", row.names = F)
#write.csv(prd, file = "~/Kaggle/Instacart/prd.csv", row.names = F)
#write.csv(users, file = "~/Kaggle/Instacart/users.csv", row.names = F)


rm(ordert, prd, users)
gc()


# Train / Test datasets ---------------------------------------------------
train <- as.data.frame(data[data$eval_set == "train",])
trainn<- train
train$eval_set <- NULL
train$user_id <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$reordered[is.na(train$reordered)] <- 0

test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
test$user_id <- NULL
test$reordered <- NULL

rm(data)
gc()


# Model -------------------------------------------------------------------
library(xgboost)

params <- list(
  "objective"           = "reg:logistic",
  "eval_metric"         = "logloss",
  "eta"                 = 0.1,
  "max_depth"           = 6,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 0.77,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)

subtrain <- train %>% sample_frac(0.5)
X <- xgb.DMatrix(as.matrix(subtrain %>% select(-reordered)), label = subtrain$reordered)
model <- xgboost(data = X, params = params, nrounds = 90)

importance <- xgb.importance(colnames(X), model = model)
xgb.ggplot.importance(importance)

rm(X, importance, subtrain)
gc()
#write.csv(train, file = "~/Kaggle/Instacart/trainning.csv", row.names = F)

#----------------------------------------------
'''
X <- xgb.DMatrix(as.matrix(train %>% select(-reordered)))
train$reordered <- predict(model, X)

train$reordered <- (test$reordered > 0.21) * 1
'''

# train_submit <- trainn %>%
#   filter(reordered == 1) %>%
#   group_by(order_id) %>%
#   summarise(
#     products = paste(product_id, collapse = " ")
#   )
# 
# missing <- data.frame(
#   order_id = unique(trainn$order_id[!trainn$order_id %in% train_submit$order_id]),
#   products = "None"
# )
# 
# train_submit <- train_submit %>% bind_rows(missing) %>% arrange(order_id)
# write.csv(train_submit, file = "~/Kaggle/Instacart/train_submit_n.csv", row.names = F)



#----------------------------------------------
X <- xgb.DMatrix(as.matrix(trainn %>% select(-reordered,-eval_set,-order_id, -product_id, -user_id )))
trainn$reordered_1 <- predict(model, X)


for (i in 19:22){
  
  treshold <- i/100
  trainn$reordered <- (trainn$reordered_1 > treshold ) * 1
  
  
  train_submit <- trainn %>%
    filter(reordered == 1) %>%
    group_by(order_id) %>%
    summarise(
      products = paste(product_id, collapse = " ")
    )
  
  missing <- data.frame(
    order_id = unique(trainn$order_id[!trainn$order_id %in% train_submit$order_id]),
    products = "None"
  )
  
  train_submit <- train_submit %>% bind_rows(missing) %>% arrange(order_id)
  fname=paste("~/Kaggle/Instacart/train_",treshold ,".csv",sep='')
  write.csv(train_submit, file = fname, row.names = F)
}
#Get optimal threshold-------------------

trainning <- fread(file.path(path, "train_submit_n"))
train_F <- trainning
train_F$products <- NULL
merge_train_F <- NULL

for(i in 19:22){
  
  fname=paste("train_",i/100,".csv",sep='')
  Threshold <- fread(file.path(path, fname))
  
  
  a <- strsplit(trainning$products,' ')
  b <- strsplit(Threshold$products,' ')
  c <- t(rbind(a,b))
  
  train_F$inter <- apply(c, 1, function(x) length(intersect(unlist(x[1]),unlist(x[2]))))
  train_F$pre <- train_F$inter/apply(c, 1, function(x) length(unlist(x[1])))
  train_F$recall <- train_F$inter/apply(c, 1, function(x) length(unlist(x[2])))
  train_F$fscore <- 2*(train_F$pre*train_F$recall)/(train_F$pre+train_F$recall)
  train_F$fscore[is.nan(train_F$fscore)] <- 0
  
  fin_train_F <- train_F %>% select(order_id, fscore)
  fin_train_F$threshold <- i/100
  
  merge_train_F <- rbind(merge_train_F, fin_train_F)
}


m_train_F<-merge_train_F %>% group_by(order_id) %>% 
  summarise(threshold=threshold[which.max(fscore)],fscore = max(fscore))



m_train_F2 <- m_train_F %>% inner_join(trainning) 
m_train_F2$threshold[m_train_F2$products=='None'] <- 0.23

write.csv(m_train_F2%>% select(-fscore, -products), file.path(path,file= "m_train_F.csv"), row.names = F)

#mean(merge_train_F$fscore[merge_train_F$threshold==0.2])

#trainning <- fread(file.path(path, "trainning_sbumit.csv"))

#write.csv(train_F file = "~/Kaggle/Instacart/train_F", row.names = F)


#Train and apply threshold-------------------
Threshold <- fread(file.path(path, "m_train_F.csv"))
#Threshold$V1 <- NULL
trainn <- trainn %>% inner_join(Threshold)
trainn$eval_set <- NULL
trainn$user_id <- NULL
trainn$product_id <- NULL
trainn$order_id <- NULL
trainn$reordered<-NULL

params <- list(
  "objective"           = "multi:softmax",
  "num_class"           = 3,
  "eval_metric"         = "mlogloss",
  "eta"                 = 0.1,
  "max_depth"           = 6,
  "min_child_weight"    = 50,
  "gamma"               = 0.70,
  #"subsample"           = 0.77,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)
#user_average_basket, user_reorder_ratio,time_since_last_order,user_reorder_distinct_products_ratio,days_compare_avg,user_total_products, days30, 
#-threshold

subtrain <- trainn %>% sample_frac(0.5)
#s_thre<-apply(subtrain%>% select(threshold, up_first_order),1,function(x) toString(x[1]))
int_thre <- apply(subtrain%>% select(threshold, up_first_order),1,function(x) (x[1]>0.19)*1+(x[1]>0.22)*1)


X <- xgb.DMatrix(as.matrix(subtrain %>% select(user_average_basket, user_reorder_ratio,time_since_last_order,user_reorder_distinct_products_ratio,
                                               days_compare_avg,user_total_products, days30, user_reorder_distinct_products, user_orders, user_period)), label = int_thre)
model_threshold <- xgboost(data = X, params = params, nrounds = 90)

importance <- xgb.importance(colnames(X), model = model_threshold)
xgb.ggplot.importance(importance)




# Apply model -------------------------------------------------------------
X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
test$reordered <- predict(model, X)
test$threshold <- predict(model_threshold, X)*0.03+0.17
#test$threshold[test$threshold==0.21]=0.5

test$reordered <- (test$reordered > test$threshold ) * 1

submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = "~/Kaggle/Instacart/submit_0.19-0.20-none", row.names = F)

